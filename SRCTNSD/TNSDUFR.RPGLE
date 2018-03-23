000100020325     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020325      *------------------------------------------------------------------------*
000300020325      * STAMPA DOTAZIONE UNITA' EDP PER PUNTO OPERATIVO - ESEGUE               *
000400020325      *------------------------------------------------------------------------*
000500020325
000600020325      *------------------------------------------------------------------------*
000700020325      * DATA BASE                                                              *
000800020325      *------------------------------------------------------------------------*
000900000000     FUNDOT01L  IF   E           K DISK
001000000000     FUNANA03L  IF   E           K DISK
001100020528     FAZORG21l  IF   E           K DISK
001200020528     fazorg01l  if   e           k disk
001300020325     ftntbe01l  if   e           k disk
001400000000     FTNSDUFP   O    E             PRINTER OFLIND(*IN01)
001500020325
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE                                                                *
001800000000     D*------------------------------------------------------------------------*
001900950227     D*------------------
002000950227     D* RIGHE IN STAMPA
002100950227     D*------------------
002200950227     D RIG             S             25    DIM(5) CTDATA PERRCD(1)
002300000000     D*------------------
002400000000     D* FILIALI
002500000000     D*------------------
002600971124     D FIL             S              3  0 DIM(500)
002700971124     D DFIL            S             25    DIM(500)
002800000000     D*------------------
002900000000     D* DECODIFICHE TIPI UNITA'
003000000000     D*------------------
003100000000     D TIP             S              2  0 DIM(100)
003200000000     D DTIP            S             25    DIM(100)
003300000000     D*------------------
003400000000     D* TABELLE CAPICONTO
003500000000     D*------------------
003600000000     D*------------------
003700000000     D* ARCHITETTURA
003800000000     D*------------------
003900000000     D KPJBA         E DS
004000000000     D  PROFIL                 1     10                                         UTENTE
004100000000     D*------------------
004200000000     D* PARAMETRI FILTRO
004300000000     D*------------------
004400960201     D PARAM           DS
004500000000     D  PARTIP                 1      2  0                                      TIPO
004600000000     D  PARMAG                 3      5  0                                      FILAIL
004700000000     D  PARLIF                 6      6                                         LIVELL
004800951103     D  PARDIF                 7      7                                         DIFF
004900971128     D*---------------------
005000971128     D* DS REPEIMENTO TERMINALs
005100971128     D*---------------------
005200971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
005300971128     D*---------------------
005400971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500971128     D*---------------------
005600971128     D WLBDA8          DS                  INZ
005700971128     D  G08DAT                 1      8  0
005800971128     D  G08INV                 9     16  0
005900971128     D  G08ERR                17     17
006000971128     D  G08TGI                18     22  0
006100020325
006200020709     d ddatiute      e ds
006300020709     d azuteds       e ds                  extname(azute00f)
006400020709     d tibs34ds      e ds                  inz
006500000000     C*------------------------------------------------------------------------*
006600000000     C* MAIN LINE
006700000000     C*------------------------------------------------------------------------*
006800000000     C*
006900000000     C* STAMPA PARAMETRI FILTRO
007000000000     C                   EXSR      STAPAR
007100000000     C*
007200000000     C* SCELTA ELABORAZIONE
007300000000     C                   EXSR      UNDOT
007400000000     C*
007500000000     C* STAMPA RIGA DI FINE PROSPETTO
007600000000     C                   WRITE     SDUFFI
007700971128     C*
007800971128     C* OPERAZIONI FINALI
007900971128     C                   EXSR      FINSR
008000000000     C*
008100000000     C                   SETON                                        LR
008200971128     C*------------------------------------------------------------------------*
008300971128     C* FINSR - OPERAZIONI FINALI
008400971128     C*------------------------------------------------------------------------*
008500971128     C     FINSR         BEGSR
008600971128     C*
008700971128     C                   CLEAR                   DSLV55
008800971128     C                   MOVEL     'C'           D55TLA
008900971128     C                   CALL      'FNLV55R'
009000971128     C                   PARM                    DSLV55
009100971128     C*
009200971128     C                   ENDSR
009300000000     C*------------------------------------------------------------------------*
009400000000     C* UNDOT - STAMPA DOTAZIONI
009500000000     C*------------------------------------------------------------------------*
009600000000     C     UNDOT         BEGSR
009700000000     C*
009800000000     C* POSIZIONAMENTO E PRIMA LETTURA DOTAZIONI (TIP + FIL)
009900000000     C                   EXSR      SETFIL
010000000000     C*
010100000000     C* FINCHE' FINE FILE
010200000000DO  1C     $FINE         DOWEQ     '0'
010300000000     C*
010400000000     C* OPERAZIONI PER NUOVO TIPO UNITA'
010500000000     C                   EXSR      NEWTIP
010600000000     C*
010700000000     C* FINCHE' FINE TIPO UNITA'
010800000000DO  2C     $FINE         DOWEQ     '0'
010900000000     C     DEPTIP        ANDEQ     DOTTIP
011000000000     C*
011100000000     C* OPERAZIONI PER NUOVO MAGAZZINO
011200000000     C                   EXSR      NEWFIL
011300000000     C*
011400000000     C* POSIZIONAMENTO E PRIMA LETTURA UNITA (FIL + TIP)
011500000000     C                   EXSR      SETFI2
011600000000     C*
011700000000     C* FINCHE' FINE FILE
011800000000DO  3C     $FIN2         DOWEQ     '0'
011900000000     C*
012000000000     C* CONTEGGIO TIPO PER FILIALE
012100000000     C                   ADD       1             TOTFIL
012200950227     C                   ADD       1             TOTTQE                         *QTA IN ESSERE X TIP
012300950227     C                   ADD       1             TOTGQE                         *QTA IN ESSERE TOTAL
012400000000     C*
012500000000     C* LETTURA SUCESSIVA
012600000000     C                   EXSR      LETFI2
012700000000E   3C                   ENDDO
012800000000     C*
012900000000     C* TOTALE PER FILIALE
013000000000     C                   EXSR      STARIG
013100000000     C*
013200000000     C* LETTURA SUCESSIVA
013300000000     C                   EXSR      LETFIL
013400000000     C*
013500000000E   2C                   ENDDO
013600951103     C*
013700950227     C* TOTALE PER TIPO (SOLO SE NON E' STATA CHIESTA SOLO UNA FILIALE)
013800951103IF  2C     $STT          IFEQ      '1'                                          *STAMPATO QC.
013900951103IF  3C     PARMAG        IFEQ      *ZEROS
014000951103IF  4C     TOTTQD        IFGT      *ZEROS
014100950227     C     TOTTQE        ORGT      *ZEROS
014200950227     C                   CLEAR                   SDUFPR
014300950227     C                   Z-ADD     DEPTIP        PRCTIP                         *DECODIFICA TIPO
014400950227     C                   MOVEL     DESTIP        PRDTIP
014500950227     C                   MOVEL     RIG(1)        PRDMAG                         *DECODIFICA TOTALE
014600950227     C                   Z-ADD     TOTTQD        PRCQTA                         *QTA IN DOTAZIONE
014700950227     C                   Z-ADD     TOTTQE        PRCQES                         *QTA IN ESSERE
014800950227     C                   WRITE     SDUFPR
014900951103E   4C                   ENDIF
015000951103E   3C                   ENDIF
015100951103E   2C                   ENDIF
015200950227     C*
015300000000E   1C                   ENDDO
015400950227     C*
015500950227     C* TOTALE GENERALE (SOLO SE NON E' STATA CHIESTA SOLO UNA FILIALE)
015600951103IF  1C     $STG          IFEQ      '1'                                          *STAMPATO QC.
015700951103IF  2C     PARMAG        IFEQ      *ZEROS
015800951103IF  3C     TOTGQD        IFGT      *ZEROS
015900950227     C     TOTGQE        ORGT      *ZEROS
016000950227     C                   CLEAR                   SDUFPR
016100950227     C                   MOVEL     RIG(2)        PRDMAG                         *DECODIFICA TOTALE
016200950227     C                   Z-ADD     TOTGQD        PRCQTA                         *QTA IN DOTAZIONE
016300950227     C                   Z-ADD     TOTGQE        PRCQES                         *QTA IN ESSERE
016400950227     C                   WRITE     SDUFPR
016500951103E   3C                   ENDIF
016600951103E   2C                   ENDIF
016700951103E   1C                   ENDIF
016800000000     C*
016900000000     C                   ENDSR
017000000000     C*------------------------------------------------------------------------*
017100000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
017200000000     C*------------------------------------------------------------------------*
017300000000     C     SETFIL        BEGSR
017400000000     C*
017500000000     C                   Z-ADD     PARTIP        KDOTIP
017600000000     C                   Z-ADD     PARMAG        KDOFIL
017700000000     C     KEYDOT        SETLL     UNDOT01L                           99        *FILE FILE
017800000000     C*
017900000000     C     *IN99         IFEQ      *ON
018000000000     C                   MOVEL     '1'           $FINE
018100000000     C                   ELSE
018200000000     C                   EXSR      LETFIL                                       *PRIMA LETTURA
018300000000     C                   END
018400000000     C*
018500000000     C                   ENDSR
018600000000     C*------------------------------------------------------------------------*
018700000000     C* LETFIL - LETTURA PROSSIMO RECORD
018800000000     C*------------------------------------------------------------------------*
018900000000     C     LETFIL        BEGSR
019000000000     C*
019100000000     C* LEGGO FINO A:
019200000000     C*     - FINE FILE
019300000000     C*     - TROVATO RECORD VALIDO
019400000000     C                   MOVEL     'N'           WRECOK
019500000000     C*
019600000000DO  1C     $FINE         DOWEQ     '0'
019700000000     C     WRECOK        ANDEQ     'N'
019800000000     C                   READ      UNDOT01L                               99
019900000000     C*
020000000000IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
020100000000     C                   MOVEL     '1'           $FINE
020200000000X   2C                   ELSE
020300000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
020400000000E   2C                   ENDIF
020500000000     C*
020600000000E   1C                   ENDDO
020700000000     C*
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
021100000000     C*------------------------------------------------------------------------*
021200000000     C     CHKREC        BEGSR
021300000000     C*
021400000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
021500950407     C*
021600950407     C* DOTAZIONE ANNULLATA, ESCLUDO
021700950407IF  1C     DOTATB        IFNE      *BLANKS
021800950407     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021900950407E   1C                   ENDIF
022000000000     C*
022100000000     C* TIPO UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCO DAL CICLO
022200000000IF  1C     PARTIP        IFGT      *ZEROS
022300000000IF  2C     PARTIP        IFNE      DOTTIP
022400000000     C                   MOVEL     '1'           $FINE
022500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022600000000E   2C                   ENDIF
022700000000E   1C                   ENDIF
022800000000     C*
022900000000     C* MAGAZZINO DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
023000000000IF  1C     PARMAG        IFGT      *ZEROS
023100000000IF  2C     PARMAG        IFNE      DOTFIL
023200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023300000000E   2C                   ENDIF
023400000000E   1C                   ENDIF
023500000000     C*
023600000000     C* TIPO FILIALE DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
023700000000     C                   Z-ADD     1             I
023800000000     C                   SETOFF                                           28
023900000000     C     DOTFIL        LOOKUP    FIL(I)                                 28
024000000000IF  1C     *IN28         IFEQ      *OFF
024100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024200000000E   1C                   ENDIF
024300941222     C*
024400941222     C* RECORD ANNULLATO, ESCLUDO
024500941222IF  1C     DOTATB        IFNE      *BLANKS
024600941222     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024700941222E   1C                   ENDIF
024800000000     C*
024900000000     C                   ENDSR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* SETFI2 - POSIZIONAMENTO E PRIMA LETTURA
025200000000     C*------------------------------------------------------------------------*
025300000000     C     SETFI2        BEGSR
025400000000     C*
025500000000     C                   Z-ADD     DEPFIL        KANMAG
025600000000     C                   Z-ADD     DEPTIP        KANTIP
025700000000     C     KEYANA        SETLL     UNANA03L                           98        *FILE FILE
025800000000     C*
025900000000     C     *IN98         IFEQ      *ON
026000000000     C                   MOVEL     '1'           $FIN2
026100000000     C                   ELSE
026200000000     C                   EXSR      LETFI2                                       *PRIMA LETTURA
026300000000     C                   END
026400000000     C*
026500000000     C                   ENDSR
026600000000     C*------------------------------------------------------------------------*
026700000000     C* LETFI2 - LETTURA PROSSIMO RECORD
026800000000     C*------------------------------------------------------------------------*
026900000000     C     LETFI2        BEGSR
027000000000     C*
027100000000     C* LEGGO FINO A:
027200000000     C*     - FINE FILE
027300000000     C*     - TROVATO RECORD VALIDO
027400000000     C                   MOVEL     'N'           WRECOK
027500000000     C*
027600000000DO  1C     $FIN2         DOWEQ     '0'
027700000000     C     WRECOK        ANDEQ     'N'
027800000000     C     KEYANA        READE     UNANA03L                               98
027900000000     C*
028000000000IF  2C     *IN98         IFEQ      *ON                                          *FINE FILE
028100000000     C                   MOVEL     '1'           $FIN2
028200000000X   2C                   ELSE
028300000000     C                   EXSR      CHKRE2                                       *CONTROLLO RECORD
028400000000E   2C                   ENDIF
028500000000     C*
028600000000E   1C                   ENDDO
028700000000     C*
028800000000     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C* CHKRE2 - CONTROLLA VALIDITA' RECORD
029100000000     C*------------------------------------------------------------------------*
029200000000     C     CHKRE2        BEGSR
029300000000     C*
029400000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
029500000000     C*
029600950407     C* UNITA' ANNULLATA, ESCLUDO
029700000000IF  1C     UNAATB        IFNE      *BLANKS
029800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029900000000E   1C                   ENDIF
030000000000     C*
030100000000     C                   ENDSR
030200000000     C*------------------------------------------------------------------------*
030300000000     C* NEWTIP - OPERAZIONI PER NUOVO TIPO UNITA'
030400000000     C*------------------------------------------------------------------------*
030500000000     C     NEWTIP        BEGSR
030600000000     C*
030700000000     C* MEMORIZZA IL NUOVO TIPO UNITA' IN UN DEPOSITO
030800000000     C                   Z-ADD     DOTTIP        DEPTIP
030900000000     C*
031000000000     C* DECODIFICA IL TIPO UNITA'
031100000000     C                   Z-ADD     1             I
031200000000     C                   SETOFF                                           28
031300000000     C     DEPTIP        LOOKUP    TIP(I)                                 28
031400000000IF  1C     *IN28         IFEQ      *ON
031500000000     C                   MOVEL     DTIP(I)       DESTIP           25
031600000000X   1C                   ELSE
031700000000     C                   MOVEL     *ALL'*'       DESTIP
031800000000E   1C                   ENDIF
031900000000     C*
032000000000     C* SALTO PAGINA SOLO SE NON HO SELEZIONATO UN PARTICOLARE MAGAZZINO
032100000000IF  1C     PARMAG        IFEQ      *ZEROS
032200000000     C                   SETON                                        01
032300000000E   1C                   ENDIF
032400950227     C*
032500950227     C* AZZERO TOALE TIPO UNITA'
032600950227     C                   Z-ADD     *ZEROS        TOTTQD                         *QTA IN DOTAZIONE
032700950227     C                   Z-ADD     *ZEROS        TOTTQE                         *QTA IN ESSERE
032800951103     C*
032900951103     C* REIMPOSTA FLAG STAMPATO QC
033000951103     C                   MOVEL     '0'           $STT
033100000000     C*
033200000000     C                   ENDSR
033300000000     C*------------------------------------------------------------------------*
033400000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
033500000000     C*------------------------------------------------------------------------*
033600000000     C     NEWFIL        BEGSR
033700000000     C*
033800000000     C* MEMORIZZA IL NUOVO MAGAZZINO IN UN DEPOSITO
033900000000     C                   Z-ADD     DOTFIL        DEPFIL
034000000000     C*
034100000000     C* DECODIFICA IL MAGAZZINO
034200000000     C                   Z-ADD     1             I
034300000000     C                   SETOFF                                           28
034400000000     C     DEPFIL        LOOKUP    FIL(I)                                 28
034500000000IF  1C     *IN28         IFEQ      *ON
034600000000     C                   MOVEL     DFIL(I)       DESFIL           25
034700000000X   1C                   ELSE
034800000000     C                   MOVEL     *ALL'*'       DESFIL
034900000000E   1C                   ENDIF
035000000000     C*
035100000000     C* AZZERA IL TOTALE MAGAZZINO
035200000000     C                   Z-ADD     *ZEROS        TOTFIL            7 0
035300000000     C*
035400000000     C* REIMPOSTA IL FNE FILE PER LETTURA ANAGRAFICA UNITA'
035500000000     C                   MOVEL     '0'           $FIN2
035600000000     C                   SETOFF                                           98
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000000000     C* STARIG - STAMPA RIGA
036100000000     C*------------------------------------------------------------------------*
036200000000     C     STARIG        BEGSR
036300950227     C*
036400950227     C* TOTALI PER TIPO E GENERALE
036500950227     C                   ADD       DOTQTA        TOTTQD                         *QTA IN DOTAZ. X TIP
036600950227     C                   ADD       DOTQTA        TOTGQD                         *QTA IN DOTAZ. GENER
036700950227     C*
036800000000     C                   Z-ADD     DEPFIL        PRCMAG
036900000000     C                   MOVEL     DESFIL        PRDMAG
037000000000     C                   Z-ADD     DOTQTA        PRCQTA
037100000000     C                   Z-ADD     TOTFIL        PRCQES
037200000000     C                   Z-ADD     *ZEROS        PRCDIF
037300000000     C     TOTFIL        SUB       DOTQTA        PRCDIF
037400951103     C*
037500951103     C* SE SELEZIONE PER DIFFERENZE, STAMPA SOLO SE LA DIFFERENAZA E + O -
037600951103IF  1C     PARDIF        IFEQ      'S'
037700951103     C     PRCDIF        ANDNE     *ZEROS
037800951103     C     PARDIF        OREQ      'N'
037900951103     C***
038000951103     C* TESTATA
038100951103     C***
038200951103IF  2C     *IN01         IFEQ      *ON
038300951103     C                   WRITE     SDUFPT                                       *TITOLO
038400951103     C                   WRITE     SDUFRI                                       *RIGA
038500951103     C*
038600951103     C* STAMPA IL TIPO UNITA'IN TESTATA SOLO SE NON HO SCELTO UNA PARTIC FILIALE
038700951103IF  3C     PARMAG        IFEQ      *ZEROS
038800951103     C                   Z-ADD     DEPTIP        PICTIP
038900951103     C                   MOVEL     DESTIP        PIDTIP
039000951103     C                   ELSE
039100951103     C                   Z-ADD     *ZEROS        PICTIP
039200951103     C                   MOVEL     *BLANKS       PIDTIP
039300951103E   3C                   ENDIF
039400951103     C*
039500951103     C                   WRITE     SDUFPI                                       *DESCRIZIONE CAMPI
039600951103     C                   WRITE     SDUFR2                                       *RIGA
039700951103     C                   SETOFF                                       01
039800951103E   2C                   ENDIF
039900951103     C***
040000951103     C* RIGA
040100951103     C***
040200951103     C* STAMPA IL TIPO UNITA'IN DETTAGLIO SOLO SE HO SCELTO UNA PARTIC FILIALE
040300951103IF  2C     PARMAG        IFEQ      *ZEROS
040400951103     C                   Z-ADD     *ZEROS        PRCTIP
040500951103     C                   MOVEL     *BLANKS       PRDTIP
040600951103     C                   ELSE
040700951103     C                   Z-ADD     DEPTIP        PRCTIP
040800951103     C                   MOVEL     DESTIP        PRDTIP
040900951103E   2C                   ENDIF
041000951103     C* STAMPA RIGA
041100000000     C                   WRITE     SDUFPR
041200951103     C                   MOVEL     '1'           $STT
041300951103     C                   MOVEL     '1'           $STG
041400951103E   1C                   ENDIF
041500000000     C*
041600000000     C                   ENDSR
041700000000     C*------------------------------------------------------------------------*
041800000000     C* CARORG - CARICA ORGANIGRAMMA
041900000000     C*------------------------------------------------------------------------*
042000000000     C     CARORG        BEGSR
042100000000     C*--------------------
042200000000     C* FILIALI
042300000000     C*--------------------
042400000000     C                   Z-ADD     *ZEROS        I                 3 0
042500000000     C                   Z-ADD     *ZEROS        KORFIL
042600000000     C*
042700020528     C     KEYORG        SETLL     AZORG21l
042800020528     C                   READ      AZORG21l                               99
042900000000     C*
043000000000     C* CICLO FINO A FINE FILE
043100000000DO  1C     *IN99         DOWEQ     *OFF
043200000000     C*
043300000000     C* ELIMINA RECORD ANNULLATI
043400000000IF  2C     ORGFVA        IFEQ      *BLANKS
043500000000     C                   MOVEL     'N'           WFILOK
043600000000     C*
043700000000     C* LIVELLO FILIALE <> LIVELLO RICHIESTO, ESCLUDO
043800000000IF  3C     PARLIF        IFEQ      'T'
043900000000     C                   MOVEL     'S'           WFILOK
044000000000E   3C                   ENDIF
044100000000     C*
044200000000IF  3C     WFILOK        IFEQ      'S'
044300000000     C                   ADD       1             I
044400000000     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
044500000000     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
044600000000E   3C                   ENDIF
044700000000E   2C                   ENDIF
044800000000     C*
044900000000     C                   SETOFF                                       1020
045000020529     C                   READ      AZORG21l                               99
045100000000E   1C                   ENDDO
045200020528
045300020528     c     keyorg        setll     azorg01l
045400020528     c                   do        *hival
045500020528     c                   read      azorg01l
045600020528     c                   if        %eof(azorg01l)
045700020528     c                   leave
045800020528     c                   endif
045900020528      * no record annullati
046000020528     c                   if        orgfva <> *blanks
046100020528     c                   iter
046200020528     c                   endif
046300020528     C* LIVELLO FILIALE <> LIVELLO RICHIESTO, ESCLUDO
046400020528IF  3C     PARLIF        IFEQ      'T'
046500020528     C                   MOVEL     'S'           WFILOK
046600020528E   3C                   ENDIF
046700020528     C*
046800020528     C* REPERISCE IL TERMINAL
046900020528     C                   CLEAR                   DSLV55
047000020528     C                   MOVEL     'P'           D55TPT
047100020528     C                   Z-ADD     ORGFIL        D55LIN
047200020528     C                   Z-ADD     DATCOR        D55DRF
047300020528     C                   CALL      'FNLV55R'
047400020528     C                   PARM                    DSLV55
047500020528IF  4C     D55ERR        IFNE      *BLANKS
047600020528     C                   Z-ADD     ORGFIL        D55TFP
047700020528E   4C                   ENDIF
047800020528     C*
047900020528IF  4C     PARLIF        IFEQ      '1'
048000020528     C     ORGFIL        ANDEQ     D55TFP
048100020528     C                   MOVEL     'S'           WFILOK
048200020528E   4C                   ENDIF
048300020528IF  4C     PARLIF        IFEQ      '2'
048400020528     C     ORGFIL        ANDNE     D55TFP
048500020528     C                   MOVEL     'S'           WFILOK
048600020528E   4C                   ENDIF
048700020528     C*
048800020528IF  3C     WFILOK        IFEQ      'S'
048900020528     c                   z-add     1             I
049000020528     c     orgfil        lookup    fil(i)                                 99
049100020528     c                   if        not *in99
049200020529     c     *zeros        lookup    fil(i)                                 99
049300020529     c                   if        *in99
049400020528     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
049500020528     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
049600020529     c                   endif
049700020529     c                   endif
049800020528E   3C                   ENDIF
049900020528
050000020528     c                   enddo
050100000000     C*
050200000000     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C* CARTAB - CARICA TABELLE
050500000000     C*------------------------------------------------------------------------*
050600000000     C     CARTAB        BEGSR
050700000000     C*
050800020325     C                   MOVEL     'UNT'         tbecod
050900000000     C                   Z-ADD     *ZEROS        I
051000000000     C*
051100020325     C     tbecod        SETLL     Tntbe01l
051200020325     C     tbecod        READE     tntbe01l                               99
051300000000     C*
051400000000DO  1C     *IN99         DOWEQ     *OFF
051500020325IF  2C     TBeatb        IFEQ      *BLANKS
051600000000     C                   ADD       1             I
051700020325     C                   MOVEL     tbeke1        TIP(I)                         *CODICE
051800020325     C                   MOVEL     tbeuni        DTIP(I)                        *DESCRIZIONE
051900000000E   2C                   ENDIF
052000000000     C*
052100020325     C     tbecod        READE     tntbe01l                               99
052200000000E   1C                   ENDDO
052300000000     C*
052400000000     C                   ENDSR
052500000000     C*------------------------------------------------------------------------*
052600000000     C* STAPAR - STAMPA PAGINA PARAMETRI
052700000000     C*------------------------------------------------------------------------*
052800000000     C     STAPAR        BEGSR
052900000000     C*
053000000000     C* TIPO UNITA'
053100000000     C                   Z-ADD     PARTIP        PACTIP
053200000000     C                   Z-ADD     1             I
053300000000     C                   SETOFF                                           28
053400000000     C     PARTIP        LOOKUP    TIP(I)                                 28
053500000000IF  1C     *IN28         IFEQ      *ON
053600000000     C                   MOVEL     DTIP(I)       PADTIP
053700000000X   1C                   ELSE
053800000000     C                   MOVEL     *ALL'*'       PADTIP
053900000000E   1C                   ENDIF
054000000000     C*
054100000000     C* MAGAZZINO
054200000000     C                   Z-ADD     PARMAG        PACMAG
054300000000     C                   Z-ADD     1             I
054400000000     C                   SETOFF                                           28
054500000000     C     PARMAG        LOOKUP    FIL(I)                                 28
054600000000IF  1C     *IN28         IFEQ      *ON
054700000000     C                   MOVEL     DFIL(I)       PADMAG
054800000000X   1C                   ELSE
054900000000     C                   MOVEL     *ALL'*'       PADMAG
055000000000E   1C                   ENDIF
055100000000     C*
055200000000     C* LIVELLI FILIALE
055300000000IF  1C     PARLIF        IFEQ      'T'
055400000000     C                   MOVEL     'TUTTI'       PACLIF
055500000000E   1C                   ENDIF
055600000000IF  1C     PARLIF        IFEQ      '1'
055700000000     C                   MOVEL     '1° LIV'      PACLIF
055800000000E   1C                   ENDIF
055900000000IF  1C     PARLIF        IFEQ      '2'
056000000000     C                   MOVEL     '2° LIV'      PACLIF
056100000000E   1C                   ENDIF
056200951103     C*
056300951103     C* SOLO PER DIFFERENZE
056400951103     C                   MOVEL     PARDIF        PACDIF
056500000000     C*
056600000000     C                   WRITE     SDUFPT                                       *INTESTAZIONE
056700000000     C                   WRITE     SDUFPA                                       *PARAMENTRI
056800000000     C*
056900000000     C                   ENDSR
057000000000     C*------------------------------------------------------------------------*
057100000000     C* *INZSR - ROUTINE INIZIALE
057200000000     C*------------------------------------------------------------------------*
057300000000     C     *INZSR        BEGSR
057400000000     C*--------------------
057500000000     C* RICEVIMENTO PARAMETRI
057600000000     C*--------------------
057700000000     C     *ENTRY        PLIST
057800000000     C                   PARM                    KPJBA
057900000000     C                   MOVEL     KPJBU         PARAM
058000000000     C*--------------------
058100000000     C* CAMPI RIFERITI AL DATA BASE
058200000000     C*--------------------
058300000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
058400000000     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
058500000000     C     *LIKE         DEFINE    DOTFIL        KDOFIL
058600000000     C     *LIKE         DEFINE    UNAMAG        KANMAG                         *UNANA00F
058700000000     C     *LIKE         DEFINE    UNATIP        KANTIP
058800000000     C     *LIKE         DEFINE    DOTTIP        DEPTIP
058900000000     C     *LIKE         DEFINE    DOTFIL        DEPFIL
059000000000     C*--------------------
059100000000     C* CHIAVI DI LETTURA
059200000000     C*--------------------
059300000000     C*
059400020529     C* CHIAVE LETTURA AZORG21l/azorg01l - TOTALE
059500000000     C     KEYORG        KLIST
059600000000     C                   KFLD                    KORFIL                         *FILIALE
059700000000     C*
059800000000     C* CHIAVE LETTURA UNDOT01L - COMPLETA
059900000000     C     KEYDOT        KLIST
060000000000     C                   KFLD                    KDOTIP                         *TIPO
060100000000     C                   KFLD                    KDOFIL                         *FILIALE
060200000000     C*
060300000000     C* CHIAVE LETTURA UNANA03L - PARZIALE
060400000000     C     KEYANA        KLIST
060500000000     C                   KFLD                    KANMAG                         *FILIALE
060600000000     C                   KFLD                    KANTIP                         *TIPO
060700000000     C*--------------------
060800000000     C* CAMPI DI CONTROLLO
060900000000     C*--------------------
061000000000     C                   MOVEL     '0'           $FINE             1            *FINE CICLO DOTAZION
061100000000     C                   MOVEL     '0'           $FIN2             1            *FINE CICLO UNITA'
061200000000     C                   MOVEL     'N'           WFILOK            1            *LIVELLO FILIALE OK
061300951103     C                   MOVEL     '0'           $STT              1            STAMPATO QC X TIPO
061400951103     C                   MOVEL     '0'           $STG              1            STAMPATO QC IN TOTAL
061500000000     C                   SETON                                        01        *SALTO PAGINA
061600950227     C*--------------------
061700950227     C* CAMPI NON RIFERITI AL DATA BASE
061800950227     C*--------------------
061900950227     C                   Z-ADD     *ZEROS        TOTTQD            7 0          *QTA TOTALE X TIPO
062000950227     C                   Z-ADD     *ZEROS        TOTTQE            7 0
062100950227     C                   Z-ADD     *ZEROS        TOTGQD            7 0          *QTA TOTALE GENERALE
062200950227     C                   Z-ADD     *ZEROS        TOTGQE            7 0
062300000000     C*
062400000000     C* CONTROLLO SE SONO IN SEDE, FILIALE DI 1° LIVELLO, FILIALE DI 2° LIVELLO
062500000000     C* E RECUPERO LA RAGIONE SOCIALE DELL'UTENTE
062600020709
062700020709     c     *dtaara       define    §azute        azuteds
062800020709     c     *dtaara       define    §datiute      ddatiute
062900020709
063000020709     c                   in(E)     *dtaara
063100020709     c                   if        %Error or rsut = *blanks
063200020709     c                   clear                   tibs34ds
063300020709     c                   call      'TIBS34R'
063400020709     c                   parm                    Tibs34Ds
063500020709     c                   in        *dtaara
063600020709     c                   endif
063700971128     C*
063800971128     C* CALCOLA LA DATA CORRENTE
063900971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
064000971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
064100971128     C                   Z-ADD     WN8           DATA              8 0
064200971128     C                   Z-ADD     WN8           G08DAT
064300971128     C                   Z-ADD     *ZEROS        G08INV
064400971128     C                   MOVEL     '0'           G08ERR
064500971128     C                   CALL      'XSRDA8'
064600971128     C                   PARM                    WLBDA8
064700971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
064800000000     C*
064900000000     C* CARICAMENTO FILIALI
065000000000     C                   EXSR      CARORG
065100000000     C*
065200000000     C* CARICAMENTO TABELLE
065300000000     C                   EXSR      CARTAB
065400000000     C*
065500000000     C                   ENDSR
065600950227     O*------------------------------------------------------------------------*
065700950227**   RIG
065800950227TOTALE TIPO UNITA'       1
065900950227TOTALE GENERALE          2
0660009502273
0661009502274
0662009502275
