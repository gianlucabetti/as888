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
004900000000     D*------------------
005000000000     D* PARAMETRI XFIL
005100000000     D*------------------
005200020709     D**!!! DSFIL         E DS
005300971128     D*---------------------
005400971128     D* DS REPEIMENTO TERMINALs
005500971128     D*---------------------
005600971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
005700971128     D*---------------------
005800971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900971128     D*---------------------
006000971128     D WLBDA8          DS                  INZ
006100971128     D  G08DAT                 1      8  0
006200971128     D  G08INV                 9     16  0
006300971128     D  G08ERR                17     17
006400971128     D  G08TGI                18     22  0
006500020325
006600020709     d ddatiute      e ds
006700020709     d azuteds       e ds                  extname(azute00f)
006800020709     d tibs34ds      e ds                  inz
006900000000     C*------------------------------------------------------------------------*
007000000000     C* MAIN LINE
007100000000     C*------------------------------------------------------------------------*
007200000000     C*
007300000000     C* STAMPA PARAMETRI FILTRO
007400000000     C                   EXSR      STAPAR
007500000000     C*
007600000000     C* SCELTA ELABORAZIONE
007700000000     C                   EXSR      UNDOT
007800000000     C*
007900000000     C* STAMPA RIGA DI FINE PROSPETTO
008000000000     C                   WRITE     SDUFFI
008100971128     C*
008200971128     C* OPERAZIONI FINALI
008300971128     C                   EXSR      FINSR
008400000000     C*
008500000000     C                   SETON                                        LR
008600971128     C*------------------------------------------------------------------------*
008700971128     C* FINSR - OPERAZIONI FINALI
008800971128     C*------------------------------------------------------------------------*
008900971128     C     FINSR         BEGSR
009000971128     C*
009100971128     C                   CLEAR                   DSLV55
009200971128     C                   MOVEL     'C'           D55TLA
009300971128     C                   CALL      'FNLV55R'
009400971128     C                   PARM                    DSLV55
009500971128     C*
009600971128     C                   ENDSR
009700000000     C*------------------------------------------------------------------------*
009800000000     C* UNDOT - STAMPA DOTAZIONI
009900000000     C*------------------------------------------------------------------------*
010000000000     C     UNDOT         BEGSR
010100000000     C*
010200000000     C* POSIZIONAMENTO E PRIMA LETTURA DOTAZIONI (TIP + FIL)
010300000000     C                   EXSR      SETFIL
010400000000     C*
010500000000     C* FINCHE' FINE FILE
010600000000DO  1C     $FINE         DOWEQ     '0'
010700000000     C*
010800000000     C* OPERAZIONI PER NUOVO TIPO UNITA'
010900000000     C                   EXSR      NEWTIP
011000000000     C*
011100000000     C* FINCHE' FINE TIPO UNITA'
011200000000DO  2C     $FINE         DOWEQ     '0'
011300000000     C     DEPTIP        ANDEQ     DOTTIP
011400000000     C*
011500000000     C* OPERAZIONI PER NUOVO MAGAZZINO
011600000000     C                   EXSR      NEWFIL
011700000000     C*
011800000000     C* POSIZIONAMENTO E PRIMA LETTURA UNITA (FIL + TIP)
011900000000     C                   EXSR      SETFI2
012000000000     C*
012100000000     C* FINCHE' FINE FILE
012200000000DO  3C     $FIN2         DOWEQ     '0'
012300000000     C*
012400000000     C* CONTEGGIO TIPO PER FILIALE
012500000000     C                   ADD       1             TOTFIL
012600950227     C                   ADD       1             TOTTQE                         *QTA IN ESSERE X TIP
012700950227     C                   ADD       1             TOTGQE                         *QTA IN ESSERE TOTAL
012800000000     C*
012900000000     C* LETTURA SUCESSIVA
013000000000     C                   EXSR      LETFI2
013100000000E   3C                   ENDDO
013200000000     C*
013300000000     C* TOTALE PER FILIALE
013400000000     C                   EXSR      STARIG
013500000000     C*
013600000000     C* LETTURA SUCESSIVA
013700000000     C                   EXSR      LETFIL
013800000000     C*
013900000000E   2C                   ENDDO
014000951103     C*
014100950227     C* TOTALE PER TIPO (SOLO SE NON E' STATA CHIESTA SOLO UNA FILIALE)
014200951103IF  2C     $STT          IFEQ      '1'                                          *STAMPATO QC.
014300951103IF  3C     PARMAG        IFEQ      *ZEROS
014400951103IF  4C     TOTTQD        IFGT      *ZEROS
014500950227     C     TOTTQE        ORGT      *ZEROS
014600950227     C                   CLEAR                   SDUFPR
014700950227     C                   Z-ADD     DEPTIP        PRCTIP                         *DECODIFICA TIPO
014800950227     C                   MOVEL     DESTIP        PRDTIP
014900950227     C                   MOVEL     RIG(1)        PRDMAG                         *DECODIFICA TOTALE
015000950227     C                   Z-ADD     TOTTQD        PRCQTA                         *QTA IN DOTAZIONE
015100950227     C                   Z-ADD     TOTTQE        PRCQES                         *QTA IN ESSERE
015200950227     C                   WRITE     SDUFPR
015300951103E   4C                   ENDIF
015400951103E   3C                   ENDIF
015500951103E   2C                   ENDIF
015600950227     C*
015700000000E   1C                   ENDDO
015800950227     C*
015900950227     C* TOTALE GENERALE (SOLO SE NON E' STATA CHIESTA SOLO UNA FILIALE)
016000951103IF  1C     $STG          IFEQ      '1'                                          *STAMPATO QC.
016100951103IF  2C     PARMAG        IFEQ      *ZEROS
016200951103IF  3C     TOTGQD        IFGT      *ZEROS
016300950227     C     TOTGQE        ORGT      *ZEROS
016400950227     C                   CLEAR                   SDUFPR
016500950227     C                   MOVEL     RIG(2)        PRDMAG                         *DECODIFICA TOTALE
016600950227     C                   Z-ADD     TOTGQD        PRCQTA                         *QTA IN DOTAZIONE
016700950227     C                   Z-ADD     TOTGQE        PRCQES                         *QTA IN ESSERE
016800950227     C                   WRITE     SDUFPR
016900951103E   3C                   ENDIF
017000951103E   2C                   ENDIF
017100951103E   1C                   ENDIF
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
017600000000     C*------------------------------------------------------------------------*
017700000000     C     SETFIL        BEGSR
017800000000     C*
017900000000     C                   Z-ADD     PARTIP        KDOTIP
018000000000     C                   Z-ADD     PARMAG        KDOFIL
018100000000     C     KEYDOT        SETLL     UNDOT01L                           99        *FILE FILE
018200000000     C*
018300000000     C     *IN99         IFEQ      *ON
018400000000     C                   MOVEL     '1'           $FINE
018500000000     C                   ELSE
018600000000     C                   EXSR      LETFIL                                       *PRIMA LETTURA
018700000000     C                   END
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* LETFIL - LETTURA PROSSIMO RECORD
019200000000     C*------------------------------------------------------------------------*
019300000000     C     LETFIL        BEGSR
019400000000     C*
019500000000     C* LEGGO FINO A:
019600000000     C*     - FINE FILE
019700000000     C*     - TROVATO RECORD VALIDO
019800000000     C                   MOVEL     'N'           WRECOK
019900000000     C*
020000000000DO  1C     $FINE         DOWEQ     '0'
020100000000     C     WRECOK        ANDEQ     'N'
020200000000     C                   READ      UNDOT01L                               99
020300000000     C*
020400000000IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
020500000000     C                   MOVEL     '1'           $FINE
020600000000X   2C                   ELSE
020700000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
020800000000E   2C                   ENDIF
020900000000     C*
021000000000E   1C                   ENDDO
021100000000     C*
021200000000     C                   ENDSR
021300000000     C*------------------------------------------------------------------------*
021400000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
021500000000     C*------------------------------------------------------------------------*
021600000000     C     CHKREC        BEGSR
021700000000     C*
021800000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
021900950407     C*
022000950407     C* DOTAZIONE ANNULLATA, ESCLUDO
022100950407IF  1C     DOTATB        IFNE      *BLANKS
022200950407     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022300950407E   1C                   ENDIF
022400000000     C*
022500000000     C* TIPO UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCO DAL CICLO
022600000000IF  1C     PARTIP        IFGT      *ZEROS
022700000000IF  2C     PARTIP        IFNE      DOTTIP
022800000000     C                   MOVEL     '1'           $FINE
022900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023000000000E   2C                   ENDIF
023100000000E   1C                   ENDIF
023200000000     C*
023300000000     C* MAGAZZINO DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
023400000000IF  1C     PARMAG        IFGT      *ZEROS
023500000000IF  2C     PARMAG        IFNE      DOTFIL
023600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023700000000E   2C                   ENDIF
023800000000E   1C                   ENDIF
023900000000     C*
024000000000     C* TIPO FILIALE DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
024100000000     C                   Z-ADD     1             I
024200000000     C                   SETOFF                                           28
024300000000     C     DOTFIL        LOOKUP    FIL(I)                                 28
024400000000IF  1C     *IN28         IFEQ      *OFF
024500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024600000000E   1C                   ENDIF
024700941222     C*
024800941222     C* RECORD ANNULLATO, ESCLUDO
024900941222IF  1C     DOTATB        IFNE      *BLANKS
025000941222     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025100941222E   1C                   ENDIF
025200000000     C*
025300000000     C                   ENDSR
025400000000     C*------------------------------------------------------------------------*
025500000000     C* SETFI2 - POSIZIONAMENTO E PRIMA LETTURA
025600000000     C*------------------------------------------------------------------------*
025700000000     C     SETFI2        BEGSR
025800000000     C*
025900000000     C                   Z-ADD     DEPFIL        KANMAG
026000000000     C                   Z-ADD     DEPTIP        KANTIP
026100000000     C     KEYANA        SETLL     UNANA03L                           98        *FILE FILE
026200000000     C*
026300000000     C     *IN98         IFEQ      *ON
026400000000     C                   MOVEL     '1'           $FIN2
026500000000     C                   ELSE
026600000000     C                   EXSR      LETFI2                                       *PRIMA LETTURA
026700000000     C                   END
026800000000     C*
026900000000     C                   ENDSR
027000000000     C*------------------------------------------------------------------------*
027100000000     C* LETFI2 - LETTURA PROSSIMO RECORD
027200000000     C*------------------------------------------------------------------------*
027300000000     C     LETFI2        BEGSR
027400000000     C*
027500000000     C* LEGGO FINO A:
027600000000     C*     - FINE FILE
027700000000     C*     - TROVATO RECORD VALIDO
027800000000     C                   MOVEL     'N'           WRECOK
027900000000     C*
028000000000DO  1C     $FIN2         DOWEQ     '0'
028100000000     C     WRECOK        ANDEQ     'N'
028200000000     C     KEYANA        READE     UNANA03L                               98
028300000000     C*
028400000000IF  2C     *IN98         IFEQ      *ON                                          *FINE FILE
028500000000     C                   MOVEL     '1'           $FIN2
028600000000X   2C                   ELSE
028700000000     C                   EXSR      CHKRE2                                       *CONTROLLO RECORD
028800000000E   2C                   ENDIF
028900000000     C*
029000000000E   1C                   ENDDO
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C* CHKRE2 - CONTROLLA VALIDITA' RECORD
029500000000     C*------------------------------------------------------------------------*
029600000000     C     CHKRE2        BEGSR
029700000000     C*
029800000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
029900000000     C*
030000950407     C* UNITA' ANNULLATA, ESCLUDO
030100000000IF  1C     UNAATB        IFNE      *BLANKS
030200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030300000000E   1C                   ENDIF
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700000000     C* NEWTIP - OPERAZIONI PER NUOVO TIPO UNITA'
030800000000     C*------------------------------------------------------------------------*
030900000000     C     NEWTIP        BEGSR
031000000000     C*
031100000000     C* MEMORIZZA IL NUOVO TIPO UNITA' IN UN DEPOSITO
031200000000     C                   Z-ADD     DOTTIP        DEPTIP
031300000000     C*
031400000000     C* DECODIFICA IL TIPO UNITA'
031500000000     C                   Z-ADD     1             I
031600000000     C                   SETOFF                                           28
031700000000     C     DEPTIP        LOOKUP    TIP(I)                                 28
031800000000IF  1C     *IN28         IFEQ      *ON
031900000000     C                   MOVEL     DTIP(I)       DESTIP           25
032000000000X   1C                   ELSE
032100000000     C                   MOVEL     *ALL'*'       DESTIP
032200000000E   1C                   ENDIF
032300000000     C*
032400000000     C* SALTO PAGINA SOLO SE NON HO SELEZIONATO UN PARTICOLARE MAGAZZINO
032500000000IF  1C     PARMAG        IFEQ      *ZEROS
032600000000     C                   SETON                                        01
032700000000E   1C                   ENDIF
032800950227     C*
032900950227     C* AZZERO TOALE TIPO UNITA'
033000950227     C                   Z-ADD     *ZEROS        TOTTQD                         *QTA IN DOTAZIONE
033100950227     C                   Z-ADD     *ZEROS        TOTTQE                         *QTA IN ESSERE
033200951103     C*
033300951103     C* REIMPOSTA FLAG STAMPATO QC
033400951103     C                   MOVEL     '0'           $STT
033500000000     C*
033600000000     C                   ENDSR
033700000000     C*------------------------------------------------------------------------*
033800000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
033900000000     C*------------------------------------------------------------------------*
034000000000     C     NEWFIL        BEGSR
034100000000     C*
034200000000     C* MEMORIZZA IL NUOVO MAGAZZINO IN UN DEPOSITO
034300000000     C                   Z-ADD     DOTFIL        DEPFIL
034400000000     C*
034500000000     C* DECODIFICA IL MAGAZZINO
034600000000     C                   Z-ADD     1             I
034700000000     C                   SETOFF                                           28
034800000000     C     DEPFIL        LOOKUP    FIL(I)                                 28
034900000000IF  1C     *IN28         IFEQ      *ON
035000000000     C                   MOVEL     DFIL(I)       DESFIL           25
035100000000X   1C                   ELSE
035200000000     C                   MOVEL     *ALL'*'       DESFIL
035300000000E   1C                   ENDIF
035400000000     C*
035500000000     C* AZZERA IL TOTALE MAGAZZINO
035600000000     C                   Z-ADD     *ZEROS        TOTFIL            7 0
035700000000     C*
035800000000     C* REIMPOSTA IL FNE FILE PER LETTURA ANAGRAFICA UNITA'
035900000000     C                   MOVEL     '0'           $FIN2
036000000000     C                   SETOFF                                           98
036100000000     C*
036200000000     C                   ENDSR
036300000000     C*------------------------------------------------------------------------*
036400000000     C* STARIG - STAMPA RIGA
036500000000     C*------------------------------------------------------------------------*
036600000000     C     STARIG        BEGSR
036700950227     C*
036800950227     C* TOTALI PER TIPO E GENERALE
036900950227     C                   ADD       DOTQTA        TOTTQD                         *QTA IN DOTAZ. X TIP
037000950227     C                   ADD       DOTQTA        TOTGQD                         *QTA IN DOTAZ. GENER
037100950227     C*
037200000000     C                   Z-ADD     DEPFIL        PRCMAG
037300000000     C                   MOVEL     DESFIL        PRDMAG
037400000000     C                   Z-ADD     DOTQTA        PRCQTA
037500000000     C                   Z-ADD     TOTFIL        PRCQES
037600000000     C                   Z-ADD     *ZEROS        PRCDIF
037700000000     C     TOTFIL        SUB       DOTQTA        PRCDIF
037800951103     C*
037900951103     C* SE SELEZIONE PER DIFFERENZE, STAMPA SOLO SE LA DIFFERENAZA E + O -
038000951103IF  1C     PARDIF        IFEQ      'S'
038100951103     C     PRCDIF        ANDNE     *ZEROS
038200951103     C     PARDIF        OREQ      'N'
038300951103     C***
038400951103     C* TESTATA
038500951103     C***
038600951103IF  2C     *IN01         IFEQ      *ON
038700951103     C                   WRITE     SDUFPT                                       *TITOLO
038800951103     C                   WRITE     SDUFRI                                       *RIGA
038900951103     C*
039000951103     C* STAMPA IL TIPO UNITA'IN TESTATA SOLO SE NON HO SCELTO UNA PARTIC FILIALE
039100951103IF  3C     PARMAG        IFEQ      *ZEROS
039200951103     C                   Z-ADD     DEPTIP        PICTIP
039300951103     C                   MOVEL     DESTIP        PIDTIP
039400951103     C                   ELSE
039500951103     C                   Z-ADD     *ZEROS        PICTIP
039600951103     C                   MOVEL     *BLANKS       PIDTIP
039700951103E   3C                   ENDIF
039800951103     C*
039900951103     C                   WRITE     SDUFPI                                       *DESCRIZIONE CAMPI
040000951103     C                   WRITE     SDUFR2                                       *RIGA
040100951103     C                   SETOFF                                       01
040200951103E   2C                   ENDIF
040300951103     C***
040400951103     C* RIGA
040500951103     C***
040600951103     C* STAMPA IL TIPO UNITA'IN DETTAGLIO SOLO SE HO SCELTO UNA PARTIC FILIALE
040700951103IF  2C     PARMAG        IFEQ      *ZEROS
040800951103     C                   Z-ADD     *ZEROS        PRCTIP
040900951103     C                   MOVEL     *BLANKS       PRDTIP
041000951103     C                   ELSE
041100951103     C                   Z-ADD     DEPTIP        PRCTIP
041200951103     C                   MOVEL     DESTIP        PRDTIP
041300951103E   2C                   ENDIF
041400951103     C* STAMPA RIGA
041500000000     C                   WRITE     SDUFPR
041600951103     C                   MOVEL     '1'           $STT
041700951103     C                   MOVEL     '1'           $STG
041800951103E   1C                   ENDIF
041900000000     C*
042000000000     C                   ENDSR
042100000000     C*------------------------------------------------------------------------*
042200000000     C* CARORG - CARICA ORGANIGRAMMA
042300000000     C*------------------------------------------------------------------------*
042400000000     C     CARORG        BEGSR
042500000000     C*--------------------
042600000000     C* FILIALI
042700000000     C*--------------------
042800000000     C                   Z-ADD     *ZEROS        I                 3 0
042900000000     C                   Z-ADD     *ZEROS        KORFIL
043000000000     C*
043100020528     C     KEYORG        SETLL     AZORG21l
043200020528     C                   READ      AZORG21l                               99
043300000000     C*
043400000000     C* CICLO FINO A FINE FILE
043500000000DO  1C     *IN99         DOWEQ     *OFF
043600000000     C*
043700000000     C* ELIMINA RECORD ANNULLATI
043800000000IF  2C     ORGFVA        IFEQ      *BLANKS
043900000000     C                   MOVEL     'N'           WFILOK
044000000000     C*
044100000000     C* LIVELLO FILIALE <> LIVELLO RICHIESTO, ESCLUDO
044200000000IF  3C     PARLIF        IFEQ      'T'
044300000000     C                   MOVEL     'S'           WFILOK
044400000000E   3C                   ENDIF
044500000000     C*
044600000000IF  3C     WFILOK        IFEQ      'S'
044700000000     C                   ADD       1             I
044800000000     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
044900000000     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
045000000000E   3C                   ENDIF
045100000000E   2C                   ENDIF
045200000000     C*
045300000000     C                   SETOFF                                       1020
045400020529     C                   READ      AZORG21l                               99
045500000000E   1C                   ENDDO
045600020528
045700020528     c     keyorg        setll     azorg01l
045800020528     c                   do        *hival
045900020528     c                   read      azorg01l
046000020528     c                   if        %eof(azorg01l)
046100020528     c                   leave
046200020528     c                   endif
046300020528      * no record annullati
046400020528     c                   if        orgfva <> *blanks
046500020528     c                   iter
046600020528     c                   endif
046700020528     C* LIVELLO FILIALE <> LIVELLO RICHIESTO, ESCLUDO
046800020528IF  3C     PARLIF        IFEQ      'T'
046900020528     C                   MOVEL     'S'           WFILOK
047000020528E   3C                   ENDIF
047100020528     C*
047200020528     C* REPERISCE IL TERMINAL
047300020528     C                   CLEAR                   DSLV55
047400020528     C                   MOVEL     'P'           D55TPT
047500020528     C                   Z-ADD     ORGFIL        D55LIN
047600020528     C                   Z-ADD     DATCOR        D55DRF
047700020528     C                   CALL      'FNLV55R'
047800020528     C                   PARM                    DSLV55
047900020528IF  4C     D55ERR        IFNE      *BLANKS
048000020528     C                   Z-ADD     ORGFIL        D55TFP
048100020528E   4C                   ENDIF
048200020528     C*
048300020528IF  4C     PARLIF        IFEQ      '1'
048400020528     C     ORGFIL        ANDEQ     D55TFP
048500020528     C                   MOVEL     'S'           WFILOK
048600020528E   4C                   ENDIF
048700020528IF  4C     PARLIF        IFEQ      '2'
048800020528     C     ORGFIL        ANDNE     D55TFP
048900020528     C                   MOVEL     'S'           WFILOK
049000020528E   4C                   ENDIF
049100020528     C*
049200020528IF  3C     WFILOK        IFEQ      'S'
049300020528     c                   z-add     1             I
049400020528     c     orgfil        lookup    fil(i)                                 99
049500020528     c                   if        not *in99
049600020529     c     *zeros        lookup    fil(i)                                 99
049700020529     c                   if        *in99
049800020528     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
049900020528     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
050000020529     c                   endif
050100020529     c                   endif
050200020528E   3C                   ENDIF
050300020528
050400020528     c                   enddo
050500000000     C*
050600000000     C                   ENDSR
050700000000     C*------------------------------------------------------------------------*
050800000000     C* CARTAB - CARICA TABELLE
050900000000     C*------------------------------------------------------------------------*
051000000000     C     CARTAB        BEGSR
051100000000     C*
051200020325     C                   MOVEL     'UNT'         tbecod
051300000000     C                   Z-ADD     *ZEROS        I
051400000000     C*
051500020325     C     tbecod        SETLL     Tntbe01l
051600020325     C     tbecod        READE     tntbe01l                               99
051700000000     C*
051800000000DO  1C     *IN99         DOWEQ     *OFF
051900020325IF  2C     TBeatb        IFEQ      *BLANKS
052000000000     C                   ADD       1             I
052100020325     C                   MOVEL     tbeke1        TIP(I)                         *CODICE
052200020325     C                   MOVEL     tbeuni        DTIP(I)                        *DESCRIZIONE
052300000000E   2C                   ENDIF
052400000000     C*
052500020325     C     tbecod        READE     tntbe01l                               99
052600000000E   1C                   ENDDO
052700000000     C*
052800000000     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C* STAPAR - STAMPA PAGINA PARAMETRI
053100000000     C*------------------------------------------------------------------------*
053200000000     C     STAPAR        BEGSR
053300000000     C*
053400000000     C* TIPO UNITA'
053500000000     C                   Z-ADD     PARTIP        PACTIP
053600000000     C                   Z-ADD     1             I
053700000000     C                   SETOFF                                           28
053800000000     C     PARTIP        LOOKUP    TIP(I)                                 28
053900000000IF  1C     *IN28         IFEQ      *ON
054000000000     C                   MOVEL     DTIP(I)       PADTIP
054100000000X   1C                   ELSE
054200000000     C                   MOVEL     *ALL'*'       PADTIP
054300000000E   1C                   ENDIF
054400000000     C*
054500000000     C* MAGAZZINO
054600000000     C                   Z-ADD     PARMAG        PACMAG
054700000000     C                   Z-ADD     1             I
054800000000     C                   SETOFF                                           28
054900000000     C     PARMAG        LOOKUP    FIL(I)                                 28
055000000000IF  1C     *IN28         IFEQ      *ON
055100000000     C                   MOVEL     DFIL(I)       PADMAG
055200000000X   1C                   ELSE
055300000000     C                   MOVEL     *ALL'*'       PADMAG
055400000000E   1C                   ENDIF
055500000000     C*
055600000000     C* LIVELLI FILIALE
055700000000IF  1C     PARLIF        IFEQ      'T'
055800000000     C                   MOVEL     'TUTTI'       PACLIF
055900000000E   1C                   ENDIF
056000000000IF  1C     PARLIF        IFEQ      '1'
056100000000     C                   MOVEL     '1° LIV'      PACLIF
056200000000E   1C                   ENDIF
056300000000IF  1C     PARLIF        IFEQ      '2'
056400000000     C                   MOVEL     '2° LIV'      PACLIF
056500000000E   1C                   ENDIF
056600951103     C*
056700951103     C* SOLO PER DIFFERENZE
056800951103     C                   MOVEL     PARDIF        PACDIF
056900000000     C*
057000000000     C                   WRITE     SDUFPT                                       *INTESTAZIONE
057100000000     C                   WRITE     SDUFPA                                       *PARAMENTRI
057200000000     C*
057300000000     C                   ENDSR
057400000000     C*------------------------------------------------------------------------*
057500000000     C* *INZSR - ROUTINE INIZIALE
057600000000     C*------------------------------------------------------------------------*
057700000000     C     *INZSR        BEGSR
057800000000     C*--------------------
057900000000     C* RICEVIMENTO PARAMETRI
058000000000     C*--------------------
058100000000     C     *ENTRY        PLIST
058200000000     C                   PARM                    KPJBA
058300000000     C                   MOVEL     KPJBU         PARAM
058400000000     C*--------------------
058500000000     C* CAMPI RIFERITI AL DATA BASE
058600000000     C*--------------------
058700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
058800000000     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
058900000000     C     *LIKE         DEFINE    DOTFIL        KDOFIL
059000000000     C     *LIKE         DEFINE    UNAMAG        KANMAG                         *UNANA00F
059100000000     C     *LIKE         DEFINE    UNATIP        KANTIP
059200000000     C     *LIKE         DEFINE    DOTTIP        DEPTIP
059300000000     C     *LIKE         DEFINE    DOTFIL        DEPFIL
059400000000     C*--------------------
059500000000     C* CHIAVI DI LETTURA
059600000000     C*--------------------
059700000000     C*
059800020529     C* CHIAVE LETTURA AZORG21l/azorg01l - TOTALE
059900000000     C     KEYORG        KLIST
060000000000     C                   KFLD                    KORFIL                         *FILIALE
060100000000     C*
060200000000     C* CHIAVE LETTURA UNDOT01L - COMPLETA
060300000000     C     KEYDOT        KLIST
060400000000     C                   KFLD                    KDOTIP                         *TIPO
060500000000     C                   KFLD                    KDOFIL                         *FILIALE
060600000000     C*
060700000000     C* CHIAVE LETTURA UNANA03L - PARZIALE
060800000000     C     KEYANA        KLIST
060900000000     C                   KFLD                    KANMAG                         *FILIALE
061000000000     C                   KFLD                    KANTIP                         *TIPO
061100000000     C*--------------------
061200000000     C* CAMPI DI CONTROLLO
061300000000     C*--------------------
061400000000     C                   MOVEL     '0'           $FINE             1            *FINE CICLO DOTAZION
061500000000     C                   MOVEL     '0'           $FIN2             1            *FINE CICLO UNITA'
061600000000     C                   MOVEL     'N'           WFILOK            1            *LIVELLO FILIALE OK
061700951103     C                   MOVEL     '0'           $STT              1            STAMPATO QC X TIPO
061800951103     C                   MOVEL     '0'           $STG              1            STAMPATO QC IN TOTAL
061900000000     C                   SETON                                        01        *SALTO PAGINA
062000950227     C*--------------------
062100950227     C* CAMPI NON RIFERITI AL DATA BASE
062200950227     C*--------------------
062300950227     C                   Z-ADD     *ZEROS        TOTTQD            7 0          *QTA TOTALE X TIPO
062400950227     C                   Z-ADD     *ZEROS        TOTTQE            7 0
062500950227     C                   Z-ADD     *ZEROS        TOTGQD            7 0          *QTA TOTALE GENERALE
062600950227     C                   Z-ADD     *ZEROS        TOTGQE            7 0
062700000000     C*
062800000000     C* CONTROLLO SE SONO IN SEDE, FILIALE DI 1° LIVELLO, FILIALE DI 2° LIVELLO
062900000000     C* E RECUPERO LA RAGIONE SOCIALE DELL'UTENTE
063000020709     C**!!!              CALL      'XFIL'
063100020709     C**!!!              PARM                    DSFIL
063200020709     C**!!!              Z-ADD     DSFICD        CODUT             1 0
063300020709
063400020709     c     *dtaara       define    §azute        azuteds
063500020709     c     *dtaara       define    §datiute      ddatiute
063600020709
063700020709     c                   in(E)     *dtaara
063800020709     c                   if        %Error or rsut = *blanks
063900020709     c                   clear                   tibs34ds
064000020709     c                   call      'TIBS34R'
064100020709     c                   parm                    Tibs34Ds
064200020709     c                   in        *dtaara
064300020709     c                   endif
064400971128     C*
064500971128     C* CALCOLA LA DATA CORRENTE
064600971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
064700971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
064800971128     C                   Z-ADD     WN8           DATA              8 0
064900971128     C                   Z-ADD     WN8           G08DAT
065000971128     C                   Z-ADD     *ZEROS        G08INV
065100971128     C                   MOVEL     '0'           G08ERR
065200971128     C                   CALL      'XSRDA8'
065300971128     C                   PARM                    WLBDA8
065400971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
065500000000     C*
065600000000     C* CARICAMENTO FILIALI
065700000000     C                   EXSR      CARORG
065800000000     C*
065900000000     C* CARICAMENTO TABELLE
066000000000     C                   EXSR      CARTAB
066100000000     C*
066200000000     C                   ENDSR
066300950227     O*------------------------------------------------------------------------*
066400950227**   RIG
066500950227TOTALE TIPO UNITA'       1
066600950227TOTALE GENERALE          2
0667009502273
0668009502274
0669009502275
