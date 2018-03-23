000100000214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000218     H*PARMS ACTGRP(*CALLER)
000300000000     H*------------------------------------------------------------------------*
000400000000     H*           - CONTROLLO DI CONGRUITA' DEL DATABASE TRAINI -              *
000500000000     H*------------------------------------------------------------------------*
000600000000     H* INPUT  - PARTRN --> NUMERO TRAINO
000700000000     H*          PARDDE --> DECORRENZA  (AAAA/MM/GG)
000800000000     H*          PARGES --> GESTIONE
000900000000     H*
001000000000     H* OUTPUT - PARERR --> FLAG ERRORE ('0'/'1')
001100000000     H*          PARMSG --> MESSAGGIO DI ERRORE
001200000000     F*------------------------------------------------------------------------*
001300000000     F* DATA BASE
001400000000     F*------------------------------------------------------------------------*
001500000821     FTNTLT01L  UF A E           K DISK    COMMIT
001600000821     FTNTLR01L  UF A E           K DISK    COMMIT
001700000821     FTNTLO01L  UF A E           K DISK    COMMIT
001800000821     FTNTLM01L  UF A E           K DISK    COMMIT
001900010828     FTITLL01L  UF A E           K DISK    COMMIT
002000000821     FTNTLN01L  UF A E           K DISK    COMMIT
002100000000     F*---
002200000000     FAZORG01L  IF   E           K DISK
002300000000     FAZCAE01L  IF   E           K DISK
002400000000     FTABEL00F  IF   E           K DISK
002500000821     FANATB01L  IF   E           K DISK    USROPN
002600000000     FTNTLO02L  IF   E           K DISK
002700000000     F                                     RENAME(TNTLO000:TNTLO2)
002800000000     FTNTLZ01L  IF   E           K DISK
002900000000     D*------------------------------------------------------------------------*
003000000000     D* SCHIERE
003100000000     D*------------------------------------------------------------------------*
003200000000     D*-------------------
003300000000     D* MESSAGGI
003400000000     D*-------------------
003500000000     D MSG             S             78    DIM(50) CTDATA PERRCD(1)             *MESSAGGI DI ERRORI
003600000000     D*-------------------
003700000000     D* SCHIERE DI MEMORIZZAZIONI DI TUTTI I DATI DEL TRAINO
003800000000     D*-------------------
003900000000     D***
004000000000     D* FERMATE
004100000000     D***
004200000000     D FTFM            S              1    DIM(50)                              *TIPO
004300000000     D FFIL            S              3  0 DIM(50)                              *FILIALE
004400000000     D FSCA            S              1    DIM(50)                              *SCARICO
004500000000     D FCAR            S              1    DIM(50)                              *CARICO
004600000000     D FKMT            S              5  0 DIM(50)                              *KILOMETRI
004700000000     D FTEM            S              4  0 DIM(50)                              *TEMPO
004800000000     D FGFV            S              1    DIM(50)                              *GIORNO APERTURA F.V
004900000000     D***
005000000000     D* ORARI
005100000000     D***
005200000000     D***
005300000000     D* METRICUBI
005400000000     D***
005500131206     D MTFP            S              3  0 DIM(500)                             *FILIALE PARTENZA
005600131206     D MTFA            S              3  0 DIM(500)                             *FILIALE ARRIVO
005700131206     D MTFF            S              3  0 DIM(500)                             *FILIALE ARRIVO FINA
005800131206     D MMCA            S              5  2 DIM(500)                             *MCUBI NORMALI
005900131206     D MMCB            S              5  2 DIM(500)                             *MCUBI BIS
006000131206     D MMCT            S              5  2 DIM(500)                             *MCUBI TRAINI SEDE
006100000000     D***
006200000000     D* LISTINI
006300000000     D***
006400000000     D LTMZ            S              2    DIM(50)                              *AUTOMEZZO
006500000000     D LPDR            S              7  0 DIM(50)                              *TRAZIONISTA
006600010828     D LILI            S             13  3 DIM(50)                              *IMPORTO LISTINO
006700010828     D LIES            S             13  3 DIM(50)                              *IMPORTO DA ESPORRE
006800000000     D LCAU            S              1    DIM(50)                              *CAUSALE CONTABILIZ.
006900010828     d ldiv            s              3    dim(50)                               divisa
007000000000     D***
007100000000     D* NOTE
007200000000     D***
007300000000     D NNFM            S              3  0 DIM(50)                              *NUMERO FERMATA
007400000000     D NPNO            S              3  0 DIM(50)                              *PROGRESSIVO
007500000000     D NTNO            S             70    DIM(50)                              *TESTO NOTA
007600000000     D*-------------------
007700000000     D* CONTROLLO "TNTL04R"
007800000000     D*-------------------
007900000000     D* PROGRESSIVO GIORNI
008000000000     D DALF            S             21    DIM(1) CTDATA PERRCD(1)              *ALFABETO (1÷21)
008100000000     D ALF             S              1    DIM(21)                              *ALFABETO (1÷21)
008200000000     D* GIORNI PARTENZA TRAINO
008300000000     D GIO             S              1    DIM(8)                               *GIORNI (1÷8)
008400000000     D* GIORNI CARICATI IN MEMORIA
008500000000     D GCM             S              1    DIM(8)                               *GIORNI (1÷8)
008600000000     D GCA             S              1    DIM(8)                               *GIORNI ANNULLATI
008700000000     D*-------------------
008800000000     D* SCHIERE DI LAVORO
008900000000     D*-------------------
009000000000     D* FILIALE + TIPO FERMATA
009100000000     D JFT             S              4    DIM(50)
009200000000     D* FERMATE
009300000000     D JTFM            S              1    DIM(50)                              *TIPO
009400000000     D JFIL            S              3  0 DIM(50)                              *FILIALE
009500000000     D JGFV            S              1    DIM(50)                              *GIORNO APERTURA F.V
009600000000     D* METRICUBI AND/RIT
009700000000     D JSEA            S              3  0 DIM(2)                               *SEQUENZA ARRIVO
009800000000     D JTFA            S              1    DIM(2)                               *TIPO FERMATA ARRIVO
009900000000     D JSEP            S              3  0 DIM(2)                               *SEQUENZA PARTENZA
010000000000     D JTFP            S              1    DIM(2)                               *TIPO FERMATA PARTEN
010100000000     D*-------------------
010200000000     D* TABELLE OCCORRENTI
010300000000     D*-------------------
010400000000     D* FILIALI
010500971125     D ORG             S              3  0 DIM(500)                             *CODICE
010600971125     D FEL             S              3  0 DIM(500)                             *FILIALE ELBORATORE
010700971125     D TFP             S              3  0 DIM(500)                             *TERMINAL PARTENZA
010800971125     D TFA             S              3  0 DIM(500)                             *TERMINAL ARRIVO
010900971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
011000000518     D PORG            S              1    DIM(500)                             *S=P.O. POSTE
011100090115     D NTW             S              3    DIM(500)                             *network
011200000000     D* AUTOMEZZI
011300000000     D TMZ             S              2    DIM(20)                              *CODICE
011400000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
011500000000     D* CAUSALI CONTABILIZZAZIONE
011600000000     D CAU             S              1    DIM(20)                              *CODICE
011700000000     D DCAU            S             30    DIM(20)                              *DESCRIZIONE
011800010907
011900010907     d w_decimali      s              3  0
012000010907
012100000000     D*------------------------------------------------------------------------*
012200000000     D* INPUT
012300000000     D*------------------------------------------------------------------------*
012400000000     D*-------------------
012500000000     D* DS MULTIPLA DI MEMORIZZAZIONE
012600000000     D*-------------------
012700000000     D* Es. (4:2)+1=3 e (3x50)=150
012800000000     D***
012900000000     D* ORARI
013000000000     D***
013100000000     D OGSE            DS                  OCCURS(8)
013200000000     D  OGGA                   1     50                                         1
013300000000     D                                     DIM(50)                              *GIORNO ARRIVO
013400000000     D  OHHA                  51    200P 0                                      4,0
013500000000     D                                     DIM(50)                              *ORA ARRIVO
013600000000     D                                     PACKEVEN
013700000000     D  OGGP                 201    250                                         1
013800000000     D                                     DIM(50)                              *GIORNO PARTENZA
013900000000     D  OHHP                 251    400P 0                                      4,0
014000000000     D                                     DIM(50)                              *ORA PARTENZA
014100000000     D                                     PACKEVEN
014200000000     D*---------------------
014300000000     D* DS TIPI TRAINI
014400000000     D*---------------------
014500000000     D DSTV          E DS
014600000000     D*---------------------
014700000000     D* DS TIPI AUTOMEZZI
014800000000     D*---------------------
014900000000     D DSTU          E DS
015000000000     D*---------------------
015100000000     D* DS CAUSALI CONTABILIZZAZIONE
015200000000     D*---------------------
015300000214     D ANGY4ZDS      E DS
015400000000     D*---------------------
015500000000     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
015600000000     D*---------------------
015700000000     D OG148         E DS
015800000518     D*---------------------
015900000518     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
016000000518     D*---------------------
016100000518     D OG143         E DS
016200971020     D*------------------
016300971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
016400971020     D*------------------
016500971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
016600010828
016700010828     d dscv          e ds
016800010828     d dged          e ds
016900010828     d tibs02ds      e ds
017000020716
017100020716     d DdatiUte      e ds
017200020716     d AzuteDs       e ds                  ExtName(Azute00f)
017300020716     d Tibs34Ds      e ds                  Inz
017400010828
017500971020     D*------------------
017600971020     D* DS "XSRDA8" - CONTROLLA DATA (8)
017700971020     D*------------------
017800971020     D WLBDA8          DS
017900971020     D  G08DAT                 1      8  0
018000971020     D  G08INV                 9     16  0
018100971020     D  G08ERR                17     17
018200971020     D  G08TGI                18     22  0
018300000000     D*------------------
018400000000     D* DS SCOMPOSIZIONE CAMPI VIDEO - MESSAGGIO VIDEO N° 24
018500000000     D*------------------
018600000000     D* MSG,24
018700000000     D                 DS
018800000000     D  ERRTFP                50     52
018900000000     D  ERRTFA                74     76
019000000000     D  ERRM24                 1     78
019100000821     D*------------------
019200000821     D* ARCHITETTURA
019300000214     D*------------------
019400000214     D KPJBA         E ds
019500000000     C*------------------------------------------------------------------------*
019600000000     C* MAIN LINE
019700000000     C*------------------------------------------------------------------------*
019800000000     C*
019900000000     C* MEMORIZZA TUTTI I DATI DEL TRAINO
020000000000     C                   EXSR      MEMTRN
020100000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
020200000000     C*
020300000000     C* ESEGUE I CONTROLLI DI CONGRUITA' FRA I DIVERSI ARCHIVI
020400000000SL  2C                   SELECT
020500000000     C* TRAINI
020600000000     C     PARGES        WHENEQ    'T'
020700000000     C                   EXSR      FERMAT                                       *CONGRUITA' FERMATE
020800000000     C                   EXSR      METRIC                                       *CONGRUITA' MCUBI
020900000000     C                   EXSR      LISTIN                                       *CONGRUITA' LISTINI
021000000000     C                   EXSR      NOTE                                         *CONGRUITA' NOTE
021100000000     C                   EXSR      TRAINI                                       *CONGRUITA' TRAINI
021200000000     C* FERMATE
021300000000     C     PARGES        WHENEQ    'F'
021400000000     C                   EXSR      FERMAT                                       *CONGRUITA' FERMATE
021500000000     C* METRICUBI
021600000000     C     PARGES        WHENEQ    'M'
021700000000     C                   EXSR      METRIC                                       *CONGRUITA' MCUBI
021800000000     C* LISTINI
021900000000     C     PARGES        WHENEQ    'L'
022000000000     C                   EXSR      LISTIN                                       *CONGRUITA' LISTINI
022100000000     C* NOTE
022200000000     C     PARGES        WHENEQ    'N'
022300000000     C                   EXSR      NOTE                                         *CONGRUITA' NOTE
022400000000E   2C                   ENDSL
022500000000E   1C                   ENDIF
022600971020     C* OPERAZIONI FINALI
022700971020     C                   EXSR      FINSR
022800971020     C*
022900000000     C                   SETON                                        LR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* TRAINI - CONTROLLO DI CONGRUITA' PER LA GESTIONE TRAINI
023200000000     C*------------------------------------------------------------------------*
023300000000     C     TRAINI        BEGSR
023400000000     C*
023500000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
023600000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDCE
023700000000     C                   MOVEL     'S'           $OK                            *RECORD OK
023800000000     C*
023900000000     C* LA FILIALE CHE ESPONE L'IMPORTO DEVE ESSERE UNA FERMATA DI CARICO
024000000000     C                   MOVEL     'N'           $OK
024100000000DO  2C     1             DO        TOTTLR        J
024200000000IF  3C     FFIL(J)       IFGT      *ZEROS
024300000000IF  4C     FFIL(J)       IFEQ      TLTFES
024400000000     C     FCAR(J)       ANDEQ     'S'
024500000000     C                   MOVEL     'S'           $OK                            *RECORD OK
024600000000     C                   LEAVE
024700000000E   4C                   ENDIF
024800000000E   3C                   ENDIF
024900000000E   2C                   ENDDO
025000000000IF  2C     $OK           IFEQ      'N'
025100000000     C                   MOVEL     '1'           PARERR
025200000000     C                   MOVEL     MSG(2)        PARMSG
025300000000E   2C                   ENDIF
025400000000E   1C                   ENDIF
025500000000     C*
025600000000     C                   ENDSR
025700000000     C*------------------------------------------------------------------------*
025800000000     C* FERMAT - CONTROLLO DI CONGRUITA' PER LA GESTIONE FERMATE
025900000000     C*------------------------------------------------------------------------*
026000000000     C     FERMAT        BEGSR
026100000000     C*
026200000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
026300000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
026400000000     C                   Z-ADD     *ZEROS        JJ                5 0          *INDICE
026500000000     C                   Z-ADD     *ZEROS        JI                5 0          *INDICE
026600000000     C                   Z-ADD     *ZEROS        DEPDDE                         *DEPOSITO DECORRENZA
026700000000     C                   MOVEL     'S'           $OK                            *RECORD OK
026800000000     C                   CLEAR                   JTFM                           *DI LAVORO
026900000000     C                   CLEAR                   JFIL
027000000000     C                   CLEAR                   JGFV
027100000000     C                   Z-ADD     *ZEROS        TO2TLR
027200000000     C***
027300000000     C* CONTROLLA I DATI CHE CI DEVONO / NON DEVONO ESSERE
027400000000     C***
027500000000     C* ----- ----- ----- -----
027600000000     C* ESISTONO LE FERMATE
027700000000     C* ----- ----- ----- -----
027800000000IF  2C     PARERR        IFEQ      '0'
027900000000IF  3C     FFIL(1)       IFEQ      *ZEROS
028000000000     C                   MOVEL     '1'           PARERR
028100000000     C                   MOVEL     MSG(11)       PARMSG
028200000000E   3C                   ENDIF
028300000000E   2C                   ENDIF
028400000000     C* ----- ----- ----- -----
028500000000     C* ESISTONO GLI ORARI DEI GIORNI DI PARTENZA PREVISTI DAL TRAINO
028600000000     C* ----- ----- ----- -----
028700000000IF  2C     PARERR        IFEQ      '0'
028800000000IF  3C     GCM(8)        IFNE      *BLANKS                                      *SI TUTTI
028900000000X   3C                   ELSE
029000000000DO  4C     1             DO        7             J
029100000000IF  5C     GIO(J)        IFNE      *BLANKS
029200000000     C     GCM(J)        ANDEQ     *BLANKS
029300000000     C                   MOVEL     '1'           PARERR
029400000000     C                   MOVEL     MSG(12)       PARMSG
029500000000E   5C                   ENDIF
029600000000E   4C                   ENDDO
029700000000E   3C                   ENDIF
029800000000E   2C                   ENDIF
029900000000     C* ----- ----- ----- -----
030000000000     C* NON ESISTONO GLI ORARI DEI GIORNI DI PARTENZA NON PREVISTI DAL TRAINO
030100000000     C* ----- ----- ----- -----
030200000000IF  2C     PARERR        IFEQ      '0'
030300000000DO  3C     1             DO        7             J
030400000000IF  4C     GIO(J)        IFEQ      *BLANKS
030500000000     C     GCM(J)        ANDNE     *BLANKS
030600000000     C                   MOVEL     '1'           PARERR
030700000000     C                   MOVEL     MSG(13)       PARMSG
030800000000E   4C                   ENDIF
030900000000E   3C                   ENDDO
031000000000E   2C                   ENDIF
031100000000     C***
031200000000     C* CONTROLLA I DATI CHE CI SONO
031300000000     C***
031400000000     C* ----- ----- ----- -----
031500000000     C* TUTTE LE FERMATE
031600000000     C* ----- ----- ----- -----
031700000000IF  2C     PARERR        IFEQ      '0'
031800000000     C                   Z-ADD     1             J
031900000000DO  3C     J             DOWLE     8                                            O R A R I
032000000000     C     PARERR        ANDEQ     '0'
032100000000IF  4C     GCM(J)        IFNE      *BLANKS
032200000000     C     J             OCCUR     OGSE
032300000000     C                   Z-ADD     1             JJ
032400000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
032500000000     C     PARERR        ANDEQ     '0'
032600000000     C* FERMATE DI TIPO "A"=ANDATA O "R"=RITORNO
032700000000IF  6C     FTFM(JJ)      IFNE      'A'
032800000000     C     FTFM(JJ)      ANDNE     'R'
032900000000     C                   MOVEL     '1'           PARERR
033000000000E   6C                   ENDIF
033100000000     C* GIORNO APERTURA F.V.= A,B,C ... SOLO SE FERMATA DI CARICO, ALTRIMENTI NO
033200000000IF  6C     FCAR(JJ)      IFEQ      'S'
033300000000     C                   SETOFF                                           28
033400000000     C     FGFV(JJ)      LOOKUP    ALF                                    28
033500000000IF  7C     *IN28         IFEQ      *OFF
033600000000     C                   MOVEL     '1'           PARERR
033700000000E   7C                   ENDIF
033800000000     C* GIORNO APERTURA F.V. MAGGIORE DEL PRECEDENTE (TRANNE LA PRIMA)
033900000000     C                   Z-ADD     *ZEROS        JI
034000000000IF  7C     JJ            IFNE      1
034100000000     C     JJ            SUB       1             JI
034200000000IF  8C     FGFV(JI)      IFGT      FGFV(JJ)
034300000000     C                   MOVEL     '1'           PARERR
034400000000E   8C                   ENDIF
034500000000E   7C                   ENDIF
034600000000X   6C                   ELSE
034700000000IF  7C     FGFV(JJ)      IFNE      *BLANKS
034800000000     C                   MOVEL     '1'           PARERR
034900000000E   7C                   ENDIF
035000000000E   6C                   ENDIF
035100000000     C* SE ARRIVO E PARTENZA PIENI, LA PARTENZA DEVE ESSERE > DELL'ARRIVO (G+HH)
035200000000IF  6C     OHHA(JJ)      IFGT      *ZEROS
035300000000     C     OHHP(JJ)      ANDGT     *ZEROS
035400000000IF  7C     OGGA(JJ)      IFGT      OGGP(JJ)                                     GG ARR > GG PAR   O
035500000000     C     OGGA(JJ)      OREQ      OGGP(JJ)                                     GG ARR = GG PAR  MA
035600000000     C     OHHA(JJ)      ANDGE     OHHP(JJ)                                     HH ARR >= GG PAR
035700000000     C                   MOVEL     '1'           PARERR
035800000000E   7C                   ENDIF
035900000000E   6C                   ENDIF
036000000000     C* SE INSERITA LA FERMATA, INSERIRE ANCHE QUALCHE DATO
036100000000IF  6C     FFIL(JJ)      IFGT      *ZEROS
036200000000IF  7C     OHHA(JJ)      IFEQ      *ZEROS
036300000000     C     OHHP(JJ)      ANDEQ     *ZEROS
036400000000     C     OGGA(JJ)      ANDEQ     *BLANKS
036500000000     C     OGGP(JJ)      ANDEQ     *BLANKS
036600000000     C                   MOVEL     '1'           PARERR
036700000000E   7C                   ENDIF
036800000000E   6C                   ENDIF
036900000000     C* SE INSERITO L'ORARIO, INSERIRE ANCHE LA FERMATA
037000000000IF  6C     FFIL(JJ)      IFEQ      *ZEROS
037100000000IF  7C     OHHA(JJ)      IFGT      *ZEROS
037200000000     C     OHHP(JJ)      ORGT      *ZEROS
037300000000     C     OGGA(JJ)      ORGT      *BLANKS
037400000000     C     OGGP(JJ)      ORGT      *BLANKS
037500000000     C                   MOVEL     '1'           PARERR
037600000000E   7C                   ENDIF
037700000000E   6C                   ENDIF
037800000000     C*
037900000000     C                   ADD       1             JJ
038000000000E   5C                   ENDDO
038100000000E   4C                   ENDIF
038200000000     C*
038300000000     C                   ADD       1             J
038400000000E   3C                   ENDDO
038500000000IF  3C     PARERR        IFEQ      '1'
038600000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
038700000000E   3C                   ENDIF
038800000000E   2C                   ENDIF
038900000000     C* ----- ----- ----- -----
039000000000     C* PRIMA E ULTIMA FERMATA
039100000000     C* ----- ----- ----- -----
039200000000IF  2C     PARERR        IFEQ      '0'
039300000000     C                   Z-ADD     1             J
039400000000DO  3C     J             DOWLE     8                                            O R A R I
039500000000     C     PARERR        ANDEQ     '0'
039600000000IF  4C     GCM(J)        IFNE      *BLANKS
039700000000     C     J             OCCUR     OGSE
039800000000     C* P R I M A
039900000000     C                   Z-ADD     1             JJ                             F E R M A T E
040000000000     C* TIPO FERMATA = 'A'
040100000000IF  5C     FTFM(JJ)      IFNE      'A'
040200000000     C                   MOVEL     '1'           PARERR
040300000000E   5C                   ENDIF
040400000000     C* DATI ARRIVO, KILOMETRI E TEMPO VUOTI
040500000000IF  5C     OGGA(JJ)      IFNE      *BLANKS
040600000000     C     OHHA(JJ)      ORNE      *ZEROS
040700000000     C     FSCA(JJ)      ORNE      *BLANKS
040800000000     C     FKMT(JJ)      ORNE      *ZEROS
040900000000     C     FTEM(JJ)      ORNE      *ZEROS
041000000000     C                   MOVEL     '1'           PARERR
041100000000E   5C                   ENDIF
041200000000     C* DATI PARTENZA PIENI
041300000000IF  5C     OGGP(JJ)      IFEQ      *BLANKS
041400000000     C     OHHP(JJ)      OREQ      *ZEROS
041500000000     C     FCAR(JJ)      OREQ      *BLANKS
041600000000     C                   MOVEL     '1'           PARERR
041700000000E   5C                   ENDIF
041800000000     C* GIORNO APERTURA F.V.='A'
041900000000IF  5C     FGFV(JJ)      IFNE      'A'
042000000000     C                   MOVEL     '1'           PARERR
042100000000E   5C                   ENDIF
042200000000     C* FILIALE PARTENZA = TFP DEL TRAINO
042300000000IF  5C     FFIL(JJ)      IFNE      TLTTFP
042400000000     C                   MOVEL     '1'           PARERR
042500000000E   5C                   ENDIF
042600000000     C* U L T I M A
042700000000     C                   Z-ADD     TOTTLR        JJ
042800000000     C* DATI PARTENZA VUOTI
042900000000IF  5C     OGGP(JJ)      IFNE      *BLANKS
043000000000     C     OHHP(JJ)      ORNE      *ZEROS
043100000000     C     FCAR(JJ)      ORNE      *BLANKS
043200000000     C                   MOVEL     '1'           PARERR
043300000000E   5C                   ENDIF
043400000000     C* DATI ARRIVO PIENI
043500000000IF  5C     OGGA(JJ)      IFEQ      *BLANKS
043600000000     C     OHHA(JJ)      OREQ      *ZEROS
043700000000     C     FSCA(JJ)      OREQ      *BLANKS
043800000000     C                   MOVEL     '1'           PARERR
043900000000E   5C                   ENDIF
044000000000     C* TRAINO CHIUSO TIPO FERMATA = 'R'
044100000000     C*    "      "   FILIALE ARRIVO = TFP DEL TRAINO
044200000000IF  5C     TLTTTR        IFEQ      'C'                                          *CHIUSO
044300000000IF  6C     FTFM(JJ)      IFNE      'R'
044400000000     C                   MOVEL     '1'           PARERR
044500000000E   6C                   ENDIF
044600000000IF  6C     FFIL(JJ)      IFNE      TLTTFP
044700000000     C                   MOVEL     '1'           PARERR
044800000000E   6C                   ENDIF
044900000000X   5C                   ELSE                                                   *NON CHIUSO
045000000000     C* TRAINO NON CHIUSO TIPO FERMATA = 'A'
045100000000     C*    "    "    "    FILIALE ARRIVO = TFA DEL TRAINO
045200000000IF  6C     FTFM(JJ)      IFNE      'A'
045300000000     C                   MOVEL     '1'           PARERR
045400000000E   6C                   ENDIF
045500000000IF  6C     FFIL(JJ)      IFNE      TLTTFA
045600000000     C                   MOVEL     '1'           PARERR
045700000000E   6C                   ENDIF
045800000000E   5C                   ENDIF
045900000000E   4C                   ENDIF
046000000000     C*
046100000000     C                   ADD       1             J
046200000000E   3C                   ENDDO
046300000000IF  3C     PARERR        IFEQ      '1'
046400000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
046500000000E   3C                   ENDIF
046600000000E   2C                   ENDIF
046700000000     C* ----- ----- ----- -----
046800000000     C* TIPO FERMATA
046900000000     C* ----- ----- ----- -----
047000000000IF  2C     PARERR        IFEQ      '0'
047100000000     C                   Z-ADD     1             J
047200000000DO  3C     J             DOWLE     8                                            O R A R I
047300000000     C     PARERR        ANDEQ     '0'
047400000000IF  4C     GCM(J)        IFNE      *BLANKS
047500000000     C     J             OCCUR     OGSE
047600000000     C                   CLEAR                   JFT                            *AZZERA CAMPI LAVORO
047700000000     C                   MOVEL     '0'           WSCHIA            1
047800000000     C                   MOVEL     '0'           WSCHIR            1
047900000000     C                   MOVEL     '0'           WNCHIA            1
048000000000     C                   MOVEL     '0'           WNCHIR            1
048100000000     C                   Z-ADD     *ZEROS        F                 4 0
048200000000     C                   Z-ADD     *ZEROS        JI
048300000000     C                   Z-ADD     1             JJ
048400000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
048500000000     C     PARERR        ANDEQ     '0'
048600000000     C* TIPI FERMATA ESISTENTI
048700000000IF  6C     TLTTTR        IFEQ      'C'                                          *C H I U S O
048800000000IF  7C     FTFM(JJ)      IFEQ      'R'
048900000000     C                   MOVEL     '1'           WSCHIR                         *CI SONO FERM RITORN
049000000000E   7C                   ENDIF
049100000000IF  7C     FTFM(JJ)      IFEQ      'A'
049200000000     C                   MOVEL     '1'           WSCHIA                         *CI SONO FERM ANDATA
049300000000E   7C                   ENDIF
049400000000X   6C                   ELSE                                                   *N O N   C H I U S O
049500000000IF  7C     FTFM(JJ)      IFEQ      'R'
049600000000     C                   MOVEL     '1'           WNCHIR                         *CI SONO FERM RITORN
049700000000E   7C                   ENDIF
049800000000IF  7C     FTFM(JJ)      IFEQ      'A'
049900000000     C                   MOVEL     '1'           WNCHIA                         *CI SONO FERM ANDATA
050000000000E   7C                   ENDIF
050100000000E   6C                   ENDIF
050200000000     C* LA STESSA FERMATA (FIL+TIPO) NON PUO' ESSERE RIPETUTA, TRANNE NEI LOCALI
050300000000     C* O NEI TRAINI INOLTRO DI TIPO LOCALE
050400000000IF  6C     TLTTTR        IFNE      'L'
050500000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
050600000000     C                   MOVEL     FFIL(JJ)      ALF4              4            *FIL + A/R
050700000000     C                   MOVE      FTFM(JJ)      ALF4
050800000000     C                   SETOFF                                       28
050900000000     C     ALF4          LOOKUP    JFT                                    28
051000000000IF  7C     *IN28         IFEQ      *ON                                          *TROVATO
051100000000     C                   MOVEL     '1'           PARERR
051200000000X   7C                   ELSE
051300000000     C                   ADD       1             F
051400000000     C                   MOVEL     ALF4          JFT(F)
051500000000E   7C                   ENDIF
051600000000E   6C                   ENDIF
051700000000     C* SE TRAINO CHIUSO LE FERMATE RITORNO DEVONO SEGUIRE LE ANDATA
051800000000     C* "     "      "   LE FERMATE ANDATA DEVONO PRECEDERE LE RITORNO
051900000000IF  6C     JJ            IFNE      1                                            *NO PRIMA FERMATA
052000000000     C     JJ            ANDNE     TOTTLR                                       *NO ULTIMA FERMATA
052100000000IF  7C     FTFM(JJ)      IFEQ      'A'
052200000000     C     JJ            SUB       1             JI
052300000000IF  8C     FTFM(JI)      IFEQ      'R'
052400000000     C                   MOVEL     '1'           PARERR
052500000000E   8C                   ENDIF
052600000000X   7C                   ELSE
052700000000     C     JJ            ADD       1             JI
052800000000IF  8C     FTFM(JI)      IFEQ      'A'
052900000000     C                   MOVEL     '1'           PARERR
053000000000E   8C                   ENDIF
053100000000E   7C                   ENDIF
053200000000E   6C                   ENDIF
053300000000     C*
053400000000     C                   ADD       1             JJ
053500000000E   5C                   ENDDO
053600000000     C* SE TRAINO CHIUSO, CI DEVONO ESSERE FERMATE DI ANDATA E RITORNO
053700000000     C* SE TRAINO NON CHIUSO, SI DEVONO ESSERE SOLO FERMATE DI ANDATA
053800000000IF  5C     TLTTTR        IFEQ      'C'                                          C H I U S O
053900000000IF  6C     WSCHIR        IFEQ      '1'
054000000000     C     WSCHIA        ANDEQ     '1'
054100000000X   6C                   ELSE
054200000000     C                   MOVEL     '1'           PARERR
054300000000E   6C                   ENDIF
054400000000X   5C                   ELSE                                                   N O N  CHIUSO
054500000000IF  6C     WNCHIR        IFEQ      '1'
054600000000     C     WNCHIA        OREQ      '0'
054700000000     C                   MOVEL     '1'           PARERR
054800000000E   6C                   ENDIF
054900000000E   5C                   ENDIF
055000000000E   4C                   ENDIF
055100000000     C*
055200000000     C                   ADD       1             J
055300000000E   3C                   ENDDO
055400000000IF  3C     PARERR        IFEQ      '1'
055500000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
055600000000E   3C                   ENDIF
055700000000E   2C                   ENDIF
055800000000     C* ----- ----- ----- -----
055900000000     C* FILIALE
056000000000     C* ----- ----- ----- -----
056100000000IF  2C     PARERR        IFEQ      '0'
056200000000     C                   Z-ADD     1             J
056300000000DO  3C     J             DOWLE     8                                            O R A R I
056400000000     C     PARERR        ANDEQ     '0'
056500000000IF  4C     GCM(J)        IFNE      *BLANKS
056600000000     C     J             OCCUR     OGSE
056700000000     C                   Z-ADD     1             JJ
056800000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
056900000000     C     PARERR        ANDEQ     '0'
057000090115     C                   Z-ADD     FFIL(JJ)      WORG
057100090115     C                   Z-ADD     *ZEROS        WFEL
057200090115     C                   Z-ADD     *ZEROS        WTFP
057300090115     C                   Z-ADD     *ZEROS        WTFA
057400090115     C                   MOVEL     *BLANKS       WPOS
057500090115     c                   clear                   wntw
057600090115     C                   EXSR      CHKFIL
057700090115     C                   Z-ADD     WFEL          DEPFEL
057800090115     C                   Z-ADD     WTFP          DEPTFP
057900090115     C                   Z-ADD     WTFA          DEPTFA
058000090115     C                   MOVEL     WPOS          DEPPOS
058100090115     C* network: memorizzo se estero o italia
058200090115     c                   clear                   depest            1
058300090115     c                   if        wntw='DPD' or wntw='FED' or wntw='EEX'
058400090115     c                   eval      depest='S'
058500090115     c                   endif
058600000000IF  6C     JJ            IFNE      1                                            *NO PRIMA
058700000000     C     JJ            ANDNE     TOTTLR                                       *NO ULTIMA
058800000000     C* DATI FILIALE
058900000000     C* A F F L U E N Z A
059000000000IF  7C     TLTTTR        IFEQ      'A'
059100000000IF  8C     FFIL(JJ)      IFNE      DEPFEL                                       *2° LIVELLO
059200000000IF  9C     DEPTFP        IFEQ      TLTTFA                                       *TFP = TFA DEL TRAIN
059300000519     C     DEPPOS        OREQ      'S'                                          *FILIALE POSTE
059400000000IF 10C     FCAR(JJ)      IFEQ      'S'                                          *SI CARICO  e
059500000000     C     FSCA(JJ)      ANDEQ     *BLANKS                                      *NO SCARICO
059600000000X  10C                   ELSE
059700000000     C                   MOVEL     '1'           PARERR
059800000000E  10C                   ENDIF
059900000000X   9C                   ELSE
060000000000     C                   MOVEL     '1'           PARERR
060100000000E   9C                   ENDIF
060200000000X   8C                   ELSE
060300000000     C                   MOVEL     '1'           PARERR
060400000000E   8C                   ENDIF
060500000000E   7C                   ENDIF
060600000000     C* D E F L U E N Z A
060700000000     C* L O C A L E
060800000000     C* I N O L T R O  -DEFLUENZA O LOCALE-
060900000000IF  7C     DEPDEF        IFEQ      'S'                                          *TIPO DEFLUENZA
061000000000     C     TLTTTR        OREQ      'L'                                          *LOCALE
061100000000     C     PARINL        OREQ      'D'                                          *INOLTRO DEFLUENZA
061200000000     C     PARINL        OREQ      'L'                                          *INOLTRO LOCALE
061300000000IF  8C     FSCA(JJ)      IFEQ      'S'                                          *SI SCARICO  e
061400000000     C     FCAR(JJ)      ANDEQ     *BLANKS                                      *NO CARICO
061500000000     C* ECCEZIONI
061600000000     C                   MOVEL     '0'           WECC              1
061700000000     C                   EXSR      CTRECC
061800000000IF  9C     DEPTFA        IFEQ      TLTTFP                                       *TERM ARR=TFP TRAINO
061900000000     C     WECC          OREQ      '1'                                          *TERM ARR ECC=TFP TR
062000000000IF 10C     FFIL(JJ)      IFNE      TLTTFP                                       *FIL <> TFP TRAINO
062100000000X  10C                   ELSE
062200000000     C                   MOVEL     '1'           PARERR
062300000000E  10C                   ENDIF
062400000000X   9C                   ELSE
062500000000     C                   MOVEL     '1'           PARERR
062600000000E   9C                   ENDIF
062700000000X   8C                   ELSE
062800000000     C                   MOVEL     '1'           PARERR
062900000000E   8C                   ENDIF
063000000000E   7C                   ENDIF
063100000000     C* SOLO  A N D A T A
063200000000     C* C H I U S O
063300000000     C* I N O L T R O  -SOLO ANDATA-
063400000000     C*---
063500000000IF  7C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
063600000000     C*---       TLTTTR    OREQ 'C'                        *CHIUSO
063700000000     C*---       PARINL    OREQ 'S'                        *INOLTRO SOLO ANDATA
063800000000     C*-ECCEZIONI
063900000000     C*---                 MOVEL'0'       WECC
064000000000     C*---                 EXSR CTREC2
064100000000IF  8C*---       FCAR,JJ   IFEQ 'S'                        SE FERMATA DI CARICO
064200000000     C*---       FFIL,JJ   ANDNEDEPFEL                     *FIL=1°LIVELLO
064300000000     C*---                 MOVEL'1'       PARERR
064400000000E   8C*---                 ENDIF
064500000000IF  8C*---       FSCA,JJ   IFEQ 'S'                        SE FERMATA DI SCARICO
064600000000IF  9C*---       FFIL,JJ   IFNE DEPTFA                     *FIL=TERM ARR OPPURE
064700000000     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
064800000000     C*---                 MOVEL'1'       PARERR
064900000000E   9C*---                 ENDIF
065000000000E   8C*---                 ENDIF
065100000000E   7C*---                 ENDIF
065200000000E   6C*---                 ENDIF
065300000000     C*
065400000000IF  7C     TLTTTR        IFEQ      'S'                                          *SOLO ANDATA
065500000000     C     TLTTTR        OREQ      'C'                                          *CHIUSO
065600000000     C     PARINL        OREQ      'S'                                          *INOLTRO SOLO ANDATA
065700000000     C* ECCEZIONI
065800000000     C                   MOVEL     '0'           WECC
065900000000     C                   Z-ADD     FFIL(JJ)      WNUM3B            3 0
066000000000     C                   EXSR      CTREC2
066100980610     C*---CASO PARTICOLARE
066200980610IF  8C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
066300980610     C*---       FFIL,JJ   ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
066400980610     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
066500980610IF  9C*---       FSCA,JJ   IFEQ 'S'                        *FERMATA DI SCARICO
066600980610     C*---       FCAR,JJ   ANDNE'S'                        *NO FERMATA DI CARICO
066700980610X   9C*---                 ELSE
066800980610     C*---                 MOVEL'1'       PARERR
066900980610E   9C*---                 ENDIF
067000980610X   8C*---                 ELSE                            *CASO NORMALE
067100980610     C*
067200980610IF  9C     FCAR(JJ)      IFEQ      'S'                                          SE FERMATA DI CARICO
067300000519IF 10C     FFIL(JJ)      IFEQ      DEPFEL                                       *FIL=1° LIVELLO
067400000519     C     DEPPOS        OREQ      'S'                                          *FILIALE POSTA
067500000519X  10C                   ELSE
067600980610     C                   MOVEL     '1'           PARERR
067700980610E  10C                   ENDIF
067800980610E   9C                   ENDIF
067900980610     C*
068000980610IF  9C*---       FSCA,JJ   IFEQ 'S'                        SE FERMATA DI SCARICO
068100980610     C*---       FFIL,JJ   ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
068200980610     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
068300980610     C*---                 MOVEL'1'       PARERR
068400980610E   9C*---                 ENDIF
068500980610E   8C*---                 ENDIF
068600000000E   7C                   ENDIF
068700000000E   6C                   ENDIF                                                  *NO PRIMA E NO ULTIM
068800000000     C* C H I U S O
068900000000IF  6C     TLTTTR        IFEQ      'C'
069000000000     C* L'ULTIMA FERMATA DI ANDATA  DEVE ESSERE = TFA DEL TRAINO
069100000000     C* E DI SOLO SCARICO
069200000000IF  7C     FTFM(JJ)      IFEQ      'A'
069300000000     C     JJ            ADD       1             JI                             *FERMATA SUCESSIVA
069400000000IF  8C     FTFM(JI)      IFEQ      'R'                                          *RITORNO
069500000000IF  9C     FFIL(JJ)      IFNE      TLTTFA
069600000000     C                   MOVEL     '1'           PARERR
069700000000E   9C                   ENDIF
069800000000IF  9C     FSCA(JJ)      IFEQ      *BLANKS
069900000000     C                   MOVEL     '1'           PARERR
070000000000E   9C                   ENDIF
070100000000     C* E I DATI PARTENZA DEVONO ESSERE VUOTI
070200000000IF  9C     OGGP(JJ)      IFNE      *BLANKS
070300000000     C     OHHP(JJ)      ORNE      *ZEROS
070400000000     C     FCAR(JJ)      ORNE      *BLANKS
070500000000     C                   MOVEL     '1'           PARERR
070600000000E   9C                   ENDIF
070700000000E   8C                   ENDIF
070800000000     C* LA PRIMA FERMATA DI RITORNO DEVE ESSERE = TFA DEL TRAINO
070900000000     C* E DI SOLO CARICO
071000000000X   7C                   ELSE
071100000000     C     JJ            SUB       1             JI                             *FERMATA PRECEDENTE
071200000000IF  8C     FTFM(JI)      IFEQ      'A'                                          *ANDATA
071300000000IF  9C     FFIL(JJ)      IFNE      TLTTFA
071400000000     C                   MOVEL     '1'           PARERR
071500000000E   9C                   ENDIF
071600000000IF  9C     FCAR(JJ)      IFEQ      *BLANKS
071700000000     C                   MOVEL     '1'           PARERR
071800000000E   9C                   ENDIF
071900000000     C* E I DATI ARRIVO DEVONO ESSERE VUOTI
072000000000IF  9C     OGGA(JJ)      IFNE      *BLANKS
072100000000     C     OHHA(JJ)      ORNE      *ZEROS
072200000000     C     FSCA(JJ)      ORNE      *BLANKS
072300000000     C                   MOVEL     '1'           PARERR
072400000000E   9C                   ENDIF
072500000000E   8C                   ENDIF
072600000000E   7C                   ENDIF
072700000000E   6C                   ENDIF
072800090115     C* C H I L O M E T R I - Solo se traino che paga la sede
072900090115     c*                       e solo se premuto F6
073000090115     C*
073100090508IF  6c***                if        deputc='S'
073200090115     c* prima fermata: memorizzo la filiale
073300090508IF  7c***                if        jj=1
073400090508     c***                z-add     ffil(jj)      wfilp
073500090508     c***                movel     depest        west
073600090508X   7c***                else
073700090115     c* dalla seconda fermata in poi se km = 0 errore
073800090115     c* se tratta da filiale estera a filiale estera no errore
073900090508IF  8c***                if        fkmt(jj)=0  and (west=' ' or depest=' ')
074000090508     C***                MOVEL     '1'           parerr
074100090508E   8c***                endif
074200090508     c***                z-add     ffil(jj)      wfilp
074300090508     c***                movel     depest        west
074400090508E   7c***                endif
074500090508E   6c***                endif
074600000000     C*
074700000000     C                   ADD       1             JJ
074800000000E   5C                   ENDDO
074900000000E   4C                   ENDIF
075000000000     C*
075100000000     C                   ADD       1             J
075200000000E   3C                   ENDDO
075300000000IF  3C     PARERR        IFEQ      '1'
075400000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
075500000000E   3C                   ENDIF
075600000000E   2C                   ENDIF
075700000000     C* ----- ----- ----- -----
075800000000     C* ORARI
075900000000     C* ----- ----- ----- -----
076000000000IF  2C     PARERR        IFEQ      '0'
076100000000     C                   Z-ADD     1             J
076200000000DO  3C     J             DOWLE     8                                            O R A R I
076300000000     C     PARERR        ANDEQ     '0'
076400000000IF  4C     GCM(J)        IFNE      *BLANKS
076500000000     C     J             OCCUR     OGSE
076600000000     C                   Z-ADD     1             JJ
076700000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
076800000000     C     PARERR        ANDEQ     '0'
076900000000     C* A R R I V O
077000000000     C* PROGRESSIVO GIORNO A,B,C ...
077100000000IF  6C     OGGA(JJ)      IFNE      *BLANKS
077200000000     C     OGGA(JJ)      LOOKUP    ALF                                    28
077300000000IF  7C     *IN28         IFEQ      *OFF
077400000000     C                   MOVEL     '1'           PARERR
077500000000E   7C                   ENDIF
077600000000E   6C                   ENDIF
077700000000     C* ORA IN HH:MM
077800000000IF  6C     OHHA(JJ)      IFNE      *ZEROS
077900000000     C                   Z-ADD     OHHA(JJ)      WORA
078000000000     C                   EXSR      CHKORA
078100000000E   6C                   ENDIF
078200000000     C* SCARICO
078300000000IF  6C     FSCA(JJ)      IFNE      *BLANKS
078400000000IF  7C     FSCA(JJ)      IFNE      'S'
078500000000     C                   MOVEL     '1'           PARERR
078600000000E   7C                   ENDIF
078700000000E   6C                   ENDIF
078800000000     C* SE SCARICO='S' ANCHE GG E ORA ARRIVO PIENI
078900000000IF  6C     FSCA(JJ)      IFNE      *BLANKS
079000000000IF  7C     OGGA(JJ)      IFEQ      *BLANKS
079100000000     C     OHHA(JJ)      OREQ      *ZEROS
079200000000     C                   MOVEL     '1'           PARERR
079300000000E   7C                   ENDIF
079400000000E   6C                   ENDIF
079500000000     C* SE GG O ORA ARRIVO PIENI ANCHE LO SCARICO = 'S'
079600000000IF  6C     OGGA(JJ)      IFNE      *BLANKS
079700000000     C     OHHA(JJ)      ORGT      *ZEROS
079800000000IF  7C     FSCA(JJ)      IFEQ      *BLANKS
079900000000     C                   MOVEL     '1'           PARERR
080000000000E   7C                   ENDIF
080100000000E   6C                   ENDIF
080200000000     C* P A R T E N Z A
080300000000     C* PROGRESSIVO GIORNO A,B,C ...
080400000000IF  6C     OGGP(JJ)      IFNE      *BLANKS
080500000000     C                   SETOFF                                           28
080600000000     C     OGGP(JJ)      LOOKUP    ALF                                    28
080700000000IF  7C     *IN28         IFEQ      *OFF
080800000000     C                   MOVEL     '1'           PARERR
080900000000E   7C                   ENDIF
081000000000E   6C                   ENDIF
081100000000     C* PROGRESSIVO GIORNO MAGGIORE/UGUALE GIORNO APERTURA FOGLIO VIAGGIO
081200000000IF  6C     OGGP(JJ)      IFNE      *BLANKS
081300000000IF  7C     OGGP(JJ)      IFLT      FGFV(JJ)
081400000000     C                   MOVEL     '1'           PARERR
081500000000E   7C                   ENDIF
081600000000E   6C                   ENDIF
081700000000     C* ORA IN HH:MM
081800000000IF  6C     OHHP(JJ)      IFNE      *ZEROS
081900000000     C                   Z-ADD     OHHP(JJ)      WORA
082000000000     C                   EXSR      CHKORA
082100000000E   6C                   ENDIF
082200000000     C* CARICO
082300000000IF  6C     FCAR(JJ)      IFNE      *BLANKS
082400000000IF  7C     FCAR(JJ)      IFNE      'S'
082500000000     C                   MOVEL     '1'           PARERR
082600000000E   7C                   ENDIF
082700000000E   6C                   ENDIF
082800000000     C* SE CARICO='S' ANCHE GG E ORA PARTENZA PIENI
082900000000IF  6C     FCAR(JJ)      IFNE      *BLANKS
083000000000IF  7C     OGGP(JJ)      IFEQ      *BLANKS
083100000000     C     OHHP(JJ)      OREQ      *ZEROS
083200000000     C                   MOVEL     '1'           PARERR
083300000000E   7C                   ENDIF
083400000000E   6C                   ENDIF
083500000000     C* SE GG O ORA PARTENZA PIENI ANCHE IL CARICO = 'S'
083600000000IF  6C     OGGP(JJ)      IFNE      *BLANKS
083700000000     C     OHHP(JJ)      ORGT      *ZEROS
083800000000IF  7C     FCAR(JJ)      IFEQ      *BLANKS
083900000000     C                   MOVEL     '1'           PARERR
084000000000E   7C                   ENDIF
084100000000E   6C                   ENDIF
084200000000     C* TEMPO
084300030715IF  6C**!!!FTEM(JJ)      IFGT      *ZEROS
084400030715     C**!!!              Z-ADD     FTEM(JJ)      WORA
084500030715     C**!!!              EXSR      CHKORA
084600030715E   6C**!!!              ENDIF
084700000000     C*
084800000000     C                   ADD       1             JJ
084900000000E   5C                   ENDDO
085000000000E   4C                   ENDIF
085100000000     C*
085200000000     C                   ADD       1             J
085300000000E   3C                   ENDDO
085400000000IF  3C     PARERR        IFEQ      '1'
085500000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
085600000000E   3C                   ENDIF
085700000000E   2C                   ENDIF
085800000000     C* ----- ----- ----- -----
085900000000     C* ALTRI CONTROLLI
086000000000     C* ----- ----- ----- -----
086100000000IF  2C*---       PARERR    IFEQ '0'
086200000000     C*---                 Z-ADD1         J
086300000000DO  3C*---       J         DOWLE8                          O R A R I
086400000000     C*---       PARERR    ANDEQ'0'
086500000000IF  4C*---       GCM,J     IFNE *BLANKS
086600000000     C*---       J         OCUR OGSE
086700000000     C*---                 Z-ADD*ZEROS    A       40
086800000000     C*---                 Z-ADD1         P       40       *AZZERA CAMPI LAVORO
086900000000DO  5C*---       P         DOWLETOTTLR
087000000000     C*---       PARERR    ANDEQ'0'
087100000000     C* TUTTO QUELLO CHE PARTE DEVE ARRIVARE (ORA PARTENZA SEMPRE PIENO), TRANNE
087200000000     C* L'ULTIMA FERMATA OPPURE SE COMINCIANO LE FERMATE DI RITORNO ('A'->'R')
087300000000     C*---       P         ADD  1         A
087400000000IF  6C*---       P         IFEQ TOTTLR                     *ULTIMA FERMATA
087500000000     C*---       FTFM,P    ORNE FTFM,A                     *ANDATA --> RITORNO
087600000000X   6C*---                 ELSE
087700000000IF  7C*---       OGGP,P    IFEQ *BLANKS
087800000000     C*---       OHHP,P    OREQ *ZEROS
087900000000     C*---                 MOVEL'1'       PARERR
088000000000E   7C*---                 ENDIF
088100000000E   6C*---                 ENDIF
088200000000     C*---
088300000000     C*---                 ADD  1         P
088400000000E   5C*---                 ENDDO
088500000000E   4C*---                 ENDIF
088600000000     C*---
088700000000     C*---                 ADD  1         J
088800000000E   3C*---                 ENDDO
088900000000IF  3C*---       PARERR    IFEQ '1'
089000000000     C*---                 MOVELMSG,20    PARMSG           *MESSAGGIO GENGERICO
089100000000E   3C*---                 ENDIF
089200000000E   2C*---                 ENDIF
089300000000     C* ----- ----- ----- -----
089400000000     C* ALTRI CONTROLLI
089500000000     C* ----- ----- ----- -----
089600000000IF  2C     PARERR        IFEQ      '0'
089700000000     C                   Z-ADD     1             J
089800000000DO  3C     J             DOWLE     8                                            O R A R I
089900000000     C     PARERR        ANDEQ     '0'
090000000000IF  4C     GCM(J)        IFNE      *BLANKS
090100000000     C     J             OCCUR     OGSE
090200000000     C                   MOVEL     *BLANKS       WALF5A            5            *AZZERA CAMPI LAVORO
090300000000     C                   MOVEL     *BLANKS       WALF5P            5
090400000000     C                   Z-ADD     *ZEROS        AA                4 0
090500000000     C                   Z-ADD     *ZEROS        A                 4 0
090600000000     C                   Z-ADD     1             P                 4 0
090700000000DO  5C     P             DOWLE     TOTTLR                                       *PARTENZA
090800000000     C     PARERR        ANDEQ     '0'
090900000000     C     P             ADD       1             A
091000000000DO  6C     A             DOWLE     TOTTLR                                       *ARRIVO
091100000000     C     PARERR        ANDEQ     '0'
091200000000     C* CIASCUN ORARIO DI PARTENZA DEVE ESSERE MINORE DI CISCUN ORARIO DI ARRIVO
091300000000     C* SUCCESSIVO, TRANNE L'ULTIMA FERMATA O SE INIZIANO LE FERMATE DI RITORNO
091400000000     C     A             SUB       1             AA                             *ARRIVO PRECEDENTE
091500000000IF  7C     P             IFEQ      TOTTLR                                       *ULTIMA FERMATA
091600000000     C     FTFM(P)       ORNE      FTFM(A)                                      *ANDATA --> RITORNO
091700000000     C     FTFM(A)       ANDNE     FTFM(AA)
091800000000X   7C                   ELSE                                                   *ESCLUDE
091900000000IF  8C     OGGA(A)       IFNE      *BLANKS                                      *SE ARRIVO PIENO
092000000000     C     OHHA(A)       ANDNE     *ZEROS
092100000000     C                   MOVEL     OGGP(P)       WALF5P                         *PARTENZA='X+9999'
092200000000     C                   MOVE      OHHP(P)       WALF5P
092300000000     C                   MOVEL     OGGA(A)       WALF5A                         *ARRIVO='X+9999'
092400000000     C                   MOVE      OHHA(A)       WALF5A
092500000000IF  9C     WALF5P        IFGE      WALF5A
092600000000     C                   MOVEL     '1'           PARERR
092700000000E   9C                   ENDIF
092800000000E   8C                   ENDIF
092900000000E   7C                   ENDIF
093000000000     C*
093100000000     C                   ADD       1             A
093200000000E   6C                   ENDDO
093300000000     C*
093400000000     C                   ADD       1             P
093500000000E   5C                   ENDDO
093600000000E   4C                   ENDIF
093700000000     C*
093800000000     C                   ADD       1             J
093900000000E   3C                   ENDDO
094000000000IF  3C     PARERR        IFEQ      '1'
094100000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
094200000000E   3C                   ENDIF
094300000000E   2C                   ENDIF
094400000000     C* ----- ----- ----- -----
094500000000     C* ALTRI CONTROLLI
094600000000     C* ----- ----- ----- -----
094700000000IF  2C     PARERR        IFEQ      '0'
094800000000     C                   Z-ADD     1             J
094900000000DO  3C     J             DOWLE     8                                            O R A R I
095000000000     C     PARERR        ANDEQ     '0'
095100000000IF  4C     GCM(J)        IFNE      *BLANKS
095200000000     C     J             OCCUR     OGSE
095300000000     C                   MOVEL     *BLANKS       WALF5A            5            *AZZERA CAMPI LAVORO
095400000000     C                   MOVEL     *BLANKS       WALF5P            5
095500000000     C                   Z-ADD     2             P                 4 0
095600000000     C                   Z-ADD     *ZEROS        PP                4 0
095700000000DO  5C     P             DOWLE     TOTTLR                                       *PARTENZA
095800000000     C     PARERR        ANDEQ     '0'
095900000000IF  6C     OGGP(P)       IFNE      *BLANKS                                      *SE PARTENZA PIENA
096000000000     C     P             SUB       1             PP
096100000000DO  7C     PP            DOWGE     1                                            *PARTENZA-1
096200000000     C     PARERR        ANDEQ     '0'
096300000000     C* CIASCUN ORARIO DI PARTENZA (SE PIENO) DEVE ESSERE MAGGIORE DI CIASCUN
096400000000     C* ORARIO DI PARTENZA PRECEDENTE TRANNE LA PRIMA FERMATA
096500000000     C                   MOVEL     OGGP(P)       WALF5P                         *PARTENZA='X+9999'
096600000000     C                   MOVE      OHHP(P)       WALF5P
096700000000     C                   MOVEL     OGGP(PP)      WALF5A                         *PARTENZA-1='X+9999'
096800000000     C                   MOVE      OHHP(PP)      WALF5A
096900000000IF  8C     WALF5P        IFLE      WALF5A
097000000000     C                   MOVEL     '1'           PARERR
097100000000E   8C                   ENDIF
097200000000     C*
097300000000     C                   SUB       1             PP
097400000000E   7C                   ENDDO
097500000000E   6C                   ENDIF
097600000000     C*
097700000000     C                   ADD       1             P
097800000000E   5C                   ENDDO
097900000000E   4C                   ENDIF
098000000000     C*
098100000000     C                   ADD       1             J
098200000000E   3C                   ENDDO
098300000000IF  3C     PARERR        IFEQ      '1'
098400000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
098500000000E   3C                   ENDIF
098600000000E   2C                   ENDIF
098700000000     C* ----- ----- ----- -----
098800000000     C* ALTRI CONTROLLI
098900000000     C* ----- ----- ----- -----
099000000000IF  2C     PARERR        IFEQ      '0'
099100000000     C                   Z-ADD     1             J
099200000000DO  3C     J             DOWLE     8                                            O R A R I
099300000000     C     PARERR        ANDEQ     '0'
099400000000IF  4C     GCM(J)        IFNE      *BLANKS
099500000000     C     J             OCCUR     OGSE
099600000000     C                   MOVEL     *BLANKS       WALF5A            5            *AZZERA CAMPI LAVORO
099700000000     C                   MOVEL     *BLANKS       WALF5P            5
099800000000     C                   Z-ADD     2             A                 4 0
099900000000     C                   Z-ADD     *ZEROS        AA                4 0
100000000000DO  5C     A             DOWLE     TOTTLR                                       *ARRIVO
100100000000     C     PARERR        ANDEQ     '0'
100200000000IF  6C     OGGA(A)       IFNE      *BLANKS                                      *SE ARRIVO PIENO
100300000000     C     A             SUB       1             AA
100400000000DO  7C     AA            DOWGE     1                                            *ARRIVO-1
100500000000     C     PARERR        ANDEQ     '0'
100600000000     C* CIASCUN ORARIO DI ARRIVO (SE PIENO) DEVE ESSERE MAGGIORE DI CIASCUN
100700000000     C* ORARIO DI ARRIVO PRECEDENTE TRANNE LA PRIMA FERMATA
100800000000     C                   MOVEL     OGGA(A)       WALF5P                         *PARTENZA='X+9999'
100900000000     C                   MOVE      OHHA(A)       WALF5P
101000000000     C                   MOVEL     OGGA(AA)      WALF5A                         *PARTENZA-1='X+9999'
101100000000     C                   MOVE      OHHA(AA)      WALF5A
101200000000IF  8C     WALF5P        IFLE      WALF5A
101300000000     C                   MOVEL     '1'           PARERR
101400000000E   8C                   ENDIF
101500000000     C*
101600000000     C                   SUB       1             AA
101700000000E   7C                   ENDDO
101800000000E   6C                   ENDIF
101900000000     C*
102000000000     C                   ADD       1             A
102100000000E   5C                   ENDDO
102200000000E   4C                   ENDIF
102300000000     C*
102400000000     C                   ADD       1             J
102500000000E   3C                   ENDDO
102600000000IF  3C     PARERR        IFEQ      '1'
102700000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
102800000000E   3C                   ENDIF
102900000000E   2C                   ENDIF
103000000000     C***
103100000000     C* CONTROLLA LE FERMATE CON QUELLE DI ALTRE DECORRENZE
103200000000     C***
103300000000     C* ----- ----- ----- -----
103400000000     C* IL N° SEQUENZA DI FERMATE+TIPO NON VARIA AL VARIARE DELLE DECORRENZE
103500000000     C* ----- ----- ----- -----
103600000000     C* SOSPESO
103700000000     C* ----- ----- ----- -----
103800000000     C* I GG APERTURA FV DI FERMATE+TIPO NON VARIA AL VARIARE DELLE DECORRENZE
103900000000     C* ----- ----- ----- -----
104000000000     C* CARICA IN MEMORIA LE FERMATE DEL TRAINO CON DECORRENZA DIVERSA
104100000000IF  2C     PARERR        IFEQ      '0'
104200000000     C                   Z-ADD     *ZEROS        I
104300000000     C                   MOVEL     'N'           $OK
104400000000     C                   Z-ADD     PARTRN        KLRTRN                         *NUMERO TRAINO
104500000000     C     KE2TLR        SETLL     TNTLR01L
104600000000     C     KE2TLR        READE(N)  TNTLR01L                               99
104700000000DO  3C     *IN99         DOWEQ     *OFF
104800000000     C     $OK           ANDEQ     'N'
104900000000     C                   Z-ADD     TLRDDE        DEPDDE                         *DEPOSITO DECORRENZA
105000000000DO  4C     *IN99         DOWEQ     *OFF
105100000000     C     DEPDDE        ANDEQ     TLRDDE
105200000000IF  5C     TLRATB        IFEQ      *BLANKS                                      *NO ANNULLATI
105300000000     C     TLRDDE        ANDNE     PARDDE                                       *NO TRAINO IN CHK
105400000000     C     TLRGFV        ANDNE     ' '
105500000000     C                   ADD       1             I
105600000000     C                   MOVEL     TLRTFM        JTFM(I)
105700000000     C                   Z-ADD     TLRFIL        JFIL(I)
105800000000     C                   MOVEL     TLRGFV        JGFV(I)
105900000000     C                   MOVEL     'S'           $OK
106000000000E   5C                   ENDIF
106100000000     C     KE2TLR        READE(N)  TNTLR01L                               99
106200000000E   4C                   ENDDO
106300000000     C                   Z-ADD     I             TO2TLR
106400000000E   3C                   ENDDO
106500000000     C*
106600000000     C* SE C'E' UN TRAINO CON DECORRENZA DIVERSA, CONTROLLA I GIORNI APERTURA FV
106700000000IF  3C     $OK           IFEQ      'S'
106800000000DO  4C     1             DO        TOTTLR        J
106900000000DO  5C     1             DO        TO2TLR        I
107000000000IF  6C     FFIL(J)       IFEQ      JFIL(I)                                      *STESSA FERMATA+TIPO
107100000000     C     FTFM(J)       ANDEQ     JTFM(I)
107200000000     C     FGFV(J)       ANDNE     ' '
107300000000IF  7C     FGFV(J)       IFNE      JGFV(I)                                      *GG APERTURA DIVERSO
107400000000     C                   MOVEL     '1'           PARERR
107500000000     C                   MOVEL     MSG(14)       PARMSG
107600000000E   7C                   ENDIF
107700000000E   6C                   ENDIF
107800000000E   5C                   ENDDO
107900000000E   4C                   ENDDO
108000000000E   3C                   ENDIF
108100000000E   2C                   ENDIF
108200000000     C*
108300000000E   1C                   ENDIF
108400000000     C*
108500000000     C                   ENDSR
108600000000     C*------------------------------------------------------------------------*
108700000000     C* METRIC - CONTROLLO DI CONGRUITA' PER LA GESTIONE MCUBI ASSEGNATI
108800000000     C*------------------------------------------------------------------------*
108900000000     C     METRIC        BEGSR
109000000000     C*
109100000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
109200000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
109300000000     C                   MOVEL     'S'           $OK                            *RECORD OK
109400000000     C***
109500000000     C* CONTROLLA I DATI CHE CI DEVONO / NON DEVONO ESSERE
109600000000     C***
109700000000     C* ----- ----- ----- -----
109800000000     C* ESISTONO I METRICUBI
109900000000     C* ----- ----- ----- -----
110000000000IF  2C     PARERR        IFEQ      '0'
110100000000IF  3C     MTFP(1)       IFEQ      *ZEROS
110200000000     C                   MOVEL     '1'           PARERR
110300000000     C                   MOVEL     MSG(21)       PARMSG
110400000000E   3C                   ENDIF
110500000000E   2C                   ENDIF
110600000000     C* ----- ----- ----- -----
110700000000     C* LE FILIALE FERMATE CHE CARICANO DEVONO ESSERE NEI MCUBI COME PARTENZA
110800000000     C* ----- ----- ----- -----
110900000000IF  2C     PARERR        IFEQ      '0'
111000000000DO  3C     1             DO        TOTTLR        JJ                             FERMATE
111100000000IF  4C     FCAR(JJ)      IFEQ      'S'
111200000000     C                   MOVEL     'N'           $OK
111300000000     C                   Z-ADD     1             J
111400000000DO  5C     J             DO        TOTTLM        J                              METRICUBI
111500000000IF  6C     FFIL(JJ)      IFEQ      MTFP(J)
111600000000     C                   MOVEL     'S'           $OK
111700000000E   6C                   ENDIF
111800000000E   5C                   ENDDO
111900000000E   5C     $OK           IFEQ      'N'
112000000000     C                   MOVEL     '1'           PARERR
112100000000     C                   MOVEL     MSG(22)       PARMSG
112200000000E   5C                   ENDIF
112300000000E   4C                   ENDIF
112400000000E   3C                   ENDDO
112500000000E   2C                   ENDIF
112600000000     C* ----- ----- ----- -----
112700000000     C* LE FILIALE FERMATE CHE SCARICANO DEVONO ESSERE NEI MCUBI COME ARRIVO
112800000000     C* ----- ----- ----- -----
112900000000IF  2C     PARERR        IFEQ      '0'
113000000000DO  3C     1             DO        TOTTLR        JJ                             FERMATE
113100000000IF  4C     FSCA(JJ)      IFEQ      'S'
113200000000     C                   MOVEL     'N'           $OK
113300000000     C                   Z-ADD     1             J
113400000000DO  5C     J             DO        TOTTLM        J                              METRICUBI
113500000000IF  6C     FFIL(JJ)      IFEQ      MTFA(J)
113600000000     C                   MOVEL     'S'           $OK
113700000000E   6C                   ENDIF
113800000000E   5C                   ENDDO
113900000000E   5C     $OK           IFEQ      'N'
114000000000     C                   MOVEL     '1'           PARERR
114100000000     C                   MOVEL     MSG(23)       PARMSG
114200000000E   5C                   ENDIF
114300000000E   4C                   ENDIF
114400000000E   3C                   ENDDO
114500000000E   2C                   ENDIF
114600000000     C* ----- ----- ----- -----
114700000000     C* BISOGNA ASSEGNARE I MC CON TLMTFP=TLTTFP, TLMTFA=TLTTFA, TLMTFF=TLTTFA
114800000000     C* ----- ----- ----- -----
114900000000IF  2C     PARERR        IFEQ      '0'
115000000000     C                   Z-ADD     1             J
115100000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
115200000000DO  3C     J             DO        TOTTLM        J                              METRICUBI
115300000000IF  4C     MTFP(J)       IFEQ      TLTTFP
115400000000     C     MTFA(J)       ANDEQ     TLTTFA
115500000000     C     MTFF(J)       ANDEQ     TLTTFA
115600000000     C                   MOVEL     'S'           $OK
115700000000E   4C                   ENDIF
115800000000E   3C                   ENDDO
115900000000IF  3C     $OK           IFEQ      'N'
116000000000     C                   MOVEL     '1'           PARERR
116100000000     C                   MOVEL     MSG(24)       ERRM24
116200000000     C                   MOVEL     TLTTFP        ERRTFP
116300000000     C                   MOVEL     TLTTFA        ERRTFA
116400000000     C                   MOVEL     ERRM24        PARMSG
116500000000E   3C                   ENDIF
116600000000E   2C                   ENDIF
116700000000     C***
116800000000     C* CONTROLLA I DATI CHE CI SONO
116900000000     C***
117000000000IF  2C     PARERR        IFEQ      '0'
117100000000     C                   Z-ADD     1             J
117200000000DO  3C     J             DOWLE     TOTTLM                                       METRICUBI
117300000000     C     PARERR        ANDEQ     '0'
117400000000     C* ----- ----- ----- -----
117500000000     C* PARTENZA
117600000000     C* ----- ----- ----- -----
117700000000     C* ESISTENTE
117800000000     C                   MOVEL     MTFP(J)       WORG
117900000000     C                   EXSR      CHKFIL
118000000000     C* ESISTENTE COME FERMATA DI CARICO
118100000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
118200000000DO  4C     1             DO        TOTTLR        JJ
118300000000IF  5C     FFIL(JJ)      IFEQ      MTFP(J)
118400000000     C     FCAR(JJ)      ANDEQ     'S'
118500000000     C                   MOVEL     'S'           $OK                            *VALIDO
118600000000E   5C                   ENDIF
118700000000E   4C                   ENDDO
118800000000IF  4C     $OK           IFEQ      'N'
118900000000     C                   MOVEL     '1'           PARERR
119000000000E   4C                   ENDIF
119100000000     C* ----- ----- ----- -----
119200000000     C* ARRIVO FINALE
119300000000     C* ----- ----- ----- -----
119400000000     C* ESISTENTE
119500000000     C                   MOVEL     MTFF(J)       WORG
119600000000     C                   Z-ADD     *ZEROS        WFEL
119700000000     C                   Z-ADD     *ZEROS        WTFP
119800000000     C                   Z-ADD     *ZEROS        WTFA
119900000000     C                   EXSR      CHKFIL
120000000000     C* NO AFFLUENZA / NO DEFLUENZA
120100000000     C* TFF=TRMINAL DI ARRIVO DA ORGANIGRAMMA   OPPURE
120200000000     C* TFP+TFA+TFF= ESISTENTE COME ECCEZIONE
120300000000     C* NB: QUESTI CONTROLLI LI FA DOPO AVER CONTROLLATO TUTTE LE 3 FILIALI
120400000000     C*
120500000000     C* DEFLUENZA
120600000000     C* TFF = TFA
120700000000IF  4C     DEPDEF        IFEQ      'S'                                          *DEFLUENZA
120800000000     C     PARINL        OREQ      'D'                                          *INOLTRO DEFLUENZA
120900000000IF  5C     MTFF(J)       IFNE      MTFA(J)                                      *TFF=TFA
121000000000     C                   MOVEL     '1'           PARERR
121100000000E   5C                   ENDIF
121200000000E   4C                   ENDIF
121300000000     C* AFFLUENZA
121400000000     C* TFF = TFP
121500000000IF  4C     TLTTTR        IFEQ      'A'                                          *AFFLUENZA
121600000000IF  5C     MTFF(J)       IFNE      MTFP(J)                                      *TFF=TFP
121700000000     C                   MOVEL     '1'           PARERR
121800000000E   5C                   ENDIF
121900000000E   4C                   ENDIF
122000000000     C* MEMORIZZA SE E' UNA FERMATA DI SCARICO PER CONTROLLI SUCCESSIVI
122100000000     C                   MOVEL     '0'           $SCA
122200000000DO  4C     1             DO        TOTTLR        JJ
122300000000IF  5C     FFIL(JJ)      IFEQ      MTFF(J)
122400000000     C     FSCA(JJ)      ANDEQ     'S'
122500000000     C                   MOVEL     '1'           $SCA                           *VALIDO
122600000000E   5C                   ENDIF
122700000000E   4C                   ENDDO
122800000000     C* ----- ----- ----- -----
122900000000     C* SCARICO
123000000000     C* ----- ----- ----- -----
123100000000     C* ESISTENTE
123200000000     C                   MOVEL     MTFA(J)       WORG
123300000000     C                   EXSR      CHKFIL
123400000000     C                   Z-ADD     WTFP          DEPTFP
123500000000     C                   Z-ADD     WTFA          DEPTFA
123600000000     C* ESISTENTE COME FERMATA DI SCARICO
123700000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
123800000000DO  4C     1             DO        TOTTLR        JJ
123900000000IF  5C     FFIL(JJ)      IFEQ      MTFA(J)
124000000000     C     FSCA(JJ)      ANDEQ     'S'
124100000000     C                   MOVEL     'S'           $OK                            *VALIDO
124200000000E   5C                   ENDIF
124300000000E   4C                   ENDDO
124400000000IF  4C     $OK           IFEQ      'N'
124500000000     C                   MOVEL     '1'           PARERR
124600000000E   4C                   ENDIF
124700000000     C* SEQ. FERMATA DI ARRIVO > SEQ. FERMATA DI PARTENZA
124800000000IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
124900000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
125000000000     C                   CLEAR                   JSEP                           *SEQUENZE PART
125100000000     C                   CLEAR                   JTFP                           *TIPO FERMATA PART
125200000000     C                   CLEAR                   JSEA                           *SEQUENZE ARR
125300000000     C                   CLEAR                   JTFA                           *TIPO FERMATA ARR
125400000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
125500000000DO  5C     1             DO        TOTTLR        JJ
125600000000IF  6C     MTFA(J)       IFEQ      FFIL(JJ)
125700000000IF  7C     FTFM(JJ)      IFEQ      'A'                                          *ANDATA
125800000000     C                   Z-ADD     JJ            JSEA(1)                        *SCARICO
125900000000     C                   MOVEL     FTFM(JJ)      JTFA(1)
126000000000X   7C                   ELSE                                                   *RITORNO
126100000000     C                   Z-ADD     JJ            JSEA(2)                        *SCARICO
126200000000     C                   MOVEL     FTFM(JJ)      JTFA(2)
126300000000E   7C                   ENDIF
126400000000E   6C                   ENDIF
126500000000IF  6C     MTFP(J)       IFEQ      FFIL(JJ)
126600000000IF  7C     FTFM(JJ)      IFEQ      'A'                                          *ANDATA
126700000000     C                   Z-ADD     JJ            JSEP(1)                        *PARTENZA
126800000000     C                   MOVEL     FTFM(JJ)      JTFP(1)
126900000000X   7C                   ELSE                                                   *RITORNO
127000000000     C                   Z-ADD     JJ            JSEP(2)                        *PARTENZA
127100000000     C                   MOVEL     FTFM(JJ)      JTFP(2)
127200000000E   7C                   ENDIF
127300000000E   6C                   ENDIF
127400000000E   5C                   ENDDO
127500000000     C* ANDATA
127600000000IF  5C     JTFA(1)       IFEQ      JTFP(1)                                      *STESSO TIPO FERMATA
127700000000     C     JSEA(1)       ANDGT     JSEP(1)                                      *SEQ ARR > SEQ PART
127800000000     C                   MOVEL     'S'           $OK                            *VALIDO
127900000000E   5C                   ENDIF
128000000000     C* RITORNO
128100000000IF  5C     JTFA(2)       IFEQ      JTFP(2)                                      *STESSO TIPO FERMATA
128200000000     C     JSEA(2)       ANDGT     JSEP(2)                                      *SEQ ARR > SEQ PART
128300000000     C                   MOVEL     'S'           $OK                            *VALIDO
128400000000E   5C                   ENDIF
128500000000IF  5C     $OK           IFEQ      'N'
128600000000     C                   MOVEL     '1'           PARERR
128700000000E   5C                   ENDIF
128800000000E   4C                   ENDIF
128900000000     C*
129000000000     C* SE LA FILIALE ARRIVO FINALE E' UNA FERMATA DI SCARICO, LA FILIALE DI
129100000000     C* SCARICO DEVE ESSERE UGUALE ALLA FILIALE ARRIVO FINALE
129200000000IF  4C     $SCA          IFEQ      'S'
129300000000     C     MTFF(J)       ANDNE     MTFA(J)
129400000000     C                   MOVEL     '1'           PARERR
129500000000E   4C                   ENDIF
129600000000     C***
129700000000     C* IL CASO PARTICOALRE ADESSO SI RICONOSCE PIU' GUARDANDO L'ORGANIGRAMMA O
129800000000     C* LE ECCEZIONI (PERCHE' FA A PACCHE CON UN ALTRO CONTROLLO), MA GUARDANDO
129900000000     C* CHE SIA UN TRAINI DI SOLA ANDATA E CHE ARRIVO E SCARICO SIANO UGUALI.
130000000000     C***
130100000000     C*--- SE LA FILIALE DI SCARICO NON E' TERMINAL ARRIVO (ORG O ECC)
130200000000     C*--- SCARICO E ARRIVO DEVONO ESSERE UGUALI
130300000000     C*---
130400000000     C*---                 MOVEL'0'       WECC
130500000000     C*---                 Z-ADDMTFA,J    WNUM3B
130600000000     C*---                 EXSR CTREC2
130700000000     C*---                 MOVELWECC      DEPEC2  1
130800000000     C* CASO PARTICOLARE
130900000000IF  4C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
131000000000     C*---       MTFA,J    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
131100000000     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
131200000000IF  5C*---       MTFF,J    IFNE MTFA,J
131300000000     C*---                 MOVEL'1'       PARERR
131400000000E   5C*---                 ENDIF
131500000000E   4C*---                 ENDIF
131600000000     C***
131700000000     C* ARRIVO
131800000000     C***
131900000000     C* SE CASO PARTICOLARE -> (VEDI SOPRA) NON FA QUESTI CONTROLLI
132000000000     C* TFP+TFA+TFF= ESISTENTE COME ECCEZIONE
132100000000     C* SE NON C'E VERIFICO SE E' UN TRANSITO
132200000000     C* SE NON C'E' L'ECCEZIONE -> TFF=TRMINAL DI ARRIVO DA ORGANIGRAMMA
132300000000IF  4C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
132400000000     C*---       MTFA,J    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
132500000000     C*---       DEPEC2    ANDNE'1'                        *FIL=TERM ARR ECC
132600000000X   4C*---                 ELSE                            *CASO NORMALE
132700000000IF  4C     TLTTTR        IFEQ      'S'                                          *SOLO ANDATA
132800000000     C     MTFA(J)       ANDEQ     MTFF(J)                                      *ARRIVO=SCARICO
132900000000     C                   ELSE                                                   *CASO NORMALE
133000000000     C* COMINCIA I CONTROLLI
133100000000IF  5C     TLTTTR        IFNE      'A'                                          *NO AFFLUENZA
133200000000     C     DEPDEF        ANDNE     'S'                                          *NO DEFLUENZA
133300000000     C     PARINL        ANDNE     'D'                                          *NO INOLTRO DEFLUENZ
133400000000     C                   MOVEL     '0'           WECC
133500000000     C                   EXSR      CTREC3
133600000000IF  6C     PARERR        IFEQ      '0'
133700000000IF  7C     WECC          IFNE      '1'
133800000000     C                   MOVEL     MTFF(J)       WORG
133900000000     C                   Z-ADD     *ZEROS        WTFA
134000000000     C                   EXSR      CHKFIL
134100000000IF  8C     MTFF(J)       IFNE      WTFA                                         *TERM ARRIVO STANDAR
134200000000     C                   MOVEL     '1'           PARERR
134300000000E   8C                   ENDIF
134400000000     c*
134500000000     C* SE OK IL TERMINAL ARRIVO, CONTROLLO SE LA FILIALE DI SCARICO E' CORRETTA
134600000000IF  8C     PARERR        IFEQ      '0'
134700000000     c*
134800000000     C* CONTROLLA ECCEZIONE CONE SCARICO
134900000000     C                   MOVEL     '0'           WECC
135000000000     C                   EXSR      CTREC4
135100000000IF  9C     PARERR        IFEQ      '0'
135200000000     C*
135300000000     C* CONTROLLA ORGANIGRAMM
135400000000IF 10C     WECC          IFNE      '1'
135500000000     C                   MOVEL     MTFA(J)       WORG
135600000000     C                   Z-ADD     *ZEROS        WTFA
135700000000     C                   EXSR      CHKFIL
135800000000     C*
135900000000IF 11C     MTFA(J)       IFNE      WTFA                                         *TERM ARRIVO STANDAR
136000000000     C                   MOVEL     '1'           PARERR
136100000000E  11C                   ENDIF
136200000000E  10C                   ENDIF
136300000000E   9C                   ENDIF
136400000000E   8C                   ENDIF
136500000000E   7C                   ENDIF
136600000000E   6C                   ENDIF
136700000000E   5C                   ENDIF
136800000000E   4C                   ENDIF
136900000000     C***
137000000000     C* PARTENZA E SCARICO
137100000000     C***
137200000000     C* LA PARTENZA DEVE ESSERE <> DALLO SCARICO TRANNE CHE NEI LOCALI
137300000000IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
137400000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
137500000000IF  5C     MTFP(J)       IFEQ      MTFA(J)
137600000000     C                   MOVEL     '1'           PARERR
137700000000E   5C                   ENDIF
137800000000E   4C                   ENDIF
137900000000     C***
138000000000     C* PARTENZA E ARRIVO
138100000000     C***
138200000000     C* LA PARTENZA DEVE ESSERE <> DALL'ARRIVO TRANNE CHE NEI LOCALI
138300000000IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
138400000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
138500000000IF  5C     MTFP(J)       IFEQ      MTFF(J)
138600000000     C                   MOVEL     '1'           PARERR
138700000000E   5C                   ENDIF
138800000000E   4C                   ENDIF
138900000000     C***
139000000000     C* PARTENZA E ARRIVO E SCARICO
139100000000     C***
139200000000     C* NEL LOCALI LA PARTENZA DEVE ESSERE UGUALE ALL'ARRIVO E ALLO SCARICO
139300000000IF  4C     TLTTTR        IFEQ      'L'                                          *LOCALE
139400000000     C     PARINL        OREQ      'L'                                          *INOLTRO LOCALE
139500000000IF  5C     MTFP(J)       IFEQ      MTFF(J)
139600000000     C     MTFA(J)       ANDEQ     MTFF(J)
139700000000X   5C                   ELSE
139800000000     C                   MOVEL     '1'           PARERR
139900000000E   5C                   ENDIF
140000000000E   4C                   ENDIF
140100000000     C***
140200000000     C* ARRIVO E SCARICO
140300000000     C***
140400000000     C* NEGLI INOLTRI ARRIVO E SCARICO DEVONO ESSERE UGUALI (NO TRANSITI)
140500000000     C* SE IL TIPO TRAINO A CUI SI RICONDUCE E' UN SOLO ANDATA
140600000000IF  4C     TLTTTR        IFEQ      'I'                                          *INOLTRO
140700000000     C     PARINL        ANDEQ     'S'                                          *TIPO SOLO ANDATA
140800000000IF  5C     MTFA(J)       IFNE      MTFF(J)
140900000000     C                   MOVEL     '1'           PARERR
141000000000E   5C                   ENDIF
141100000000E   4C                   ENDIF
141200000000     C* ----- ----- ----- -----
141300000000     C* MC NORMALI / BIS
141400000000     C* ----- ----- ----- -----
141500000000IF  1C     TLTBIS        IFEQ      *BLANKS                                      *TRAINO NORMALE
141600000000IF  2C     MMCA(J)       IFEQ      *ZEROS
141700000000     C                   MOVEL     '1'           PARERR
141800000000E   2C                   ENDIF
141900000000X   1C                   ELSE                                                   *TRAINO BIS
142000000000IF  2C     MMCB(J)       IFEQ      *ZEROS
142100000000     C                   MOVEL     '1'           PARERR
142200000000E   2C                   ENDIF
142300000000E   1C                   ENDIF
142400000000     C* ----- ----- ----- -----
142500000000     C* MC ASSEGNATI
142600000000     C* ----- ----- ----- -----
142700000000IF  4C*---       TLTBIS    IFEQ 'E'                        *ECCEZIONALE
142800000000     C*---       MMCA,J    ANDEQ*ZEROS                     *MC ASS OBBLIGATORI
142900000000     C*---                 MOVEL'1'       PARERR
143000000000E   4C*---                 ENDIF
143100000000     C*
143200000000     C                   ADD       1             J
143300000000E   3C                   ENDDO
143400000000IF  3C     PARERR        IFEQ      '1'
143500000000     C                   MOVEL     MSG(30)       PARMSG
143600000000E   3C                   ENDIF
143700000000E   2C                   ENDIF
143800000000     C*
143900000000E   1C                   ENDIF
144000000000     C*
144100000000     C                   ENDSR
144200000000     C*------------------------------------------------------------------------*
144300000000     C* LISTIN - CONTROLLO DI CONGRUITA' PER LA GESTIONE LISTINI
144400000000     C*------------------------------------------------------------------------*
144500000000     C     LISTIN        BEGSR
144600000000     C*
144700000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
144800000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
144900000000     C                   MOVEL     'S'           $OK                            *RECORD OK
145000000000     C***
145100000000     C* CONTROLLA I DATI CHE CI DEVONO ESSERE
145200000000     C***
145300000000     C* ----- ----- ----- -----
145400000000     C* ESISTONO I LISTINI
145500000000     C* ----- ----- ----- -----
145600000000IF  2C     PARERR        IFEQ      '0'
145700000000IF  3C     LPDR(1)       IFEQ      *ZEROS
145800000000     C                   MOVEL     '1'           PARERR
145900000000     C                   MOVEL     MSG(31)       PARMSG
146000000000E   3C                   ENDIF
146100000000E   2C                   ENDIF
146200000000     C* ----- ----- ----- -----
146300000000     C*--- DEVE ESISTERE ALMENO UN LISTINO CON IL TIPO AUTOMEZZO UGUALE A QUELLO
146400000000     C*--- DEL TRAINO O UGUALE A '99' (LISTINO STANDARD)
146500000000     C* ----- ----- ----- -----
146600000000IF  2C*---       PARERR    IFEQ '0'                        *NO ERRORI
146700000000     C*---                 MOVEL'N'       $OK
146800000000     C*---                 Z-ADD1         J
146900000000DO  3C*---       J         DOWLETOTTLL                     LISTINI
147000000000IF  4C*---       LTMZ,J    IFEQ TLTTMZ
147100000000     C*---       LTMZ,J    OREQ '99'
147200000000     C*---                 MOVEL'S'       $OK
147300000000E   4C*---                 ENDIF
147400000000     C*---                 ADD  1         J
147500000000E   3C*---                 ENDDO
147600000000IF  3C*---       $OK       IFEQ 'N'
147700000000     C*---                 MOVEL'1'       PARERR
147800000000     C*---                 MOVELMSG,32    PARMSG
147900000000E   3C*---                 ENDIF
148000000000E   2C*---                 ENDIF
148100000000     C***
148200000000     C* CONTROLLA I DATI IMMESSI
148300000000     C***
148400000000IF  2C     PARERR        IFEQ      '0'                                          *NO ERRORI
148500000000     C                   Z-ADD     1             J
148600000000DO  3C     J             DOWLE     TOTTLL                                       LISTINI
148700000000     C     PARERR        ANDEQ     '0'
148800000000     C* ----- ----- ----- -----
148900000000     C* TIPO AUTOMEZZO
149000000000     C* ----- ----- ----- -----
149100000000     C                   MOVEL     LTMZ(J)       WTMZ
149200000000     C                   EXSR      CHKTMZ
149300000000     C* ----- ----- ----- -----
149400000000     C* TRAZIONISTA
149500000000     C* ----- ----- ----- -----
149600000000     C                   Z-ADD     LPDR(J)       WPDR
149700000000     C                   EXSR      CHKPDR
149800010907     C* ----- ----- ----- -----
149900010907     C* DIVISA
150000010907     C* ----- ----- ----- -----
150100010907     c                   if        ldiv(j) <> *blanks
150200010907     c                   exsr      chkdiv
150300010907     c                   else
150400010907     c                   movel     '1'           parerr
150500010907     c                   endif
150600000000     C* ----- ----- ----- -----
150700000000     C* IMPORTO DI LISTINO
150800000000     C* ----- ----- ----- -----
150900180102IF  4C     LILI(J)       IFle      *ZEROS
151000000000     C                   MOVEL     '1'           PARERR
151100000000E   4C                   ENDIF
151200010907      * controllo se ok i decimali
151300010907     c                   move      lili(j)       w_decimali
151400010907     c                   if        w_decimali > 0
151500010907     C                             and §cvfdc <> 'S'
151600010907     c                   eval      parerr = '1'
151700010907     c                   endif
151800000000     C* ----- ----- ----- -----
151900000000     C* IMPORTO DA ESPORRE SUL F.V.
152000000000     C* ----- ----- ----- -----
152100000000IF  4C     LIES(J)       IFEQ      *ZEROS
152200000000     C     LIES(J)       OREQ      LILI(J)
152300000000X   4C                   ELSE
152400000000     C                   MOVEL     '1'           PARERR
152500000000E   4C                   ENDIF
152600000000     C* ----- ----- ----- -----
152700000000     C* CAUSALE DI CONTABILIZZAZIONE
152800000000     C* ----- ----- ----- -----
152900020716     c                   If        DutLpo = 'S'
153000000000     C                   MOVEL     LCAU(J)       WCAU
153100000000     C                   EXSR      CHKCAU
153200000821E   4C                   ENDIF
153300010828
153400000000     C                   ADD       1             J
153500000000E   3C                   ENDDO
153600000000IF  3C     PARERR        IFEQ      '1'
153700000000     C                   MOVEL     MSG(40)       PARMSG
153800000000E   3C                   ENDIF
153900000000E   2C                   ENDIF
154000000000     C*
154100000000E   1C                   ENDIF
154200000000     C*
154300000000     C                   ENDSR
154400000000     C*------------------------------------------------------------------------*
154500000000     C* NOTE - CONTROLLO DI CONGRUITA' PER LA GESTIONE NOTE
154600000000     C*------------------------------------------------------------------------*
154700000000     C     NOTE          BEGSR
154800000000     C*
154900000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
155000000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
155100000000     C                   Z-ADD     *ZEROS        JJ                5 0          *INDICE
155200000000     C                   MOVEL     'S'           $OK                            *RECORD OK
155300000000     C***
155400000000     C* CONTROLLA I DATI IMMESSI
155500000000     C***
155600000000IF  2C     PARERR        IFEQ      '0'                                          *NO ERRORI
155700000000     C                   Z-ADD     1             J
155800000000DO  3C     J             DOWLE     TOTTLN                                       NOTE
155900000000     C     PARERR        ANDEQ     '0'
156000000000     C* ----- ----- ----- -----
156100000000     C* NUMERO FERMATA ESISTENTE
156200000000     C* ----- ----- ----- -----
156300000000IF  4C     NNFM(J)       IFNE      *ZEROS
156400000000     C                   Z-ADD     NNFM(J)       JJ
156500000000IF  5C     FFIL(JJ)      IFEQ      *ZEROS
156600000000     C                   MOVEL     '1'           PARERR
156700000000E   5C                   ENDIF
156800000000E   4C                   ENDIF
156900000000     C*-------------------
157000000000     C* PROGRESSIVO NOTA
157100000000     C*-------------------
157200000000IF  4C     NPNO(J)       IFEQ      *ZEROS
157300000000     C                   MOVEL     '1'           PARERR
157400000000E   4C                   ENDIF
157500000000     C*-------------------
157600000000     C* TESTO NOTA
157700000000     C*-------------------
157800000000IF  4C     NTNO(J)       IFEQ      *BLANKS
157900000000     C                   MOVEL     '1'           PARERR
158000000000E   4C                   ENDIF
158100000000     C*
158200000000     C                   ADD       1             J
158300000000E   3C                   ENDDO
158400000000IF  3C     PARERR        IFEQ      '1'
158500000000     C                   MOVEL     MSG(50)       PARMSG
158600000000E   3C                   ENDIF
158700000000E   2C                   ENDIF
158800000000     C*
158900000000E   1C                   ENDIF
159000000000     C*
159100000000     C                   ENDSR
159200000000     C*------------------------------------------------------------------------*
159300000000     C* CTRECC - CONTROLLA LE ECCEZIONI (1)
159400000000     C*------------------------------------------------------------------------*
159500000000     C     CTRECC        BEGSR
159600000000     C*
159700000000     C     *LOVAL        SETLL     AZCAE01L
159800000000     C                   READ      AZCAE01L                               99
159900000000DO  1C     *IN99         DOWEQ     *OFF
160000000000     C     WECC          ANDEQ     '0'
160100970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
160200970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
160300970717     C     CAETFP        ANDGT     *ZEROS
160400970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
160500970717     C     CAETFA        ANDGT     *ZEROS
160600970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
160700000000     C     CAETFA        ANDEQ     FFIL(JJ)                                     *FILIALE ARRIVO
160800000000     C     CAETFE        ANDEQ     TLTTFP                                       *TERM ARR ECC=TFP TR
160900000000     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
161000000000     C     CAEDSC        ANDGE     TLTDSC
161100000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
161200970717E   4C                   ENDIF
161300970717E   3C                   ENDIF
161400970717E   2C                   ENDIF
161500000000     C                   READ      AZCAE01L                               99
161600000000E   1C                   ENDDO
161700000000     C*
161800000000     C                   ENDSR
161900000000     C*------------------------------------------------------------------------*
162000000000     C* CTREC2 - CONTROLLA LE ECCEZIONI (2)
162100000000     C*------------------------------------------------------------------------*
162200000000     C     CTREC2        BEGSR
162300000000     C*
162400000000     C     *LOVAL        SETLL     AZCAE01L
162500000000     C                   READ      AZCAE01L                               99
162600000000DO  1C     *IN99         DOWEQ     *OFF
162700000000     C     WECC          ANDEQ     '0'
162800970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
162900970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
163000970717     C     CAETFP        ANDGT     *ZEROS
163100970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
163200970717     C     CAETFA        ANDGT     *ZEROS
163300970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
163400000000     C     CAETFE        ANDEQ     WNUM3B                                       *FIL ARRIVO/SCARICO
163500000000     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
163600000000     C     CAEDSC        ANDGE     TLTDSC
163700000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
163800970717E   4C                   ENDIF
163900970717E   3C                   ENDIF
164000970717E   2C                   ENDIF
164100000000     C                   READ      AZCAE01L                               99
164200000000E   1C                   ENDDO
164300000000     C*
164400000000     C                   ENDSR
164500000000     C*------------------------------------------------------------------------*
164600000000     C* CTREC3 - CONTROLLA LE ECCEZIONI (3)
164700000000     C*------------------------------------------------------------------------*
164800000000     C     CTREC3        BEGSR
164900000000     C*
165000000000     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
165100000000     C                   Z-ADD     MTFP(J)       KAETFP                         *PARTENZA
165200000000     C                   Z-ADD     MTFF(J)       KAETFA                         *ARRIVO
165300000000     C     KEYCAE        SETLL     AZCAE01L
165400000000     C     KEYCAE        READE     AZCAE01L                               99
165500000000DO  1C     *IN99         DOWEQ     *OFF
165600000000     C     WECC          ANDEQ     '0'
165700970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
165800970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
165900970717     C     CAETFP        ANDGT     *ZEROS
166000970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
166100970717     C     CAETFA        ANDGT     *ZEROS
166200970717IF  4C     CAEDDE        IFLE      TLTDDE                                       IN DECORRENZA
166300000000     C     CAEDSC        ANDGE     TLTDSC
166400970717IF  5C     CAETFE        IFEQ      MTFA(J)                                      *ECCEZ=SCARICO
166500000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
166600970717X   5C                   ELSE
166700970717IF  6C     CAETFE        IFEQ      MTFF(J)                                      *ECCEZ=ARRIVO FINALE
166800000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
166900970717X   6C                   ELSE
167000000000     C                   MOVEL     '1'           PARERR                         *ERRORE     EZIONE
167100970717E   6C                   ENDIF
167200970717E   5C                   ENDIF
167300000000     C*
167400000000     C* NON SI POSSONO ACCETTARE ECCEZIONI A CAVALLO DI UNA VALIDITA' TRAINO
167500970717X   4C                   ELSE
167600970717IF  5C     CAEDDE        IFGT      TLTDSC
167700000000     C     CAEDSC        ORLT      TLTDDE
167800970717X   5C                   ELSE
167900000000     C                   MOVEL     '1'           PARERR                         *ERRORE     EZIONE
168000970717E   5C                   ENDIF
168100970717E   4C                   ENDIF
168200000000E   3C                   ENDIF
168300000000E   2C                   ENDIF
168400000000     C     KEYCAE        READE     AZCAE01L                               99
168500000000E   1C                   ENDDO
168600000000     C*
168700000000     C                   ENDSR
168800000000     C*------------------------------------------------------------------------*
168900000000     C* CTREC4 - CONTROLLA LE ECCEZIONI (4)
169000000000     C*------------------------------------------------------------------------*
169100000000     C     CTREC4        BEGSR
169200000000     C*
169300000000     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
169400000000     C                   Z-ADD     MTFP(J)       KAETFP                         *PARTENZA
169500000000     C                   Z-ADD     MTFA(J)       KAETFA                         *SCARICO
169600000000     C     KEYCAE        SETLL     AZCAE01L
169700000000     C     KEYCAE        READE     AZCAE01L                               99
169800000000DO  1C     *IN99         DOWEQ     *OFF
169900000000     C     WECC          ANDEQ     '0'
170000970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
170100970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
170200970717     C     CAETFP        ANDGT     *ZEROS
170300970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
170400970717     C     CAETFA        ANDGT     *ZEROS
170500970717IF  4C     CAEDDE        IFLE      TLTDDE                                       IN DECORRENZA
170600000000     C     CAEDSC        ANDGE     TLTDSC
170700970717IF  5C     CAETFE        IFEQ      MTFA(J)                                      *ECCEZ=SCARICO
170800000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
170900970717X   5C                   ELSE
171000000000     C                   MOVEL     '1'           PARERR                         *ERRORE ECCEZIONE
171100970717E   5C                   ENDIF
171200000000     C*
171300000000     C* NON SI POSSONO ACCETTARE ECCEZIONI A CAVALLO DI UNA VALIDITA' TRAINO
171400970717X   4C                   ELSE
171500970717IF  5C     CAEDDE        IFGT      TLTDSC
171600000000     C     CAEDSC        ORLT      TLTDDE
171700970717X   5C                   ELSE
171800000000     C                   MOVEL     '1'           PARERR                         *ERRORE ECCEZIONE
171900970717E   5C                   ENDIF
172000970717E   4C                   ENDIF
172100970717E   3C                   ENDIF
172200000000E   2C                   ENDIF
172300000000     C     KEYCAE        READE     AZCAE01L                               99
172400000000E   1C                   ENDDO
172500000000     C*
172600000000     C                   ENDSR
172700000000     C*------------------------------------------------------------------------*
172800000000     C* CHKFIL - CONTROLLA LA FILIALE
172900000000     C*------------------------------------------------------------------------*
173000000000     C     CHKFIL        BEGSR
173100000000     C*
173200000519     C     *LIKE         DEFINE    ORGFIL        WORG                           *FILIALE
173300971128     C     *LIKE         DEFINE    ORGFIL        WFEL                           *FILIALE ELABORATORE
173400971020     C     *LIKE         DEFINE    ORGFIL        WTFP                           *TERMINAL PARTENZA
173500971020     C     *LIKE         DEFINE    ORGFIL        WTFA                           *TERMINAL ARRIVO
173600000519     C     *LIKE         DEFINE    CAEEPA        WPOS                           *FILIALE
173700090115     C     *LIKE         DEFINE    §ogntw        Wntw                           *CARICO/SCARICO
173800000000     C*
173900000000IF  1C     WORG          IFEQ      *ZEROS
174000000000     C                   MOVEL     '1'           PARERR
174100000000X   1C                   ELSE
174200000000     C                   SETOFF                                           28
174300000000     C                   Z-ADD     1             I
174400000000     C     WORG          LOOKUP    ORG(I)                                 28
174500000000IF  2C     *IN28         IFEQ      *OFF
174600000000     C                   MOVEL     '1'           PARERR
174700000000X   2C                   ELSE
174800000000     C                   Z-ADD     FEL(I)        WFEL
174900000000     C                   Z-ADD     TFP(I)        WTFP
175000000000     C                   Z-ADD     TFA(I)        WTFA
175100000519     C                   MOVEL     PORG(I)       WPOS
175200090115     c                   movel     ntw(I)        wntw
175300000519E   2C                   ENDIF
175400000000E   1C                   ENDIF
175500000000     C*
175600000000     C                   ENDSR
175700000000     C*------------------------------------------------------------------------*
175800000000     C* CHKORA - CONTROLLA L'ORA
175900000000     C*------------------------------------------------------------------------*
176000000000     C     CHKORA        BEGSR
176100000000     C*
176200000000     C     *LIKE         DEFINE    TLOHHA        WORA
176300000000     C                   Z-ADD     *ZEROS        WHH               2 0
176400000000     C                   Z-ADD     *ZEROS        WMM               2 0
176500000000     C* ORE E MINUTI
176600000000IF  1C     WORA          IFEQ      *ZEROS
176700000000     C                   MOVEL     '1'           PARERR
176800000000E   1C                   ENDIF
176900000000     C* ORE
177000000000     C                   MOVEL     WORA          WHH
177100000000IF  1C     WHH           IFGE      0                                            0 -> 24
177200000000     C     WHH           ANDLE     24
177300000000     C                   ELSE
177400000000     C                   MOVEL     '1'           PARERR
177500000000E   1C                   ENDIF
177600000000     C* MINUTI
177700000000     C                   MOVE      WORA          WMM
177800000000IF  1C     WMM           IFGE      0                                            0 -> 59
177900000000     C     WMM           ANDLE     59
178000000000     C                   ELSE
178100000000     C                   MOVEL     '1'           PARERR
178200000000E   1C                   ENDIF
178300000000     C* ORE 24:00
178400000000IF  1C     WHH           IFEQ      24                                           *SOLO 24:00
178500000000     C     WMM           ANDGT     0
178600000000     C                   MOVEL     '1'           PARERR
178700000000E   1C                   ENDIF
178800000000     C*
178900000000     C                   ENDSR
179000000000     C*------------------------------------------------------------------------*
179100000000     C* CHKTMZ - CONTROLLA IL TIPO AUTOMEZZO
179200000000     C*------------------------------------------------------------------------*
179300000000     C     CHKTMZ        BEGSR
179400000000     C*
179500000000     C     *LIKE         DEFINE    TLLTMZ        WTMZ                           *AUTOMEZZO
179600000000     C*
179700000000IF  1C     WTMZ          IFEQ      *BLANKS
179800000000     C                   MOVEL     '1'           PARERR
179900000000X   1C                   ELSE
180000000000     C                   SETOFF                                           28
180100000000     C     WTMZ          LOOKUP    TMZ                                    28
180200000000     C                   MOVEL     WTMZ          WALF1             1
180300000000IF  2C     *IN28         IFEQ      *ON
180400000000     C     WALF1         ANDNE     '9'
180500000000     C     WTMZ          OREQ      '99'
180600000000X   2C                   ELSE
180700000000     C                   MOVEL     '1'           PARERR
180800000000E   2C                   ENDIF
180900000000E   1C                   ENDIF
181000000000     C*
181100000000     C                   ENDSR
181200000000     C*------------------------------------------------------------------------*
181300000000     C* CHKPDR - CONTROLLA IL TRAZIONISTA
181400000000     C*------------------------------------------------------------------------*
181500000000     C     CHKPDR        BEGSR
181600000000     C*
181700000000     C     *LIKE         DEFINE    TLLPDR        WPDR
181800000000     C*
181900000821     C                   EVAL      KLZTIP = 'T '
182000000000     C                   MOVEL     WPDR          KLZPDR
182100000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
182200000000IF  1C     *IN28         IFEQ      *ON
182300000000     C     WPDR          ANDNE     9999999
182400000000     C                   MOVEL     '1'           PARERR
182500000000E   1C                   ENDIF
182600000000     C*
182700000000     C                   ENDSR
182800000000     C*------------------------------------------------------------------------*
182900000000     C* CHKCAU - CONTROLLA LA CAUSALE DI CONTABILIZZAZIONE
183000000000     C*------------------------------------------------------------------------*
183100000000     C     CHKCAU        BEGSR
183200000000     C*
183300000000     C     *LIKE         DEFINE    TLLCAU        WCAU
183400000000     C*
183500000000IF  1C     WCAU          IFEQ      *BLANKS
183600000000     C                   MOVEL     '1'           PARERR
183700000000X   1C                   ELSE
183800000000     C                   SETOFF                                           28
183900000000     C     WCAU          LOOKUP    CAU                                    28
184000000000IF  2C     *IN28         IFEQ      *OFF
184100000000     C                   MOVEL     '1'           PARERR
184200000000E   2C                   ENDIF
184300000000E   1C                   ENDIF
184400000000     C*
184500000000     C                   ENDSR
184600010828     C*------------------------------------------------------------------------*
184700010828     C* CHKDIV - CONTROLLA LA DIVISA
184800010828     C*          INPUT  -> Wdiv  - divisa
184900010828     C*          OUTPUT ->
185000010828     C*------------------------------------------------------------------------*
185100010828     c     CHKdiv        BEGSR
185200010828
185300010828      * ESISTENTE
185400010828     c                   clear                   dscv
185500020716     c                   Z-Add     1             KtbKut
185600010828     c                   movel     'CV'          ktbcod
185700010828     c                   movel(p)  ldiv(j)       ktbkey
185800010828     c     keytab        chain     tabel00f
185900010828     c                   if        not%found(tabel00f)
186000010828     c                   movel     '1'           parerr
186100010828     c                   endif
186200010828     c                   movel     tbluni        dscv
186300010828      * consentita nei traini
186400010828     c                   if        §cvtra <> 'S'
186500010828     c                   movel     '1'           parerr
186600010828     c                   endif
186700010828
186800010828     c                   ENDSR
186900000000     C*------------------------------------------------------------------------*
187000000000     C* MEMTRN - MEMORIZZA TUTTI I DATI DEL TRAINO
187100000000     C*------------------------------------------------------------------------*
187200000000     C     MEMTRN        BEGSR
187300000000     C*
187400000000     C* REIMPOSTA I CONTATORI RECORD
187500000000     C                   Z-ADD     *ZEROS        I                 5 0          *INDICE
187600000000     C                   Z-ADD     *ZEROS        TOTTLR            5 0          *N° TOTALE FERMATE
187700000000     C                   Z-ADD     *ZEROS        TOTTLM            5 0          *N° TOTALE MCUBI
187800000000     C                   Z-ADD     *ZEROS        TOTTLL            5 0          *N° TOTALE LISTINI
187900000000     C                   Z-ADD     *ZEROS        TOTTLN            5 0          *N° TOTALE NOTE
188000000000     C* TRAINO
188100000000     C                   Z-ADD     PARTRN        KLTTRN                         *NUMERO TRAINO
188200000000     C                   Z-ADD     PARDDE        KLTDDE                         *DATA DECORRENZA
188300000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
188400000000IF  1C     *IN99         IFEQ      *OFF
188500000000     C                   MOVEA     TLTGSE        GIO                            *GG PARTENZA TRAINO
188600020716     c                   Z-Add     1             KtbKut
188700000000     C                   MOVEL     'TV'          KTBCOD
188800000000     C                   MOVEL(P)  TLTTTR        KTBKEY
188900000000     C     KEYTAB        CHAIN     TABEL00F                           99
189000000000     C                   MOVEL     TBLUNI        DSTV
189100000000     C                   MOVEL     §TVDEF        DEPDEF                         *TRAINO DEFLUENZA
189200090115     C                   MOVEL     §TVutc        DEPutc
189300000000     C* FERMATE
189400000000     C                   Z-ADD     PARTRN        KLRTRN                         *NUMERO TRAINO
189500000000     C                   Z-ADD     PARDDE        KLRDDE                         *DATA DECORRENZA
189600000000     C     KEYTLR        SETLL     TNTLR01L
189700000000     C     KEYTLR        READE(N)  TNTLR01L                               99
189800000000     C                   Z-ADD     *ZEROS        I
189900000000DO  2C     *IN99         DOWEQ     *OFF
190000000000IF  3C     TLRATB        IFEQ      *BLANKS
190100000000     C                   ADD       1             I
190200000000     C                   MOVEL     TLRTFM        FTFM(I)
190300000000     C                   Z-ADD     TLRFIL        FFIL(I)
190400000000     C                   MOVEL     TLRSCA        FSCA(I)
190500000000     C                   MOVEL     TLRCAR        FCAR(I)
190600000000     C                   Z-ADD     TLRKMT        FKMT(I)
190700000000     C                   Z-ADD     TLRTEM        FTEM(I)
190800000000     C                   MOVEL     TLRGFV        FGFV(I)
190900000000E   3C                   ENDIF
191000000000     C     KEYTLR        READE(N)  TNTLR01L                               99
191100000000E   2C                   ENDDO
191200000000     C                   Z-ADD     I             TOTTLR
191300000000     C* ORARI
191400000000     C                   Z-ADD     PARTRN        KLOTRN                         *NUMERO TRAINO
191500000000     C                   Z-ADD     PARDDE        KLODDE                         *DATA DECORRENZA
191600000000     C     KEYTLO        SETLL     TNTLO02L
191700000000     C     KEYTLO        READE     TNTLO02L                               99
191800000000     C                   Z-ADD     *ZEROS        I
191900000000DO  2C     *IN99         DOWEQ     *OFF
192000000000     C                   MOVEL     TLOGSE        DEPGSE
192100000000     C                   Z-ADD     *ZEROS        I
192200000000DO  3C     *IN99         DOWEQ     *OFF
192300000000     C     TLOGSE        ANDEQ     DEPGSE
192400000000IF  4C     TLOATB        IFEQ      *BLANKS
192500000000IF  5C     TLOGSE        IFEQ      ' '
192600000000     C                   Z-ADD     8             WG                1 0          *GIORNO SETTIMANA
192700000000X   5C                   ELSE
192800000000     C                   MOVEL     TLOGSE        WG
192900000000E   5C                   ENDIF
193000000000     C     WG            OCCUR     OGSE
193100000000     C                   ADD       1             I
193200000000     C                   MOVEL     TLOGGA        OGGA(I)
193300000000     C                   Z-ADD     TLOHHA        OHHA(I)
193400000000     C                   MOVEL     TLOGGP        OGGP(I)
193500000000     C                   Z-ADD     TLOHHP        OHHP(I)
193600000000     C                   MOVEL     WG            GCM(WG)                        *ORARIO PRESENTE
193700000000     C                   MOVEL     *BLANKS       GCA(WG)
193800000000E   4C                   ENDIF
193900000000     C     KEYTLO        READE     TNTLO02L                               99
194000000000E   3C                   ENDDO
194100000000E   2C                   ENDDO
194200000000     C* MCUBI ASSEGNATI
194300000000     C                   Z-ADD     PARTRN        KLMTRN                         *NUMERO TRAINO
194400000000     C                   Z-ADD     PARDDE        KLMDDE                         *DATA DECORRENZA
194500000000     C     KEYTLM        SETLL     TNTLM01L
194600000000     C     KEYTLM        READE(N)  TNTLM01L                               99
194700000000     C                   Z-ADD     *ZEROS        I
194800000000DO  2C     *IN99         DOWEQ     *OFF
194900000000IF  3C     TLMATB        IFEQ      *BLANKS
195000000000     C                   ADD       1             I
195100000000     C                   Z-ADD     TLMTFP        MTFP(I)
195200000000     C                   Z-ADD     TLMTFA        MTFA(I)
195300000000     C                   Z-ADD     TLMTFF        MTFF(I)
195400000000     C                   Z-ADD     TLMMCA        MMCA(I)
195500000000     C                   Z-ADD     TLMMCB        MMCB(I)
195600000000     C                   Z-ADD     TLMMCT        MMCT(I)
195700000000E   3C                   ENDIF
195800000000     C     KEYTLM        READE(N)  TNTLM01L                               99
195900000000E   2C                   ENDDO
196000000000     C                   Z-ADD     I             TOTTLM
196100000000     C* LISTINI
196200000000     C                   Z-ADD     PARTRN        KLLTRN                         *NUMERO TRAINO
196300000000     C                   Z-ADD     PARDDE        KLLDDE                         *DATA DECORRENZA
196400010828     C     KEYTLL        SETLL     TITLL01L
196500010828     C     KEYTLL        READE(N)  TITLL01L                               99
196600000000     C                   Z-ADD     *ZEROS        I
196700000000DO  2C     *IN99         DOWEQ     *OFF
196800000000IF  3C     TLLATB        IFEQ      *BLANKS
196900000000     C                   ADD       1             I
197000000000     C                   Z-ADD     TLLPDR        LPDR(I)
197100000000     C                   MOVEL     TLLTMZ        LTMZ(I)
197200000000     C                   Z-ADD     TLLILI        LILI(I)
197300000000     C                   Z-ADD     TLLIES        LIES(I)
197400000000     C                   MOVEL     TLLCAU        LCAU(I)
197500010828     c                   movel     tlldiv        ldiv(i)
197600000000E   3C                   ENDIF
197700010828     C     KEYTLL        READE(N)  TITLL01L                               99
197800000000E   2C                   ENDDO
197900000000     C                   Z-ADD     I             TOTTLL
198000000000     C* NOTE
198100000000     C                   Z-ADD     PARTRN        KLNTRN                         *NUMERO TRAINO
198200000000     C                   Z-ADD     PARDDE        KLNDDE                         *DATA DECORRENZA
198300000000     C     KEYTLN        SETLL     TNTLN01L
198400000000     C     KEYTLN        READE(N)  TNTLN01L                               99
198500000000     C                   Z-ADD     *ZEROS        I
198600000000DO  2C     *IN99         DOWEQ     *OFF
198700000000IF  3C     TLNATB        IFEQ      *BLANKS
198800000000     C                   ADD       1             I
198900000000     C                   Z-ADD     TLNNFM        NNFM(I)
199000000000     C                   Z-ADD     TLNPNO        NPNO(I)
199100000000     C                   MOVEL     TLNTNO        NTNO(I)
199200000000E   3C                   ENDIF
199300000000     C     KEYTLN        READE(N)  TNTLN01L                               99
199400000000E   2C                   ENDDO
199500000000     C                   Z-ADD     I             TOTTLN
199600000000     C*
199700000000X   1C                   ELSE
199800000000     C                   MOVEL     '1'           PARERR
199900000000     C                   MOVEL     MSG(1)        PARMSG
200000000000     C                   MOVEL     '1'           PARERR
200100000000E   1C                   ENDIF
200200000000     C*
200300000000     C                   ENDSR
200400000000     C*------------------------------------------------------------------------*
200500000000     C* CARTAB - CARICA LE TABELLE
200600000000     C*------------------------------------------------------------------------*
200700000000     C     CARTAB        BEGSR
200800000000     C*--------------------
200900000000     C* FILIALI
201000000000     C*--------------------
201100000518     C                   CLEAR                   OG148
201200000518     C                   CLEAR                   OG143
201300000000     C                   Z-ADD     *ZEROS        I
201400000000     C     *LOVAL        SETLL     AZORG01L
201500000000     C                   READ      AZORG01L                               99
201600000000DO  1C     *IN99         DOWEQ     *OFF
201700000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
201800000000     C                   MOVEL     ORGDE8        OG148
201900000000IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
202000000000     C                   ADD       1             I
202100000000     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
202200000000     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
202300000518     C                   MOVEL     ORGDE3        OG143
202400000518     C                   MOVEL     §OGPT         PORG(I)                        *P.O. POSTE
202500090115     C                   MOVEL     §OGntw        ntw(I)                         *network
202600971020     C                   CLEAR                   DSLV55
202700971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
202800971020     C                   MOVEL     *BLANKS       D55TPT                         *PARTENZA/ARRIVO
202900971020     C                   Z-ADD     ORGFIL        D55LIN                         *LINEA
203000980603     C                   Z-ADD     PARDDE        D55DRF                         *DATA RIFERIMENTO
203100971020     C                   CALL      'FNLV55R'
203200971020     C                   PARM                    DSLV55
203300971128IF  4C     D55ERR        IFNE      *BLANKS
203400971128     C                   Z-ADD     ORGFIL        D55TFP
203500971128     C                   Z-ADD     ORGFIL        D55TFA
203600971128E   4C                   ENDIF
203700000518     C                   Z-ADD     D55TFA        TFA(I)                         *TERMINAL PARTENZA
203800000518     C                   Z-ADD     D55TFP        TFP(I)                         *TERMINAL PARTENZA
203900000518     C                   Z-ADD     D55TFP        FEL(I)                         *FILIALE ELABORATORE
204000000000E   3C                   ENDIF
204100000000E   2C                   ENDIF
204200000000     C                   READ      AZORG01L                               99
204300000000E   1C                   ENDDO
204400000000     C*--------------------
204500000000     C* TIPI AUTOMEZZI
204600000000     C*--------------------
204700000000     C                   Z-ADD     *ZEROS        I
204800020716     c                   Z-Add     1             KtbKut
204900000000     C                   MOVEL     'TU'          KTBCOD
205000000000     C     KE2TAB        SETLL     TABEL00F
205100000000     C     KE2TAB        READE     TABEL00F                               99
205200000000DO  1C     *IN99         DOWEQ     *OFF
205300000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
205400000000     C                   ADD       1             I
205500000000     C                   MOVEL     TBLKEY        TMZ(I)                         *CODICE
205600000000     C                   MOVEL     TBLUNI        DSTU
205700000000     C                   MOVEL     §TUDES        DTMZ(I)                        *DESCRIZIONE
205800000000E   2C                   ENDIF
205900000000     C     KE2TAB        READE     TABEL00F                               99
206000000000E   1C                   ENDDO
206100000000     C*--------------------
206200000000     C* CAUSALI CONTABILIZZAZIONE
206300000000     C*--------------------
206400020716     c                   If        DutLpo = 'S'
206500000821     C                   OPEN      ANATB01L
206600000000     C                   Z-ADD     *ZEROS        I
206700020716     c                   Z-Add     1             KtbKut
206800000218     C                   MOVE      KNSIF         WCOMO4            4
206900000218     C                   MOVEL     WCOMO4        WSOC              3
207000000218     C                   movel     WSOC          KAtazi
207100000214     C                   MOVEL     'Y4Z'         KatCOD
207200000214     C     KEATAB        CHAIN     ANATB01L                           99
207300000821IF  2C     *IN99         IFEQ      *ON
207400000218     C                   movel     '888'         KAtazi
207500000214     C     KEATAB        CHAIN     ANATB01L                           99
207600000821E   2C                   ENDIF
207700000821DO  2C     *IN99         DOWEQ     *OFF
207800000214     C                   ADD       1             I
207900000214     C                   MOVEL     ATBKEY        CAU(I)
208000000214     C                   MOVEL     ATBUNI        ANGY4ZDS
208100000214     C                   MOVEL     §4ZDES        DCAU(I)
208200000214     C     KEATAB        READE     ANATB01L                               99
208300000821E   2C                   ENDDO
208400000821     C                   CLOSE     ANATB01L
208500000821E   1C                   ENDIF
208600000000     C*--------------------
208700000000     C* ALFABETO
208800000000     C*--------------------
208900000000     C                   MOVEA     DALF          ALF
209000000000     C*
209100000000     C                   ENDSR
209200971020     C*------------------------------------------------------------------------*
209300971020     C* FINSR - OPERAZIONI FINALI
209400971020     C*------------------------------------------------------------------------*
209500971020     C     FINSR         BEGSR
209600971020     C* FNLV55R
209700971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
209800971020     C                   MOVEL     'C'           D55TLA
209900971020     C                   CALL      'FNLV55R'
210000971020     C                   PARM                    DSLV55
210100971020     C*
210200971020     C                   ENDSR
210300000000     C*------------------------------------------------------------------------*
210400000000     C* *INZSR - IMPOSTAZIONI INIZIALI
210500000000     C*------------------------------------------------------------------------*
210600000000     C     *INZSR        BEGSR
210700000000     C*--------------------
210800000000     C* RICEVIMENTO PARAMETRI
210900000000     C*--------------------
211000000000     C     *ENTRY        PLIST
211100000000     C                   PARM                    PARTRN                         *TRAINO
211200000000     C                   PARM                    PARDDE                         *DECORRENZA
211300000000     C                   PARM                    PARINL                         *TIPO TRAINO INOLTRO
211400000000     C                   PARM                    PARERR            1            *FLAG ERRORE
211500000000     C                   PARM                    PARMSG           80            *MESSAGGIO DI ERRORE
211600000000     C                   PARM                    PARGES            1            *GESTIONE
211700000214     C                   PARM                    KPJBA
211800000000     C*--------------------
211900000000     C* VARIABILI RIFERITE AL DATA BASE
212000000000     C*--------------------
212100000214     C     *LIKE         DEFINE    ATBCOD        KATCOD
212200000214     C     *LIKE         DEFINE    ATBAZI        KATAZI
212300000000     C     *LIKE         DEFINE    TLTTRN        PARTRN                         *PARAMETRI
212400000000     C     *LIKE         DEFINE    TLTDDE        PARDDE
212500000000     C     *LIKE         DEFINE    TLTTTR        PARINL
212600000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
212700000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
212800000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
212900000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
213000000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
213100000000     C     *LIKE         DEFINE    TLODDE        KLODDE
213200000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
213300000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
213400010828     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
213500000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
213600000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
213700000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
213800000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR                         *TNTLZ00F
213900000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
214000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
214100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
214200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
214300000000     C     *LIKE         DEFINE    CAEEPA        KAEEPA                         *AZCAE01L
214400000000     C     *LIKE         DEFINE    CAETFP        KAETFP
214500000000     C     *LIKE         DEFINE    CAETFA        KAETFA
214600000000     C     *LIKE         DEFINE    TLOGSE        DEPGSE                         *DEPOSITI
214700000000     C     *LIKE         DEFINE    TLRDDE        DEPDDE
214800000000     C     *LIKE         DEFINE    §TVDEF        DEPDEF
214900090115     C     *LIKE         DEFINE    §TVutc        DEPutc
215000090115     C     *LIKE         DEFINE    tlrfil        wfilp
215100000000     C*--------------------
215200000000     C* VARIABILI NON RIFERITE AL DATA BASE
215300000000     C*--------------------
215400000000     C     *LIKE         DEFINE    TOTTLR        TO2TLR                         *TOTALE DI LAVORO
215500000000     C     *LIKE         DEFINE    WFEL          DEPFEL                         *DEPOSITI
215600000000     C     *LIKE         DEFINE    WTFP          DEPTFP
215700000000     C     *LIKE         DEFINE    WTFA          DEPTFA
215800000519     C     *LIKE         DEFINE    WPOS          DEPPOS
215900090115     C     *LIKE         DEFINE    depest        West
216000000000     C*--------------------
216100000000     C* VARIABILI DI CONTROLLO
216200000000     C*--------------------
216300000000     C                   MOVEL     'S'           $OK               1            *RECORD VALIDO
216400000000     C                   MOVEL     'S'           $SCA              1            *FERMATA DI SCARICO
216500000000     C*--------------------
216600000000     C* CHIAVI DI LETTURA
216700000000     C*--------------------
216800000214     C* LETTURA ANATB01L - PARZIALE
216900000214     C     KEATAB        KLIST
217000000214     C                   KFLD                    KATCOD
217100000214     C                   KFLD                    KATAZI
217200000000     C* LETTURA TNTLT01L - COMPLETA
217300000000     C     KEYTLT        KLIST
217400000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
217500000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
217600000000     C* LETTURA TNTLR01L - PARZIALE
217700000000     C     KEYTLR        KLIST
217800000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
217900000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
218000000000     C* LETTURA TNTLR01L - PARZIALE
218100000000     C     KE2TLR        KLIST
218200000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
218300000000     C* LETTURA TNTLO01L - PARZIALE
218400000000     C     KEYTLO        KLIST
218500000000     C                   KFLD                    KLOTRN                         *NUMERO TRAINO
218600000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
218700000000     C* LETTURA TNTLM01L - PARZIALE
218800000000     C     KEYTLM        KLIST
218900000000     C                   KFLD                    KLMTRN                         *NUMERO TRAINO
219000000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
219100010828     C* LETTURA TITLL01L - PARZIALE
219200000000     C     KEYTLL        KLIST
219300000000     C                   KFLD                    KLLTRN                         *NUMERO TRAINO
219400000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
219500000000     C* LETTURA TNTLN01L - PARZIALE
219600000000     C     KEYTLN        KLIST
219700000000     C                   KFLD                    KLNTRN                         *NUMERO TRAINO
219800000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
219900000000     C* LETTURA TNTLZ01L - COMPLETA
220000000000     C     KEYTLZ        KLIST
220100000821     C                   KFLD                    KLZTIP                         *TIPO
220200000821     C                   KFLD                    KLZPDR                         *CODICE
220300000000     C* LETTURA TABEL00F - COMPLETA
220400000000     C     KEYTAB        KLIST
220500000000     C                   KFLD                    KTBKUT                         *UTENTE
220600000000     C                   KFLD                    KTBCOD                         *TABELLA
220700000000     C                   KFLD                    KTBKEY                         *CHIAVE
220800000000     C* LETTURA TABEL00F - PARZIALE
220900000000     C     KE2TAB        KLIST
221000000000     C                   KFLD                    KTBKUT                         *UTENTE
221100010830     C                   KFLD                    KTBCOD                         *TABELLA
221200000000     C* LETTURA AZCAE01L - PARZIALE
221300000000     C     KEYCAE        KLIST
221400000000     C                   KFLD                    KAEEPA                         *PARTENZA/ARRIVO
221500000000     C                   KFLD                    KAETFP                         *PARTENZA
221600000000     C                   KFLD                    KAETFA                         *ARRIVO
221700971020     C*
221800971020     C* CALCOLA LA DATA CORRENTE
221900971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
222000971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
222100971020     C                   Z-ADD     N8            G08DAT
222200971020     C                   Z-ADD     *ZEROS        G08INV
222300971020     C                   MOVEL     '0'           G08ERR
222400971020     C                   CALL      'XSRDA8'
222500971020     C                   PARM                    WLBDA8
222600971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
222700000000     C*
222800000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
222900000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
223000000000IF  1C     *IN99         IFEQ      *ON
223100000000     C     *IN99         ANDEQ     *OFF
223200000000     C* TRAINI
223300000000     C                   UPDATE    TNTLT000
223400000000     C                   WRITE     TNTLT000
223500000000     C* FERMATE
223600000000     C                   UPDATE    TNTLR000
223700000000     C                   WRITE     TNTLR000
223800000000     C* ORARI
223900000000     C                   READ      TNTLO000                               28
224000000000     C                   UPDATE    TNTLO000
224100000000     C                   WRITE     TNTLO000
224200000000     C* MCUBI ASSEGNATI
224300000000     C                   UPDATE    TNTLM000
224400000000     C                   WRITE     TNTLM000
224500000000     C* LISTINI
224600010828     C                   UPDATE    TITLL000
224700010828     C                   WRITE     TITLL000
224800000000     C* NOTE
224900000000     C                   UPDATE    TNTLN000
225000000000     C                   WRITE     TNTLN000
225100000000E   1C                   ENDIF
225200000000     C*
225300000000     C* REPERISCE I DATI UTENTE
225400020716
225500020716     c     *dtaara       define    §azute        azuteds
225600020716     c     *dtaara       define    §datiute      ddatiute
225700020716
225800020716     c                   in(E)     *dtaara
225900020716     c                   if        %Error or rsut = *blanks
226000020716     c                   clear                   tibs34ds
226100020716     c                   call      'TIBS34R'
226200020716     c                   parm                    Tibs34Ds
226300020716     c                   in        *dtaara
226400020716     c                   endif
226500000000     C*
226600000000     C* CARICA LE TABELLE OCCORRENTI
226700000000     C                   EXSR      CARTAB
226800000000     C*
226900000000     C                   ENDSR
227000000218     C*------------------------------------------------------------------------*
227100000218** MS    MESSAGGI DI ERRORE
227200000000ERR Traini: Traino inesistente                                                 1
227300000000ERR Traini: La filiale importo deve essere fermata di carico                   2
227400000000ERR Traini: 3
227500000000ERR Traini: 4
227600000000ERR Traini: 5
227700000000ERR Traini: 6
227800000000ERR Traini: 7
227900000000ERR Traini: 8
228000000000ERR Traini: 9
228100000000ERR Traini: 10
228200000000ERR Fermate: Inserire le fermate per il traino                                11
228300000000ERR Fermate: Inserire gli orari per tutti i giorni previsti dal traino        12
228400000000ERR Fermate: Non inserire gli orari dei giorni non previsti dal traino        13
228500000000ERR Fermate: I gg apertura FV non devono variare al variare della decorrenza  14
228600000000ERR Fermate: 15
228700000000ERR Fermate: 16
228800000000ERR Fermate: 17
228900000000ERR Fermate: 18
229000000000ERR Fermate: 19
229100000000ERR Fermate: Andare in GESTIONE FERMATE e ricontrollare i dati immessi        20
229200000000ERR Mcubi: Inserire i metricubi per il traino                                 21
229300000000ERR Mcubi: Le fermate che caricano devono essere nei MC come fil. partenza    22
229400000000ERR Mcubi: Le fermate che scaricano devono essere nei MC come fil. scarico    23
229500000000ERR Mcubi: Bisogna assegnare i Mcubi: partenza = 999, scarico e arrivo = 999  24
229600000000ERR Mcubi: 25
229700000000ERR Mcubi: 26
229800000000ERR Mcubi: 27
229900000000ERR Mcubi: 28
230000000000ERR Mcubi: 29
230100000000ERR Mcubi: Andare in GESTIONE MCUBI e ricontrollare i dati immessi            30
230200000000ERR Listini: Inserire almeno un listino                                       31
230300000000ERR Listini: Deve esistere un listino con tipo automezzo = al traino o = 99   32
230400000000ERR Listini: 33
230500000000ERR Listini: 34
230600000000ERR Listini: 35
230700000000ERR Listini: 36
230800000000ERR Listini: 37
230900000000ERR Listini: 38
231000000000ERR Listini: 39
231100000000ERR Listini: Andare in GESTIONE LISTINI e ricontrollare i dati immessi        40
231200000000ERR Note: 41
231300000000ERR Note: 42
231400000000ERR Note: 43
231500000000ERR Note: 44
231600000000ERR Note: 45
231700000000ERR Note: 46
231800000000ERR Note: 47
231900000000ERR Note: 48
232000000000ERR Note: 49
232100000000ERR Note: Andare in GESTIONE NOTE e ricontrollare i dati immessi              50
232200000000** -DALF- ALFABETO
232300000000ABCDEFGHILMNOPQRSTUVZ
