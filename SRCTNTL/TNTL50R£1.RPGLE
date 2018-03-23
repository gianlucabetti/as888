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
005500000000     D MTFP            S              3  0 DIM(100)                             *FILIALE PARTENZA
005600000000     D MTFA            S              3  0 DIM(100)                             *FILIALE ARRIVO
005700000000     D MTFF            S              3  0 DIM(100)                             *FILIALE ARRIVO FINA
005800000000     D MMCA            S              5  2 DIM(100)                             *MCUBI NORMALI
005900000000     D MMCB            S              5  2 DIM(100)                             *MCUBI BIS
006000000000     D MMCT            S              5  2 DIM(100)                             *MCUBI TRAINI SEDE
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
011100000000     D* AUTOMEZZI
011200000000     D TMZ             S              2    DIM(20)                              *CODICE
011300000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
011400000000     D* CAUSALI CONTABILIZZAZIONE
011500000000     D CAU             S              1    DIM(20)                              *CODICE
011600000000     D DCAU            S             30    DIM(20)                              *DESCRIZIONE
011700010907
011800010907     d w_decimali      s              3  0
011900010907
012000000000     D*------------------------------------------------------------------------*
012100000000     D* INPUT
012200000000     D*------------------------------------------------------------------------*
012300000000     D*-------------------
012400000000     D* DS MULTIPLA DI MEMORIZZAZIONE
012500000000     D*-------------------
012600000000     D* Es. (4:2)+1=3 e (3x50)=150
012700000000     D***
012800000000     D* ORARI
012900000000     D***
013000000000     D OGSE            DS                  OCCURS(8)
013100000000     D  OGGA                   1     50                                         1
013200000000     D                                     DIM(50)                              *GIORNO ARRIVO
013300000000     D  OHHA                  51    200P 0                                      4,0
013400000000     D                                     DIM(50)                              *ORA ARRIVO
013500000000     D                                     PACKEVEN
013600000000     D  OGGP                 201    250                                         1
013700000000     D                                     DIM(50)                              *GIORNO PARTENZA
013800000000     D  OHHP                 251    400P 0                                      4,0
013900000000     D                                     DIM(50)                              *ORA PARTENZA
014000000000     D                                     PACKEVEN
014100000000     D*---------------------
014200000000     D* DS TIPI TRAINI
014300000000     D*---------------------
014400000000     D DSTV          E DS
014500000000     D*---------------------
014600000000     D* DS TIPI AUTOMEZZI
014700000000     D*---------------------
014800000000     D DSTU          E DS
014900000000     D*---------------------
015000000000     D* DS CAUSALI CONTABILIZZAZIONE
015100000000     D*---------------------
015200000214     D ANGY4ZDS      E DS
015300000000     D*---------------------
015400000000     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
015500000000     D*---------------------
015600000000     D OG148         E DS
015700000518     D*---------------------
015800000518     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
015900000518     D*---------------------
016000000518     D OG143         E DS
016100971020     D*------------------
016200971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
016300971020     D*------------------
016400971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
016500010828
016600010828     d dscv          e ds
016700010828     d dged          e ds
016800010828     d tibs02ds      e ds
016900020716
017000020716     d DdatiUte      e ds
017100020716     d AzuteDs       e ds                  ExtName(Azute00f)
017200020716     d Tibs34Ds      e ds                  Inz
017300010828
017400971020     D*------------------
017500971020     D* DS "XSRDA8" - CONTROLLA DATA (8)
017600971020     D*------------------
017700971020     D WLBDA8          DS
017800971020     D  G08DAT                 1      8  0
017900971020     D  G08INV                 9     16  0
018000971020     D  G08ERR                17     17
018100971020     D  G08TGI                18     22  0
018200000000     D*------------------
018300000000     D* DS SCOMPOSIZIONE CAMPI VIDEO - MESSAGGIO VIDEO N° 24
018400000000     D*------------------
018500000000     D* MSG,24
018600000000     D                 DS
018700000000     D  ERRTFP                50     52
018800000000     D  ERRTFA                74     76
018900000000     D  ERRM24                 1     78
019000000821     D*------------------
019100000821     D* ARCHITETTURA
019200000214     D*------------------
019300000214     D KPJBA         E ds
019400000000     C*------------------------------------------------------------------------*
019500000000     C* MAIN LINE
019600000000     C*------------------------------------------------------------------------*
019700000000     C*
019800000000     C* MEMORIZZA TUTTI I DATI DEL TRAINO
019900000000     C                   EXSR      MEMTRN
020000000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
020100000000     C*
020200000000     C* ESEGUE I CONTROLLI DI CONGRUITA' FRA I DIVERSI ARCHIVI
020300000000SL  2C                   SELECT
020400000000     C* TRAINI
020500000000     C     PARGES        WHENEQ    'T'
020600000000     C                   EXSR      FERMAT                                       *CONGRUITA' FERMATE
020700000000     C                   EXSR      METRIC                                       *CONGRUITA' MCUBI
020800000000     C                   EXSR      LISTIN                                       *CONGRUITA' LISTINI
020900000000     C                   EXSR      NOTE                                         *CONGRUITA' NOTE
021000000000     C                   EXSR      TRAINI                                       *CONGRUITA' TRAINI
021100000000     C* FERMATE
021200000000     C     PARGES        WHENEQ    'F'
021300000000     C                   EXSR      FERMAT                                       *CONGRUITA' FERMATE
021400000000     C* METRICUBI
021500000000     C     PARGES        WHENEQ    'M'
021600000000     C                   EXSR      METRIC                                       *CONGRUITA' MCUBI
021700000000     C* LISTINI
021800000000     C     PARGES        WHENEQ    'L'
021900000000     C                   EXSR      LISTIN                                       *CONGRUITA' LISTINI
022000000000     C* NOTE
022100000000     C     PARGES        WHENEQ    'N'
022200000000     C                   EXSR      NOTE                                         *CONGRUITA' NOTE
022300000000E   2C                   ENDSL
022400000000E   1C                   ENDIF
022500971020     C* OPERAZIONI FINALI
022600971020     C                   EXSR      FINSR
022700971020     C*
022800000000     C                   SETON                                        LR
022900000000     C*------------------------------------------------------------------------*
023000000000     C* TRAINI - CONTROLLO DI CONGRUITA' PER LA GESTIONE TRAINI
023100000000     C*------------------------------------------------------------------------*
023200000000     C     TRAINI        BEGSR
023300000000     C*
023400000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
023500000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDCE
023600000000     C                   MOVEL     'S'           $OK                            *RECORD OK
023700000000     C*
023800000000     C* LA FILIALE CHE ESPONE L'IMPORTO DEVE ESSERE UNA FERMATA DI CARICO
023900000000     C                   MOVEL     'N'           $OK
024000000000DO  2C     1             DO        TOTTLR        J
024100000000IF  3C     FFIL(J)       IFGT      *ZEROS
024200000000IF  4C     FFIL(J)       IFEQ      TLTFES
024300000000     C     FCAR(J)       ANDEQ     'S'
024400000000     C                   MOVEL     'S'           $OK                            *RECORD OK
024500000000     C                   LEAVE
024600000000E   4C                   ENDIF
024700000000E   3C                   ENDIF
024800000000E   2C                   ENDDO
024900000000IF  2C     $OK           IFEQ      'N'
025000000000     C                   MOVEL     '1'           PARERR
025100000000     C                   MOVEL     MSG(2)        PARMSG
025200000000E   2C                   ENDIF
025300000000E   1C                   ENDIF
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700000000     C* FERMAT - CONTROLLO DI CONGRUITA' PER LA GESTIONE FERMATE
025800000000     C*------------------------------------------------------------------------*
025900000000     C     FERMAT        BEGSR
026000000000     C*
026100000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
026200000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
026300000000     C                   Z-ADD     *ZEROS        JJ                5 0          *INDICE
026400000000     C                   Z-ADD     *ZEROS        JI                5 0          *INDICE
026500000000     C                   Z-ADD     *ZEROS        DEPDDE                         *DEPOSITO DECORRENZA
026600000000     C                   MOVEL     'S'           $OK                            *RECORD OK
026700000000     C                   CLEAR                   JTFM                           *DI LAVORO
026800000000     C                   CLEAR                   JFIL
026900000000     C                   CLEAR                   JGFV
027000000000     C                   Z-ADD     *ZEROS        TO2TLR
027100000000     C***
027200000000     C* CONTROLLA I DATI CHE CI DEVONO / NON DEVONO ESSERE
027300000000     C***
027400000000     C* ----- ----- ----- -----
027500000000     C* ESISTONO LE FERMATE
027600000000     C* ----- ----- ----- -----
027700000000IF  2C     PARERR        IFEQ      '0'
027800000000IF  3C     FFIL(1)       IFEQ      *ZEROS
027900000000     C                   MOVEL     '1'           PARERR
028000000000     C                   MOVEL     MSG(11)       PARMSG
028100000000E   3C                   ENDIF
028200000000E   2C                   ENDIF
028300000000     C* ----- ----- ----- -----
028400000000     C* ESISTONO GLI ORARI DEI GIORNI DI PARTENZA PREVISTI DAL TRAINO
028500000000     C* ----- ----- ----- -----
028600000000IF  2C     PARERR        IFEQ      '0'
028700000000IF  3C     GCM(8)        IFNE      *BLANKS                                      *SI TUTTI
028800000000X   3C                   ELSE
028900000000DO  4C     1             DO        7             J
029000000000IF  5C     GIO(J)        IFNE      *BLANKS
029100000000     C     GCM(J)        ANDEQ     *BLANKS
029200000000     C                   MOVEL     '1'           PARERR
029300000000     C                   MOVEL     MSG(12)       PARMSG
029400000000E   5C                   ENDIF
029500000000E   4C                   ENDDO
029600000000E   3C                   ENDIF
029700000000E   2C                   ENDIF
029800000000     C* ----- ----- ----- -----
029900000000     C* NON ESISTONO GLI ORARI DEI GIORNI DI PARTENZA NON PREVISTI DAL TRAINO
030000000000     C* ----- ----- ----- -----
030100000000IF  2C     PARERR        IFEQ      '0'
030200000000DO  3C     1             DO        7             J
030300000000IF  4C     GIO(J)        IFEQ      *BLANKS
030400000000     C     GCM(J)        ANDNE     *BLANKS
030500000000     C                   MOVEL     '1'           PARERR
030600000000     C                   MOVEL     MSG(13)       PARMSG
030700000000E   4C                   ENDIF
030800000000E   3C                   ENDDO
030900000000E   2C                   ENDIF
031000000000     C***
031100000000     C* CONTROLLA I DATI CHE CI SONO
031200000000     C***
031300000000     C* ----- ----- ----- -----
031400000000     C* TUTTE LE FERMATE
031500000000     C* ----- ----- ----- -----
031600000000IF  2C     PARERR        IFEQ      '0'
031700000000     C                   Z-ADD     1             J
031800000000DO  3C     J             DOWLE     8                                            O R A R I
031900000000     C     PARERR        ANDEQ     '0'
032000000000IF  4C     GCM(J)        IFNE      *BLANKS
032100000000     C     J             OCCUR     OGSE
032200000000     C                   Z-ADD     1             JJ
032300000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
032400000000     C     PARERR        ANDEQ     '0'
032500000000     C* FERMATE DI TIPO "A"=ANDATA O "R"=RITORNO
032600000000IF  6C     FTFM(JJ)      IFNE      'A'
032700000000     C     FTFM(JJ)      ANDNE     'R'
032800000000     C                   MOVEL     '1'           PARERR
032900000000E   6C                   ENDIF
033000000000     C* GIORNO APERTURA F.V.= A,B,C ... SOLO SE FERMATA DI CARICO, ALTRIMENTI NO
033100000000IF  6C     FCAR(JJ)      IFEQ      'S'
033200000000     C                   SETOFF                                           28
033300000000     C     FGFV(JJ)      LOOKUP    ALF                                    28
033400000000IF  7C     *IN28         IFEQ      *OFF
033500000000     C                   MOVEL     '1'           PARERR
033600000000E   7C                   ENDIF
033700000000     C* GIORNO APERTURA F.V. MAGGIORE DEL PRECEDENTE (TRANNE LA PRIMA)
033800000000     C                   Z-ADD     *ZEROS        JI
033900000000IF  7C     JJ            IFNE      1
034000000000     C     JJ            SUB       1             JI
034100000000IF  8C     FGFV(JI)      IFGT      FGFV(JJ)
034200000000     C                   MOVEL     '1'           PARERR
034300000000E   8C                   ENDIF
034400000000E   7C                   ENDIF
034500000000X   6C                   ELSE
034600000000IF  7C     FGFV(JJ)      IFNE      *BLANKS
034700000000     C                   MOVEL     '1'           PARERR
034800000000E   7C                   ENDIF
034900000000E   6C                   ENDIF
035000000000     C* SE ARRIVO E PARTENZA PIENI, LA PARTENZA DEVE ESSERE > DELL'ARRIVO (G+HH)
035100000000IF  6C     OHHA(JJ)      IFGT      *ZEROS
035200000000     C     OHHP(JJ)      ANDGT     *ZEROS
035300000000IF  7C     OGGA(JJ)      IFGT      OGGP(JJ)                                     GG ARR > GG PAR   O
035400000000     C     OGGA(JJ)      OREQ      OGGP(JJ)                                     GG ARR = GG PAR  MA
035500000000     C     OHHA(JJ)      ANDGE     OHHP(JJ)                                     HH ARR >= GG PAR
035600000000     C                   MOVEL     '1'           PARERR
035700000000E   7C                   ENDIF
035800000000E   6C                   ENDIF
035900000000     C* SE INSERITA LA FERMATA, INSERIRE ANCHE QUALCHE DATO
036000000000IF  6C     FFIL(JJ)      IFGT      *ZEROS
036100000000IF  7C     OHHA(JJ)      IFEQ      *ZEROS
036200000000     C     OHHP(JJ)      ANDEQ     *ZEROS
036300000000     C     OGGA(JJ)      ANDEQ     *BLANKS
036400000000     C     OGGP(JJ)      ANDEQ     *BLANKS
036500000000     C                   MOVEL     '1'           PARERR
036600000000E   7C                   ENDIF
036700000000E   6C                   ENDIF
036800000000     C* SE INSERITO L'ORARIO, INSERIRE ANCHE LA FERMATA
036900000000IF  6C     FFIL(JJ)      IFEQ      *ZEROS
037000000000IF  7C     OHHA(JJ)      IFGT      *ZEROS
037100000000     C     OHHP(JJ)      ORGT      *ZEROS
037200000000     C     OGGA(JJ)      ORGT      *BLANKS
037300000000     C     OGGP(JJ)      ORGT      *BLANKS
037400000000     C                   MOVEL     '1'           PARERR
037500000000E   7C                   ENDIF
037600000000E   6C                   ENDIF
037700000000     C*
037800000000     C                   ADD       1             JJ
037900000000E   5C                   ENDDO
038000000000E   4C                   ENDIF
038100000000     C*
038200000000     C                   ADD       1             J
038300000000E   3C                   ENDDO
038400000000IF  3C     PARERR        IFEQ      '1'
038500000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
038600000000E   3C                   ENDIF
038700000000E   2C                   ENDIF
038800000000     C* ----- ----- ----- -----
038900000000     C* PRIMA E ULTIMA FERMATA
039000000000     C* ----- ----- ----- -----
039100000000IF  2C     PARERR        IFEQ      '0'
039200000000     C                   Z-ADD     1             J
039300000000DO  3C     J             DOWLE     8                                            O R A R I
039400000000     C     PARERR        ANDEQ     '0'
039500000000IF  4C     GCM(J)        IFNE      *BLANKS
039600000000     C     J             OCCUR     OGSE
039700000000     C* P R I M A
039800000000     C                   Z-ADD     1             JJ                             F E R M A T E
039900000000     C* TIPO FERMATA = 'A'
040000000000IF  5C     FTFM(JJ)      IFNE      'A'
040100000000     C                   MOVEL     '1'           PARERR
040200000000E   5C                   ENDIF
040300000000     C* DATI ARRIVO, KILOMETRI E TEMPO VUOTI
040400000000IF  5C     OGGA(JJ)      IFNE      *BLANKS
040500000000     C     OHHA(JJ)      ORNE      *ZEROS
040600000000     C     FSCA(JJ)      ORNE      *BLANKS
040700000000     C     FKMT(JJ)      ORNE      *ZEROS
040800000000     C     FTEM(JJ)      ORNE      *ZEROS
040900000000     C                   MOVEL     '1'           PARERR
041000000000E   5C                   ENDIF
041100000000     C* DATI PARTENZA PIENI
041200000000IF  5C     OGGP(JJ)      IFEQ      *BLANKS
041300000000     C     OHHP(JJ)      OREQ      *ZEROS
041400000000     C     FCAR(JJ)      OREQ      *BLANKS
041500000000     C                   MOVEL     '1'           PARERR
041600000000E   5C                   ENDIF
041700000000     C* GIORNO APERTURA F.V.='A'
041800000000IF  5C     FGFV(JJ)      IFNE      'A'
041900000000     C                   MOVEL     '1'           PARERR
042000000000E   5C                   ENDIF
042100000000     C* FILIALE PARTENZA = TFP DEL TRAINO
042200000000IF  5C     FFIL(JJ)      IFNE      TLTTFP
042300000000     C                   MOVEL     '1'           PARERR
042400000000E   5C                   ENDIF
042500000000     C* U L T I M A
042600000000     C                   Z-ADD     TOTTLR        JJ
042700000000     C* DATI PARTENZA VUOTI
042800000000IF  5C     OGGP(JJ)      IFNE      *BLANKS
042900000000     C     OHHP(JJ)      ORNE      *ZEROS
043000000000     C     FCAR(JJ)      ORNE      *BLANKS
043100000000     C                   MOVEL     '1'           PARERR
043200000000E   5C                   ENDIF
043300000000     C* DATI ARRIVO PIENI
043400000000IF  5C     OGGA(JJ)      IFEQ      *BLANKS
043500000000     C     OHHA(JJ)      OREQ      *ZEROS
043600000000     C     FSCA(JJ)      OREQ      *BLANKS
043700000000     C                   MOVEL     '1'           PARERR
043800000000E   5C                   ENDIF
043900000000     C* TRAINO CHIUSO TIPO FERMATA = 'R'
044000000000     C*    "      "   FILIALE ARRIVO = TFP DEL TRAINO
044100000000IF  5C     TLTTTR        IFEQ      'C'                                          *CHIUSO
044200000000IF  6C     FTFM(JJ)      IFNE      'R'
044300000000     C                   MOVEL     '1'           PARERR
044400000000E   6C                   ENDIF
044500000000IF  6C     FFIL(JJ)      IFNE      TLTTFP
044600000000     C                   MOVEL     '1'           PARERR
044700000000E   6C                   ENDIF
044800000000X   5C                   ELSE                                                   *NON CHIUSO
044900000000     C* TRAINO NON CHIUSO TIPO FERMATA = 'A'
045000000000     C*    "    "    "    FILIALE ARRIVO = TFA DEL TRAINO
045100000000IF  6C     FTFM(JJ)      IFNE      'A'
045200000000     C                   MOVEL     '1'           PARERR
045300000000E   6C                   ENDIF
045400000000IF  6C     FFIL(JJ)      IFNE      TLTTFA
045500000000     C                   MOVEL     '1'           PARERR
045600000000E   6C                   ENDIF
045700000000E   5C                   ENDIF
045800000000E   4C                   ENDIF
045900000000     C*
046000000000     C                   ADD       1             J
046100000000E   3C                   ENDDO
046200000000IF  3C     PARERR        IFEQ      '1'
046300000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
046400000000E   3C                   ENDIF
046500000000E   2C                   ENDIF
046600000000     C* ----- ----- ----- -----
046700000000     C* TIPO FERMATA
046800000000     C* ----- ----- ----- -----
046900000000IF  2C     PARERR        IFEQ      '0'
047000000000     C                   Z-ADD     1             J
047100000000DO  3C     J             DOWLE     8                                            O R A R I
047200000000     C     PARERR        ANDEQ     '0'
047300000000IF  4C     GCM(J)        IFNE      *BLANKS
047400000000     C     J             OCCUR     OGSE
047500000000     C                   CLEAR                   JFT                            *AZZERA CAMPI LAVORO
047600000000     C                   MOVEL     '0'           WSCHIA            1
047700000000     C                   MOVEL     '0'           WSCHIR            1
047800000000     C                   MOVEL     '0'           WNCHIA            1
047900000000     C                   MOVEL     '0'           WNCHIR            1
048000000000     C                   Z-ADD     *ZEROS        F                 4 0
048100000000     C                   Z-ADD     *ZEROS        JI
048200000000     C                   Z-ADD     1             JJ
048300000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
048400000000     C     PARERR        ANDEQ     '0'
048500000000     C* TIPI FERMATA ESISTENTI
048600000000IF  6C     TLTTTR        IFEQ      'C'                                          *C H I U S O
048700000000IF  7C     FTFM(JJ)      IFEQ      'R'
048800000000     C                   MOVEL     '1'           WSCHIR                         *CI SONO FERM RITORN
048900000000E   7C                   ENDIF
049000000000IF  7C     FTFM(JJ)      IFEQ      'A'
049100000000     C                   MOVEL     '1'           WSCHIA                         *CI SONO FERM ANDATA
049200000000E   7C                   ENDIF
049300000000X   6C                   ELSE                                                   *N O N   C H I U S O
049400000000IF  7C     FTFM(JJ)      IFEQ      'R'
049500000000     C                   MOVEL     '1'           WNCHIR                         *CI SONO FERM RITORN
049600000000E   7C                   ENDIF
049700000000IF  7C     FTFM(JJ)      IFEQ      'A'
049800000000     C                   MOVEL     '1'           WNCHIA                         *CI SONO FERM ANDATA
049900000000E   7C                   ENDIF
050000000000E   6C                   ENDIF
050100000000     C* LA STESSA FERMATA (FIL+TIPO) NON PUO' ESSERE RIPETUTA, TRANNE NEI LOCALI
050200000000     C* O NEI TRAINI INOLTRO DI TIPO LOCALE
050300000000IF  6C     TLTTTR        IFNE      'L'
050400000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
050500000000     C                   MOVEL     FFIL(JJ)      ALF4              4            *FIL + A/R
050600000000     C                   MOVE      FTFM(JJ)      ALF4
050700000000     C                   SETOFF                                       28
050800000000     C     ALF4          LOOKUP    JFT                                    28
050900000000IF  7C     *IN28         IFEQ      *ON                                          *TROVATO
051000000000     C                   MOVEL     '1'           PARERR
051100000000X   7C                   ELSE
051200000000     C                   ADD       1             F
051300000000     C                   MOVEL     ALF4          JFT(F)
051400000000E   7C                   ENDIF
051500000000E   6C                   ENDIF
051600000000     C* SE TRAINO CHIUSO LE FERMATE RITORNO DEVONO SEGUIRE LE ANDATA
051700000000     C* "     "      "   LE FERMATE ANDATA DEVONO PRECEDERE LE RITORNO
051800000000IF  6C     JJ            IFNE      1                                            *NO PRIMA FERMATA
051900000000     C     JJ            ANDNE     TOTTLR                                       *NO ULTIMA FERMATA
052000000000IF  7C     FTFM(JJ)      IFEQ      'A'
052100000000     C     JJ            SUB       1             JI
052200000000IF  8C     FTFM(JI)      IFEQ      'R'
052300000000     C                   MOVEL     '1'           PARERR
052400000000E   8C                   ENDIF
052500000000X   7C                   ELSE
052600000000     C     JJ            ADD       1             JI
052700000000IF  8C     FTFM(JI)      IFEQ      'A'
052800000000     C                   MOVEL     '1'           PARERR
052900000000E   8C                   ENDIF
053000000000E   7C                   ENDIF
053100000000E   6C                   ENDIF
053200000000     C*
053300000000     C                   ADD       1             JJ
053400000000E   5C                   ENDDO
053500000000     C* SE TRAINO CHIUSO, CI DEVONO ESSERE FERMATE DI ANDATA E RITORNO
053600000000     C* SE TRAINO NON CHIUSO, SI DEVONO ESSERE SOLO FERMATE DI ANDATA
053700000000IF  5C     TLTTTR        IFEQ      'C'                                          C H I U S O
053800000000IF  6C     WSCHIR        IFEQ      '1'
053900000000     C     WSCHIA        ANDEQ     '1'
054000000000X   6C                   ELSE
054100000000     C                   MOVEL     '1'           PARERR
054200000000E   6C                   ENDIF
054300000000X   5C                   ELSE                                                   N O N  CHIUSO
054400000000IF  6C     WNCHIR        IFEQ      '1'
054500000000     C     WNCHIA        OREQ      '0'
054600000000     C                   MOVEL     '1'           PARERR
054700000000E   6C                   ENDIF
054800000000E   5C                   ENDIF
054900000000E   4C                   ENDIF
055000000000     C*
055100000000     C                   ADD       1             J
055200000000E   3C                   ENDDO
055300000000IF  3C     PARERR        IFEQ      '1'
055400000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
055500000000E   3C                   ENDIF
055600000000E   2C                   ENDIF
055700000000     C* ----- ----- ----- -----
055800000000     C* FILIALE
055900000000     C* ----- ----- ----- -----
056000000000IF  2C     PARERR        IFEQ      '0'
056100000000     C                   Z-ADD     1             J
056200000000DO  3C     J             DOWLE     8                                            O R A R I
056300000000     C     PARERR        ANDEQ     '0'
056400000000IF  4C     GCM(J)        IFNE      *BLANKS
056500000000     C     J             OCCUR     OGSE
056600000000     C                   Z-ADD     1             JJ
056700000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
056800000000     C     PARERR        ANDEQ     '0'
056900000000IF  6C     JJ            IFNE      1                                            *NO PRIMA
057000000000     C     JJ            ANDNE     TOTTLR                                       *NO ULTIMA
057100000000     C* DATI FILIALE
057200000000     C                   Z-ADD     FFIL(JJ)      WORG
057300000000     C                   Z-ADD     *ZEROS        WFEL
057400000000     C                   Z-ADD     *ZEROS        WTFP
057500000000     C                   Z-ADD     *ZEROS        WTFA
057600000519     C                   MOVEL     *BLANKS       WPOS
057700000000     C                   EXSR      CHKFIL
057800000000     C                   Z-ADD     WFEL          DEPFEL
057900000000     C                   Z-ADD     WTFP          DEPTFP
058000000000     C                   Z-ADD     WTFA          DEPTFA
058100000519     C                   MOVEL     WPOS          DEPPOS
058200000000     C* A F F L U E N Z A
058300000000IF  7C     TLTTTR        IFEQ      'A'
058400000000IF  8C     FFIL(JJ)      IFNE      DEPFEL                                       *2° LIVELLO
058500000000IF  9C     DEPTFP        IFEQ      TLTTFA                                       *TFP = TFA DEL TRAIN
058600000519     C     DEPPOS        OREQ      'S'                                          *FILIALE POSTE
058700000000IF 10C     FCAR(JJ)      IFEQ      'S'                                          *SI CARICO  e
058800000000     C     FSCA(JJ)      ANDEQ     *BLANKS                                      *NO SCARICO
058900000000X  10C                   ELSE
059000000000     C                   MOVEL     '1'           PARERR
059100000000E  10C                   ENDIF
059200000000X   9C                   ELSE
059300000000     C                   MOVEL     '1'           PARERR
059400000000E   9C                   ENDIF
059500000000X   8C                   ELSE
059600000000     C                   MOVEL     '1'           PARERR
059700000000E   8C                   ENDIF
059800000000E   7C                   ENDIF
059900000000     C* D E F L U E N Z A
060000000000     C* L O C A L E
060100000000     C* I N O L T R O  -DEFLUENZA O LOCALE-
060200000000IF  7C     DEPDEF        IFEQ      'S'                                          *TIPO DEFLUENZA
060300000000     C     TLTTTR        OREQ      'L'                                          *LOCALE
060400000000     C     PARINL        OREQ      'D'                                          *INOLTRO DEFLUENZA
060500000000     C     PARINL        OREQ      'L'                                          *INOLTRO LOCALE
060600000000IF  8C     FSCA(JJ)      IFEQ      'S'                                          *SI SCARICO  e
060700000000     C     FCAR(JJ)      ANDEQ     *BLANKS                                      *NO CARICO
060800000000     C* ECCEZIONI
060900000000     C                   MOVEL     '0'           WECC              1
061000000000     C                   EXSR      CTRECC
061100000000IF  9C     DEPTFA        IFEQ      TLTTFP                                       *TERM ARR=TFP TRAINO
061200000000     C     WECC          OREQ      '1'                                          *TERM ARR ECC=TFP TR
061300000000IF 10C     FFIL(JJ)      IFNE      TLTTFP                                       *FIL <> TFP TRAINO
061400000000X  10C                   ELSE
061500000000     C                   MOVEL     '1'           PARERR
061600000000E  10C                   ENDIF
061700000000X   9C                   ELSE
061800000000     C                   MOVEL     '1'           PARERR
061900000000E   9C                   ENDIF
062000000000X   8C                   ELSE
062100000000     C                   MOVEL     '1'           PARERR
062200000000E   8C                   ENDIF
062300000000E   7C                   ENDIF
062400000000     C* SOLO  A N D A T A
062500000000     C* C H I U S O
062600000000     C* I N O L T R O  -SOLO ANDATA-
062700000000     C*---
062800000000IF  7C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
062900000000     C*---       TLTTTR    OREQ 'C'                        *CHIUSO
063000000000     C*---       PARINL    OREQ 'S'                        *INOLTRO SOLO ANDATA
063100000000     C*-ECCEZIONI
063200000000     C*---                 MOVEL'0'       WECC
063300000000     C*---                 EXSR CTREC2
063400000000IF  8C*---       FCAR,JJ   IFEQ 'S'                        SE FERMATA DI CARICO
063500000000     C*---       FFIL,JJ   ANDNEDEPFEL                     *FIL=1°LIVELLO
063600000000     C*---                 MOVEL'1'       PARERR
063700000000E   8C*---                 ENDIF
063800000000IF  8C*---       FSCA,JJ   IFEQ 'S'                        SE FERMATA DI SCARICO
063900000000IF  9C*---       FFIL,JJ   IFNE DEPTFA                     *FIL=TERM ARR OPPURE
064000000000     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
064100000000     C*---                 MOVEL'1'       PARERR
064200000000E   9C*---                 ENDIF
064300000000E   8C*---                 ENDIF
064400000000E   7C*---                 ENDIF
064500000000E   6C*---                 ENDIF
064600000000     C*
064700000000IF  7C     TLTTTR        IFEQ      'S'                                          *SOLO ANDATA
064800000000     C     TLTTTR        OREQ      'C'                                          *CHIUSO
064900000000     C     PARINL        OREQ      'S'                                          *INOLTRO SOLO ANDATA
065000000000     C* ECCEZIONI
065100000000     C                   MOVEL     '0'           WECC
065200000000     C                   Z-ADD     FFIL(JJ)      WNUM3B            3 0
065300000000     C                   EXSR      CTREC2
065400980610     C*---CASO PARTICOLARE
065500980610IF  8C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
065600980610     C*---       FFIL,JJ   ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
065700980610     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
065800980610IF  9C*---       FSCA,JJ   IFEQ 'S'                        *FERMATA DI SCARICO
065900980610     C*---       FCAR,JJ   ANDNE'S'                        *NO FERMATA DI CARICO
066000980610X   9C*---                 ELSE
066100980610     C*---                 MOVEL'1'       PARERR
066200980610E   9C*---                 ENDIF
066300980610X   8C*---                 ELSE                            *CASO NORMALE
066400980610     C*
066500980610IF  9C     FCAR(JJ)      IFEQ      'S'                                          SE FERMATA DI CARICO
066600000519IF 10C     FFIL(JJ)      IFEQ      DEPFEL                                       *FIL=1° LIVELLO
066700000519     C     DEPPOS        OREQ      'S'                                          *FILIALE POSTA
066800000519X  10C                   ELSE
066900980610     C                   MOVEL     '1'           PARERR
067000980610E  10C                   ENDIF
067100980610E   9C                   ENDIF
067200980610     C*
067300980610IF  9C*---       FSCA,JJ   IFEQ 'S'                        SE FERMATA DI SCARICO
067400980610     C*---       FFIL,JJ   ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
067500980610     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
067600980610     C*---                 MOVEL'1'       PARERR
067700980610E   9C*---                 ENDIF
067800980610E   8C*---                 ENDIF
067900000000E   7C                   ENDIF
068000000000E   6C                   ENDIF                                                  *NO PRIMA E NO ULTIM
068100000000     C* C H I U S O
068200000000IF  6C     TLTTTR        IFEQ      'C'
068300000000     C* L'ULTIMA FERMATA DI ANDATA  DEVE ESSERE = TFA DEL TRAINO
068400000000     C* E DI SOLO SCARICO
068500000000IF  7C     FTFM(JJ)      IFEQ      'A'
068600000000     C     JJ            ADD       1             JI                             *FERMATA SUCESSIVA
068700000000IF  8C     FTFM(JI)      IFEQ      'R'                                          *RITORNO
068800000000IF  9C     FFIL(JJ)      IFNE      TLTTFA
068900000000     C                   MOVEL     '1'           PARERR
069000000000E   9C                   ENDIF
069100000000IF  9C     FSCA(JJ)      IFEQ      *BLANKS
069200000000     C                   MOVEL     '1'           PARERR
069300000000E   9C                   ENDIF
069400000000     C* E I DATI PARTENZA DEVONO ESSERE VUOTI
069500000000IF  9C     OGGP(JJ)      IFNE      *BLANKS
069600000000     C     OHHP(JJ)      ORNE      *ZEROS
069700000000     C     FCAR(JJ)      ORNE      *BLANKS
069800000000     C                   MOVEL     '1'           PARERR
069900000000E   9C                   ENDIF
070000000000E   8C                   ENDIF
070100000000     C* LA PRIMA FERMATA DI RITORNO DEVE ESSERE = TFA DEL TRAINO
070200000000     C* E DI SOLO CARICO
070300000000X   7C                   ELSE
070400000000     C     JJ            SUB       1             JI                             *FERMATA PRECEDENTE
070500000000IF  8C     FTFM(JI)      IFEQ      'A'                                          *ANDATA
070600000000IF  9C     FFIL(JJ)      IFNE      TLTTFA
070700000000     C                   MOVEL     '1'           PARERR
070800000000E   9C                   ENDIF
070900000000IF  9C     FCAR(JJ)      IFEQ      *BLANKS
071000000000     C                   MOVEL     '1'           PARERR
071100000000E   9C                   ENDIF
071200000000     C* E I DATI ARRIVO DEVONO ESSERE VUOTI
071300000000IF  9C     OGGA(JJ)      IFNE      *BLANKS
071400000000     C     OHHA(JJ)      ORNE      *ZEROS
071500000000     C     FSCA(JJ)      ORNE      *BLANKS
071600000000     C                   MOVEL     '1'           PARERR
071700000000E   9C                   ENDIF
071800000000E   8C                   ENDIF
071900000000E   7C                   ENDIF
072000000000E   6C                   ENDIF
072100000000     C*
072200000000     C                   ADD       1             JJ
072300000000E   5C                   ENDDO
072400000000E   4C                   ENDIF
072500000000     C*
072600000000     C                   ADD       1             J
072700000000E   3C                   ENDDO
072800000000IF  3C     PARERR        IFEQ      '1'
072900000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
073000000000E   3C                   ENDIF
073100000000E   2C                   ENDIF
073200000000     C* ----- ----- ----- -----
073300000000     C* ORARI
073400000000     C* ----- ----- ----- -----
073500000000IF  2C     PARERR        IFEQ      '0'
073600000000     C                   Z-ADD     1             J
073700000000DO  3C     J             DOWLE     8                                            O R A R I
073800000000     C     PARERR        ANDEQ     '0'
073900000000IF  4C     GCM(J)        IFNE      *BLANKS
074000000000     C     J             OCCUR     OGSE
074100000000     C                   Z-ADD     1             JJ
074200000000DO  5C     JJ            DOWLE     TOTTLR                                       F E R M A T E
074300000000     C     PARERR        ANDEQ     '0'
074400000000     C* A R R I V O
074500000000     C* PROGRESSIVO GIORNO A,B,C ...
074600000000IF  6C     OGGA(JJ)      IFNE      *BLANKS
074700000000     C     OGGA(JJ)      LOOKUP    ALF                                    28
074800000000IF  7C     *IN28         IFEQ      *OFF
074900000000     C                   MOVEL     '1'           PARERR
075000000000E   7C                   ENDIF
075100000000E   6C                   ENDIF
075200000000     C* ORA IN HH:MM
075300000000IF  6C     OHHA(JJ)      IFNE      *ZEROS
075400000000     C                   Z-ADD     OHHA(JJ)      WORA
075500000000     C                   EXSR      CHKORA
075600000000E   6C                   ENDIF
075700000000     C* SCARICO
075800000000IF  6C     FSCA(JJ)      IFNE      *BLANKS
075900000000IF  7C     FSCA(JJ)      IFNE      'S'
076000000000     C                   MOVEL     '1'           PARERR
076100000000E   7C                   ENDIF
076200000000E   6C                   ENDIF
076300000000     C* SE SCARICO='S' ANCHE GG E ORA ARRIVO PIENI
076400000000IF  6C     FSCA(JJ)      IFNE      *BLANKS
076500000000IF  7C     OGGA(JJ)      IFEQ      *BLANKS
076600000000     C     OHHA(JJ)      OREQ      *ZEROS
076700000000     C                   MOVEL     '1'           PARERR
076800000000E   7C                   ENDIF
076900000000E   6C                   ENDIF
077000000000     C* SE GG O ORA ARRIVO PIENI ANCHE LO SCARICO = 'S'
077100000000IF  6C     OGGA(JJ)      IFNE      *BLANKS
077200000000     C     OHHA(JJ)      ORGT      *ZEROS
077300000000IF  7C     FSCA(JJ)      IFEQ      *BLANKS
077400000000     C                   MOVEL     '1'           PARERR
077500000000E   7C                   ENDIF
077600000000E   6C                   ENDIF
077700000000     C* P A R T E N Z A
077800000000     C* PROGRESSIVO GIORNO A,B,C ...
077900000000IF  6C     OGGP(JJ)      IFNE      *BLANKS
078000000000     C                   SETOFF                                           28
078100000000     C     OGGP(JJ)      LOOKUP    ALF                                    28
078200000000IF  7C     *IN28         IFEQ      *OFF
078300000000     C                   MOVEL     '1'           PARERR
078400000000E   7C                   ENDIF
078500000000E   6C                   ENDIF
078600000000     C* PROGRESSIVO GIORNO MAGGIORE/UGUALE GIORNO APERTURA FOGLIO VIAGGIO
078700000000IF  6C     OGGP(JJ)      IFNE      *BLANKS
078800000000IF  7C     OGGP(JJ)      IFLT      FGFV(JJ)
078900000000     C                   MOVEL     '1'           PARERR
079000000000E   7C                   ENDIF
079100000000E   6C                   ENDIF
079200000000     C* ORA IN HH:MM
079300000000IF  6C     OHHP(JJ)      IFNE      *ZEROS
079400000000     C                   Z-ADD     OHHP(JJ)      WORA
079500000000     C                   EXSR      CHKORA
079600000000E   6C                   ENDIF
079700000000     C* CARICO
079800000000IF  6C     FCAR(JJ)      IFNE      *BLANKS
079900000000IF  7C     FCAR(JJ)      IFNE      'S'
080000000000     C                   MOVEL     '1'           PARERR
080100000000E   7C                   ENDIF
080200000000E   6C                   ENDIF
080300000000     C* SE CARICO='S' ANCHE GG E ORA PARTENZA PIENI
080400000000IF  6C     FCAR(JJ)      IFNE      *BLANKS
080500000000IF  7C     OGGP(JJ)      IFEQ      *BLANKS
080600000000     C     OHHP(JJ)      OREQ      *ZEROS
080700000000     C                   MOVEL     '1'           PARERR
080800000000E   7C                   ENDIF
080900000000E   6C                   ENDIF
081000000000     C* SE GG O ORA PARTENZA PIENI ANCHE IL CARICO = 'S'
081100000000IF  6C     OGGP(JJ)      IFNE      *BLANKS
081200000000     C     OHHP(JJ)      ORGT      *ZEROS
081300000000IF  7C     FCAR(JJ)      IFEQ      *BLANKS
081400000000     C                   MOVEL     '1'           PARERR
081500000000E   7C                   ENDIF
081600000000E   6C                   ENDIF
081700000000     C* TEMPO
081800030715IF  6C**!!!FTEM(JJ)      IFGT      *ZEROS
081900030715     C**!!!              Z-ADD     FTEM(JJ)      WORA
082000030715     C**!!!              EXSR      CHKORA
082100030715E   6C**!!!              ENDIF
082200000000     C*
082300000000     C                   ADD       1             JJ
082400000000E   5C                   ENDDO
082500000000E   4C                   ENDIF
082600000000     C*
082700000000     C                   ADD       1             J
082800000000E   3C                   ENDDO
082900000000IF  3C     PARERR        IFEQ      '1'
083000000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
083100000000E   3C                   ENDIF
083200000000E   2C                   ENDIF
083300000000     C* ----- ----- ----- -----
083400000000     C* ALTRI CONTROLLI
083500000000     C* ----- ----- ----- -----
083600000000IF  2C*---       PARERR    IFEQ '0'
083700000000     C*---                 Z-ADD1         J
083800000000DO  3C*---       J         DOWLE8                          O R A R I
083900000000     C*---       PARERR    ANDEQ'0'
084000000000IF  4C*---       GCM,J     IFNE *BLANKS
084100000000     C*---       J         OCUR OGSE
084200000000     C*---                 Z-ADD*ZEROS    A       40
084300000000     C*---                 Z-ADD1         P       40       *AZZERA CAMPI LAVORO
084400000000DO  5C*---       P         DOWLETOTTLR
084500000000     C*---       PARERR    ANDEQ'0'
084600000000     C* TUTTO QUELLO CHE PARTE DEVE ARRIVARE (ORA PARTENZA SEMPRE PIENO), TRANNE
084700000000     C* L'ULTIMA FERMATA OPPURE SE COMINCIANO LE FERMATE DI RITORNO ('A'->'R')
084800000000     C*---       P         ADD  1         A
084900000000IF  6C*---       P         IFEQ TOTTLR                     *ULTIMA FERMATA
085000000000     C*---       FTFM,P    ORNE FTFM,A                     *ANDATA --> RITORNO
085100000000X   6C*---                 ELSE
085200000000IF  7C*---       OGGP,P    IFEQ *BLANKS
085300000000     C*---       OHHP,P    OREQ *ZEROS
085400000000     C*---                 MOVEL'1'       PARERR
085500000000E   7C*---                 ENDIF
085600000000E   6C*---                 ENDIF
085700000000     C*---
085800000000     C*---                 ADD  1         P
085900000000E   5C*---                 ENDDO
086000000000E   4C*---                 ENDIF
086100000000     C*---
086200000000     C*---                 ADD  1         J
086300000000E   3C*---                 ENDDO
086400000000IF  3C*---       PARERR    IFEQ '1'
086500000000     C*---                 MOVELMSG,20    PARMSG           *MESSAGGIO GENGERICO
086600000000E   3C*---                 ENDIF
086700000000E   2C*---                 ENDIF
086800000000     C* ----- ----- ----- -----
086900000000     C* ALTRI CONTROLLI
087000000000     C* ----- ----- ----- -----
087100000000IF  2C     PARERR        IFEQ      '0'
087200000000     C                   Z-ADD     1             J
087300000000DO  3C     J             DOWLE     8                                            O R A R I
087400000000     C     PARERR        ANDEQ     '0'
087500000000IF  4C     GCM(J)        IFNE      *BLANKS
087600000000     C     J             OCCUR     OGSE
087700000000     C                   MOVEL     *BLANKS       WALF5A            5            *AZZERA CAMPI LAVORO
087800000000     C                   MOVEL     *BLANKS       WALF5P            5
087900000000     C                   Z-ADD     *ZEROS        AA                4 0
088000000000     C                   Z-ADD     *ZEROS        A                 4 0
088100000000     C                   Z-ADD     1             P                 4 0
088200000000DO  5C     P             DOWLE     TOTTLR                                       *PARTENZA
088300000000     C     PARERR        ANDEQ     '0'
088400000000     C     P             ADD       1             A
088500000000DO  6C     A             DOWLE     TOTTLR                                       *ARRIVO
088600000000     C     PARERR        ANDEQ     '0'
088700000000     C* CIASCUN ORARIO DI PARTENZA DEVE ESSERE MINORE DI CISCUN ORARIO DI ARRIVO
088800000000     C* SUCCESSIVO, TRANNE L'ULTIMA FERMATA O SE INIZIANO LE FERMATE DI RITORNO
088900000000     C     A             SUB       1             AA                             *ARRIVO PRECEDENTE
089000000000IF  7C     P             IFEQ      TOTTLR                                       *ULTIMA FERMATA
089100000000     C     FTFM(P)       ORNE      FTFM(A)                                      *ANDATA --> RITORNO
089200000000     C     FTFM(A)       ANDNE     FTFM(AA)
089300000000X   7C                   ELSE                                                   *ESCLUDE
089400000000IF  8C     OGGA(A)       IFNE      *BLANKS                                      *SE ARRIVO PIENO
089500000000     C     OHHA(A)       ANDNE     *ZEROS
089600000000     C                   MOVEL     OGGP(P)       WALF5P                         *PARTENZA='X+9999'
089700000000     C                   MOVE      OHHP(P)       WALF5P
089800000000     C                   MOVEL     OGGA(A)       WALF5A                         *ARRIVO='X+9999'
089900000000     C                   MOVE      OHHA(A)       WALF5A
090000000000IF  9C     WALF5P        IFGE      WALF5A
090100000000     C                   MOVEL     '1'           PARERR
090200000000E   9C                   ENDIF
090300000000E   8C                   ENDIF
090400000000E   7C                   ENDIF
090500000000     C*
090600000000     C                   ADD       1             A
090700000000E   6C                   ENDDO
090800000000     C*
090900000000     C                   ADD       1             P
091000000000E   5C                   ENDDO
091100000000E   4C                   ENDIF
091200000000     C*
091300000000     C                   ADD       1             J
091400000000E   3C                   ENDDO
091500000000IF  3C     PARERR        IFEQ      '1'
091600000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
091700000000E   3C                   ENDIF
091800000000E   2C                   ENDIF
091900000000     C* ----- ----- ----- -----
092000000000     C* ALTRI CONTROLLI
092100000000     C* ----- ----- ----- -----
092200000000IF  2C     PARERR        IFEQ      '0'
092300000000     C                   Z-ADD     1             J
092400000000DO  3C     J             DOWLE     8                                            O R A R I
092500000000     C     PARERR        ANDEQ     '0'
092600000000IF  4C     GCM(J)        IFNE      *BLANKS
092700000000     C     J             OCCUR     OGSE
092800000000     C                   MOVEL     *BLANKS       WALF5A            5            *AZZERA CAMPI LAVORO
092900000000     C                   MOVEL     *BLANKS       WALF5P            5
093000000000     C                   Z-ADD     2             P                 4 0
093100000000     C                   Z-ADD     *ZEROS        PP                4 0
093200000000DO  5C     P             DOWLE     TOTTLR                                       *PARTENZA
093300000000     C     PARERR        ANDEQ     '0'
093400000000IF  6C     OGGP(P)       IFNE      *BLANKS                                      *SE PARTENZA PIENA
093500000000     C     P             SUB       1             PP
093600000000DO  7C     PP            DOWGE     1                                            *PARTENZA-1
093700000000     C     PARERR        ANDEQ     '0'
093800000000     C* CIASCUN ORARIO DI PARTENZA (SE PIENO) DEVE ESSERE MAGGIORE DI CIASCUN
093900000000     C* ORARIO DI PARTENZA PRECEDENTE TRANNE LA PRIMA FERMATA
094000000000     C                   MOVEL     OGGP(P)       WALF5P                         *PARTENZA='X+9999'
094100000000     C                   MOVE      OHHP(P)       WALF5P
094200000000     C                   MOVEL     OGGP(PP)      WALF5A                         *PARTENZA-1='X+9999'
094300000000     C                   MOVE      OHHP(PP)      WALF5A
094400000000IF  8C     WALF5P        IFLE      WALF5A
094500000000     C                   MOVEL     '1'           PARERR
094600000000E   8C                   ENDIF
094700000000     C*
094800000000     C                   SUB       1             PP
094900000000E   7C                   ENDDO
095000000000E   6C                   ENDIF
095100000000     C*
095200000000     C                   ADD       1             P
095300000000E   5C                   ENDDO
095400000000E   4C                   ENDIF
095500000000     C*
095600000000     C                   ADD       1             J
095700000000E   3C                   ENDDO
095800000000IF  3C     PARERR        IFEQ      '1'
095900000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
096000000000E   3C                   ENDIF
096100000000E   2C                   ENDIF
096200000000     C* ----- ----- ----- -----
096300000000     C* ALTRI CONTROLLI
096400000000     C* ----- ----- ----- -----
096500000000IF  2C     PARERR        IFEQ      '0'
096600000000     C                   Z-ADD     1             J
096700000000DO  3C     J             DOWLE     8                                            O R A R I
096800000000     C     PARERR        ANDEQ     '0'
096900000000IF  4C     GCM(J)        IFNE      *BLANKS
097000000000     C     J             OCCUR     OGSE
097100000000     C                   MOVEL     *BLANKS       WALF5A            5            *AZZERA CAMPI LAVORO
097200000000     C                   MOVEL     *BLANKS       WALF5P            5
097300000000     C                   Z-ADD     2             A                 4 0
097400000000     C                   Z-ADD     *ZEROS        AA                4 0
097500000000DO  5C     A             DOWLE     TOTTLR                                       *ARRIVO
097600000000     C     PARERR        ANDEQ     '0'
097700000000IF  6C     OGGA(A)       IFNE      *BLANKS                                      *SE ARRIVO PIENO
097800000000     C     A             SUB       1             AA
097900000000DO  7C     AA            DOWGE     1                                            *ARRIVO-1
098000000000     C     PARERR        ANDEQ     '0'
098100000000     C* CIASCUN ORARIO DI ARRIVO (SE PIENO) DEVE ESSERE MAGGIORE DI CIASCUN
098200000000     C* ORARIO DI ARRIVO PRECEDENTE TRANNE LA PRIMA FERMATA
098300000000     C                   MOVEL     OGGA(A)       WALF5P                         *PARTENZA='X+9999'
098400000000     C                   MOVE      OHHA(A)       WALF5P
098500000000     C                   MOVEL     OGGA(AA)      WALF5A                         *PARTENZA-1='X+9999'
098600000000     C                   MOVE      OHHA(AA)      WALF5A
098700000000IF  8C     WALF5P        IFLE      WALF5A
098800000000     C                   MOVEL     '1'           PARERR
098900000000E   8C                   ENDIF
099000000000     C*
099100000000     C                   SUB       1             AA
099200000000E   7C                   ENDDO
099300000000E   6C                   ENDIF
099400000000     C*
099500000000     C                   ADD       1             A
099600000000E   5C                   ENDDO
099700000000E   4C                   ENDIF
099800000000     C*
099900000000     C                   ADD       1             J
100000000000E   3C                   ENDDO
100100000000IF  3C     PARERR        IFEQ      '1'
100200000000     C                   MOVEL     MSG(20)       PARMSG                         *MESSAGGIO GENGERICO
100300000000E   3C                   ENDIF
100400000000E   2C                   ENDIF
100500000000     C***
100600000000     C* CONTROLLA LE FERMATE CON QUELLE DI ALTRE DECORRENZE
100700000000     C***
100800000000     C* ----- ----- ----- -----
100900000000     C* IL N° SEQUENZA DI FERMATE+TIPO NON VARIA AL VARIARE DELLE DECORRENZE
101000000000     C* ----- ----- ----- -----
101100000000     C* SOSPESO
101200000000     C* ----- ----- ----- -----
101300000000     C* I GG APERTURA FV DI FERMATE+TIPO NON VARIA AL VARIARE DELLE DECORRENZE
101400000000     C* ----- ----- ----- -----
101500000000     C* CARICA IN MEMORIA LE FERMATE DEL TRAINO CON DECORRENZA DIVERSA
101600000000IF  2C     PARERR        IFEQ      '0'
101700000000     C                   Z-ADD     *ZEROS        I
101800000000     C                   MOVEL     'N'           $OK
101900000000     C                   Z-ADD     PARTRN        KLRTRN                         *NUMERO TRAINO
102000000000     C     KE2TLR        SETLL     TNTLR01L
102100000000     C     KE2TLR        READE(N)  TNTLR01L                               99
102200000000DO  3C     *IN99         DOWEQ     *OFF
102300000000     C     $OK           ANDEQ     'N'
102400000000     C                   Z-ADD     TLRDDE        DEPDDE                         *DEPOSITO DECORRENZA
102500000000DO  4C     *IN99         DOWEQ     *OFF
102600000000     C     DEPDDE        ANDEQ     TLRDDE
102700000000IF  5C     TLRATB        IFEQ      *BLANKS                                      *NO ANNULLATI
102800000000     C     TLRDDE        ANDNE     PARDDE                                       *NO TRAINO IN CHK
102900000000     C     TLRGFV        ANDNE     ' '
103000000000     C                   ADD       1             I
103100000000     C                   MOVEL     TLRTFM        JTFM(I)
103200000000     C                   Z-ADD     TLRFIL        JFIL(I)
103300000000     C                   MOVEL     TLRGFV        JGFV(I)
103400000000     C                   MOVEL     'S'           $OK
103500000000E   5C                   ENDIF
103600000000     C     KE2TLR        READE(N)  TNTLR01L                               99
103700000000E   4C                   ENDDO
103800000000     C                   Z-ADD     I             TO2TLR
103900000000E   3C                   ENDDO
104000000000     C*
104100000000     C* SE C'E' UN TRAINO CON DECORRENZA DIVERSA, CONTROLLA I GIORNI APERTURA FV
104200000000IF  3C     $OK           IFEQ      'S'
104300000000DO  4C     1             DO        TOTTLR        J
104400000000DO  5C     1             DO        TO2TLR        I
104500000000IF  6C     FFIL(J)       IFEQ      JFIL(I)                                      *STESSA FERMATA+TIPO
104600000000     C     FTFM(J)       ANDEQ     JTFM(I)
104700000000     C     FGFV(J)       ANDNE     ' '
104800000000IF  7C     FGFV(J)       IFNE      JGFV(I)                                      *GG APERTURA DIVERSO
104900000000     C                   MOVEL     '1'           PARERR
105000000000     C                   MOVEL     MSG(14)       PARMSG
105100000000E   7C                   ENDIF
105200000000E   6C                   ENDIF
105300000000E   5C                   ENDDO
105400000000E   4C                   ENDDO
105500000000E   3C                   ENDIF
105600000000E   2C                   ENDIF
105700000000     C*
105800000000E   1C                   ENDIF
105900000000     C*
106000000000     C                   ENDSR
106100000000     C*------------------------------------------------------------------------*
106200000000     C* METRIC - CONTROLLO DI CONGRUITA' PER LA GESTIONE MCUBI ASSEGNATI
106300000000     C*------------------------------------------------------------------------*
106400000000     C     METRIC        BEGSR
106500000000     C*
106600000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
106700000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
106800000000     C                   MOVEL     'S'           $OK                            *RECORD OK
106900000000     C***
107000000000     C* CONTROLLA I DATI CHE CI DEVONO / NON DEVONO ESSERE
107100000000     C***
107200000000     C* ----- ----- ----- -----
107300000000     C* ESISTONO I METRICUBI
107400000000     C* ----- ----- ----- -----
107500000000IF  2C     PARERR        IFEQ      '0'
107600000000IF  3C     MTFP(1)       IFEQ      *ZEROS
107700000000     C                   MOVEL     '1'           PARERR
107800000000     C                   MOVEL     MSG(21)       PARMSG
107900000000E   3C                   ENDIF
108000000000E   2C                   ENDIF
108100000000     C* ----- ----- ----- -----
108200000000     C* LE FILIALE FERMATE CHE CARICANO DEVONO ESSERE NEI MCUBI COME PARTENZA
108300000000     C* ----- ----- ----- -----
108400000000IF  2C     PARERR        IFEQ      '0'
108500000000DO  3C     1             DO        TOTTLR        JJ                             FERMATE
108600000000IF  4C     FCAR(JJ)      IFEQ      'S'
108700000000     C                   MOVEL     'N'           $OK
108800000000     C                   Z-ADD     1             J
108900000000DO  5C     J             DO        TOTTLM        J                              METRICUBI
109000000000IF  6C     FFIL(JJ)      IFEQ      MTFP(J)
109100000000     C                   MOVEL     'S'           $OK
109200000000E   6C                   ENDIF
109300000000E   5C                   ENDDO
109400000000E   5C     $OK           IFEQ      'N'
109500000000     C                   MOVEL     '1'           PARERR
109600000000     C                   MOVEL     MSG(22)       PARMSG
109700000000E   5C                   ENDIF
109800000000E   4C                   ENDIF
109900000000E   3C                   ENDDO
110000000000E   2C                   ENDIF
110100000000     C* ----- ----- ----- -----
110200000000     C* LE FILIALE FERMATE CHE SCARICANO DEVONO ESSERE NEI MCUBI COME ARRIVO
110300000000     C* ----- ----- ----- -----
110400000000IF  2C     PARERR        IFEQ      '0'
110500000000DO  3C     1             DO        TOTTLR        JJ                             FERMATE
110600000000IF  4C     FSCA(JJ)      IFEQ      'S'
110700000000     C                   MOVEL     'N'           $OK
110800000000     C                   Z-ADD     1             J
110900000000DO  5C     J             DO        TOTTLM        J                              METRICUBI
111000000000IF  6C     FFIL(JJ)      IFEQ      MTFA(J)
111100000000     C                   MOVEL     'S'           $OK
111200000000E   6C                   ENDIF
111300000000E   5C                   ENDDO
111400000000E   5C     $OK           IFEQ      'N'
111500000000     C                   MOVEL     '1'           PARERR
111600000000     C                   MOVEL     MSG(23)       PARMSG
111700000000E   5C                   ENDIF
111800000000E   4C                   ENDIF
111900000000E   3C                   ENDDO
112000000000E   2C                   ENDIF
112100000000     C* ----- ----- ----- -----
112200000000     C* BISOGNA ASSEGNARE I MC CON TLMTFP=TLTTFP, TLMTFA=TLTTFA, TLMTFF=TLTTFA
112300000000     C* ----- ----- ----- -----
112400000000IF  2C     PARERR        IFEQ      '0'
112500000000     C                   Z-ADD     1             J
112600000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
112700000000DO  3C     J             DO        TOTTLM        J                              METRICUBI
112800000000IF  4C     MTFP(J)       IFEQ      TLTTFP
112900000000     C     MTFA(J)       ANDEQ     TLTTFA
113000000000     C     MTFF(J)       ANDEQ     TLTTFA
113100000000     C                   MOVEL     'S'           $OK
113200000000E   4C                   ENDIF
113300000000E   3C                   ENDDO
113400000000IF  3C     $OK           IFEQ      'N'
113500000000     C                   MOVEL     '1'           PARERR
113600000000     C                   MOVEL     MSG(24)       ERRM24
113700000000     C                   MOVEL     TLTTFP        ERRTFP
113800000000     C                   MOVEL     TLTTFA        ERRTFA
113900000000     C                   MOVEL     ERRM24        PARMSG
114000000000E   3C                   ENDIF
114100000000E   2C                   ENDIF
114200000000     C***
114300000000     C* CONTROLLA I DATI CHE CI SONO
114400000000     C***
114500000000IF  2C     PARERR        IFEQ      '0'
114600000000     C                   Z-ADD     1             J
114700000000DO  3C     J             DOWLE     TOTTLM                                       METRICUBI
114800000000     C     PARERR        ANDEQ     '0'
114900000000     C* ----- ----- ----- -----
115000000000     C* PARTENZA
115100000000     C* ----- ----- ----- -----
115200000000     C* ESISTENTE
115300000000     C                   MOVEL     MTFP(J)       WORG
115400000000     C                   EXSR      CHKFIL
115500000000     C* ESISTENTE COME FERMATA DI CARICO
115600000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
115700000000DO  4C     1             DO        TOTTLR        JJ
115800000000IF  5C     FFIL(JJ)      IFEQ      MTFP(J)
115900000000     C     FCAR(JJ)      ANDEQ     'S'
116000000000     C                   MOVEL     'S'           $OK                            *VALIDO
116100000000E   5C                   ENDIF
116200000000E   4C                   ENDDO
116300000000IF  4C     $OK           IFEQ      'N'
116400000000     C                   MOVEL     '1'           PARERR
116500000000E   4C                   ENDIF
116600000000     C* ----- ----- ----- -----
116700000000     C* ARRIVO FINALE
116800000000     C* ----- ----- ----- -----
116900000000     C* ESISTENTE
117000000000     C                   MOVEL     MTFF(J)       WORG
117100000000     C                   Z-ADD     *ZEROS        WFEL
117200000000     C                   Z-ADD     *ZEROS        WTFP
117300000000     C                   Z-ADD     *ZEROS        WTFA
117400000000     C                   EXSR      CHKFIL
117500000000     C* NO AFFLUENZA / NO DEFLUENZA
117600000000     C* TFF=TRMINAL DI ARRIVO DA ORGANIGRAMMA   OPPURE
117700000000     C* TFP+TFA+TFF= ESISTENTE COME ECCEZIONE
117800000000     C* NB: QUESTI CONTROLLI LI FA DOPO AVER CONTROLLATO TUTTE LE 3 FILIALI
117900000000     C*
118000000000     C* DEFLUENZA
118100000000     C* TFF = TFA
118200000000IF  4C     DEPDEF        IFEQ      'S'                                          *DEFLUENZA
118300000000     C     PARINL        OREQ      'D'                                          *INOLTRO DEFLUENZA
118400000000IF  5C     MTFF(J)       IFNE      MTFA(J)                                      *TFF=TFA
118500000000     C                   MOVEL     '1'           PARERR
118600000000E   5C                   ENDIF
118700000000E   4C                   ENDIF
118800000000     C* AFFLUENZA
118900000000     C* TFF = TFP
119000000000IF  4C     TLTTTR        IFEQ      'A'                                          *AFFLUENZA
119100000000IF  5C     MTFF(J)       IFNE      MTFP(J)                                      *TFF=TFP
119200000000     C                   MOVEL     '1'           PARERR
119300000000E   5C                   ENDIF
119400000000E   4C                   ENDIF
119500000000     C* MEMORIZZA SE E' UNA FERMATA DI SCARICO PER CONTROLLI SUCCESSIVI
119600000000     C                   MOVEL     '0'           $SCA
119700000000DO  4C     1             DO        TOTTLR        JJ
119800000000IF  5C     FFIL(JJ)      IFEQ      MTFF(J)
119900000000     C     FSCA(JJ)      ANDEQ     'S'
120000000000     C                   MOVEL     '1'           $SCA                           *VALIDO
120100000000E   5C                   ENDIF
120200000000E   4C                   ENDDO
120300000000     C* ----- ----- ----- -----
120400000000     C* SCARICO
120500000000     C* ----- ----- ----- -----
120600000000     C* ESISTENTE
120700000000     C                   MOVEL     MTFA(J)       WORG
120800000000     C                   EXSR      CHKFIL
120900000000     C                   Z-ADD     WTFP          DEPTFP
121000000000     C                   Z-ADD     WTFA          DEPTFA
121100000000     C* ESISTENTE COME FERMATA DI SCARICO
121200000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
121300000000DO  4C     1             DO        TOTTLR        JJ
121400000000IF  5C     FFIL(JJ)      IFEQ      MTFA(J)
121500000000     C     FSCA(JJ)      ANDEQ     'S'
121600000000     C                   MOVEL     'S'           $OK                            *VALIDO
121700000000E   5C                   ENDIF
121800000000E   4C                   ENDDO
121900000000IF  4C     $OK           IFEQ      'N'
122000000000     C                   MOVEL     '1'           PARERR
122100000000E   4C                   ENDIF
122200000000     C* SEQ. FERMATA DI ARRIVO > SEQ. FERMATA DI PARTENZA
122300000000IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
122400000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
122500000000     C                   CLEAR                   JSEP                           *SEQUENZE PART
122600000000     C                   CLEAR                   JTFP                           *TIPO FERMATA PART
122700000000     C                   CLEAR                   JSEA                           *SEQUENZE ARR
122800000000     C                   CLEAR                   JTFA                           *TIPO FERMATA ARR
122900000000     C                   MOVEL     'N'           $OK                            *NON VALIDO
123000000000DO  5C     1             DO        TOTTLR        JJ
123100000000IF  6C     MTFA(J)       IFEQ      FFIL(JJ)
123200000000IF  7C     FTFM(JJ)      IFEQ      'A'                                          *ANDATA
123300000000     C                   Z-ADD     JJ            JSEA(1)                        *SCARICO
123400000000     C                   MOVEL     FTFM(JJ)      JTFA(1)
123500000000X   7C                   ELSE                                                   *RITORNO
123600000000     C                   Z-ADD     JJ            JSEA(2)                        *SCARICO
123700000000     C                   MOVEL     FTFM(JJ)      JTFA(2)
123800000000E   7C                   ENDIF
123900000000E   6C                   ENDIF
124000000000IF  6C     MTFP(J)       IFEQ      FFIL(JJ)
124100000000IF  7C     FTFM(JJ)      IFEQ      'A'                                          *ANDATA
124200000000     C                   Z-ADD     JJ            JSEP(1)                        *PARTENZA
124300000000     C                   MOVEL     FTFM(JJ)      JTFP(1)
124400000000X   7C                   ELSE                                                   *RITORNO
124500000000     C                   Z-ADD     JJ            JSEP(2)                        *PARTENZA
124600000000     C                   MOVEL     FTFM(JJ)      JTFP(2)
124700000000E   7C                   ENDIF
124800000000E   6C                   ENDIF
124900000000E   5C                   ENDDO
125000000000     C* ANDATA
125100000000IF  5C     JTFA(1)       IFEQ      JTFP(1)                                      *STESSO TIPO FERMATA
125200000000     C     JSEA(1)       ANDGT     JSEP(1)                                      *SEQ ARR > SEQ PART
125300000000     C                   MOVEL     'S'           $OK                            *VALIDO
125400000000E   5C                   ENDIF
125500000000     C* RITORNO
125600000000IF  5C     JTFA(2)       IFEQ      JTFP(2)                                      *STESSO TIPO FERMATA
125700000000     C     JSEA(2)       ANDGT     JSEP(2)                                      *SEQ ARR > SEQ PART
125800000000     C                   MOVEL     'S'           $OK                            *VALIDO
125900000000E   5C                   ENDIF
126000000000IF  5C     $OK           IFEQ      'N'
126100000000     C                   MOVEL     '1'           PARERR
126200000000E   5C                   ENDIF
126300000000E   4C                   ENDIF
126400000000     C*
126500000000     C* SE LA FILIALE ARRIVO FINALE E' UNA FERMATA DI SCARICO, LA FILIALE DI
126600000000     C* SCARICO DEVE ESSERE UGUALE ALLA FILIALE ARRIVO FINALE
126700000000IF  4C     $SCA          IFEQ      'S'
126800000000     C     MTFF(J)       ANDNE     MTFA(J)
126900000000     C                   MOVEL     '1'           PARERR
127000000000E   4C                   ENDIF
127100000000     C***
127200000000     C* IL CASO PARTICOALRE ADESSO SI RICONOSCE PIU' GUARDANDO L'ORGANIGRAMMA O
127300000000     C* LE ECCEZIONI (PERCHE' FA A PACCHE CON UN ALTRO CONTROLLO), MA GUARDANDO
127400000000     C* CHE SIA UN TRAINI DI SOLA ANDATA E CHE ARRIVO E SCARICO SIANO UGUALI.
127500000000     C***
127600000000     C*--- SE LA FILIALE DI SCARICO NON E' TERMINAL ARRIVO (ORG O ECC)
127700000000     C*--- SCARICO E ARRIVO DEVONO ESSERE UGUALI
127800000000     C*---
127900000000     C*---                 MOVEL'0'       WECC
128000000000     C*---                 Z-ADDMTFA,J    WNUM3B
128100000000     C*---                 EXSR CTREC2
128200000000     C*---                 MOVELWECC      DEPEC2  1
128300000000     C* CASO PARTICOLARE
128400000000IF  4C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
128500000000     C*---       MTFA,J    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
128600000000     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
128700000000IF  5C*---       MTFF,J    IFNE MTFA,J
128800000000     C*---                 MOVEL'1'       PARERR
128900000000E   5C*---                 ENDIF
129000000000E   4C*---                 ENDIF
129100000000     C***
129200000000     C* ARRIVO
129300000000     C***
129400000000     C* SE CASO PARTICOLARE -> (VEDI SOPRA) NON FA QUESTI CONTROLLI
129500000000     C* TFP+TFA+TFF= ESISTENTE COME ECCEZIONE
129600000000     C* SE NON C'E VERIFICO SE E' UN TRANSITO
129700000000     C* SE NON C'E' L'ECCEZIONE -> TFF=TRMINAL DI ARRIVO DA ORGANIGRAMMA
129800000000IF  4C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
129900000000     C*---       MTFA,J    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
130000000000     C*---       DEPEC2    ANDNE'1'                        *FIL=TERM ARR ECC
130100000000X   4C*---                 ELSE                            *CASO NORMALE
130200000000IF  4C     TLTTTR        IFEQ      'S'                                          *SOLO ANDATA
130300000000     C     MTFA(J)       ANDEQ     MTFF(J)                                      *ARRIVO=SCARICO
130400000000     C                   ELSE                                                   *CASO NORMALE
130500000000     C* COMINCIA I CONTROLLI
130600000000IF  5C     TLTTTR        IFNE      'A'                                          *NO AFFLUENZA
130700000000     C     DEPDEF        ANDNE     'S'                                          *NO DEFLUENZA
130800000000     C     PARINL        ANDNE     'D'                                          *NO INOLTRO DEFLUENZ
130900000000     C                   MOVEL     '0'           WECC
131000000000     C                   EXSR      CTREC3
131100000000IF  6C     PARERR        IFEQ      '0'
131200000000IF  7C     WECC          IFNE      '1'
131300000000     C                   MOVEL     MTFF(J)       WORG
131400000000     C                   Z-ADD     *ZEROS        WTFA
131500000000     C                   EXSR      CHKFIL
131600000000IF  8C     MTFF(J)       IFNE      WTFA                                         *TERM ARRIVO STANDAR
131700000000     C                   MOVEL     '1'           PARERR
131800000000E   8C                   ENDIF
131900000000     c*
132000000000     C* SE OK IL TERMINAL ARRIVO, CONTROLLO SE LA FILIALE DI SCARICO E' CORRETTA
132100000000IF  8C     PARERR        IFEQ      '0'
132200000000     c*
132300000000     C* CONTROLLA ECCEZIONE CONE SCARICO
132400000000     C                   MOVEL     '0'           WECC
132500000000     C                   EXSR      CTREC4
132600000000IF  9C     PARERR        IFEQ      '0'
132700000000     C*
132800000000     C* CONTROLLA ORGANIGRAMM
132900000000IF 10C     WECC          IFNE      '1'
133000000000     C                   MOVEL     MTFA(J)       WORG
133100000000     C                   Z-ADD     *ZEROS        WTFA
133200000000     C                   EXSR      CHKFIL
133300000000     C*
133400000000IF 11C     MTFA(J)       IFNE      WTFA                                         *TERM ARRIVO STANDAR
133500000000     C                   MOVEL     '1'           PARERR
133600000000E  11C                   ENDIF
133700000000E  10C                   ENDIF
133800000000E   9C                   ENDIF
133900000000E   8C                   ENDIF
134000000000E   7C                   ENDIF
134100000000E   6C                   ENDIF
134200000000E   5C                   ENDIF
134300000000E   4C                   ENDIF
134400000000     C***
134500000000     C* PARTENZA E SCARICO
134600000000     C***
134700000000     C* LA PARTENZA DEVE ESSERE <> DALLO SCARICO TRANNE CHE NEI LOCALI
134800000000IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
134900000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
135000000000IF  5C     MTFP(J)       IFEQ      MTFA(J)
135100000000     C                   MOVEL     '1'           PARERR
135200000000E   5C                   ENDIF
135300000000E   4C                   ENDIF
135400000000     C***
135500000000     C* PARTENZA E ARRIVO
135600000000     C***
135700000000     C* LA PARTENZA DEVE ESSERE <> DALL'ARRIVO TRANNE CHE NEI LOCALI
135800000000IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
135900000000     C     PARINL        ANDNE     'L'                                          *NO INOLTRO LOCALE
136000000000IF  5C     MTFP(J)       IFEQ      MTFF(J)
136100000000     C                   MOVEL     '1'           PARERR
136200000000E   5C                   ENDIF
136300000000E   4C                   ENDIF
136400000000     C***
136500000000     C* PARTENZA E ARRIVO E SCARICO
136600000000     C***
136700000000     C* NEL LOCALI LA PARTENZA DEVE ESSERE UGUALE ALL'ARRIVO E ALLO SCARICO
136800000000IF  4C     TLTTTR        IFEQ      'L'                                          *LOCALE
136900000000     C     PARINL        OREQ      'L'                                          *INOLTRO LOCALE
137000000000IF  5C     MTFP(J)       IFEQ      MTFF(J)
137100000000     C     MTFA(J)       ANDEQ     MTFF(J)
137200000000X   5C                   ELSE
137300000000     C                   MOVEL     '1'           PARERR
137400000000E   5C                   ENDIF
137500000000E   4C                   ENDIF
137600000000     C***
137700000000     C* ARRIVO E SCARICO
137800000000     C***
137900000000     C* NEGLI INOLTRI ARRIVO E SCARICO DEVONO ESSERE UGUALI (NO TRANSITI)
138000000000     C* SE IL TIPO TRAINO A CUI SI RICONDUCE E' UN SOLO ANDATA
138100000000IF  4C     TLTTTR        IFEQ      'I'                                          *INOLTRO
138200000000     C     PARINL        ANDEQ     'S'                                          *TIPO SOLO ANDATA
138300000000IF  5C     MTFA(J)       IFNE      MTFF(J)
138400000000     C                   MOVEL     '1'           PARERR
138500000000E   5C                   ENDIF
138600000000E   4C                   ENDIF
138700000000     C* ----- ----- ----- -----
138800000000     C* MC NORMALI / BIS
138900000000     C* ----- ----- ----- -----
139000000000IF  1C     TLTBIS        IFEQ      *BLANKS                                      *TRAINO NORMALE
139100000000IF  2C     MMCA(J)       IFEQ      *ZEROS
139200000000     C                   MOVEL     '1'           PARERR
139300000000E   2C                   ENDIF
139400000000X   1C                   ELSE                                                   *TRAINO BIS
139500000000IF  2C     MMCB(J)       IFEQ      *ZEROS
139600000000     C                   MOVEL     '1'           PARERR
139700000000E   2C                   ENDIF
139800000000E   1C                   ENDIF
139900000000     C* ----- ----- ----- -----
140000000000     C* MC ASSEGNATI
140100000000     C* ----- ----- ----- -----
140200000000IF  4C*---       TLTBIS    IFEQ 'E'                        *ECCEZIONALE
140300000000     C*---       MMCA,J    ANDEQ*ZEROS                     *MC ASS OBBLIGATORI
140400000000     C*---                 MOVEL'1'       PARERR
140500000000E   4C*---                 ENDIF
140600000000     C*
140700000000     C                   ADD       1             J
140800000000E   3C                   ENDDO
140900000000IF  3C     PARERR        IFEQ      '1'
141000000000     C                   MOVEL     MSG(30)       PARMSG
141100000000E   3C                   ENDIF
141200000000E   2C                   ENDIF
141300000000     C*
141400000000E   1C                   ENDIF
141500000000     C*
141600000000     C                   ENDSR
141700000000     C*------------------------------------------------------------------------*
141800000000     C* LISTIN - CONTROLLO DI CONGRUITA' PER LA GESTIONE LISTINI
141900000000     C*------------------------------------------------------------------------*
142000000000     C     LISTIN        BEGSR
142100000000     C*
142200000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
142300000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
142400000000     C                   MOVEL     'S'           $OK                            *RECORD OK
142500000000     C***
142600000000     C* CONTROLLA I DATI CHE CI DEVONO ESSERE
142700000000     C***
142800000000     C* ----- ----- ----- -----
142900000000     C* ESISTONO I LISTINI
143000000000     C* ----- ----- ----- -----
143100000000IF  2C     PARERR        IFEQ      '0'
143200000000IF  3C     LPDR(1)       IFEQ      *ZEROS
143300000000     C                   MOVEL     '1'           PARERR
143400000000     C                   MOVEL     MSG(31)       PARMSG
143500000000E   3C                   ENDIF
143600000000E   2C                   ENDIF
143700000000     C* ----- ----- ----- -----
143800000000     C*--- DEVE ESISTERE ALMENO UN LISTINO CON IL TIPO AUTOMEZZO UGUALE A QUELLO
143900000000     C*--- DEL TRAINO O UGUALE A '99' (LISTINO STANDARD)
144000000000     C* ----- ----- ----- -----
144100000000IF  2C*---       PARERR    IFEQ '0'                        *NO ERRORI
144200000000     C*---                 MOVEL'N'       $OK
144300000000     C*---                 Z-ADD1         J
144400000000DO  3C*---       J         DOWLETOTTLL                     LISTINI
144500000000IF  4C*---       LTMZ,J    IFEQ TLTTMZ
144600000000     C*---       LTMZ,J    OREQ '99'
144700000000     C*---                 MOVEL'S'       $OK
144800000000E   4C*---                 ENDIF
144900000000     C*---                 ADD  1         J
145000000000E   3C*---                 ENDDO
145100000000IF  3C*---       $OK       IFEQ 'N'
145200000000     C*---                 MOVEL'1'       PARERR
145300000000     C*---                 MOVELMSG,32    PARMSG
145400000000E   3C*---                 ENDIF
145500000000E   2C*---                 ENDIF
145600000000     C***
145700000000     C* CONTROLLA I DATI IMMESSI
145800000000     C***
145900000000IF  2C     PARERR        IFEQ      '0'                                          *NO ERRORI
146000000000     C                   Z-ADD     1             J
146100000000DO  3C     J             DOWLE     TOTTLL                                       LISTINI
146200000000     C     PARERR        ANDEQ     '0'
146300000000     C* ----- ----- ----- -----
146400000000     C* TIPO AUTOMEZZO
146500000000     C* ----- ----- ----- -----
146600000000     C                   MOVEL     LTMZ(J)       WTMZ
146700000000     C                   EXSR      CHKTMZ
146800000000     C* ----- ----- ----- -----
146900000000     C* TRAZIONISTA
147000000000     C* ----- ----- ----- -----
147100000000     C                   Z-ADD     LPDR(J)       WPDR
147200000000     C                   EXSR      CHKPDR
147300010907     C* ----- ----- ----- -----
147400010907     C* DIVISA
147500010907     C* ----- ----- ----- -----
147600010907     c                   if        ldiv(j) <> *blanks
147700010907     c                   exsr      chkdiv
147800010907     c                   else
147900010907     c                   movel     '1'           parerr
148000010907     c                   endif
148100000000     C* ----- ----- ----- -----
148200000000     C* IMPORTO DI LISTINO
148300000000     C* ----- ----- ----- -----
148400000000IF  4C     LILI(J)       IFEQ      *ZEROS
148500000000     C                   MOVEL     '1'           PARERR
148600000000E   4C                   ENDIF
148700010907      * controllo se ok i decimali
148800010907     c                   move      lili(j)       w_decimali
148900010907     c                   if        w_decimali > 0
149000010907     C                             and §cvfdc <> 'S'
149100010907     c                   eval      parerr = '1'
149200010907     c                   endif
149300000000     C* ----- ----- ----- -----
149400000000     C* IMPORTO DA ESPORRE SUL F.V.
149500000000     C* ----- ----- ----- -----
149600000000IF  4C     LIES(J)       IFEQ      *ZEROS
149700000000     C     LIES(J)       OREQ      LILI(J)
149800000000X   4C                   ELSE
149900000000     C                   MOVEL     '1'           PARERR
150000000000E   4C                   ENDIF
150100000000     C* ----- ----- ----- -----
150200000000     C* CAUSALE DI CONTABILIZZAZIONE
150300000000     C* ----- ----- ----- -----
150400020716     c                   If        DutLpo = 'S'
150500000000     C                   MOVEL     LCAU(J)       WCAU
150600000000     C                   EXSR      CHKCAU
150700000821E   4C                   ENDIF
150800010828
150900000000     C                   ADD       1             J
151000000000E   3C                   ENDDO
151100000000IF  3C     PARERR        IFEQ      '1'
151200000000     C                   MOVEL     MSG(40)       PARMSG
151300000000E   3C                   ENDIF
151400000000E   2C                   ENDIF
151500000000     C*
151600000000E   1C                   ENDIF
151700000000     C*
151800000000     C                   ENDSR
151900000000     C*------------------------------------------------------------------------*
152000000000     C* NOTE - CONTROLLO DI CONGRUITA' PER LA GESTIONE NOTE
152100000000     C*------------------------------------------------------------------------*
152200000000     C     NOTE          BEGSR
152300000000     C*
152400000000IF  1C     PARERR        IFEQ      '0'                                          *NO ERRORI
152500000000     C                   Z-ADD     *ZEROS        J                 5 0          *INDICE
152600000000     C                   Z-ADD     *ZEROS        JJ                5 0          *INDICE
152700000000     C                   MOVEL     'S'           $OK                            *RECORD OK
152800000000     C***
152900000000     C* CONTROLLA I DATI IMMESSI
153000000000     C***
153100000000IF  2C     PARERR        IFEQ      '0'                                          *NO ERRORI
153200000000     C                   Z-ADD     1             J
153300000000DO  3C     J             DOWLE     TOTTLN                                       NOTE
153400000000     C     PARERR        ANDEQ     '0'
153500000000     C* ----- ----- ----- -----
153600000000     C* NUMERO FERMATA ESISTENTE
153700000000     C* ----- ----- ----- -----
153800000000IF  4C     NNFM(J)       IFNE      *ZEROS
153900000000     C                   Z-ADD     NNFM(J)       JJ
154000000000IF  5C     FFIL(JJ)      IFEQ      *ZEROS
154100000000     C                   MOVEL     '1'           PARERR
154200000000E   5C                   ENDIF
154300000000E   4C                   ENDIF
154400000000     C*-------------------
154500000000     C* PROGRESSIVO NOTA
154600000000     C*-------------------
154700000000IF  4C     NPNO(J)       IFEQ      *ZEROS
154800000000     C                   MOVEL     '1'           PARERR
154900000000E   4C                   ENDIF
155000000000     C*-------------------
155100000000     C* TESTO NOTA
155200000000     C*-------------------
155300000000IF  4C     NTNO(J)       IFEQ      *BLANKS
155400000000     C                   MOVEL     '1'           PARERR
155500000000E   4C                   ENDIF
155600000000     C*
155700000000     C                   ADD       1             J
155800000000E   3C                   ENDDO
155900000000IF  3C     PARERR        IFEQ      '1'
156000000000     C                   MOVEL     MSG(50)       PARMSG
156100000000E   3C                   ENDIF
156200000000E   2C                   ENDIF
156300000000     C*
156400000000E   1C                   ENDIF
156500000000     C*
156600000000     C                   ENDSR
156700000000     C*------------------------------------------------------------------------*
156800000000     C* CTRECC - CONTROLLA LE ECCEZIONI (1)
156900000000     C*------------------------------------------------------------------------*
157000000000     C     CTRECC        BEGSR
157100000000     C*
157200000000     C     *LOVAL        SETLL     AZCAE01L
157300000000     C                   READ      AZCAE01L                               99
157400000000DO  1C     *IN99         DOWEQ     *OFF
157500000000     C     WECC          ANDEQ     '0'
157600970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
157700970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
157800970717     C     CAETFP        ANDGT     *ZEROS
157900970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
158000970717     C     CAETFA        ANDGT     *ZEROS
158100970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
158200000000     C     CAETFA        ANDEQ     FFIL(JJ)                                     *FILIALE ARRIVO
158300000000     C     CAETFE        ANDEQ     TLTTFP                                       *TERM ARR ECC=TFP TR
158400000000     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
158500000000     C     CAEDSC        ANDGE     TLTDSC
158600000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
158700970717E   4C                   ENDIF
158800970717E   3C                   ENDIF
158900970717E   2C                   ENDIF
159000000000     C                   READ      AZCAE01L                               99
159100000000E   1C                   ENDDO
159200000000     C*
159300000000     C                   ENDSR
159400000000     C*------------------------------------------------------------------------*
159500000000     C* CTREC2 - CONTROLLA LE ECCEZIONI (2)
159600000000     C*------------------------------------------------------------------------*
159700000000     C     CTREC2        BEGSR
159800000000     C*
159900000000     C     *LOVAL        SETLL     AZCAE01L
160000000000     C                   READ      AZCAE01L                               99
160100000000DO  1C     *IN99         DOWEQ     *OFF
160200000000     C     WECC          ANDEQ     '0'
160300970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
160400970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
160500970717     C     CAETFP        ANDGT     *ZEROS
160600970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
160700970717     C     CAETFA        ANDGT     *ZEROS
160800970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
160900000000     C     CAETFE        ANDEQ     WNUM3B                                       *FIL ARRIVO/SCARICO
161000000000     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
161100000000     C     CAEDSC        ANDGE     TLTDSC
161200000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
161300970717E   4C                   ENDIF
161400970717E   3C                   ENDIF
161500970717E   2C                   ENDIF
161600000000     C                   READ      AZCAE01L                               99
161700000000E   1C                   ENDDO
161800000000     C*
161900000000     C                   ENDSR
162000000000     C*------------------------------------------------------------------------*
162100000000     C* CTREC3 - CONTROLLA LE ECCEZIONI (3)
162200000000     C*------------------------------------------------------------------------*
162300000000     C     CTREC3        BEGSR
162400000000     C*
162500000000     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
162600000000     C                   Z-ADD     MTFP(J)       KAETFP                         *PARTENZA
162700000000     C                   Z-ADD     MTFF(J)       KAETFA                         *ARRIVO
162800000000     C     KEYCAE        SETLL     AZCAE01L
162900000000     C     KEYCAE        READE     AZCAE01L                               99
163000000000DO  1C     *IN99         DOWEQ     *OFF
163100000000     C     WECC          ANDEQ     '0'
163200970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
163300970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
163400970717     C     CAETFP        ANDGT     *ZEROS
163500970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
163600970717     C     CAETFA        ANDGT     *ZEROS
163700970717IF  4C     CAEDDE        IFLE      TLTDDE                                       IN DECORRENZA
163800000000     C     CAEDSC        ANDGE     TLTDSC
163900970717IF  5C     CAETFE        IFEQ      MTFA(J)                                      *ECCEZ=SCARICO
164000000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
164100970717X   5C                   ELSE
164200970717IF  6C     CAETFE        IFEQ      MTFF(J)                                      *ECCEZ=ARRIVO FINALE
164300000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
164400970717X   6C                   ELSE
164500000000     C                   MOVEL     '1'           PARERR                         *ERRORE     EZIONE
164600970717E   6C                   ENDIF
164700970717E   5C                   ENDIF
164800000000     C*
164900000000     C* NON SI POSSONO ACCETTARE ECCEZIONI A CAVALLO DI UNA VALIDITA' TRAINO
165000970717X   4C                   ELSE
165100970717IF  5C     CAEDDE        IFGT      TLTDSC
165200000000     C     CAEDSC        ORLT      TLTDDE
165300970717X   5C                   ELSE
165400000000     C                   MOVEL     '1'           PARERR                         *ERRORE     EZIONE
165500970717E   5C                   ENDIF
165600970717E   4C                   ENDIF
165700000000E   3C                   ENDIF
165800000000E   2C                   ENDIF
165900000000     C     KEYCAE        READE     AZCAE01L                               99
166000000000E   1C                   ENDDO
166100000000     C*
166200000000     C                   ENDSR
166300000000     C*------------------------------------------------------------------------*
166400000000     C* CTREC4 - CONTROLLA LE ECCEZIONI (4)
166500000000     C*------------------------------------------------------------------------*
166600000000     C     CTREC4        BEGSR
166700000000     C*
166800000000     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
166900000000     C                   Z-ADD     MTFP(J)       KAETFP                         *PARTENZA
167000000000     C                   Z-ADD     MTFA(J)       KAETFA                         *SCARICO
167100000000     C     KEYCAE        SETLL     AZCAE01L
167200000000     C     KEYCAE        READE     AZCAE01L                               99
167300000000DO  1C     *IN99         DOWEQ     *OFF
167400000000     C     WECC          ANDEQ     '0'
167500970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
167600970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
167700970717     C     CAETFP        ANDGT     *ZEROS
167800970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
167900970717     C     CAETFA        ANDGT     *ZEROS
168000970717IF  4C     CAEDDE        IFLE      TLTDDE                                       IN DECORRENZA
168100000000     C     CAEDSC        ANDGE     TLTDSC
168200970717IF  5C     CAETFE        IFEQ      MTFA(J)                                      *ECCEZ=SCARICO
168300000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
168400970717X   5C                   ELSE
168500000000     C                   MOVEL     '1'           PARERR                         *ERRORE ECCEZIONE
168600970717E   5C                   ENDIF
168700000000     C*
168800000000     C* NON SI POSSONO ACCETTARE ECCEZIONI A CAVALLO DI UNA VALIDITA' TRAINO
168900970717X   4C                   ELSE
169000970717IF  5C     CAEDDE        IFGT      TLTDSC
169100000000     C     CAEDSC        ORLT      TLTDDE
169200970717X   5C                   ELSE
169300000000     C                   MOVEL     '1'           PARERR                         *ERRORE ECCEZIONE
169400970717E   5C                   ENDIF
169500970717E   4C                   ENDIF
169600970717E   3C                   ENDIF
169700000000E   2C                   ENDIF
169800000000     C     KEYCAE        READE     AZCAE01L                               99
169900000000E   1C                   ENDDO
170000000000     C*
170100000000     C                   ENDSR
170200000000     C*------------------------------------------------------------------------*
170300000000     C* CHKFIL - CONTROLLA LA FILIALE
170400000000     C*------------------------------------------------------------------------*
170500000000     C     CHKFIL        BEGSR
170600000000     C*
170700000519     C     *LIKE         DEFINE    ORGFIL        WORG                           *FILIALE
170800971128     C     *LIKE         DEFINE    ORGFIL        WFEL                           *FILIALE ELABORATORE
170900971020     C     *LIKE         DEFINE    ORGFIL        WTFP                           *TERMINAL PARTENZA
171000971020     C     *LIKE         DEFINE    ORGFIL        WTFA                           *TERMINAL ARRIVO
171100000519     C     *LIKE         DEFINE    CAEEPA        WPOS                           *FILIALE
171200000000     C*
171300000000IF  1C     WORG          IFEQ      *ZEROS
171400000000     C                   MOVEL     '1'           PARERR
171500000000X   1C                   ELSE
171600000000     C                   SETOFF                                           28
171700000000     C                   Z-ADD     1             I
171800000000     C     WORG          LOOKUP    ORG(I)                                 28
171900000000IF  2C     *IN28         IFEQ      *OFF
172000000000     C                   MOVEL     '1'           PARERR
172100000000X   2C                   ELSE
172200000000     C                   Z-ADD     FEL(I)        WFEL
172300000000     C                   Z-ADD     TFP(I)        WTFP
172400000000     C                   Z-ADD     TFA(I)        WTFA
172500000519     C                   MOVEL     PORG(I)       WPOS
172600000519E   2C                   ENDIF
172700000000E   1C                   ENDIF
172800000000     C*
172900000000     C                   ENDSR
173000000000     C*------------------------------------------------------------------------*
173100000000     C* CHKORA - CONTROLLA L'ORA
173200000000     C*------------------------------------------------------------------------*
173300000000     C     CHKORA        BEGSR
173400000000     C*
173500000000     C     *LIKE         DEFINE    TLOHHA        WORA
173600000000     C                   Z-ADD     *ZEROS        WHH               2 0
173700000000     C                   Z-ADD     *ZEROS        WMM               2 0
173800000000     C* ORE E MINUTI
173900000000IF  1C     WORA          IFEQ      *ZEROS
174000000000     C                   MOVEL     '1'           PARERR
174100000000E   1C                   ENDIF
174200000000     C* ORE
174300000000     C                   MOVEL     WORA          WHH
174400000000IF  1C     WHH           IFGE      0                                            0 -> 24
174500000000     C     WHH           ANDLE     24
174600000000     C                   ELSE
174700000000     C                   MOVEL     '1'           PARERR
174800000000E   1C                   ENDIF
174900000000     C* MINUTI
175000000000     C                   MOVE      WORA          WMM
175100000000IF  1C     WMM           IFGE      0                                            0 -> 59
175200000000     C     WMM           ANDLE     59
175300000000     C                   ELSE
175400000000     C                   MOVEL     '1'           PARERR
175500000000E   1C                   ENDIF
175600000000     C* ORE 24:00
175700000000IF  1C     WHH           IFEQ      24                                           *SOLO 24:00
175800000000     C     WMM           ANDGT     0
175900000000     C                   MOVEL     '1'           PARERR
176000000000E   1C                   ENDIF
176100000000     C*
176200000000     C                   ENDSR
176300000000     C*------------------------------------------------------------------------*
176400000000     C* CHKTMZ - CONTROLLA IL TIPO AUTOMEZZO
176500000000     C*------------------------------------------------------------------------*
176600000000     C     CHKTMZ        BEGSR
176700000000     C*
176800000000     C     *LIKE         DEFINE    TLLTMZ        WTMZ                           *AUTOMEZZO
176900000000     C*
177000000000IF  1C     WTMZ          IFEQ      *BLANKS
177100000000     C                   MOVEL     '1'           PARERR
177200000000X   1C                   ELSE
177300000000     C                   SETOFF                                           28
177400000000     C     WTMZ          LOOKUP    TMZ                                    28
177500000000     C                   MOVEL     WTMZ          WALF1             1
177600000000IF  2C     *IN28         IFEQ      *ON
177700000000     C     WALF1         ANDNE     '9'
177800000000     C     WTMZ          OREQ      '99'
177900000000X   2C                   ELSE
178000000000     C                   MOVEL     '1'           PARERR
178100000000E   2C                   ENDIF
178200000000E   1C                   ENDIF
178300000000     C*
178400000000     C                   ENDSR
178500000000     C*------------------------------------------------------------------------*
178600000000     C* CHKPDR - CONTROLLA IL TRAZIONISTA
178700000000     C*------------------------------------------------------------------------*
178800000000     C     CHKPDR        BEGSR
178900000000     C*
179000000000     C     *LIKE         DEFINE    TLLPDR        WPDR
179100000000     C*
179200000821     C                   EVAL      KLZTIP = 'T '
179300000000     C                   MOVEL     WPDR          KLZPDR
179400000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
179500000000IF  1C     *IN28         IFEQ      *ON
179600000000     C     WPDR          ANDNE     9999999
179700000000     C                   MOVEL     '1'           PARERR
179800000000E   1C                   ENDIF
179900000000     C*
180000000000     C                   ENDSR
180100000000     C*------------------------------------------------------------------------*
180200000000     C* CHKCAU - CONTROLLA LA CAUSALE DI CONTABILIZZAZIONE
180300000000     C*------------------------------------------------------------------------*
180400000000     C     CHKCAU        BEGSR
180500000000     C*
180600000000     C     *LIKE         DEFINE    TLLCAU        WCAU
180700000000     C*
180800000000IF  1C     WCAU          IFEQ      *BLANKS
180900000000     C                   MOVEL     '1'           PARERR
181000000000X   1C                   ELSE
181100000000     C                   SETOFF                                           28
181200000000     C     WCAU          LOOKUP    CAU                                    28
181300000000IF  2C     *IN28         IFEQ      *OFF
181400000000     C                   MOVEL     '1'           PARERR
181500000000E   2C                   ENDIF
181600000000E   1C                   ENDIF
181700000000     C*
181800000000     C                   ENDSR
181900010828     C*------------------------------------------------------------------------*
182000010828     C* CHKDIV - CONTROLLA LA DIVISA
182100010828     C*          INPUT  -> Wdiv  - divisa
182200010828     C*          OUTPUT ->
182300010828     C*------------------------------------------------------------------------*
182400010828     c     CHKdiv        BEGSR
182500010828
182600010828      * ESISTENTE
182700010828     c                   clear                   dscv
182800020716     c                   Z-Add     1             KtbKut
182900010828     c                   movel     'CV'          ktbcod
183000010828     c                   movel(p)  ldiv(j)       ktbkey
183100010828     c     keytab        chain     tabel00f
183200010828     c                   if        not%found(tabel00f)
183300010828     c                   movel     '1'           parerr
183400010828     c                   endif
183500010828     c                   movel     tbluni        dscv
183600010828      * consentita nei traini
183700010828     c                   if        §cvtra <> 'S'
183800010828     c                   movel     '1'           parerr
183900010828     c                   endif
184000010828
184100010828     c                   ENDSR
184200000000     C*------------------------------------------------------------------------*
184300000000     C* MEMTRN - MEMORIZZA TUTTI I DATI DEL TRAINO
184400000000     C*------------------------------------------------------------------------*
184500000000     C     MEMTRN        BEGSR
184600000000     C*
184700000000     C* REIMPOSTA I CONTATORI RECORD
184800000000     C                   Z-ADD     *ZEROS        I                 5 0          *INDICE
184900000000     C                   Z-ADD     *ZEROS        TOTTLR            5 0          *N° TOTALE FERMATE
185000000000     C                   Z-ADD     *ZEROS        TOTTLM            5 0          *N° TOTALE MCUBI
185100000000     C                   Z-ADD     *ZEROS        TOTTLL            5 0          *N° TOTALE LISTINI
185200000000     C                   Z-ADD     *ZEROS        TOTTLN            5 0          *N° TOTALE NOTE
185300000000     C* TRAINO
185400000000     C                   Z-ADD     PARTRN        KLTTRN                         *NUMERO TRAINO
185500000000     C                   Z-ADD     PARDDE        KLTDDE                         *DATA DECORRENZA
185600000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
185700000000IF  1C     *IN99         IFEQ      *OFF
185800000000     C                   MOVEA     TLTGSE        GIO                            *GG PARTENZA TRAINO
185900020716     c                   Z-Add     1             KtbKut
186000000000     C                   MOVEL     'TV'          KTBCOD
186100000000     C                   MOVEL(P)  TLTTTR        KTBKEY
186200000000     C     KEYTAB        CHAIN     TABEL00F                           99
186300000000     C                   MOVEL     TBLUNI        DSTV
186400000000     C                   MOVEL     §TVDEF        DEPDEF                         *TRAINO DEFLUENZA
186500000000     C* FERMATE
186600000000     C                   Z-ADD     PARTRN        KLRTRN                         *NUMERO TRAINO
186700000000     C                   Z-ADD     PARDDE        KLRDDE                         *DATA DECORRENZA
186800000000     C     KEYTLR        SETLL     TNTLR01L
186900000000     C     KEYTLR        READE(N)  TNTLR01L                               99
187000000000     C                   Z-ADD     *ZEROS        I
187100000000DO  2C     *IN99         DOWEQ     *OFF
187200000000IF  3C     TLRATB        IFEQ      *BLANKS
187300000000     C                   ADD       1             I
187400000000     C                   MOVEL     TLRTFM        FTFM(I)
187500000000     C                   Z-ADD     TLRFIL        FFIL(I)
187600000000     C                   MOVEL     TLRSCA        FSCA(I)
187700000000     C                   MOVEL     TLRCAR        FCAR(I)
187800000000     C                   Z-ADD     TLRKMT        FKMT(I)
187900000000     C                   Z-ADD     TLRTEM        FTEM(I)
188000000000     C                   MOVEL     TLRGFV        FGFV(I)
188100000000E   3C                   ENDIF
188200000000     C     KEYTLR        READE(N)  TNTLR01L                               99
188300000000E   2C                   ENDDO
188400000000     C                   Z-ADD     I             TOTTLR
188500000000     C* ORARI
188600000000     C                   Z-ADD     PARTRN        KLOTRN                         *NUMERO TRAINO
188700000000     C                   Z-ADD     PARDDE        KLODDE                         *DATA DECORRENZA
188800000000     C     KEYTLO        SETLL     TNTLO02L
188900000000     C     KEYTLO        READE     TNTLO02L                               99
189000000000     C                   Z-ADD     *ZEROS        I
189100000000DO  2C     *IN99         DOWEQ     *OFF
189200000000     C                   MOVEL     TLOGSE        DEPGSE
189300000000     C                   Z-ADD     *ZEROS        I
189400000000DO  3C     *IN99         DOWEQ     *OFF
189500000000     C     TLOGSE        ANDEQ     DEPGSE
189600000000IF  4C     TLOATB        IFEQ      *BLANKS
189700000000IF  5C     TLOGSE        IFEQ      ' '
189800000000     C                   Z-ADD     8             WG                1 0          *GIORNO SETTIMANA
189900000000X   5C                   ELSE
190000000000     C                   MOVEL     TLOGSE        WG
190100000000E   5C                   ENDIF
190200000000     C     WG            OCCUR     OGSE
190300000000     C                   ADD       1             I
190400000000     C                   MOVEL     TLOGGA        OGGA(I)
190500000000     C                   Z-ADD     TLOHHA        OHHA(I)
190600000000     C                   MOVEL     TLOGGP        OGGP(I)
190700000000     C                   Z-ADD     TLOHHP        OHHP(I)
190800000000     C                   MOVEL     WG            GCM(WG)                        *ORARIO PRESENTE
190900000000     C                   MOVEL     *BLANKS       GCA(WG)
191000000000E   4C                   ENDIF
191100000000     C     KEYTLO        READE     TNTLO02L                               99
191200000000E   3C                   ENDDO
191300000000E   2C                   ENDDO
191400000000     C* MCUBI ASSEGNATI
191500000000     C                   Z-ADD     PARTRN        KLMTRN                         *NUMERO TRAINO
191600000000     C                   Z-ADD     PARDDE        KLMDDE                         *DATA DECORRENZA
191700000000     C     KEYTLM        SETLL     TNTLM01L
191800000000     C     KEYTLM        READE(N)  TNTLM01L                               99
191900000000     C                   Z-ADD     *ZEROS        I
192000000000DO  2C     *IN99         DOWEQ     *OFF
192100000000IF  3C     TLMATB        IFEQ      *BLANKS
192200000000     C                   ADD       1             I
192300000000     C                   Z-ADD     TLMTFP        MTFP(I)
192400000000     C                   Z-ADD     TLMTFA        MTFA(I)
192500000000     C                   Z-ADD     TLMTFF        MTFF(I)
192600000000     C                   Z-ADD     TLMMCA        MMCA(I)
192700000000     C                   Z-ADD     TLMMCB        MMCB(I)
192800000000     C                   Z-ADD     TLMMCT        MMCT(I)
192900000000E   3C                   ENDIF
193000000000     C     KEYTLM        READE(N)  TNTLM01L                               99
193100000000E   2C                   ENDDO
193200000000     C                   Z-ADD     I             TOTTLM
193300000000     C* LISTINI
193400000000     C                   Z-ADD     PARTRN        KLLTRN                         *NUMERO TRAINO
193500000000     C                   Z-ADD     PARDDE        KLLDDE                         *DATA DECORRENZA
193600010828     C     KEYTLL        SETLL     TITLL01L
193700010828     C     KEYTLL        READE(N)  TITLL01L                               99
193800000000     C                   Z-ADD     *ZEROS        I
193900000000DO  2C     *IN99         DOWEQ     *OFF
194000000000IF  3C     TLLATB        IFEQ      *BLANKS
194100000000     C                   ADD       1             I
194200000000     C                   Z-ADD     TLLPDR        LPDR(I)
194300000000     C                   MOVEL     TLLTMZ        LTMZ(I)
194400000000     C                   Z-ADD     TLLILI        LILI(I)
194500000000     C                   Z-ADD     TLLIES        LIES(I)
194600000000     C                   MOVEL     TLLCAU        LCAU(I)
194700010828     c                   movel     tlldiv        ldiv(i)
194800000000E   3C                   ENDIF
194900010828     C     KEYTLL        READE(N)  TITLL01L                               99
195000000000E   2C                   ENDDO
195100000000     C                   Z-ADD     I             TOTTLL
195200000000     C* NOTE
195300000000     C                   Z-ADD     PARTRN        KLNTRN                         *NUMERO TRAINO
195400000000     C                   Z-ADD     PARDDE        KLNDDE                         *DATA DECORRENZA
195500000000     C     KEYTLN        SETLL     TNTLN01L
195600000000     C     KEYTLN        READE(N)  TNTLN01L                               99
195700000000     C                   Z-ADD     *ZEROS        I
195800000000DO  2C     *IN99         DOWEQ     *OFF
195900000000IF  3C     TLNATB        IFEQ      *BLANKS
196000000000     C                   ADD       1             I
196100000000     C                   Z-ADD     TLNNFM        NNFM(I)
196200000000     C                   Z-ADD     TLNPNO        NPNO(I)
196300000000     C                   MOVEL     TLNTNO        NTNO(I)
196400000000E   3C                   ENDIF
196500000000     C     KEYTLN        READE(N)  TNTLN01L                               99
196600000000E   2C                   ENDDO
196700000000     C                   Z-ADD     I             TOTTLN
196800000000     C*
196900000000X   1C                   ELSE
197000000000     C                   MOVEL     '1'           PARERR
197100000000     C                   MOVEL     MSG(1)        PARMSG
197200000000     C                   MOVEL     '1'           PARERR
197300000000E   1C                   ENDIF
197400000000     C*
197500000000     C                   ENDSR
197600000000     C*------------------------------------------------------------------------*
197700000000     C* CARTAB - CARICA LE TABELLE
197800000000     C*------------------------------------------------------------------------*
197900000000     C     CARTAB        BEGSR
198000000000     C*--------------------
198100000000     C* FILIALI
198200000000     C*--------------------
198300000518     C                   CLEAR                   OG148
198400000518     C                   CLEAR                   OG143
198500000000     C                   Z-ADD     *ZEROS        I
198600000000     C     *LOVAL        SETLL     AZORG01L
198700000000     C                   READ      AZORG01L                               99
198800000000DO  1C     *IN99         DOWEQ     *OFF
198900000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
199000000000     C                   MOVEL     ORGDE8        OG148
199100000000IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
199200000000     C                   ADD       1             I
199300000000     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
199400000000     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
199500000518     C                   MOVEL     ORGDE3        OG143
199600000518     C                   MOVEL     §OGPT         PORG(I)                        *P.O. POSTE
199700971020     C                   CLEAR                   DSLV55
199800971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
199900971020     C                   MOVEL     *BLANKS       D55TPT                         *PARTENZA/ARRIVO
200000971020     C                   Z-ADD     ORGFIL        D55LIN                         *LINEA
200100980603     C                   Z-ADD     PARDDE        D55DRF                         *DATA RIFERIMENTO
200200971020     C                   CALL      'FNLV55R'
200300971020     C                   PARM                    DSLV55
200400971128IF  4C     D55ERR        IFNE      *BLANKS
200500971128     C                   Z-ADD     ORGFIL        D55TFP
200600971128     C                   Z-ADD     ORGFIL        D55TFA
200700971128E   4C                   ENDIF
200800000518     C                   Z-ADD     D55TFA        TFA(I)                         *TERMINAL PARTENZA
200900000518     C                   Z-ADD     D55TFP        TFP(I)                         *TERMINAL PARTENZA
201000000518     C                   Z-ADD     D55TFP        FEL(I)                         *FILIALE ELABORATORE
201100000000E   3C                   ENDIF
201200000000E   2C                   ENDIF
201300000000     C                   READ      AZORG01L                               99
201400000000E   1C                   ENDDO
201500000000     C*--------------------
201600000000     C* TIPI AUTOMEZZI
201700000000     C*--------------------
201800000000     C                   Z-ADD     *ZEROS        I
201900020716     c                   Z-Add     1             KtbKut
202000000000     C                   MOVEL     'TU'          KTBCOD
202100000000     C     KE2TAB        SETLL     TABEL00F
202200000000     C     KE2TAB        READE     TABEL00F                               99
202300000000DO  1C     *IN99         DOWEQ     *OFF
202400000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
202500000000     C                   ADD       1             I
202600000000     C                   MOVEL     TBLKEY        TMZ(I)                         *CODICE
202700000000     C                   MOVEL     TBLUNI        DSTU
202800000000     C                   MOVEL     §TUDES        DTMZ(I)                        *DESCRIZIONE
202900000000E   2C                   ENDIF
203000000000     C     KE2TAB        READE     TABEL00F                               99
203100000000E   1C                   ENDDO
203200000000     C*--------------------
203300000000     C* CAUSALI CONTABILIZZAZIONE
203400000000     C*--------------------
203500020716     c                   If        DutLpo = 'S'
203600000821     C                   OPEN      ANATB01L
203700000000     C                   Z-ADD     *ZEROS        I
203800020716     c                   Z-Add     1             KtbKut
203900000218     C                   MOVE      KNSIF         WCOMO4            4
204000000218     C                   MOVEL     WCOMO4        WSOC              3
204100000218     C                   movel     WSOC          KAtazi
204200000214     C                   MOVEL     'Y4Z'         KatCOD
204300000214     C     KEATAB        CHAIN     ANATB01L                           99
204400000821IF  2C     *IN99         IFEQ      *ON
204500000218     C                   movel     '888'         KAtazi
204600000214     C     KEATAB        CHAIN     ANATB01L                           99
204700000821E   2C                   ENDIF
204800000821DO  2C     *IN99         DOWEQ     *OFF
204900000214     C                   ADD       1             I
205000000214     C                   MOVEL     ATBKEY        CAU(I)
205100000214     C                   MOVEL     ATBUNI        ANGY4ZDS
205200000214     C                   MOVEL     §4ZDES        DCAU(I)
205300000214     C     KEATAB        READE     ANATB01L                               99
205400000821E   2C                   ENDDO
205500000821     C                   CLOSE     ANATB01L
205600000821E   1C                   ENDIF
205700000000     C*--------------------
205800000000     C* ALFABETO
205900000000     C*--------------------
206000000000     C                   MOVEA     DALF          ALF
206100000000     C*
206200000000     C                   ENDSR
206300971020     C*------------------------------------------------------------------------*
206400971020     C* FINSR - OPERAZIONI FINALI
206500971020     C*------------------------------------------------------------------------*
206600971020     C     FINSR         BEGSR
206700971020     C* FNLV55R
206800971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
206900971020     C                   MOVEL     'C'           D55TLA
207000971020     C                   CALL      'FNLV55R'
207100971020     C                   PARM                    DSLV55
207200971020     C*
207300971020     C                   ENDSR
207400000000     C*------------------------------------------------------------------------*
207500000000     C* *INZSR - IMPOSTAZIONI INIZIALI
207600000000     C*------------------------------------------------------------------------*
207700000000     C     *INZSR        BEGSR
207800000000     C*--------------------
207900000000     C* RICEVIMENTO PARAMETRI
208000000000     C*--------------------
208100000000     C     *ENTRY        PLIST
208200000000     C                   PARM                    PARTRN                         *TRAINO
208300000000     C                   PARM                    PARDDE                         *DECORRENZA
208400000000     C                   PARM                    PARINL                         *TIPO TRAINO INOLTRO
208500000000     C                   PARM                    PARERR            1            *FLAG ERRORE
208600000000     C                   PARM                    PARMSG           80            *MESSAGGIO DI ERRORE
208700000000     C                   PARM                    PARGES            1            *GESTIONE
208800000214     C                   PARM                    KPJBA
208900000000     C*--------------------
209000000000     C* VARIABILI RIFERITE AL DATA BASE
209100000000     C*--------------------
209200000214     C     *LIKE         DEFINE    ATBCOD        KATCOD
209300000214     C     *LIKE         DEFINE    ATBAZI        KATAZI
209400000000     C     *LIKE         DEFINE    TLTTRN        PARTRN                         *PARAMETRI
209500000000     C     *LIKE         DEFINE    TLTDDE        PARDDE
209600000000     C     *LIKE         DEFINE    TLTTTR        PARINL
209700000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
209800000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
209900000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
210000000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
210100000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
210200000000     C     *LIKE         DEFINE    TLODDE        KLODDE
210300000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
210400000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
210500010828     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
210600000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
210700000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
210800000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
210900000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR                         *TNTLZ00F
211000000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
211100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
211200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
211300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
211400000000     C     *LIKE         DEFINE    CAEEPA        KAEEPA                         *AZCAE01L
211500000000     C     *LIKE         DEFINE    CAETFP        KAETFP
211600000000     C     *LIKE         DEFINE    CAETFA        KAETFA
211700000000     C     *LIKE         DEFINE    TLOGSE        DEPGSE                         *DEPOSITI
211800000000     C     *LIKE         DEFINE    TLRDDE        DEPDDE
211900000000     C     *LIKE         DEFINE    §TVDEF        DEPDEF
212000000000     C*--------------------
212100000000     C* VARIABILI NON RIFERITE AL DATA BASE
212200000000     C*--------------------
212300000000     C     *LIKE         DEFINE    TOTTLR        TO2TLR                         *TOTALE DI LAVORO
212400000000     C     *LIKE         DEFINE    WFEL          DEPFEL                         *DEPOSITI
212500000000     C     *LIKE         DEFINE    WTFP          DEPTFP
212600000000     C     *LIKE         DEFINE    WTFA          DEPTFA
212700000519     C     *LIKE         DEFINE    WPOS          DEPPOS
212800000000     C*--------------------
212900000000     C* VARIABILI DI CONTROLLO
213000000000     C*--------------------
213100000000     C                   MOVEL     'S'           $OK               1            *RECORD VALIDO
213200000000     C                   MOVEL     'S'           $SCA              1            *FERMATA DI SCARICO
213300000000     C*--------------------
213400000000     C* CHIAVI DI LETTURA
213500000000     C*--------------------
213600000214     C* LETTURA ANATB01L - PARZIALE
213700000214     C     KEATAB        KLIST
213800000214     C                   KFLD                    KATCOD
213900000214     C                   KFLD                    KATAZI
214000000000     C* LETTURA TNTLT01L - COMPLETA
214100000000     C     KEYTLT        KLIST
214200000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
214300000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
214400000000     C* LETTURA TNTLR01L - PARZIALE
214500000000     C     KEYTLR        KLIST
214600000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
214700000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
214800000000     C* LETTURA TNTLR01L - PARZIALE
214900000000     C     KE2TLR        KLIST
215000000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
215100000000     C* LETTURA TNTLO01L - PARZIALE
215200000000     C     KEYTLO        KLIST
215300000000     C                   KFLD                    KLOTRN                         *NUMERO TRAINO
215400000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
215500000000     C* LETTURA TNTLM01L - PARZIALE
215600000000     C     KEYTLM        KLIST
215700000000     C                   KFLD                    KLMTRN                         *NUMERO TRAINO
215800000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
215900010828     C* LETTURA TITLL01L - PARZIALE
216000000000     C     KEYTLL        KLIST
216100000000     C                   KFLD                    KLLTRN                         *NUMERO TRAINO
216200000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
216300000000     C* LETTURA TNTLN01L - PARZIALE
216400000000     C     KEYTLN        KLIST
216500000000     C                   KFLD                    KLNTRN                         *NUMERO TRAINO
216600000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
216700000000     C* LETTURA TNTLZ01L - COMPLETA
216800000000     C     KEYTLZ        KLIST
216900000821     C                   KFLD                    KLZTIP                         *TIPO
217000000821     C                   KFLD                    KLZPDR                         *CODICE
217100000000     C* LETTURA TABEL00F - COMPLETA
217200000000     C     KEYTAB        KLIST
217300000000     C                   KFLD                    KTBKUT                         *UTENTE
217400000000     C                   KFLD                    KTBCOD                         *TABELLA
217500000000     C                   KFLD                    KTBKEY                         *CHIAVE
217600000000     C* LETTURA TABEL00F - PARZIALE
217700000000     C     KE2TAB        KLIST
217800000000     C                   KFLD                    KTBKUT                         *UTENTE
217900010830     C                   KFLD                    KTBCOD                         *TABELLA
218000000000     C* LETTURA AZCAE01L - PARZIALE
218100000000     C     KEYCAE        KLIST
218200000000     C                   KFLD                    KAEEPA                         *PARTENZA/ARRIVO
218300000000     C                   KFLD                    KAETFP                         *PARTENZA
218400000000     C                   KFLD                    KAETFA                         *ARRIVO
218500971020     C*
218600971020     C* CALCOLA LA DATA CORRENTE
218700971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
218800971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
218900971020     C                   Z-ADD     N8            G08DAT
219000971020     C                   Z-ADD     *ZEROS        G08INV
219100971020     C                   MOVEL     '0'           G08ERR
219200971020     C                   CALL      'XSRDA8'
219300971020     C                   PARM                    WLBDA8
219400971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
219500000000     C*
219600000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
219700000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
219800000000IF  1C     *IN99         IFEQ      *ON
219900000000     C     *IN99         ANDEQ     *OFF
220000000000     C* TRAINI
220100000000     C                   UPDATE    TNTLT000
220200000000     C                   WRITE     TNTLT000
220300000000     C* FERMATE
220400000000     C                   UPDATE    TNTLR000
220500000000     C                   WRITE     TNTLR000
220600000000     C* ORARI
220700000000     C                   READ      TNTLO000                               28
220800000000     C                   UPDATE    TNTLO000
220900000000     C                   WRITE     TNTLO000
221000000000     C* MCUBI ASSEGNATI
221100000000     C                   UPDATE    TNTLM000
221200000000     C                   WRITE     TNTLM000
221300000000     C* LISTINI
221400010828     C                   UPDATE    TITLL000
221500010828     C                   WRITE     TITLL000
221600000000     C* NOTE
221700000000     C                   UPDATE    TNTLN000
221800000000     C                   WRITE     TNTLN000
221900000000E   1C                   ENDIF
222000000000     C*
222100000000     C* REPERISCE I DATI UTENTE
222200020716
222300020716     c     *dtaara       define    §azute        azuteds
222400020716     c     *dtaara       define    §datiute      ddatiute
222500020716
222600020716     c                   in(E)     *dtaara
222700020716     c                   if        %Error or rsut = *blanks
222800020716     c                   clear                   tibs34ds
222900020716     c                   call      'TIBS34R'
223000020716     c                   parm                    Tibs34Ds
223100020716     c                   in        *dtaara
223200020716     c                   endif
223300000000     C*
223400000000     C* CARICA LE TABELLE OCCORRENTI
223500000000     C                   EXSR      CARTAB
223600000000     C*
223700000000     C                   ENDSR
223800000218     C*------------------------------------------------------------------------*
223900000218** MS    MESSAGGI DI ERRORE
224000000000ERR Traini: Traino inesistente                                                 1
224100000000ERR Traini: La filiale importo deve essere fermata di carico                   2
224200000000ERR Traini: 3
224300000000ERR Traini: 4
224400000000ERR Traini: 5
224500000000ERR Traini: 6
224600000000ERR Traini: 7
224700000000ERR Traini: 8
224800000000ERR Traini: 9
224900000000ERR Traini: 10
225000000000ERR Fermate: Inserire le fermate per il traino                                11
225100000000ERR Fermate: Inserire gli orari per tutti i giorni previsti dal traino        12
225200000000ERR Fermate: Non inserire gli orari dei giorni non previsti dal traino        13
225300000000ERR Fermate: I gg apertura FV non devono variare al variare della decorrenza  14
225400000000ERR Fermate: 15
225500000000ERR Fermate: 16
225600000000ERR Fermate: 17
225700000000ERR Fermate: 18
225800000000ERR Fermate: 19
225900000000ERR Fermate: Andare in GESTIONE FERMATE e ricontrollare i dati immessi        20
226000000000ERR Mcubi: Inserire i metricubi per il traino                                 21
226100000000ERR Mcubi: Le fermate che caricano devono essere nei MC come fil. partenza    22
226200000000ERR Mcubi: Le fermate che scaricano devono essere nei MC come fil. scarico    23
226300000000ERR Mcubi: Bisogna assegnare i Mcubi: partenza = 999, scarico e arrivo = 999  24
226400000000ERR Mcubi: 25
226500000000ERR Mcubi: 26
226600000000ERR Mcubi: 27
226700000000ERR Mcubi: 28
226800000000ERR Mcubi: 29
226900000000ERR Mcubi: Andare in GESTIONE MCUBI e ricontrollare i dati immessi            30
227000000000ERR Listini: Inserire almeno un listino                                       31
227100000000ERR Listini: Deve esistere un listino con tipo automezzo = al traino o = 99   32
227200000000ERR Listini: 33
227300000000ERR Listini: 34
227400000000ERR Listini: 35
227500000000ERR Listini: 36
227600000000ERR Listini: 37
227700000000ERR Listini: 38
227800000000ERR Listini: 39
227900000000ERR Listini: Andare in GESTIONE LISTINI e ricontrollare i dati immessi        40
228000000000ERR Note: 41
228100000000ERR Note: 42
228200000000ERR Note: 43
228300000000ERR Note: 44
228400000000ERR Note: 45
228500000000ERR Note: 46
228600000000ERR Note: 47
228700000000ERR Note: 48
228800000000ERR Note: 49
228900000000ERR Note: Andare in GESTIONE NOTE e ricontrollare i dati immessi              50
229000000000** -DALF- ALFABETO
229100000000ABCDEFGHILMNOPQRSTUVZ
