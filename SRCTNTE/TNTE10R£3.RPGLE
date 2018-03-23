000100970430     H*------------------------------------------------------------------------*
000200011025     H*                   TARIFFE DA C/E - CALCOLA COMPETENZE                  *
000300970305     H*------------------------------------------------------------------------*
000400970305     H* PARTENDO DAI DATI TIPO / SCAGLIONI / CODICI TASSAZIONE DI UNA TARIFFA
000500970305     H* CALCOLA LE COMPETENZE PER OGNI SCAGLIONE E PER OGNI CODICE TASSAZIONE
000600970305     H* E LE SCRIVE NEL FILE DI LAVORO DELLA PROCEDURA.
000700961128     H*------------------------------------------------------------------------*
000800961128     H DECEDIT('0,') DATEDIT(*DMY.)
000900961128     F*------------------------------------------------------------------------*
001000961128     F* DATA BASE
001100961128     F*------------------------------------------------------------------------*
001200961128     FECEVT01L  IF   E           K DISK
001300140115     FECEVD06L  IF   E           K DISK
001400961128     FECEVD03L  IF   E           K DISK
001500010807     F                                     RENAME(ECEVD000:ECEVD3)
001600961128     FECEVD04L  IF   E           K DISK
001700010807     F                                     RENAME(ECEVD000:ECEVD4)
001800961128     FECEVM01L  IF   E           K DISK
001900010926     FTEETC01L  IF   E           K DISK
002000010926     FTEETS01L  IF   E           K DISK
002100011017     FTEETT01L  IF   E           K DISK
002200010926     FTEETW01L  UF A E           K DISK
002300010926     FTEETWC1L  UF A E           K DISK
002400961128     FTABEL00F  IF   E           K DISK
002500011008     FTNTBE01L  IF   E           K DISK
002600961128     FAZORG01L  IF   E           K DISK
002700961128     D*------------------------------------------------------------------------*
002800961128     D* SCHIERE
002900961128     D*------------------------------------------------------------------------*
003000961128     D*-------------------
003100961128     D* COMANDI
003200961128     D*-------------------
003300961128     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003400961128     D*-------------------
003500961128     D* MESSAGGI
003600961128     D*-------------------
003700961128     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
003800961128     D*-------------------
003900970204     D* VOCI (INDICE=CODICE VOCE)
004000961128     D*-------------------
004100040524     D VOCA            S              3  0 DIM(300)                             *COD.VOCE CARICATA
004200961128     D VPRG            S              3  0 DIM(999)                             *PROGRESSIVO VOCE
004300961128     D VTVO            S              1    DIM(999)                             *TIPO VOCE
004400961128     D VDDV            S              8  0 DIM(999)                             *DATA DECORRENZA VOC
004500961128     D VDSV            S              8  0 DIM(999)                             *DATA SCADENZA VOCE
004600961128     D VVRV            S              2    DIM(999)                             *TIPO TARIFFA VOCE '
004700961128     D VTIV            S              2    DIM(999)                             *TIPO RICERCA SCAGL'
004800961128     D VSPT            S              1    DIM(999)                             *SVILUPPO PESO TASSA
004900961128     D VCMP            S              3  0 DIM(999)                             *COD.COMPETENZA
005000961128     D VRFI            S              2    DIM(999)                             *RIF.TARIF.FIL.ITA
005100961128     D VRFE            S              2    DIM(999)                             *RIF.TARIF.FIL.EST
005200961128     D VRTI            S              2    DIM(999)                             *RIF.TARIF.TASS.ITA
005300961128     D VRTE            S              2    DIM(999)                             *RIF.TARIF.TASS.EST
005400961128     D VRPI            S              2    DIM(999)                             *RIF.TARIF.CAP.ITA
005500961128     D VRPE            S              2    DIM(999)                             *RIF.TARIF.CAP.EST
005600961128     D VRKI            S              2    DIM(999)                             *RIF.TARIF.CLIENTE.I
005700961128     D VRKE            S              2    DIM(999)                             *RIF.TARIF.CLIENTE.E
005800011025     D VARF            S             21  8 DIM(999)                             *ARROTONDAM. FINO LI
005900011025     D VARL            S             21  8 DIM(999)                             *ARROTONDAM. LIMITE
006000011025     D VARO            S             21  8 DIM(999)                             *ARROTONDAM. OLTRE L
006100011025     D VIMP            S             21  8 DIM(999)                             *IMPORTO CALCOLATO V
006200011025     D VIM             S             21  8 DIM(999)                             *IMPORTO DA SCR. SU
006300030428     D VRPV            S              3  1 DIM(999)
006400030428     D VSPS            S              1    DIM(999)
006500961128     D VC              S              1    DIM(999)                             *VOCE DA CALCOLARE S
006600961128     D VV              S              1    DIM(999)                             *VOCE VALIDA S/N
006700000215     D*
006800970220     D VP              S              9  5 DIM(999)                             *% DA CONSIDERARE
006900961128     D VI              S              1    DIM(999)                             *INGLOBA NEL PORTO S
007000040811     D vb              S              1    DIM(999)                             *INGLOBA NEL PORTO S
007100970207     D VS              S              1  0 DIM(999)                             *SEQUENZA CALCOLO
007200961128     D VRDL            S              3  0 DIM(999)                             *RAGGRUPPAMENTO DELT
007300961128     D*------------------
007400961128     D* IMPORTI RAGGRUPPAMENTI DELTA
007500961128     D*------------------
007600011025     D IRDL            S             21  8 DIM(50)
007700961128     D*-------------------
007800961128     D* SCAGLIONI
007900961128     D*-------------------
008000011025     D SSGL            S             21  8 DIM(10)                              *SCAGLIONE SUPERIORE
008100961128     D SPSC            S              7  1 DIM(10)                              *PESO RIF. CALCOLO
008200961128     D SCSC            S              7  0 DIM(10)                              *COLLI RIF. CALCOLO
008300961128     D SVSC            S              7  3 DIM(10)                              *VOLUME RIF. CALCOLO
008400961128     D SPPE            S              4  1 DIM(10)                              *% PESO
008500011025     D SINL            S             21  8 DIM(10)                              *IMPORTO INOLTRO
008600970402     D SSDE            S              1    DIM(10)                              *SEGNO DELTA
008700970402     D SPDE            S              4  1 DIM(10)                              *% DELTA
008800970317     D SRPV            S              3  1 DIM(10)                              *RAPP. PESO/VOL.
008900961128     D*-------------------
009000961128     D* CODICI TASSAZIONE
009100961128     D*-------------------
009200050103     D TCTS            S              2    DIM(600)                             *CODICE
009300050103     D TRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
009400050103     D TPPE            S              4  1 DIM(600)                             *% PESO
009500050103     D TLNA            S              3  0 DIM(600)                             *LINEA ARRIVO
009600050103     D TTFA            S              3  0 DIM(600)                             *TERMINAL ARRIVO
009700050103     D TRAP            S              2    DIM(600)                             *REGIONE APPARTENENZ
009800050103     D TNAR            S              2    DIM(600)                             *NAZIONE APPARTENENZ
009900961128     D*-------------------
010000961128     D* TARIFFE DI CARTELLO
010100961128     D*-------------------
010200021112     D CVOC            S              3  0 DIM(300)                             CODICE TARIFFA
010300021112     D CPRG            S              3  0 DIM(300)                             *PROGRESSIVO TARIFFA
010400021112     D CFT1            S              3  0 DIM(300)                             *FILIALE ... PARTENZ
010500021112     D CFT2            S              3  0 DIM(300)                             *FILIALE ... ARRIVO
010600021112     D CCLI            S              7  0 DIM(300)                             *CLIENTE
010700021112     D CCTR            S              3  0 DIM(300)                             *TARIFFA
010800021112     D CCTS            S              2    DIM(300)                             *CODICE TASSAZIONE
010900140115     D Ccad            S              9    DIM(300)
011000140115     D Cnaz            S              3    DIM(300)
011100021112     D CDDT            S              8  0 DIM(300)                             *DATA DECORRENZA
011200021112     D CDST            S              8  0 DIM(300)                             *DATA SCADENZA
011300021112     D CSGI            S             21  8 DIM(300)                             *SCAGLIONE INFERIORE
011400021112     D CSGF            S             21  8 DIM(300)                             *SCAGLIONE SUPERIORE
011500021112     D CITR            S             21  8 DIM(300)                             *IMPORTO UNITARIO
011600021112     D CIMI            S             21  8 DIM(300)                             *IMPORTO MINIMO TARI
011700021112     D CIMA            S             21  8 DIM(300)                             *IMPORTO MINIMO TARI
011800021112     D CSEG            S              1    DIM(300)                             *SEGNO IMPORTO TARIF
011900021112     D CPER            S              5  2 DIM(300)                             *PERCENTUALE DA APPL
012000021112     D CRPV            S              3  1 DIM(300)                             *RAPPORTO PESO/VOLUM
012100961128     D*-------------------
012200961128     D* TARIFFE DELL'ULTIMA FILIALE DEL CLIENTE
012300961128     D*-------------------
012400021112     D FVOC            S              3  0 DIM(600)                             CODICE TARIFFA
012500021112     D FPRG            S              3  0 DIM(600)                             *PROGRESSIVO TARIFFA
012600021112     D FFT1            S              3  0 DIM(600)                             *FILIALE ... PARTENZ
012700021112     D FFT2            S              3  0 DIM(600)                             *FILIALE ... ARRIVO
012800021112     D FCLI            S              7  0 DIM(600)                             *CLIENTE
012900021112     D FCTR            S              3  0 DIM(600)                             *TARIFFA
013000021112     D FCTS            S              2    DIM(600)                             *CODICE TASSAZIONE
013100140115     D Fcad            S              9    DIM(600)
013200140115     D Fnaz            S              3    DIM(600)
013300021112     D FDDT            S              8  0 DIM(600)                             *DATA DECORRENZA
013400021112     D FDST            S              8  0 DIM(600)                             *DATA SCADENZA
013500021112     D FSGI            S             21  8 DIM(600)                             *SCAGLIONE INFERIORE
013600021112     D FSGF            S             21  8 DIM(600)                             *SCAGLIONE SUPERIORE
013700021112     D FITR            S             21  8 DIM(600)                             *IMPORTO UNITARIO
013800021112     D FIMI            S             21  8 DIM(600)                             *IMPORTO MINIMO TARI
013900021112     D FIMA            S             21  8 DIM(600)                             *IMPORTO MINIMO TARI
014000021112     D FSEG            S              1    DIM(600)                             *SEGNO IMPORTO TARIF
014100021112     D FPER            S              5  2 DIM(600)                             *PERCENTUALE DA APPL
014200021112     D FRPV            S              3  1 DIM(600)                             *RAPPORTO PESO/VOLUM
014300961128     D*-------------------
014400961128     D* MAGGIORAZIONI
014500961128     D*-------------------
014600050104     D MVOM            S              3  0 DIM(600)                             *CODICE VOCE MAGG.
014700050104     D MPRM            S              3  0 DIM(600)                             *PROGRESSIVO VOCE MA
014800050104     D MVOC            S              3  0 DIM(600)                             *CODICE VOCE DA MAGG
014900050104     D MPRG            S              3  0 DIM(600)                             *PROGRESSIVO VOCE DA
015000961128     D*-------------------
015100961128     D* TIPI TARIFFA
015200961128     D*-------------------
015300961128     D C2R             S              2    DIM(100)                             *TIPO TARIFFA
015400961128     D T2R             S              1    DIM(100)                             *TIPO VALORIZ. Q,V,F
015500011025     D Q2R             S             21  8 DIM(100)                             *QUANTITA'
015600971211     D*****************************************************************
015700971211     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
015800971211     D FL1             S              1    DIM(999)                             *FLAG ITALIA/ESTERO
015900980710     D DIT             S              3    DIM(999)                             *FLAG BAR/SDI
016000000215     D DPD             S              1    DIM(999)                             *FLAG FILIALE DPD (S
016100020503     D NTW             S              3    DIM(999)                             *NETWORK
016200010801     D CTS             S              2    DIM(999)                             *C.TASSAZIONE FILIAL
016300010801     D*-------------------
016400010801     D* TRATTAMENTO MERCE
016500010801     D*-------------------
016600010801     D CTM             S              2    DIM(100)                             *CODCICE
016700010801     D CF12            S              1    DIM(100)                             *FLAG "DISK C"
016800961128     D*------------------------------------------------------------------------*
016900961128     D* INPUT
017000961128     D*------------------------------------------------------------------------*
017100091123     D tnte04ds      E DS
017200140115     D ecevdds       E DS                  extname(ecevd06l)
017300030428     D*-------------------
017400030428     D* DS LETTURA CAMPO EVTXXX
017500030428     D*-------------------
017600030428     D DEVTXXX       E DS
017700961128     D*-------------------
017800961128     D* DS TIPI TARIFFA
017900961128     D*-------------------
018000961128     D DS2R          E DS
018100961128     D*-------------------
018200961128     D* DS CODICI TASSAZIONE
018300961128     D*-------------------
018400961128     D DSCT          E DS
018500000215     D*-------------------
018600000215     D* DS ESTERNA DESCRIZIONE 43 ORGANIGRAMMA
018700000215     D*-------------------
018800000215     D OG143         E DS
018900010801     D*-------------------
019000010801     D* DS ESTERNA TABELLA 7O (CODICI TRATTAMENTO MERCE)
019100010801     D*-------------------
019200010801     D DS1B          E DS
019300961128     D*-------------------
019400961128     D* DS SCOMPOSIZIONE DATA IN ANNO/MESE/GIONRO
019500961128     D*-------------------
019600961128     D DS§DAT          DS
019700961128     D  DSSEC                  1      2  0
019800961128     D  DSANN                  3      4  0
019900961128     D  DSAAS                  1      4  0
020000961128     D  DSMES                  5      6  0
020100961128     D  DSGIO                  7      8  0
020200961128     D  DSMGS                  5      8  0
020300961128     D  DSDAT6                 3      8  0
020400961128     D  DSDAT8                 1      8  0
020500961128     D DS§TIM          DS
020600961128     D  DTORA                  1      2  0
020700961128     D  DTMIN                  3      4  0
020800961128     D  DTSCN                  5      6  0
020900961128     D  DTGIO                  7      8  0
021000961128     D  DTMES                  9     10  0
021100961128     D  DTSEC                 11     12  0
021200961128     D  DTANN                 13     14  0
021300961128     D  DTORA6                 1      6  0
021400961128     D  DTTIME                 1     14  0
021500961128     D*-------------------
021600961128     D* DS MESSAGGIO MANCA TARIFFA
021700961128     D*-------------------
021800961128     D                 DS
021900961128     D  DSMSG                  1     80
022000961128     D  DSVOCE                29     31
022100961128     D  DSTT1                 33     35
022200961128     D  DSTT2                 37     39
022300970210     D  DSCTE                 41     42
022400970210     D  DSNAR                 61     63
022500970210     D  DSCAP                 65     74
022600970210     D  DSCTS                 70     71
022700961128     D  DSDER                  1     74
022800961128     D  DSCER                 75     78
022900970910     D*------------------
023000970910     D* DS "TISI95R" - CONTROLLO CAP
023100970910     D*------------------
023200970910     D DSSI95        E DS                  EXTNAME(TISI95DS)
023300970910     D*------------------
023400970910     D* DS "FNLV55R" - REPERIMENTO TERMINALs
023500970910     D*------------------
023600970910     D DSLV55        E DS                  EXTNAME(FNLV55DS)
023700961128     D*-------------------
023800961128     D* DS ESTERNA IN INPUT AL PGM
023900961128     D*-------------------
024000961128     D DSTE00        E DS                  EXTNAME(TNTE00DS)
024100970214     D*-------------------
024200970214     D* INFDS - INFORMAZIONI PROGRAMMA
024300970214     D*-------------------
024400970213     D                SDS
024500980909     D  JOB                  244    253
024600970213     D  USER                 254    263
024700961128     D*-------------------
024800961128     D* ARCHITETTURA
024900961128     D*-------------------
025000961128     D KPJBA         E DS
025100011008     D*------------------
025200011008     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
025300011008     D*------------------
025400011008     D DSGEDCE       E DS
025500011017     D*------------------
025600011017     D* DS LETTURA TABELLA "GED" TIPO RECORD "AC"
025700011017     D*------------------
025800011017     D DGEDAC        E DS
025900011008     D*-------------------
026000011008     D* CONVERSIONI IMPORTI
026100011008     D*-------------------
026200011008     D YEUR          E DS                  EXTNAME(YEURCODS)
026300020729     D*------------------
026400020729     D* DS PER LETTURA TABELLA "NTW"
026500020729     D*------------------
026600020729     D DNTW          E DS
026700961128     C*------------------------------------------------------------------------*
026800961128     C* MAIN LINE
026900961128     C*------------------------------------------------------------------------*
027000961128     C*
027100961128     C* ESEGUE ROUTINE INIZIALE
027200961128     C                   EXSR      RUTINZ
027300961128     C*
027400961128     C* SE ERRORE RUTINZ ESCO PGM
027500961128     C     $ERR          IFEQ      'N'
027600970910     C*
027700970910     C* RECUPERA INSTRADAMENTO -PARTENZA-
027800970910     C     $ERR          IFEQ      'N'
027900970910     C                   EXSR      REPINP
028000970910     C                   ENDIF
028100970131     C*
028200970131     C* IMPOSTA LA SEQUENZA IN TUTTE LE VOCI
028300970131     C     $ERR          IFEQ      'N'
028400970131     C                   EXSR      IMPSEQ
028500970131     C                   END
028600961128     C*
028700961128     C* CARICA DATI TIPO
028800961128     C     $ERR          IFEQ      'N'
028900961128     C                   EXSR      CARETC
029000961128     C                   END
029100961128     C*
029200961128     C* CARICA COD.TASSAZIONE
029300961128     C     $ERR          IFEQ      'N'
029400961128     C                   EXSR      CARETT
029500961128     C                   END
029600961128     C*
029700961128     C* CARICA SCAGLIONI
029800961128     C     $ERR          IFEQ      'N'
029900961128     C                   EXSR      CARETS
030000961128     C                   END
030100961128     C*
030200961128     C* CALCOLA VALORI TABELLA '2R' (QTA TIPO TARIFFA) PER SEQ. 1
030300961128     C     $ERR          IFEQ      'N'
030400961128     C                   EXSR      CAL2R1
030500961128     C                   END
030600961128     C*
030700970207     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE E CONTROLLA VALIDITA
030800961128     C     $ERR          IFEQ      'N'
030900961128     C                   EXSR      CHKVOC
031000961128     C                   END
031100961128     C*
031200961128     C* VALORIZZA LE VOCI DA CALCOLARE
031300961128     C     $ERR          IFEQ      'N'
031400961128     C                   Z-ADD     1             WSEQ              6 0
031500961128     C                   EXSR      VALVOC
031600961128     C                   END
031700961128     C*
031800970225     C* CICLO PER TUTTI GLI SCAGLIONI ----------------------------------------->
0319009611282    C                   DO        WULTSC        S                 6 0
0320009611283    C     SSGL(S)       IFNE      *ZEROS
032100980903     C*
032200980903     C* CERCA LA LINEA ARRIVO E IL TEMINAL ARRIVO PER OGNI COD.TASSAZ. UTILIZZANDO
032300980903     C* ANCHE IL PESO DELLO SCAGLIONE (SE PESO > XX CONSEGNA SDI)
032400980903     C     $ERR          IFEQ      'N'
032500980903     C                   EXSR      CERLNA
032600980903     C                   END
032700961128     C*
032800961128     C* CALCOLA VALORI TABELLA '2R' (QTA TIPO TARIFFA) PER SEQ. 2
032900961128     C     $ERR          IFEQ      'N'
033000961128     C                   EXSR      CAL2R2
033100961128     C                   END
033200961128     C*
033300970207     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE E CONTROLLA VALIDITA
033400961128     C     $ERR          IFEQ      'N'
033500961128     C                   EXSR      CHKVOC
033600961128     C                   END
033700961128     C*
033800961128     C* VALORIZZA LE VOCI DA CALCOLARE
033900961128     C     $ERR          IFEQ      'N'
034000961128     C                   Z-ADD     2             WSEQ
034100961128     C                   EXSR      VALVOC
034200961128     C                   END
034300961128     C*
034400970225     C* CICLO PER TUTTI I CODICI TASSAZIONE ARRIVO ---------------------------->
0345009611284    C                   DO        WULTCT        T                 6 0
0346009611285    C     TCTS(T)       IFNE      *BLANKS
034700961128     C*
034800970207     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE E CONTROLLA VALIDITA
034900961128     C     $ERR          IFEQ      'N'
035000961128     C                   EXSR      CHKVOC
035100961128     C                   END
035200000704     C*
035300000704     C* DETERMINA PERCENTUALI PER FORMULA (% ESPRESSO,COMM.LE,PAGAM.)
035400000704     C     $ERR          IFEQ      'N'
035500000704     C                   EXSR      PERFOR
035600000704     C                   END
035700961128     C*
035800961128     C* VALORIZZA LE VOCI DA CALCOLARE
035900961128     C     $ERR          IFEQ      'N'
036000961128     C                   Z-ADD     3             WSEQ
036100961128     C                   EXSR      VALVOC
036200961128     C                   END
036300961128     C*
036400970206     C* CALCOLA FORMULA RICARICHI + RAGGUPPAMENTO DELTA
036500961128     C     $ERR          IFEQ      'N'
036600961128     C                   EXSR      CALTOT
036700961128     C                   END
036800961128     C*
036900011017     C* SCRIVE IL FILE DI LAVORO TEETW
037000961128     C     $ERR          IFEQ      'N'
037100961128     C                   EXSR      SCRETW
037200961128     C                   END
037300961128     C*
037400010926     C* SCRIVE IL FILE DI LAVORO TEETWC SE LANCIO IN DEBUG
037500961128     C     $ERR          IFEQ      'N'
037600970207     C     D00DBG        ANDEQ     'S'
037700970207     C                   EXSR      SCREWC
037800970207     C                   END
037900961128     C*
0380009611285    C                   ENDIF                                                  *C.TASS=0
0381009611284    C                   ENDDO                                                  *SEQ. 3
038200961128     C*
0383009611283    C                   ENDIF                                                  *SCAGL=0
0384009611282    C                   ENDDO                                                  *SEQ. 2
038500961128     C*
0386009611281    C                   ENDIF                                                  *IF ERR
038700961128     C*
038800970910     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
038900970910     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
039000970910     C                   MOVEL     'C'           I95TLA
039100970910     C                   CALL      'TISI95R'
039200970910     C                   PARM                    DSSI95
039300970910     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
039400970910     C                   MOVEL     'C'           D55TLA
039500970910     C                   CALL      'FNLV55R'
039600970910     C                   PARM                    DSLV55
039700961128     C                   SETON                                        LR
039800970910     C*------------------------------------------------------------------------*
039900970910     C* REPINP - RECUPERA INTRADAMENTO -PARTENZA-
040000970910     C*------------------------------------------------------------------------*
040100970910     C     REPINP        BEGSR
040200970910     C*
040300970910     C* TERMINAL PARTENZA
040400970910     C                   CLEAR                   DSLV55
040500970910     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
040600970910     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
040700970910     C                   Z-ADD     D00LNP        D55LIN                         *LINEA
040800970910     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
040900970910     C                   CALL      'FNLV55R'
041000970910     C                   PARM                    DSLV55
041100970910IF  1C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
041200970910     C                   MOVEL     D55MSG        $MSG
041300970910     C                   EXSR      SCRERR                                       *IMPOSTA ERRORE
041400970910X   1C                   ELSE                                                   *TROVATO
041500970910     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
041600970910E   1C                   ENDIF
041700970910     C*
041800970910     C* CODICE TASSAZIONE PARTENZA
041900970910     C                   Z-ADD     D00LNP        KORFIL
042000970910     C     KEYORG        CHAIN     AZORG01L                           98
042100970910IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATO
042200970910     C                   MOVEL     MSG(1)        $MSG
042300970910     C                   EXSR      SCRERR                                       *IMPOSTA ERRORE
042400970910X   1C                   ELSE                                                   *TROVATA
042500970910     C                   MOVEL     ORGCTS        WCTP                           *CTS MITTENTE
042600970910E   1C                   ENDIF
042700970910     C*
042800970910     C                   ENDSR
042900970211     C*------------------------------------------------------------------------*
043000970211     C* IMPSEQ -IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
043100970211     C*------------------------------------------------------------------------*
043200970211     C     IMPSEQ        BEGSR
043300970211     C*
043400970211     C* LEGGO TUTTO IL FILE TESTATE VOCI E PER TUTTE LE VOCI VALIDE
043500970211     C* ALLA DATA IN INPUT IMPOSTO LA SEQUENZA
043600970211     C                   Z-ADD     *ZEROS        I
043700970211     C                   Z-ADD     *ZEROS        EVTVOC
043800970211     C                   Z-ADD     *ZEROS        EVTPRG
043900970211     C     KEYEVT        SETLL     ECEVT01L                           99
044000970211     C  N99              READ      ECEVT01L                               99
0441009702111    C     *IN99         DOWEQ     '0'                                          *FINE FILE
0442009702112    C     EVTATB        IFEQ      *BLANKS                                      *NON E' ANNULLATO
044300970211     C     D00DTC        ANDGE     EVTDDV                                       *MAGG. DECORRENZA
044400970211     C     D00DTC        ANDLE     EVTDSV                                       *MIN. SCADENZA
044500970211     C                   ADD       1             I
044600970211     C                   Z-ADD     EVTVOC        VOCA(I)
044700970211     C                   Z-ADD     EVTVOC        V
044800970211     C                   Z-ADD     EVTPRG        VPRG(V)
044900970211     C                   MOVEL     EVTTVO        VTVO(V)
045000970211     C                   Z-ADD     EVTDDV        VDDV(V)
045100970211     C                   Z-ADD     EVTDSV        VDSV(V)
045200970211     C                   MOVEL     EVTVRV        VVRV(V)
045300970211     C                   MOVEL     EVTTIV        VTIV(V)
045400970211     C                   MOVEL     EVTSPT        VSPT(V)
045500970211     C                   Z-ADD     EVTCMP        VCMP(V)
045600970211     C                   MOVEL     EVTRFI        VRFI(V)
045700970211     C                   MOVEL     EVTRFE        VRFE(V)
045800970211     C                   MOVEL     EVTRTI        VRTI(V)
045900970211     C                   MOVEL     EVTRTE        VRTE(V)
046000970211     C                   MOVEL     EVTRPI        VRPI(V)
046100970211     C                   MOVEL     EVTRPE        VRPE(V)
046200970211     C                   MOVEL     EVTRKI        VRKI(V)
046300970211     C                   MOVEL     EVTRKE        VRKE(V)
046400970211     C                   Z-ADD     EVTARF        VARF(V)
046500970211     C                   Z-ADD     EVTARL        VARL(V)
046600970211     C                   Z-ADD     EVTARO        VARO(V)
046700970211     C                   Z-ADD     EVTRDL        VRDL(V)                        *RAGGR.DELTA
046800030428     C                   MOVEL     EVTXXX        DEVTXXX
046900030428     C                   Z-ADD     EVTRPV        VRPV(V)
047000030428     C                   MOVEL     EVTSPS        VSPS(V)
047100970211     C                   EXSR      SEQVOC                                       *DETERMINA SEQ.CALC.
0472009702112    C                   ENDIF
047300970211     C                   READ      ECEVT01L                               99
0474009702111    C                   ENDDO
047500970211     C*
047600970211     C* MEMORIZZA N.ELEMENTI SCHIERA VOCI CARICATE
047700970211     C                   Z-ADD     I             NVOCA             3 0          *N.ELEM. VOCI CAR.
047800970211     C*
047900970211     C* X LE VOCI FORMULONE (SEDE COMM.+PAGAM.) E DELTA FORZO SEQ. 3
048000970211     C* PERCHE SONO CALCOLATE PER OGNI C.TASS. E PER OGNI SCAGL.
048100980710     C                   Z-ADD     3             VS(270)                        *UFF.COMM. BART
048200040609     C                   Z-ADD     3             VS(305)                        *UFF.COMM. BART
048300970211     C                   Z-ADD     3             VS(300)                        *INCASSO E RISCHI FI
048400970211     C                   Z-ADD     3             VS(900)                        *DELTA
048500970211     C*
048600990422     C* X LE VOCI ATTIVATE IN BASE ALLA LINEA DI ARR. FORZO SEQ.3
048700990422     C* LE VOCI DIPENDENTI DAI COD.TASSAZIONE SONO GIA' FORZATE IN SEQVOC
048800990422     C* LE MAGGIORAZIONI NON IMPORTA METTERLE PERCHE' SONO GIA' FORZATE IN SEQVOC
048900970211     C                   Z-ADD     3             VS(71)                         *UFF.PARTENZA EXPORT
049000000228     C                   Z-ADD     3             VS(74)                         *UFF.PARTENZA EXPORT
049100010801     C                   Z-ADD     3             VS(275)                        *ASSISTENZA IMP/EXP
049200010801     C                   Z-ADD     3             VS(276)                        *ASSISTENZA IMP/EXP
049300040610     C                   Z-ADD     3             VS(288)                        *UFF.PARTENZA EXPORT
049400030429     C                   Z-ADD     3             VS(166)                        *MONOCOLLO GERMANIA
049500040610     C                   Z-ADD     3             VS(170)                        *MONOCOLLO GERMANIA
049600970211     C                   Z-ADD     3             VS(171)                        *ARR.EXPORT (X SPED.
049700970211     C                   Z-ADD     3             VS(172)                        *ARR.EXPORT (A PESO)
049800130529     C                   Z-ADD     3             VS(173)                        *ARR.EXPORT (A PESO)
049900000228     C                   Z-ADD     3             VS(175)                        *ARR.EXPORT DPD (A C
050000030428     C                   Z-ADD     3             VS(176)                        *ARR.EXPORT DPD (A P
050100030430     C                   Z-ADD     3             VS(231)                        *TRAINI DEFL. ESTERO
050200000228     C                   Z-ADD     3             VS(233)                        *TRAINI DEFL. ESTERO
050300970211     C                   Z-ADD     3             VS(114)                        *SCARICO DEFL.EXPORT
050400970211     C                   Z-ADD     3             VS(124)                        *SMISTAM.DEFL.EXPORT
050500970211     C                   Z-ADD     3             VS(152)                        *CARICO DEFL.EXPORT
050600970211     C                   Z-ADD     3             VS(230)                        *TRAINI DEFLUENZA
050700970211     C                   Z-ADD     3             VS(111)                        *SCARICO DEFLUENZA
050800970211     C                   Z-ADD     3             VS(121)                        *SMISTAM.DEFLUENZA
050900970211     C                   Z-ADD     3             VS(151)                        *CARICO DEFLUENZA
051000010112     C                   Z-ADD     3             VS(161)                        *CARRIAG.CONS.NEW
051100010112     C                   Z-ADD     3             VS(177)                        *STOP CONS.NEW
051200000228     C                   Z-ADD     3             VS(251)                        *C/AS UFF.ARR
051300000228     C                   Z-ADD     3             VS(252)                        *C/AS PADR.ARR
051400000228     C                   Z-ADD     3             VS(256)                        *C/AS ARR.EXP (A VAL
051500000228     C                   Z-ADD     3             VS(257)                        *C/AS ARR.EXP (A %)
051600000228     C                   Z-ADD     3             VS(280)                        *C/AS ASSIST.EXP
051700100422     C                   Z-ADD     3             VS(810)                        *SEDE-CORR-CL.A
051800100422     C                   Z-ADD     3             VS(811)                        *SEDE-CORR-CL.B
051900100422     C                   Z-ADD     3             VS(812)                        *SEDE-CORR-CL.C
052000100422     C                   Z-ADD     3             VS(819)                        *SEDE-CORR-CL.T
052100100422     C                   Z-ADD     3             VS(813)                        *SEDE-ESPR-CL.A
052200100422     C                   Z-ADD     3             VS(814)                        *SEDE-ESPR-CL.B
052300100422     C                   Z-ADD     3             VS(815)                        *SEDE-ESPR-CL.C
052400100422     C                   Z-ADD     3             VS(820)                        *SEDE-ESPR-CL.T
052500100422     C                   Z-ADD     3             VS(816)                        *SEDE-DIST-CL.A
052600100422     C                   Z-ADD     3             VS(817)                        *SEDE-DIST-CL.B
052700100422     C                   Z-ADD     3             VS(818)                        *SEDE-DIST-CL.C
052800100422     C                   Z-ADD     3             VS(821)                        *SEDE-DIST-CL.T
052900100422     C                   Z-ADD     3             VS(822)                        *SEDE 88/99
053000100422     C                   Z-ADD     3             VS(823)                        *SEDE-EXP.DPD
053100011030     C                   Z-ADD     3             VS(278)                        *CONSEGNA ISOLA
053200011030     C                   Z-ADD     3             VS(279)                        *RITIRI   ISOLA
053300040621     C                   Z-ADD     3             VS(285)                        *RITIRI   ISOLA
053400051107     C                   Z-ADD     3             VS(323)                        *RITIRI   ISOLA
053500051107     C                   Z-ADD     3             VS(327)                        *RITIRI   ISOLA
053600051107     C                   Z-ADD     3             VS(328)                        *RITIRI   ISOLA
053700070522     C                   Z-ADD     3             VS(331)                        *RITIRI   ISOLA
053800070523     C                   Z-ADD     3             VS(268)                        *RITIRI   ISOLA
053900081031     C                   Z-ADD     3             VS(340)                        *RITIRI   ISOLA
054000090122     C                   Z-ADD     3             VS(332)                        *ARR.EXPORT (X SPED.
054100980710     C*
054200970211     C                   ENDSR
054300970211     C*------------------------------------------------------------------------*
054400970211     C* SEQVOC - DETERMINA LA SEQUENZA DI CALCOLO DI UNA VOCE
054500970211     C*           1 = VOCI NON DIPENDENTI DA SCAGLIONI O CT/LN/TER ARRIVO
054600970211     C*           2 = VOCI NON DIPENDENTI DA CT/LN/TER ARRIVO MA DIPENDENTI
054700970211     C*               DA SCAGLIONI
054800970211     C*           3 = VOCI DIPENDENTI DA SCAGLIONI E CT/LN/TER ARRIVO
054900970211     C*------------------------------------------------------------------------*
055000970211     C     SEQVOC        BEGSR
055100970211     C*
055200970211     C                   Z-ADD     EVTVOC        V
055300970211     C                   Z-ADD     1             VS(V)
055400970211     C* CONTROLLO SE DIPENDENTE DAL TIPO TARIFFA O DAGLI SCAGLIONI
055500970211     C* CAMPI: TIPO TARIFFA VOCE O TIPO RICERCA SCAGLIONI
055600970211     C     EVTVRV        IFEQ      '00'                                         *RIF.VOCE=QLI
055700970211     C     EVTVRV        OREQ      '01'                                         *RIF.VOCE=COLLI
055800970211     C     EVTVRV        OREQ      '03'                                         *RIF.VOCE=KG
055900970211     C     EVTVRV        OREQ      '04'                                         *RIF.VOCE=VOLUME
056000030429     C     EVTVRV        OREQ      '05'                                         *RIF.VOCE=KG TASSAB
056100000228     C     EVTVRV        OREQ      '11'                                         *RIF.VOCE=KG X GG GI
056200970211     C     EVTTIV        OREQ      '00'                                         *RIC.SCAG=QLI
056300970211     C     EVTTIV        OREQ      '01'                                         *RIC.SCAG=COLLI
056400970211     C     EVTTIV        OREQ      '03'                                         *RIC.SCAG=KG
056500970211     C     EVTTIV        OREQ      '04'                                         *RIC.SCAG=VOLUME
056600030429     C     EVTTIV        OREQ      '05'                                         *RIC.SCAG=KG TASSAB
056700000228     C     EVTTIV        OREQ      '11'                                         *RIC.SCAG=KG X GG GI
056800970211     C                   Z-ADD     2             VS(V)
056900970211     C                   ENDIF
057000970211     C*
057100970211     C* CONTROLLO SE DIPENDENTE DA DATI DI ARRIVO
057200970211     C* CAMPI: RIFERIMENTO TARIFFA FIL/C.TASS/CAP
057300970211     C* RIFERIMENTO TARIFFA FILIALE ITALIA
057400970211     C     EVTRFI        IFEQ      'LA'                                         *LINEA ARR.
057500970211     C     EVTRFI        OREQ      'TA'                                         *TERM. ARR.
057600970211     C     EVTRFI        OREQ      'TL'                                         *DEFL.
057700970211     C     EVTRFI        OREQ      'TT'                                         *TRAINO
057800970211     C     EVTRFI        OREQ      'T1'                                         *TRANSITO 1
057900970211     C     EVTRFI        OREQ      'T2'                                         *TRANSITO 2
058000040608     C     EVTRFI        OREQ      'T3'                                         *TRANSITO 2
058100040608     C     EVTRFI        OREQ      'T4'                                         *TRANSITO 2
058200040608     C     EVTRFI        OREQ      'T5'                                         *TRANSITO 2
058300970211     C*
058400970211     C* RIFERIMENTO TARIFFA FILIALE ESTERO
058500970211     C     EVTRFE        OREQ      'LA'                                         *LINEA ARR.
058600970211     C     EVTRFE        OREQ      'TA'                                         *TERM. ARR.
058700970211     C     EVTRFE        OREQ      'TL'                                         *DEFL.
058800970211     C     EVTRFE        OREQ      'TT'                                         *TRAINO
058900970211     C     EVTRFE        OREQ      'T1'                                         *TRANSITO 1
059000970211     C     EVTRFE        OREQ      'T2'                                         *TRANSITO 2
059100040608     C     EVTRFE        OREQ      'T3'                                         *TRANSITO 2
059200040608     C     EVTRFE        OREQ      'T4'                                         *TRANSITO 2
059300040608     C     EVTRFE        OREQ      'T5'                                         *TRANSITO 2
059400970211     C*
059500970211     C* RIFERIMENTO TARIFFA C.TASSAZ. ITALIA
059600970211     C     EVTRTI        OREQ      'SA'                                         *C.TASS. ARRIVO
059700970211     C*
059800970211     C* RIFERIMENTO TARIFFA C.TASSAZ. ESTERO
059900970211     C     EVTRTE        OREQ      'SA'                                         *C.TASS. ARRIVO
060000970211     C*
060100970211     C* RIFERIMENTO TARIFFA CAP ITALIA
060200970211     C     EVTRTI        OREQ      'PA'                                         *CAP ARRIVO
060300970211     C*
060400970211     C* RIFERIMENTO TARIFFA CAP ESTERO
060500970211     C     EVTRTE        OREQ      'PA'                                         *CAP ARRIVO
060600970211     C*
060700970211     C* SE E' UNA MAGGIORAZIONE
060800970211     C     EVTTVO        OREQ      'M'                                          *MAGGIORAZIONE
060900970211     C                   Z-ADD     3             VS(V)
061000970211     C                   ENDIF
061100970211     C*
061200970211     C                   ENDSR
061300970211     C*------------------------------------------------------------------------*
061400970211     C* CARETC - CARICA DATI TIPO
061500970211     C*------------------------------------------------------------------------*
061600970211     C     CARETC        BEGSR
061700970211     C*
061800091123     C                   MOVEL     'S'           KTCDSF                         *SEDE/FIL
061900970211     C                   MOVEL     D00CTO        KTCCTO                         *TARIF/OFF
062000970211     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE
062100970211     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
062200970211     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
062300970211     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
062400011008     C     KEYETC        CHAIN     TEETC01L                           99
062500970211     C     *IN99         IFEQ      *ON
062600970211     C                   MOVEL     MSG(24)       $MSG
062700970211     C                   EXSR      SCRERR
062800011008     C                   ELSE
062900011017     C     D00DIV        IFNE      WDIVCE
063000011008     C                   EXSR      CNVETC
063100011008     C                   ENDIF
063200011008     C                   ENDIF
063300970211     C*
063400970211     C                   ENDSR
063500970211     C*------------------------------------------------------------------------*
063600970211     C* CARETT - CARICA I CODICI TASSAZIONE
063700970211     C*------------------------------------------------------------------------*
063800970211     C     CARETT        BEGSR
063900970211     C*
064000970211     C                   Z-ADD     *ZEROS        I
064100091123     C                   MOVEL     'S'           KTTDSF                         *SEDE/FIL
064200970211     C                   MOVEL     D00CTO        KTTCTO                         *TARIF/OFF
064300970211     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE
064400970211     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
064500970211     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
064600970211     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
064700970211     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
064800011017     C     KE2ETT        SETLL     TEETT01L                           98
064900011017     C  N98              READ      TEETT01L                               98
065000970211     C     *IN98         IFEQ      '1'
065100970211     C                   MOVEL     MSG(26)       $MSG
065200970211     C                   EXSR      SCRERR
065300970211     C                   END
065400970211     C*
065500970211DO 1 C     *IN98         DOWEQ     '0'
065600970211     C     $ERR          ANDEQ     'N'
065700091123     C     ETTDSF        ANDEQ     'S'
065800980708     C     ETTCTO        ANDEQ     D00CTO
065900970211     C     ETTKSC        ANDEQ     D00KSC
066000970211     C     ETTCTR        ANDEQ     D00CTR
066100970211     C     ETTPRG        ANDEQ     D00PRG
066200970211     C     ETTLNP        ANDEQ     D00LNP
066300970211     C     ETTFIE        ANDEQ     D00FIE
066400970211IF 2 C     ETTATB        IFEQ      *BLANKS
066500970211     C     ETTRCT        ANDNE     *BLANKS
066600970211     C                   ADD       1             I
066700970211     C                   MOVEL     ETTCTS        TCTS(I)                        *COD.TASS.
066800970211     C                   MOVEL     ETTRCT        TRCT(I)                        *RAGGR. COD. TASS.
066900970211     C                   Z-ADD     ETTPPE        TPPE(I)                        *% PESO
067000970211     C*
067100970211     C* RECUPERA LINEA ARRIVO E TERMINAL ARRIVO
067200970211     C                   Z-ADD     CODUT         KTBUTE
067300970211     C                   MOVEL     'CT'          KTBCOD
067400970211     C                   MOVEL     ETTCTS        KTBKEY
067500970211     C     KEYTAB        CHAIN     TABEL00F                           99
067600970211     C     *IN99         IFEQ      *ON
067700970211     C                   MOVEL     MSG(27)       DSMSG                          *C.TASS. NON TROVATO
067800970211     C                   MOVEL     ETTCTS        DSCTE                          *C.TASS.
067900970211     C                   MOVEL     DSMSG         $MSG                           *MANCA TARIFFA
068000970211     C                   EXSR      SCRERR
068100970211     C                   END
068200970211IF 3 C     $ERR          IFEQ      'N'
068300970211     C                   MOVEL     TBLUNI        DSCT
068400970211     C*
068500970211     C                   MOVEL     §CTRAP        TRAP(I)                        *REGIONE APPARTENENZ
068600970211     C     §CTNAR        IFEQ      *BLANKS
068700970211     C                   MOVEL     'IT'          TNAR(I)                        *NAZIONE APPARTENENZ
068800970211     C                   ELSE
068900970211     C                   MOVEL     'EE'          TNAR(I)
069000970211     C                   ENDIF
069100970211E  3 C                   ENDIF
069200970211E  2 C                   ENDIF
069300011017     C                   READ      TEETT01L                               98
069400970211E  1 C                   ENDDO
069500970211     C                   Z-ADD     I             WULTCT            5 0
069600970211     C*
069700970211     C                   ENDSR
069800980903     C*------------------------------------------------------------------------*
069900980903     C* CERLNA - CERCA LINEA ARRIVO E TERMINAL ARRIVO PER OGNI COD.TASSAZIONE
070000980903     C*------------------------------------------------------------------------*
070100980903     C     CERLNA        BEGSR
070200980903     C*
070300980903     C* CICLO PER TUTTI I CODICI TASSAZIONE ARRIVO
070400980903DO  1C                   DO        WULTCT        I
070500980903     C*
070600980903     C* RECUPERA LINEA ARRIVO E TERMINAL ARRIVO
070700980903     C                   Z-ADD     CODUT         KTBUTE
070800980903     C                   MOVEL     'CT'          KTBCOD
070900980903     C                   MOVEL     TCTS(I)       KTBKEY
071000980903     C     KEYTAB        CHAIN     TABEL00F                           99
071100980903     C     *IN99         IFEQ      *ON
071200980903     C                   MOVEL     MSG(27)       DSMSG                          *C.TASS. NON TROVATO
071300980903     C                   MOVEL     TCTS(I)       DSCTE                          *C.TASS.
071400980903     C                   MOVEL     DSMSG         $MSG                           *MANCA TARIFFA
071500980903     C                   EXSR      SCRERR
071600980903     C                   END
071700980903IF  2C     $ERR          IFEQ      'N'
071800980903     C                   MOVEL     TBLUNI        DSCT
071900980903     C*
072000980903     C* LINEA ARRIVO
072100980903     C                   CLEAR                   DSSI95
072200980903     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
072300980903     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
072400980903     C                   MOVEL     §CTNAR        I95NAR                         *NAZIONE
072500980903     C                   MOVEL     §CTCAP        I95CAP                         *CAP
072600980903     C                   Z-ADD     SPSC(S)       I95LKG                         *PESO
072700980903     C                   Z-ADD     SVSC(S)       I95LMC                         *VOLUME
072800980903     C                   Z-ADD     WTFP          I95TFP                         *TERMINAL PARTENZA
072900980903     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
073000980903     C                   Z-ADD     D00DTC        I95DAT                         *DATA RIFERIMENTO
073100980903     C                   CALL      'TISI95R'
073200980903     C                   PARM                    DSSI95
073300980903IF  3C     O95ERR        IFNE      *BLANKS                                      *NON TROVATO
073400980903     C                   MOVEL     O95MSG        $MSG
073500980903     C                   EXSR      SCRERR
073600980903X   3C                   ELSE                                                   *TROVATO
073700980903     C                   Z-ADD     O95LNA        TLNA(I)                        *LINEA ARRIVO
073800980903     C* TERMINAL ARRIVO
073900980903     C                   CLEAR                   DSLV55
074000980903     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
074100980903     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
074200980903     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
074300980903     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
074400980903     C                   Z-ADD     D00LNP        D55LNP                         *LINEA PARTENZA
074500980903     C                   CALL      'FNLV55R'
074600980903     C                   PARM                    DSLV55
074700980903IF  4C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
074800980903     C                   MOVEL     D55MSG        $MSG
074900980903     C                   EXSR      SCRERR
075000980903X   4C                   ELSE                                                   *TROVATO
075100980903     C                   Z-ADD     D55TFA        TTFA(I)                        *TERMINAL ARRIVO
075200980903E   4C                   ENDIF
075300980903E   3C                   ENDIF
075400980903E   2C                   ENDIF
075500980903E   1C                   ENDDO
075600980903     C*
075700980903     C                   ENDSR
075800970211     C*------------------------------------------------------------------------*
075900970211     C* CARETS - CARICA GLI SCAGLIONI
076000970211     C*------------------------------------------------------------------------*
076100970211     C     CARETS        BEGSR
076200970211     C*
076300970211     C                   Z-ADD     *ZEROS        I
076400091123     C                   MOVEL     'S'           KTSDSF                         *SEDE/FIL
076500970211     C                   MOVEL     D00CTO        KTSCTO                         *TARIF/OFF
076600970211     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE
076700970211     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
076800970211     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
076900970211     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
077000010926     C     KE2ETS        SETLL     TEETS01L                           99
077100010926     C  N99              READ      TEETS01L                               99
077200970211     C     *IN99         IFEQ      '1'
077300970211     C                   MOVEL     MSG(25)       $MSG
077400970211     C                   EXSR      SCRERR
077500970211     C                   END
0776009702111    C     *IN99         DOWEQ     '0'
077700091123     C     ETSDSF        ANDEQ     'S'
077800980708     C     ETSCTO        ANDEQ     D00CTO
077900970211     C     ETSKSC        ANDEQ     D00KSC
078000970211     C     ETSCTR        ANDEQ     D00CTR
078100970211     C     ETSPRG        ANDEQ     D00PRG
078200970211     C     ETSLNP        ANDEQ     D00LNP
0783009702112    C     ETSATB        IFEQ      *BLANKS
078400011017     C     D00DIV        IFNE      WDIVCE
078500011008     C                   EXSR      CNVETS
078600011008     C                   ENDIF
078700970211     C                   ADD       1             I
078800970211     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPER.
078900970211     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO RIF. CALC.
079000970211     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI RIF. CALC.
079100970211     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOL. RIF. CALC.
079200970211     C                   Z-ADD     ETSPPE        SPPE(I)                        *PERC. PESO
079300970317     C                   Z-ADD     ETSINL        SINL(I)                        *IMPORTO INOLTRO
079400970402     C                   MOVEL     ETSSDE        SSDE(I)                        *SEGNO DELTA
079500970402     C                   Z-ADD     ETSPDE        SPDE(I)                        *% DELTA
079600970317     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPP. PESO/VOL.
0797009702112    C                   END
079800010926     C                   READ      TEETS01L                               99
0799009702111    C                   END
080000970211     C                   Z-ADD     I             WULTSC            5 0
080100970211     C*
080200970211     C                   ENDSR
080300961128     C*------------------------------------------------------------------------*
080400961128     C* CAL2R1 - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA) X SEQUENZA 1
080500961128     C*------------------------------------------------------------------------*
080600961128     C     CAL2R1        BEGSR
080700961128     C*
080800961128     C* TIPO -02- SPEDIZIONI --------------------------------------------------*
080900961128     C                   Z-ADD     1             I
081000961128     C     '02'          LOOKUP    C2R(I)                                 98
081100961128     C     *IN98         IFEQ      '0'
081200961128     C                   MOVEL     MSG(6)        $MSG                           *TIPO TARIF.02 NON T
081300961128     C                   EXSR      SCRERR
081400961128     C                   ELSE
081500961128     C                   Z-ADD     1             Q2R(I)                         *UNO
081600961128     C                   ENDIF
081700961128     C*
081800040811     C* TIPO -06- BANCALI -----------------------------------------------------*
081900040802     C                   Z-ADD     1             I
082000040802     C     '06'          LOOKUP    C2R(I)                                 98
082100040802     C     *IN98         IFEQ      '0'
082200040802     C                   MOVEL     MSG(28)       $MSG                           *TIPO TARIF.02 NON T
082300040802     C                   EXSR      SCRERR
082400040811     C                   ELSE
082500040811     C                   Z-ADD     1             Q2R(I)                         *UNO
082600040802     C                   ENDIF
082700040802     C*
082800961128     C* TIPO -09- STOP RITIRO -------------------------------------------------*
082900961128     C                   Z-ADD     1             I
083000961128     C     '09'          LOOKUP    C2R(I)                                 98
083100961128IF  1C     *IN98         IFEQ      '0'
083200961128     C                   MOVEL     MSG(9)        $MSG                           *TIPO TARIF.09 NON T
083300961128     C                   EXSR      SCRERR
083400961128X   1C                   ELSE
083500961128     C                   Z-ADD     1             Q2R(I)
083600961128     C     ETCSPR        IFGT      1
083700961128     C     1             DIV       ETCSPR        WNSTOP           12 6          *1 / N. DI STOP
083800961128     C     WNSTOP        IFLT      0,001
083900961128     C                   Z-ADD     0,001         Q2R(I)
084000961128     C                   ELSE
084100961128     C                   Z-ADD     WNSTOP        Q2R(I)
084200961128     C                   ENDIF
084300961128     C                   ENDIF
084400961128E   1C                   ENDIF
084500961128     C*
084600961128     C* TIPO -10- STOP CONSEGNA -----------------------------------------------*
084700961128     C                   Z-ADD     1             I
084800961128     C     '10'          LOOKUP    C2R(I)                                 98
084900961128IF  1C     *IN98         IFEQ      '0'
085000961128     C                   MOVEL     MSG(16)       $MSG                           *TIPO TARIF.10 NON T
085100961128     C                   EXSR      SCRERR
085200961128X   1C                   ELSE
085300961128     C                   Z-ADD     1             Q2R(I)
085400961128     C     ETCSPC        IFGT      1
085500961128     C     1             DIV       ETCSPC        WNSTOP           12 6          *1 / N. DI STOP
085600961128     C     WNSTOP        IFLT      0,001
085700961128     C                   Z-ADD     0,001         Q2R(I)
085800961128     C                   ELSE
085900961128     C                   Z-ADD     WNSTOP        Q2R(I)
086000961128     C                   ENDIF
086100961128     C                   ENDIF
086200961128E   1C                   ENDIF
086300961128     C*
086400961128     C* TIPO -22- CONTRASEGNO -------------------------------------------------*
086500961128     C                   Z-ADD     1             I
086600961128     C     '22'          LOOKUP    C2R(I)                                 98
086700961128     C     *IN98         IFEQ      '0'
086800961128     C                   MOVEL     MSG(12)       $MSG                           *TIPO TARIF.22 NON T
086900961128     C                   EXSR      SCRERR
087000961128     C                   ELSE
087100961128     C                   Z-ADD     ETCCAS        Q2R(I)                         *IMPORTO CONTRASSEGN
087200961128     C                   ENDIF
087300961128     C*
087400961128     C* TIPO -23- VALORE MERCE ASSICURATA -------------------------------------*
087500961128     C                   Z-ADD     1             I
087600961128     C     '23'          LOOKUP    C2R(I)                                 98
087700961128     C     *IN98         IFEQ      '0'
087800961128     C                   MOVEL     MSG(13)       $MSG                           *TIPO TARIF.23 NON T
087900961128     C                   EXSR      SCRERR
088000961128     C                   ELSE
088100961128     C                   Z-ADD     ETCASC        Q2R(I)                         *VAL MERCE ASSIC.
088200961128     C                   ENDIF
088300961128     C*
088400961128     C* TIPO -24- NUMERO DI GIORNI PAGAMENTO ----------------------------------*
088500961128     C                   Z-ADD     1             I
088600961128     C     '24'          LOOKUP    C2R(I)                                 98
088700961128     C     *IN98         IFEQ      '0'
088800961128     C                   MOVEL     MSG(14)       $MSG                           *TIPO TARIF.24 NON T
088900961128     C                   EXSR      SCRERR
089000961128     C                   ELSE
089100961128     C                   Z-ADD     ETCGGP        Q2R(I)                         *GIORNI PAGAM.
089200961128     C                   ENDIF
089300000228     C*
089400000228     C* TIPO -12- NUMERO DI GIORNI GIACENZA -----------------------------------*
089500000228     C                   Z-ADD     1             I
089600000228     C     '12'          LOOKUP    C2R(I)                                 98
089700000228     C     *IN98         IFEQ      '0'
089800000228     C                   MOVEL     MSG(10)       $MSG                           *TIPO TARIF.12 NON T
089900000228     C                   EXSR      SCRERR
090000000228     C                   ELSE
090100000228     C     ETCGGG        IFEQ      *ZEROS
090200000228     C                   Z-ADD     1             Q2R(I)                         *GIORNI GIACENZA
090300000228     C                   ELSE
090400000228     C                   Z-ADD     ETCGGG        Q2R(I)                         *GIORNI GIACENZA
090500000228     C                   ENDIF
090600000228     C                   ENDIF
090700961128     C*
090800961128     C                   ENDSR
090900961128     C*------------------------------------------------------------------------*
091000961128     C* CAL2R2 - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA) X SEQUENZA 2
091100961128     C*------------------------------------------------------------------------*
091200961128     C     CAL2R2        BEGSR
091300961128     C*
091400961128     C* TIPO -01- COLLI -------------------------------------------------------*
091500961128     C                   Z-ADD     1             I
091600961128     C     '01'          LOOKUP    C2R(I)                                 98
091700961128     C     *IN98         IFEQ      '0'
091800961128     C                   MOVEL     MSG(5)        $MSG                           *TIPO TARIF.01 NON T
091900961128     C                   EXSR      SCRERR
092000961128     C                   ELSE
092100961128     C                   Z-ADD     SCSC(S)       Q2R(I)                         *NUMERO COLLI
092200961128     C                   ENDIF
092300961128     C*
092400961128     C* TIPO -03- CHILOGRAMMI -------------------------------------------------*
092500961128     C                   Z-ADD     1             I
092600961128     C     '03'          LOOKUP    C2R(I)                                 98
092700961128     C     *IN98         IFEQ      '0'
092800961128     C                   MOVEL     MSG(7)        $MSG                           *TIPO TARIF.03 NON T
092900961128     C                   EXSR      SCRERR
093000961128     C                   ELSE
093100961128     C                   Z-ADD     SPSC(S)       Q2R(I)                         *PESO KG.
093200961128     C                   ENDIF
093300961128     C*
093400961128     C* TIPO -04- VOLUME ------------------------------------------------------*
093500961128     C                   Z-ADD     1             I
093600961128     C     '04'          LOOKUP    C2R(I)                                 98
093700961128     C     *IN98         IFEQ      '0'
093800961128     C                   MOVEL     MSG(8)        $MSG                           *TIPO TARIF.04 NON T
093900961128     C                   EXSR      SCRERR
094000961128     C                   ELSE
094100961128     C                   Z-ADD     SVSC(S)       Q2R(I)                         *VOLUME
094200961128     C                   ENDIF
094300040809     C*
094400000228     C* TIPO -11- CHILOGRAMMI PER GIORNI GIACENZA -----------------------------*
094500000228     C                   Z-ADD     1             I
094600000228     C     '11'          LOOKUP    C2R(I)                                 98
094700000228     C     *IN98         IFEQ      '0'
094800000228     C                   MOVEL     MSG(11)       $MSG                           *TIPO TARIF.11 NON T
094900000228     C                   EXSR      SCRERR
095000000228     C                   ELSE
095100000228     C     ETCGGG        IFEQ      *ZEROS
095200011025     C                   Z-ADD     1             WT0155           21 8
095300000228     C                   ELSE
095400000228     C                   Z-ADD     ETCGGG        WT0155
095500000228     C                   ENDIF
095600000228     C     SPSC(S)       MULT      WT0155        Q2R(I)
095700000228     C                   ENDIF
095800961128     C*
095900961128     C                   ENDSR
096000961128     C*------------------------------------------------------------------------*
096100970207     C* CHKVOC - DETERMINA LE VOCI DA CALCOLARE E CONTROLLA VALIDITA'
096200961128     C*------------------------------------------------------------------------*
096300961128     C     CHKVOC        BEGSR
096400970207     C*
096500970207     C* IMPOSTA A NO TUTTI I FLAG 'DA CALCOLARE' DELLE VOCI
096600040811     C* LA SCHIERA VB MI SERVE AD INDIVIDUARE SE BANCALI BARTOLINI O BANCALI
096700040811     C* BNB
096800040811     C                   CLEAR                   VB                             *VOCE DA CALC.
096900040811     C                   MOVEL     'N'           VC                             *VOCE DA CALC.
097000970207     C                   Z-ADD     100           VP                             * % DA CONSIDERARE
097100970207     C                   MOVEL     'S'           VI                             *INGLOBA S/N
097200970207     C*
097300970207     C* IMPOSTA A SI TUTTE LE VOCI SEMPRE PRESENTI
097400970207     C                   MOVEL     'S'           VC(10)                         *STOP RITIRO
097500970207     C                   MOVEL     'S'           VC(50)                         *SMISTAMENTO PART.
097600970207     C                   MOVEL     'S'           VC(60)                         *CARICO-SP.PART.-IMP
097700970207     C                   MOVEL     'S'           VC(70)                         *UFFICIO PART.
097800970207     C                   MOVEL     'S'           VC(80)                         *SPESE VARIE PART. 1
097900970207     C                   MOVEL     'S'           VC(81)                         *SPESE VARIE PART. 2
098000970207     C                   MOVEL     'S'           VC(110)                        *SCARICO-SP.ARR-IMA
098100970207     C                   MOVEL     'S'           VC(120)                        *SMISTAMENTO ARR.
098200970207     C                   MOVEL     'S'           VC(130)                        *SPUNTA USCITA
098300970207     C                   MOVEL     'S'           VC(180)                        *UFFICIO ARRIVI
098400970207     C                   MOVEL     'S'           VC(190)                        *SPESE VARIE ARR. 1
098500970207     C                   MOVEL     'S'           VC(191)                        *SPESE VARIE ARR. 2
098600970207     C                   MOVEL     'S'           VC(192)                        *SCORTE NA/PA A SPED
098700061207     C                   MOVEL     'S'           VC(193)                        *SCORTE NA/PA A SPED
098800021028     C                   MOVEL     'S'           VC(194)                        *SCORTE NA/PA A KG.
098900970207     C                   MOVEL     'S'           VC(300)                        *PAGAMENTO
099000980717     C                   MOVEL     'S'           VC(320)                        *SERV.SEDE(A SPED.)
099100000215     C                   MOVEL     'S'           VC(321)                        *SERV.SEDE(A PESO)
099200000215     C                   MOVEL     'S'           VC(322)                        *SERV.SEDE(A COLLO)
099300090909     C     D00DTC        IFGT      20090900
099400090909     C                   MOVEL     'S'           VC(112)
099500090909     C                   MOVEL     'S'           VC(122)
099600090909     C                   MOVEL     'S'           VC(137)
099700090909     c                   end
099800050926     C* CONTROLLO SE ESISTE =======> MERCE DI VALORE
099900050926     C     D00DTC        IFGT      20051000
100000050926     c     etcval        andgt     0
100100050926     C                   MOVEL     'S'           VC(19)
100200050926     C                   Z-ADD     etcval        VP(19)                         * % DA CONSIDER.
100300050926     C                   MOVEL     'S'           VC(21)
100400050926     C                   Z-ADD     etcval        VP(21)                         * % DA CONSIDER.
100500050926     C                   ENDIF
100600000721     C*
100700970207     C                   MOVEL     'S'           VC(900)                        *DELTA
100800041213     C* =============> adeguamento tariffariorio 2005
100900041213     C     D00DTC        IFGT      20050100
101000091123     C     D00DTC        andlt     20100100
101100041213     C                   MOVEL     'S'           VC(600)
101200041213     C                   MOVEL     'S'           VC(601)
101300041213     C                   MOVEL     'S'           VC(602)
101400041213     C                   MOVEL     'S'           VC(603)
101500041213     C                   MOVEL     'S'           VC(604)
101600040608     C                   ENDIF
101700061207     c* attivo sempre le maggiorazioni di adeguamento tariffario anno 2007
101800061207     C     D00DTC        IFGT      20070100
101900091123     C     D00DTC        andlt     20100100
102000061207     C                   MOVEL     'S'           VC(605)
102100061207     C                   MOVEL     'S'           VC(606)
102200061207     c                   end
102300091123     C                   IF        d00dtc > 20090400   and
102400091123     C                             d00dtc < 20100100
102500090908     C                   MOVEL     'S'           VC(607)
102600090908     c                   end
102700091123     C                   IF        d00dtc > 20090900 and
102800091123     C                             d00dtc < 20100100
102900090908     C                   MOVEL     'S'           VC(608)
103000090908     c                   end
103100041213     C* =============> immagine digitalizzata
103200041213     C     D00DTC        IFGT      20040700
103300041213     C                   MOVEL     'S'           VC(188)                        *CARICO CONSEGNA
103400041213     C                   ENDIF
103500021105     c*
103600021105     C* =============> ETICHETTATURA e SCARICO e PICKING e CARICO, FINO al 2003
103700040609     c* escludeva la 140 e 150 (carico aut.) erroneamente
103800021105     C     D00DTC        IFLT      20030000
103900021105     C                   MOVEL     'S'           VC(30)                         *SCARICO RITIRO
104000021105     C                   MOVEL     'S'           VC(40)                         *ETICHETTATURA
104100040609     C                   ENDIF
104200021105     C                   MOVEL     'S'           VC(140)                        *PICKING
104300021105     C                   MOVEL     'S'           VC(150)                        *CARICO CONSEGNA
104400040609     C*                  ENDIF
104500021105     c*
104600021105     C* =============> ETICHETTATURA e SCARICO, DOPO il 2002
104700021105IF  1C     D00DTC        IFGT      20030000
104800021105     C*
104900021105IF  2C     ETCDKC        IFNE      'S'                                          *NO procedura disc C
105000021105IF  3C     ETCIX1        IFEQ      *BLANKS                                      *autotrasportatore
105100021105     C                   MOVEL     'S'           VC(40)                         *ETICHETTATURA
105200021105     C                   MOVEL     'S'           VC(57)                         *CONTR.ENTRATA MERCI
105300021105X   3C                   ELSE                                                   *NO autotrasportator
105400021105IF  4C     ETCIX1        IFEQ      'C'                                          *cooperativa
105500021105     C                   MOVEL     'S'           VC(42)                         *ETICHETTATURA
105600021105E   4C                   ENDIF
105700021105E   3C                   ENDIF
105800021105E   2C                   ENDIF
105900021105     c*
106000021105IF  2C     ETCDKC        IFEQ      'S'                                          *SI procedura disc C
106100021105IF  3C     ETCIX1        IFEQ      *BLANKS                                      *autotrasportatore
106200030919IF  4C     D00DTC        IFLT      20030922
106300030919     C                   MOVEL     'S'           VC(43)
106400030919X   4c                   ELSE
106500030919IF  5C     ETCPCM        IFGT      *ZEROS                                       *merce a magazzino
106600030918     C                   MOVEL     'S'           VC(44)                         *PREPAR.ETIC.DISC C
106700030919X   5c                   ELSE                                                   *NO merce a magazz.
106800021105     C                   MOVEL     'S'           VC(43)                         *PREPAR.ETIC.DISC C
106900030919E   5C                   ENDIF
107000030919E   4C                   ENDIF
107100030919E   3C                   ENDIF
107200021105E   2C                   ENDIF
107300021105     C*
107400021105IF  2C     ETCIX2        IFEQ      *BLANKS                                      *autotrasportatore
107500021105     C                   MOVEL     'S'           VC(30)                         *SCARICO RITIRO
107600021105X   2C                   ELSE                                                   *cooperativa
107700021105     C                   MOVEL     'S'           VC(33)                         *SCARICO RITIRO
107800021105E   2C                   ENDIF
107900021105     C*
108000021105E   1C                   ENDIF
108100021105     C*
108200021105     C* CONTROLLO SE ESISTE =======> DISK 'A' E DISK 'B'
108300021105     C     ETCBOL        IFEQ      'B'
108400021105     C                   MOVEL     'S'           VC(510)                        *DISK 'B'
108500021105     C                   MOVEL     'S'           VC(511)                        *DISK 'A/B'
108600021105     C                   MOVEL     'S'           VC(55)                         *QUADRAT.DISK B
108700021105     C                   ENDIF
108800021105     C     ETCBOL        IFEQ      'A'
108900021105     C                   MOVEL     'S'           VC(511)                        *DISK 'A/B'
109000021105     C                   ENDIF
109100021105     C*
109200021105     C* CONTROLLA SE ESISTE =======> DISCK 'C'
109300021105     C     ETCDKC        IFEQ      'S'
109400021105     C                   MOVEL     'S'           VC(56)
109500021105     C                   ENDIF
109600050121     C* CONTROLLO SE ESISTE =======> CONSEGNA FUORI MISURA
109700050121     c     ETCPCF        IFGT      0
109800050121     C                   MOVEL     'S'           VC(475)
109900050121     C                   Z-ADD     ETCPCF        VP(475)                        * % DA CONSIDER.
110000050121     C                   MOVEL     ETCIFU        VI(475)                        *INGLOBA S/N
110100050121     C                   ENDIF
110200040802     C* CONTROLLO SE ESISTE =======> DESTINATARIO DISAGIATO
110300040802     c     etcdes        ifgt      0
110400040802     C                   MOVEL     'S'           VC(534)
110500040802     C                   Z-ADD     etcdes        VP(534)                        * % DA CONSIDER.
110600050805     C                   MOVEL     ETCIde        VI(534)                        *INGLOBA S/N
110700040802     C                   MOVEL     'S'           VC(535)
110800040802     C                   Z-ADD     etcdes        VP(535)                        * % DA CONSIDER.
110900050805     C                   MOVEL     ETCIde        VI(535)                        *INGLOBA S/N
111000040802     C                   ENDIF
111100040809     C* CONTROLLO SE ESISTE =======> BANCALI BAN
111200040802     c     etcnba        ifgt      0
111300040802     C                   MOVEL     'S'           VC(17)
111400040811     C                   MOVEL     'B'           VB(17)
111500040824     C                   Z-ADD     etcban        VP(17)
111600050805     C                   MOVEL     ETCIba        VI(17)                         *INGLOBA S/N
111700040802     C                   MOVEL     'S'           VC(119)
111800040811     C                   MOVEL     'B'           VB(119)
111900040824     C                   Z-ADD     etcban        VP(119)
112000050805     C                   MOVEL     ETCIba        VI(119)                        *INGLOBA S/N
112100040802     C                   MOVEL     'S'           VC(142)
112200040811     C                   MOVEL     'B'           VB(142)
112300040824     C                   Z-ADD     etcban        VP(142)
112400050805     C                   MOVEL     ETCIba        VI(142)                        *INGLOBA S/N
112500040802     C                   MOVEL     'S'           VC(186)
112600040811     C                   MOVEL     'B'           VB(186)
112700040824     C                   Z-ADD     etcban        VP(186)
112800050805     C                   MOVEL     ETCIba        VI(186)                        *INGLOBA S/N
112900040802     C                   MOVEL     'S'           VC(213)
113000040811     C                   MOVEL     'B'           VB(213)
113100040824     C                   Z-ADD     etcban        VP(213)
113200050805     C                   MOVEL     ETCIba        VI(213)                        *INGLOBA S/N
113300040802     C                   ENDIF
113400040811     c* BANCALI oltre 20 colli non imbancalate
113500040811     c     etcbnb        ifgt      0
113600040811     C                   MOVEL     'S'           VC(34)
113700040811     C                   MOVEL     'O'           VB(34)
113800040811     C                   Z-ADD     ETCBNB        VP(34)
113900040811     C                   Z-ADD     2             VS(34)
114000040811     C                   ENDIF
114100050805     C* CONTROLLA SE ESISTE =======> packing list
114200050808IF  1C     etcpkl        IFeq      'S'
114300050805     C     D00DTC        andgt     20050900
114400050805     C                   MOVEL     'S'           VC(189)
114500050805E   1C                   ENDIF
114600000215     C*
114700970207     C* CONTROLLO SE ESISTE =======> AFFLUENZA
114800970207     C*    LINEA PARTENZA DIVERSA DAL TERMINAL PARTENZA
114900970207     C     D00LNP        IFNE      WTFP
115000970207     C                   MOVEL     'S'           VC(220)                        *TRAINI AFFLUENZA
115100970207     C                   MOVEL     'S'           VC(31)                         *SCARICO AFFLUENZA
115200970207     C                   MOVEL     'S'           VC(51)                         *SMISTAM.AFFLUENZA
115300970207     C                   MOVEL     'S'           VC(61)                         *CARICO AFFLUENZA
115400050926     c* merce di valore
115500050926IF  1C     etcval        IFgt      0
115600050926IF  0C     d00dtc        ANDGT     20051000
115700050926     C                   MOVEL     'S'           VC(20)
115800050926     c                   Z-ADD     etcval        VP(20)                         * % DA CONSIDER.
115900050926     c                   end
116000970207     C                   ENDIF
116100970207     C*
116200970207     C* CONTROLLO SE ESISTE =======> GIACENZA
116300970207     C     ETCPGI        IFGT      *ZEROS
116400970207     C                   MOVEL     'S'           VC(260)                        *GIACENZA UFF.PART.
116500970207     C                   Z-ADD     ETCPGI        VP(260)                        * % DA CONSIDER.
116600970207     C                   MOVEL     ETCIGI        VI(260)                        *INGLOBA S/N
116700970207     C                   MOVEL     'S'           VC(261)                        *GIACENZA UFF.ARR.
116800970207     C                   Z-ADD     ETCPGI        VP(261)                        * % DA CONSIDER.
116900970207     C                   MOVEL     ETCIGI        VI(261)                        *INGLOBA S/N
117000970207     C                   MOVEL     'S'           VC(262)                        *GIACENZA MAG.ARR.
117100970207     C                   Z-ADD     ETCPGI        VP(262)                        * % DA CONSIDER.
117200970207     C                   MOVEL     ETCIGI        VI(262)                        *INGLOBA S/N
117300970207     C                   ENDIF
117400970207     C*
117500060314     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE AC PLUS
117600060314     C                   MOVE      D00KSC        KSCIS             4 0
1177000603141    C     KSCIS         IFEQ      8888
117800970226     C     KSCIS         OREQ      9999
117900060314     C     ETCPAC        IFGT      *ZEROS
118000970226     C                   MOVEL     'S'           VC(291)                        *ASSIC. X CONTO 88/9
118100970226     C                   Z-ADD     ETCPAC        VP(291)                        * % DA CONSIDER.
118200970226     C                   MOVEL     ETCIAS        VI(291)                        *INGLOBA S/N
118300060314     c                   end
118400060314x1   C                   ELSE
1185000603142    C     ETCPAC        IFGT      *ZEROS
118600970207     C                   MOVEL     'S'           VC(290)                        *ASSIC. PER CONTO
118700970207     C                   Z-ADD     ETCPAC        VP(290)                        * % DA CONSIDER.
118800970207     C                   MOVEL     ETCIAS        VI(290)                        *INGLOBA S/N
1189000603143    C                   if        d00dtc > 20050800                            *FINO al 2002
119000051110     C                   MOVEL     'S'           VC(292)
119100051110     C                   Z-ADD     ETCPAC        VP(292)                        * % DA CONSIDER.
119200051110     C                   MOVEL     ETCIAS        VI(292)                        *INGLOBA S/N
1193000603143    c                   end
1194009702262    C                   ENDIF
119500970207     C*
119600970207     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE RCV
119700060316     c* la competenza x rcv viene sempre attribuita x rcv o x ac base
119800970207     C     ETCPAR        IFGT      *ZEROS
119900060316     C     ETCxx1        orGT      *ZEROS
120000100224     C* attivo nuova voce 286 da febbraio 2010 solo per
120100100224     C* Estero, la voce 281 da febbraio sarà solo                      venta
120200100224     C* per Italia
120300100224     c                   if        d00dtc > 20100200  and
120400100224     C                             d00fie = 'E'
120500100224     C                   MOVEL     'S'           VC(286)
120600100224     c                   if        etcpar > 0
120700100224     C                   Z-ADD     ETCPAR        VP(286)                        * % DA CONSIDER.
120800100224     c                   else
120900100224     C                   Z-ADD     ETCxx1        VP(286)                        * % DA CONSIDER.
121000100224     c                   end
121100100224     C                   MOVEL     'S'           VI(286)                        *INGLOBA S/N
121200100224     c                   else
121300970207     C                   MOVEL     'S'           VC(281)                        *ASSIC. RCV DA 06/96
121400060316     c                   if        etcpar > 0
121500970207     C                   Z-ADD     ETCPAR        VP(281)                        * % DA CONSIDER.
121600060316     c                   else
121700060316     C                   Z-ADD     ETCxx1        VP(281)                        * % DA CONSIDER.
121800060316     c                   end
121900060314     c* x rcv inglobo sempre nel porto
122000060314     C                   MOVEL     'S'           VI(281)                        *INGLOBA S/N
122100970207     C                   ENDIF
122200100224     C                   ENDIF
122300060314     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE AC base
122400060314     C                   if        d00dtc > 20060300 and etcxx1 > 0             *FINO al 2002
122500060314     c* x ac base non inglobo nel porto
122600060314     c* cliente top o direzionale
122700060314     c                   select
122800060314     c                   when      etcclv = 'T' or etcclv = 'D'
122900060314     C                   MOVEL     'S'           VC(293)
123000060314     C                   Z-ADD     ETCxx1        VP(293)                        * % DA CONSIDER.
123100060314     C                   MOVEL     'N'           VI(293)                        *INGLOBA S/N
123200060314     c* cliente A
123300060314     c                   when      etcclv = 'A'
123400060314     C                   MOVEL     'S'           VC(294)
123500060314     C                   Z-ADD     ETCxx1        VP(294)                        * % DA CONSIDER.
123600060314     C                   MOVEL     'N'           VI(294)                        *INGLOBA S/N
123700060314     c* cliente B o C
123800060314     c                   when      etcclv = 'B' or etcclv = 'C'
123900060314     C                   MOVEL     'S'           VC(295)
124000060314     C                   Z-ADD     ETCxx1        VP(295)                        * % DA CONSIDER.
124100060314     C                   MOVEL     'N'           VI(295)                        *INGLOBA S/N
124200060314     C                   endsl
124300060314     C                   endif
1244000603141    C                   ENDIF
124500970207     C*
124600970207     C* CONTROLLO SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
124700970207     C     ETCPCA        IFGT      *ZEROS
124800131120     C*    ETCPCS        IFGT      *ZEROS                                       *CONS. SUPERMERCATI
124900131120     C*                  ELSE
125000021028     C                   MOVEL     'S'           VC(240)                        *CONS. SU APPUNT.
125100021028     C                   Z-ADD     ETCPCA        VP(240)                        * % DA CONSIDER.
125200021028     C                   MOVEL     ETCIAP        VI(240)                        *INGLOBA S/N
125300131120     C*                  ENDIF
125400021028     C                   ENDIF
125500970207     C*
125600970207     C* CONTROLLO SE ESISTE =======> CONSEGNA AI SUPERMERCATI
125700970207     C     ETCPCS        IFGT      *ZEROS
125800970207     C                   MOVEL     'S'           VC(242)                        *CONS. AI SUPERMERCA
125900970207     C                   Z-ADD     ETCPCS        VP(242)                        * % DA CONSIDER.
126000970207     C                   MOVEL     ETCISU        VI(242)                        *INGLOBA S/N
126100980121     C                   MOVEL     'S'           VC(247)                        *CONS. AI SUPERMERCA
126200980121     C                   Z-ADD     ETCPCS        VP(247)                        * % DA CONSIDER.
126300980121     C                   MOVEL     ETCISU        VI(247)                        *INGLOBA S/N
126400970207     C                   ENDIF
126500970207     C*
126600970207     C* CONTROLLO SE ESISTE =======> CONSEGNA AI PIANI
126700970207     C     ETCPCP        IFGT      *ZEROS
126800970207     C                   MOVEL     'S'           VC(241)                        *CONS. AI PIANI
126900970207     C                   Z-ADD     ETCPCP        VP(241)                        * % DA CONSIDER.
127000970207     C                   MOVEL     ETCIPI        VI(241)                        *INGLOBA S/N
127100970207     C                   ENDIF
127200970207     C*
127300970207     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
127400970207     C     ETCCAS        IFGT      *ZEROS
127500060224     c**** mittente
127600970207     C     ETCPAM        IFGT      *ZEROS                                       *MITTENTE
127700060224     c******* non clienti informatizzati
127800060224     C                   IF        (etcbol <> 'A' and
127900060224     C                             etcbol <> 'B' and
128000060224     C                             etcdkc <> 'S' and d00dtc > 20060200) or
128100060224IF  2C                             d00dtc < 20060200
128200060224     C                   MOVEL     'S'           VC(255)
128300060224     C                   Z-ADD     ETCPAM        VP(255)                        * % DA CONSIDER.
128400060224     C                   MOVEL     ETCICA        VI(255)                        *INGLOBA S/N
128500060224     c                   end
128600060224     c******* clienti informatizzati
128700060224     C                   IF        (etcbol = 'A' or
128800060224     C                             etcbol = 'B' or
128900060224     C                             etcdkc = 'S' ) and d00dtc > 20060200
129000060224     C                   MOVEL     'S'           VC(258)
129100060224     C                   Z-ADD     ETCPAM        VP(258)                        * % DA CONSIDER.
129200060224     C                   MOVEL     ETCICA        VI(258)                        *INGLOBA S/N
129300060224     c                   end
129400970207     C                   MOVEL     'S'           VC(254)                        *CONTRASS.MITTENTE
129500970207     C                   Z-ADD     ETCPAM        VP(254)                        * % DA CONSIDER.
129600970207     C                   MOVEL     ETCICA        VI(254)                        *INGLOBA S/N
129700970207     C                   ENDIF
129800060224     c**** bartolini
129900970226     C     ETCPAB        IFGT      *ZEROS                                       *BARTOLINI
130000060224     c******* non clienti informatizzati
130100060224     C                   IF        (etcbol <> 'A' and
130200060224     C                             etcbol <> 'B' and
130300060224     C                             etcdkc <> 'S' and d00dtc > 20060200) or
130400060224IF  2C                             d00dtc < 20060200
130500060224     C                   MOVEL     'S'           VC(250)
130600060224     C                   Z-ADD     ETCPAB        VP(250)                        * % DA CONSIDER.
130700060224     C                   MOVEL     ETCICA        VI(250)                        *INGLOBA S/N
130800060224     c                   end
130900970207     C                   MOVEL     'S'           VC(253)                        *CONTRASS.SEDE
131000970207     C                   Z-ADD     ETCPAB        VP(253)                        * % DA CONSIDER.
131100970207     C                   MOVEL     ETCICA        VI(253)                        *INGLOBA S/N
131200970207     C                   ENDIF
131300970207     C                   ENDIF
131400970207     C*
131500970207     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
131600011030     C                   Z-ADD     *ZEROS        N63               6 3
131700011030     C     ETCPFD        IFGT      *ZEROS
131800011030     c     ETCCIS        ORGT      *ZEROS
131900011030     C     ETCPFD        ADD       ETCCIS        N63
132000011030     C     N63           IFGT      100
132100011030     C                   Z-ADD     100           N63
132200011030     C                   ENDIF
132300011030     C                   MOVEL     'S'           VC(532)                        *FERMO DEPOSITO
132400011030     C                   Z-ADD     N63           VP(532)                        * % DA CONSIDER.
132500011030     C                   ENDIF
132600021028     C*
132700021028     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
132800021028     C     ETCPFD        IFGT      *ZEROS
132900021028     C                   MOVEL     'S'           VC(184)                        *FERMO DEPOSITO
133000021028     C                   Z-ADD     ETCPFD        VP(184)                        * % DA CONSIDER.
133100021028E   2C                   ENDIF
133200970207     C*
133300011009     C* CONTROLLO SE ESISTE =======> MERCE A MAGAZZINO
133400011030     C                   Z-ADD     *ZEROS        N63
133500011030     C     ETCPCM        IFGT      *ZEROS
133600011030     C     ETCSAM        ORGT      *ZEROS
133700011030     C     ETCPCM        ADD       ETCSAM        N63
133800011030     C     N63           IFGT      100
133900011030     C                   Z-ADD     100           N63
134000011030     C                   ENDIF
134100011030     C                   MOVEL     'S'           VC(521)                        *CONS. A MAGAZZINO N
134200011030     C                   Z-ADD     N63           VP(521)                        * % DA CONSIDER.
134300011030     C                   ENDIF
134400970207     C*
134500010801     C* CONTROLLO SE ESISTE =======> COMPETENZA SEDE -CLIENTE CODIFICATO-
134600010801     C*     SE NON E' CLIENTE 8888 O 9999
134700010801     C                   MOVE      D00KSC        KSCIS             4 0
134800010801     C     KSCIS         IFNE      8888
134900010801     C     KSCIS         ANDNE     9999
135000980910     C     ETCESP        IFGT      *ZEROS
135100010109     C                   MOVEL     'S'           VC(533)                        *ESPRESSO NEW
135200010109     C                   Z-ADD     ETCESP        VP(533)                        * % DA CONSIDER.
135300010109     C                   MOVEL     ETCIES        VI(533)                        *INGLOBA S/N
135400001206     C                   ENDIF
135500010801     C                   ENDIF
135600970429     C*
135700970429     C* CONTROLLO SE ESISTE =======> DDT (DOCUM.TRASP.DA CONS. AL DEST.)
135800970429     C     ETCPCD        IFGT      *ZEROS
135900970429     C                   MOVEL     'S'           VC(72)                         *UFF.PART DDT
136000970429     C                   Z-ADD     ETCPCD        VP(72)                         * % DA CONSIDER.
136100970429     C                   MOVEL     ETCIDD        VI(72)                         *INGLOBA S/N
136200970429     C                   MOVEL     'S'           VC(182)                        *UFF.ARR. DDT
136300970429     C                   Z-ADD     ETCPCD        VP(182)                        * % DA CONSIDER.
136400970429     C                   MOVEL     ETCIDD        VI(182)                        *INGLOBA S/N
136500970429     C                   ENDIF
136600970729     C*
136700970729     C* CONTROLLO SE ESISTE =======> LOCALITA' DISAGIATE
136800970729     C     ETCPCL        IFGT      *ZEROS
136900970729     C                   MOVEL     'S'           VC(246)                        *LOC.DISAG.
137000970729     C                   Z-ADD     ETCPCL        VP(246)                        * % DA CONSIDER.
137100970729     C                   MOVEL     ETCILO        VI(246)                        *INGLOBA S/N
137200970729     C                   ENDIF
137300980202     C*
137400020912     C* CONTROLLO SE ESISTE =======>
137500980710     C                   MOVEL     'S'           VC(270)                        *SEDE COMM.LE   BART
137600000721     C                   MOVEL     'S'           VC(592)                        *MAGG.FEDERAL EXPRES
137700000721     C                   MOVEL     'S'           VC(274)                        *ASSIST.CLIENTI FEDE
137800010109     C                   MOVEL     'S'           VC(523)                        *CCM OLIWEB NEW
137900010109     C                   MOVEL     'S'           VC(594)                        *MAGG.FEDEX NEW
138000021031     C                   MOVEL     'S'           VC(595)                        *MAGG.FEDEX NEW 89
138100980710     C*
138200980710     C* CONTROLLO SE ESISTE =======> TRAINI LINEA
138300001206     C                   MOVEL     'S'           VC(212)                        *TRAINI LINEA
138400981023     C*
138500981023     C* CONTROLLO SE ESISTE =======> CARRIAGGIO RITIRO BARTOLINI/SDI
138600001206     C                   MOVEL     'S'           VC(23)                         *CARRIAGGIO RITIRO N
138700990422     C*
138800990422     C* CONTROLLO SE ESISTE =======> PREPAGATI
138900990422     C*    SE CLIENTE 8888
139000990422     C                   MOVE      D00KSC        KSCIS             4 0
139100990422     C     KSCIS         IFEQ      8888
139200990422     C                   MOVEL     'S'           VC(266)                        *PREPAGATI
139300990422     C                   ENDIF
139400980710     C*
139500980710     C*
139600980710     C* SOLO SE GIA' ENTRATI NEL LOOP DEGLI SCAGLIONI -----------------------------
1397009807101    C     S             IFGT      *ZEROS
139800980710     C     SPSC(S)       ANDGT     *ZEROS
139900000228     C*
140000000228     C* CONTROLLO SE ESISTE =======> GIACENZA
140100000228     C     ETCPGI        IFGT      *ZEROS
140200000228     C     SPSC(S)       IFLE      100                                          *PESO RIFERIMENTO <1
140300000228     C                   MOVEL     'S'           VC(267)                        *GIACENZA £ X GG
140400000228     C                   Z-ADD     ETCPGI        VP(267)                        * % DA CONSIDER.
140500000228     C                   MOVEL     ETCIGI        VI(267)                        *INGLOBA S/N
140600000228     C                   ELSE
140700000228     C                   MOVEL     'S'           VC(265)                        *GIACENZA £ X KG/GG
140800000228     C                   Z-ADD     ETCPGI        VP(265)                        * % DA CONSIDER.
140900000228     C                   MOVEL     ETCIGI        VI(265)                        *INGLOBA S/N
141000000228     C                   ENDIF
141100000228     C                   ENDIF
141200980710     C*
141300980710     C                   ENDIF                                                  *SOLO SEQ. 2 -------
141400980202     C*
141500970207     C*
141600980710     C* SOLO SE GIA' ENTRATI NEL LOOP DEI COD.TASSAZ. -----------------------------
141700990422     C* ATT.NE !! SE SI INSERISCE UNA VOCE IN QUESTO PARAGRAFO, FORZARE SEQ=3 NELLA
141800990422     C* SUBROUTINE IMPSEQ
1419009712151    C     T             IFGT      *ZEROS
142000971215     C     TCTS(T)       ANDNE     *BLANKS
142100111003     C                   Z-ADD     TLNA(T)       F
142200111003      * ADEGUAMENTO "MOMENTANEO" TARIFFE EEX
142300111003      * partenza euroexpress, FRANCO
142400111003     c     NTW(F)        IFEQ      'EEX'
142500111003     C     etcfra        andeq     100
142600111003     C     D00DTC        andgt     20110900
142700111003     C                   MOVEL     'S'           VC(549)
142800111003     C                   ENDIF
142900081031      * arrivo euroexpress, franco, loc.disagiate
143000081031      * la 341 non è stata attivata perchè sono i resi e non i
143100081031      * ritiri, inoltre la % loc. disagiate nella creazione dati
143200081031      * tipo è solo per le consegne
143300081031     c     NTW(F)        IFEQ      'EEX'
143400081031     C     ETCFRA        andeq     100
143500081031     C     ETCPCL        ANDGT     0
143600081031     C     D00DTC        andgt     20081026
143700081031     C                   MOVEL     'S'           VC(340)
143800081031     C                   ENDIF
143900970207     C*
144000970207     C* CONTROLLO SE ESISTE =======> ESTERO - EXPORT
144100970207     C*    LINEA ARRIVO   > 300
144200971211     C                   Z-ADD     TLNA(T)       F
144300971211     C     FL1(F)        IFEQ      'E'
144400000217     C*
144500000217     C     DPD(F)        IFEQ      'S'                                          *LINEA ARRIVO DPD
144600000217     C                   MOVEL     'S'           VC(74)                         *UFF.PART.EXPORT DPD
144700010801     C                   MOVEL     'S'           VC(276)                        *ASSIST.IMP/EXP DPD
144800040609     c* attivo la 305 FINANZIAM.TARIFFE IMPORT DPD solo se franco e lna=DPD
144900040609     c* no nel caso di assegnati import
145000040611     c                   if        etcfra = 100 and
145100040611     C                             d00dtc > 20040700                            *FINO al 2002
145200040609     C                   MOVEL     'S'           VC(305)                        *MAGG.FEDEX NEW 89
145300040609     c                   end
145400040611     c* attivo la 288 COMP.INOLTRI ISOLE ESTERE DPD                    =DPD
145500040610     c                   if        etccis > 0
145600040610     C                   MOVEL     'S'           VC(288)                        *MAGG.FEDEX NEW 89
145700040610     C                   Z-ADD     etccis        VP(288)                        * % DA CONSIDER.
145800040610     c                   end
145900000217     C                   ELSE
146000000217     C                   MOVEL     'S'           VC(71)                         *UFF.PARTENZA EXPORT
146100020503     C     NTW(F)        IFNE      'FED'
146200010801     C                   MOVEL     'S'           VC(275)                        *ASSISTENZA IMP/EXP
146300020503     C                   MOVEL     'S'           VC(555)
146400030527     C                   MOVEL     'S'           VC(556)
146500020503     C                   ENDIF
146600010109     C                   ENDIF
146700010109     C                   MOVEL     'S'           VC(543)                        *AZZ.COMP.ARR.ITA NE
146800000217     C     DPD(F)        IFEQ      'S'                                          *LINEA ARRIVO DPD
146900000217     C                   MOVEL     'S'           VC(175)                        *ARR.EXP.DPD (COLLO)
147000000217     C                   ELSE                                                   *LINEA ARR. NON DPD
147100020503     C     NTW(F)        IFNE      'FED'
147200090122     C     D00DTC        IFGT      20090000                                     *FINO al 2002
147300090122     C                   MOVEL     'S'           VC(332)                        *ARR.EXPORT (X SPED.
147400090122     C                   END
147500091019     C     D00DTC        IFGT      20030000                                     *FINO al 2002
147600050524     c                   movel     tcts(t)       wdcts             1
147700030429     C     SCSC(S)       IFEQ      1
147800030429     C     SPSC(S)       ANDLE     30
147900050524     C*    TLNA(T)       ANDEQ     310
148000070523     C     wdcts         ANDEQ     'D'
148100030429     C     ETCCAS        ANDEQ     *ZEROS
148200070523     C     SCSC(S)       oreq      1
148300070523     C     SPSC(S)       ANDLE     30
148400070523     C     tcts(t)       ANDEQ     'W4'
148500070523     C     ETCCAS        ANDEQ     *ZEROS
148600030429     C                   MOVEL     'S'           VC(166)
148700040611     c                   if        etccis > 0  and
148800040611     C                             d00dtc > 20040100                            *FINO al 2002
148900040610     c* non memorizzo la schiera se ingloba al porto VI(170) xchè nella
149000040610     c* voce c'é il raggruppamento = 001 bisognerebbe forzare un rag.
149100040610     c* delta fittizio se chiesto di non inglobarlo al porto
149200040610     C                   MOVEL     'S'           VC(170)
149300040610     C                   Z-ADD     etccis        VP(170)                        * % DA CONSIDER.
149400040610     c                   end
149500030429     C                   ELSE
149600030429     C                   MOVEL     'S'           VC(171)
149700030429     C                   MOVEL     'S'           VC(172)
149800130529IF  5C                   IF        d00dtc > 20130600
149900130529     C                   MOVEL     'S'           VC(173)
150000130529     c                   end
150100130529IF  5C                   IF        d00dtc > 20051106
150200130529     C                   MOVEL     'S'           VC(323)
150300051107     C                   MOVEL     'S'           VC(327)
150400051107     C                   MOVEL     'S'           VC(328)
150500051107     c                   end
150600070522IF  5C                   IF        d00dtc > 20070100
150700070522     C                   MOVEL     'S'           VC(331)
150800070522     C                   MOVEL     'S'           VC(552)
150900070522     c                   end
151000040610     c* 547 MAGG.AUTOSTRADA AUSTRIA ARRIVO
151100040611     C                   if        d00dtc > 20040100                            *FINO al 2002
151200040610     C                   MOVEL     'S'           VC(547)
151300040610     c* 545 MAGGIORAZ.CONSEGNE ISOLE GERM E CONSEGNA ISOLE
151400040610     c     etccis        ifne      0
151500040610     C                   MOVEL     'S'           VC(545)
151600040610     C                   Z-ADD     etccis        VP(545)                        * % DA CONSIDER.
151700040610     c                   end
151800040611     c                   end
151900030429     C                   ENDIF
152000030428     c                   ENDIF
152100020503     C                   ENDIF
152200000217     C                   ENDIF
152300970207     C                   ENDIF
152400970207     C*
152500970207     C* CONTROLLO SE ESISTE =======> DEFLUENZA
152600970207     C*    LINEA ARRIVO DIVERSA DAL TERMINAL ARRIVO
152700971211     C                   Z-ADD     TLNA(T)       F
152800970207     C     TLNA(T)       IFNE      TTFA(T)
152900971211     C     FL1(F)        IFEQ      'E'                                          *ESTERO
153000970207     C                   MOVEL     'S'           VC(114)                        *SCARICO DEFL.EXPORT
153100970207     C                   MOVEL     'S'           VC(124)                        *SMISTAM.DEFL.EXPORT
153200970207     C                   MOVEL     'S'           VC(152)                        *CARICO DEFL.EXPORT
153300000217     C     DPD(F)        IFEQ      'S'                                          *LINEA ARRIVO DPD
153400000217     C                   MOVEL     'S'           VC(233)                        *TRAINI DEFL.EST.DPD
153500000217     C                   ELSE                                                   *LINEA ARRIVO NON DP
153600001206     C                   MOVEL     'S'           VC(231)                        *TRAINI DEFL.EST.
153700000217     C                   ENDIF
153800970207     C                   ELSE                                                   *ITALIA
153900970207     C                   MOVEL     'S'           VC(230)                        *TRAINI DEFLUENZA
154000970207     C                   MOVEL     'S'           VC(111)                        *SCARICO DEFLUENZA
154100970207     C                   MOVEL     'S'           VC(121)                        *SMISTAM.DEFLUENZA
154200970207     C                   MOVEL     'S'           VC(151)                        *CARICO DEFLUENZA
154300110905IF  0C     d00dtc        ifge      20110829
154400110905     C                   MOVEL     'S'           VC(548)
154500110905     c                   end
154600090909IF  0C     d00dtc        ifgt      20090900
154700090909     C                   MOVEL     'S'           VC(113)
154800090909     C                   MOVEL     'S'           VC(125)
154900090909     C                   MOVEL     'S'           VC(154)
155000090909     c                   end
155100050926     c* merce di valore
155200050926IF  1C     etcval        IFgt      0
155300050926IF  0C     d00dtc        ANDGT     20051000
155400050926     C                   MOVEL     'S'           VC(22)
155500050926     c                   Z-ADD     etcval        VP(22)                         * % DA CONSIDER.
155600050926     c                   end
155700970207     C                   ENDIF
155800970207     C                   ENDIF
155900970207     C*
156000970207     C* CONTROLLO SE ESISTE =======> LOCALI
156100980710     C*    LINEA DI PART. = LINEA DI ARR. = TERM.PART. = TERM.ARR.
156200970207     C     D00LNP        IFEQ      TLNA(T)
156300980710     C     D00LNP        ANDEQ     TTFA(T)
156400980710     C     D00LNP        ANDEQ     WTFP
156500970207     C                   MOVEL     'S'           VC(500)                        *LOCALI 1
156600970207     C                   ENDIF
156700980710     C*
156800980710     C* CONTROLLO SE ESISTE =======> AFFLUENZA/DEFLUENZA SU STESSO TERMINAL
156900980710     C*    SE TERM.PART. = TERM.ARR. E LINEA ARR. <> LINEA PART.
157000980710     C     WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
157100980710     C     D00LNP        ANDNE     WTFP                                         *SEC.LIVELLO PART.
157200980710     C     TLNA(T)       ANDNE     TTFA(T)                                      *SEC.LIVELLO ARR.
157300980710     C                   MOVEL     'S'           VC(560)                        *AZZ.DOPPIA AFL/DEFL
157400980710     C                   ENDIF
157500980710     C*
157600980710     C* CONTROLLO SE ESISTE =======> LINEA PART. = TERM. EXPORT
157700980710     C*    SE TERM.PART. = TERM.ARR. = LINEA PART. E EXPORT
157800980710     C                   Z-ADD     TLNA(T)       F
157900980710     C     WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
158000980710     C     D00LNP        ANDEQ     WTFP                                         *PART. DA TERMINAL
158100980710     C     FL1(F)        ANDEQ     'E'                                          *EXPORT
158200980710     C                   MOVEL     'S'           VC(561)                        *AZZ.SCARICO AFFL.
158300980710     C                   ENDIF
158400980826     C*
158500980826     C* CONTROLLO SE ESISTE =======> LINEA PART. E' 2°LIV. TERM. EXPORT
158600980826     C*    SE TERM.PART.=TERM.ARR. E LINEA PART.<>TERM.PART. E EXPORT
158700980826     C                   Z-ADD     TLNA(T)       F
158800980826     C     WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
158900980826     C     D00LNP        ANDNE     WTFP                                         *2°LIV. DEL T.P.
159000980826     C     FL1(F)        ANDEQ     'E'                                          *EXPORT
159100980826     C                   MOVEL     'S'           VC(570)                        *AZZ.COMP.MAG.EXP.
159200980826     C                   ENDIF
159300040611     C* SOLO SE franco, EEX, e giacenza -------------------------------------------
159400040611     C                   Z-ADD     TLNA(T)       F
159500040611     C     ntw(F)        IFEQ      'EEX'
159600040611     C     etcfra        andeq     100
159700040611     C     etcpgi        andgt     0
159800040611     C                   MOVEL     'S'           VC(590)                        *CONS. A MAGAZZINO N
159900040611     C                   Z-ADD     etcpgi        VP(590)                        * % DA CONSIDER.
160000070522     C                   IF        d00dtc > 20070100
160100070522     C                   MOVEL     'S'           VC(268)
160200070523     C                   Z-ADD     etcpgi        VP(268)                        * % DA CONSIDER.
160300070523     C                   MOVEL     ETCIGI        VI(268)                        *INGLOBA S/N
160400070522     C                   MOVEL     'S'           VC(553)
160500070522     C                   Z-ADD     etcpgi        VP(553)                        * % DA CONSIDER.
160600070522     C                   ENDIF
160700040611     C                   ENDIF
160800980826     C*
160900980826     C* CONTROLLO SE ESISTE =======> LINEA ARR. E' 2°LIV. TERM. IMPORT
161000980826     C*    SE TERM.PART.=TERM.ARR. E LINEA ARR.<>TERM.ARR. E IMPORT
161100040608     c* asterisco l'attivazione della mag. 571 xchè la lnp non può essere
161200040608     c* estero
161300040608     C*                  Z-ADD     D00LNP        F
161400040608     C*    WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
161500040608     C*    TLNA(T)       ANDNE     TTFA(T)                                      *2°LIV. DEL T.A.
161600040608     C*    FL1(F)        ANDEQ     'E'                                          *IMPORT
161700040608     C*                  MOVEL     'S'           VC(571)                        *AZZ.COMP.MAG.IMP.
161800040608     C*                  ENDIF
161900981023     C*
162000981023     C* CONTROLLO SE ESISTE =======> CARRIAGGIO/STOP CONSEGNA BARTOLINI/SDI
162100010112     C                   MOVEL     'S'           VC(161)                        *CARRIAG.CONS.NEW
162200010112     C                   MOVEL     'S'           VC(177)                        *STOP DI CONS.NEW
162300011009     C*
162400011009     C* CONTROLLO SE ESISTE =======> CONSEGNA ISOLA
162500011030     C     ETCCIS        IFGT      *ZEROS
162600131120     C*    ETCPFD        ANDEQ     *ZEROS
162700020104     C                   Z-ADD     TLNA(T)       F
162800020104     C     FL1(F)        IFEQ      'E'                                          *LINEA ARRIVO ESTERO
162900020104     C                   ELSE                                                   *LINEA ARRITO ITALIA
163000011030     C                   MOVEL     'S'           VC(278)                        *CONSEGNA ISOLA
163100011030     C                   Z-ADD     ETCCIS        VP(278)                        * % DA CONSIDERARE
163200011030     C                   MOVEL     ETCISO        VI(278)                        *INGLOBA S/N
163300020104     C                   ENDIF
163400020104     C                   ENDIF
163500011030     C*
163600011009     C* CONTROLLO SE ESISTE =======> RITIRO ISOLA
163700011030     C     ETCSAM        IFGT      *ZEROS
163800131120     C*    ETCPFD        ANDEQ     *ZEROS
163900011030     C                   MOVEL     'S'           VC(279)                        *RITIRO ISOLA
164000011030     C                   Z-ADD     ETCSAM        VP(279)                        * % DA CONSIDERARE
164100011030     C                   MOVEL     ETCINR        VI(279)                        *INGLOBA S/N
164200040621     C                   MOVEL     'S'           VC(285)                        *RITIRO ISOLA
164300040621     C                   Z-ADD     ETCSAM        VP(285)                        * % DA CONSIDERARE
164400040621     C                   MOVEL     ETCINR        VI(285)                        *INGLOBA S/N
164500011030     C                   ENDIF
164600000228     C*
164700000228     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
164800000228     C     ETCCAS        IFGT      *ZEROS
164900000228     C                   Z-ADD     TLNA(T)       F
165000000228     C     FL1(F)        IFEQ      'E'                                          *ESTERO
165100000228     C                   MOVEL     'S'           VC(256)                        *C/ASS.ARR.EXP (A VA
165200000228     C     ETCPAB        ADD       ETCPAM        VP(256)                        * % DA CONSIDER.
165300000228     C                   MOVEL     ETCICA        VI(256)                        *INGLOBA S/N
165400000228     C                   MOVEL     'S'           VC(257)                        *C/ASS.ARR.EXP.(A %)
165500000228     C     ETCPAB        ADD       ETCPAM        VP(257)                        * % DA CONSIDER.
165600000228     C                   MOVEL     ETCICA        VI(257)                        *INGLOBA S/N
165700000228     C                   MOVEL     'S'           VC(280)                        *C/ASS ASSIST.EXP.
165800000228     C     ETCPAB        ADD       ETCPAM        VP(280)                        * % DA CONSIDER.
165900000228     C                   MOVEL     ETCICA        VI(280)                        *INGLOBA S/N
166000000228     C                   ELSE                                                   *ITALIA
166100000228     C                   MOVEL     'S'           VC(251)                        *CONTRASS.UFF.ARR.
166200000228     C     ETCPAB        ADD       ETCPAM        VP(251)                        * % DA CONSIDER.
166300000228     C                   MOVEL     ETCICA        VI(251)                        *INGLOBA S/N
166400000228     C                   MOVEL     'S'           VC(252)                        *CONTRASS.PADRON.ARR
166500000228     C     ETCPAB        ADD       ETCPAM        VP(252)                        * % DA CONSIDER.
166600000228     C                   MOVEL     ETCICA        VI(252)                        *INGLOBA S/N
166700000228     C                   ENDIF
166800000228     C                   ENDIF
166900000302     C*
167000000302     C* CONTROLLO SE ESISTE =======> COMPETENZA SEDE
167100000303     C*    SE LINEA PARTENZA E LINEA ARRIVO <> DPD
167200010801     C                   Z-ADD     D00LNP        F
167300010801     C                   Z-ADD     TLNA(T)       F1                4 0
167400010801IF  2C     DPD(F)        IFNE      'S'                                          *L.PARTENZA NON DPD
167500010801     C     DPD(F1)       ANDNE     'S'                                          *L.ARRIVO   NON DPD
167600010801     C*    SE CLIENTE 8888 O 9999
167700010801     C                   MOVE      D00KSC        KSCIS             4 0
167800010801IF  3C     KSCIS         IFEQ      8888
167900010801     C     KSCIS         OREQ      9999
168000100422     C                   MOVEL     'S'           VC(822)                        *RICARICO SEDE 88/99
168100010801E   3C                   ENDIF
168200010801     C*    SE NON E' CLIENTE 8888 O 9999
168300010801IF  3C     KSCIS         IFNE      8888
168400010801     C     KSCIS         ANDNE     9999
168500000303IF  4C     D00TSP        IFEQ      'D'
168600000303IF  5C     ETCCLV        IFEQ      'A'                                          *CLASSE
168700100422     C                   MOVEL     'S'           VC(816)                        *SEDE-DIST-CL.A
168800000303E   5C                   ENDIF
168900000303IF  5C     ETCCLV        IFEQ      'B'                                          *CLASSE
169000100422     C                   MOVEL     'S'           VC(817)                        *SEDE-DIST-CL.B
169100000303E   5C                   ENDIF
169200000303IF  5C     ETCCLV        IFEQ      'C'
169300100422     C                   MOVEL     'S'           VC(818)                        *SEDE-DIST-CL.C
169400000303E   5C                   ENDIF
169500000303IF  5C     ETCCLV        IFEQ      'T'
169600020213     C     ETCCLV        OREQ      'D'
169700100422     C                   MOVEL     'S'           VC(821)                        *SEDE-DIST-CL.T
169800000303E   5C                   ENDIF
169900000303E   4C                   ENDIF
170000000303     C*
170100000303IF  4C     D00TSP        IFEQ      'C'                                          *CORRIERE(NORMALE)
170200000303IF  5C     ETCCLV        IFEQ      'A'                                          *CLASSE
170300100422     C                   MOVEL     'S'           VC(810)                        *SEDE-CORR-CL.A
170400000303E   5C                   ENDIF
170500000303IF  5C     ETCCLV        IFEQ      'B'
170600100422     C                   MOVEL     'S'           VC(811)                        *SEDE-CORR-CL.B
170700000303E   5C                   ENDIF
170800000303IF  5C     ETCCLV        IFEQ      'C'
170900100422     C                   MOVEL     'S'           VC(812)                        *SEDE-CORR-CL.C
171000000303E   5C                   ENDIF
171100000303IF  5C     ETCCLV        IFEQ      'T'
171200020213     C     ETCCLV        OREQ      'D'
171300100422     C                   MOVEL     'S'           VC(819)                        *SEDE-CORR-CL.T
171400000303E   5C                   ENDIF
171500000303E   4C                   ENDIF
171600000303     C*
171700000303IF  4C     D00TSP        IFEQ      'E'                                          *ESPRESSO
171800000303IF  5C     ETCCLV        IFEQ      'A'                                          *CLASSE
171900100422     C                   MOVEL     'S'           VC(813)                        *SEDE-ESPR-CL.A
172000000303E   5C                   ENDIF
172100000303IF  5C     ETCCLV        IFEQ      'B'
172200100422     C                   MOVEL     'S'           VC(814)                        *SEDE-ESPR-CL.B
172300000303E   5C                   ENDIF
172400000303IF  5C     ETCCLV        IFEQ      'C'
172500100422     C                   MOVEL     'S'           VC(815)                        *SEDE-ESPR-CL.C
172600000303E   5C                   ENDIF
172700000303IF  5C     ETCCLV        IFEQ      'T'
172800020213     C     ETCCLV        OREQ      'D'
172900100422     C                   MOVEL     'S'           VC(820)                        *SEDE-ESPR-CL.T
173000000303E   5C                   ENDIF
173100000303E   4C                   ENDIF
173200000303E   3C                   ENDIF
173300000303E   2C                   ENDIF
173400000303     C* CONTROLLO SE ESISTE =======> RICARICO SEDE EXPORT DPD
173500040609     C*    FRANCHI EXPORT ( ASSEGNATI IMPORT non può essere xchè la lnp = I)
173600000303     C                   Z-ADD     TLNA(T)       F
173700000303IF  2C     DPD(F)        IFEQ      'S'                                          *L.ARRIVO DPD
173800040609     c* non è d'applicare se la % franco = 0 e %assegnato = 100
173900040609     c     etcfra        andeq     100
174000100422     C                   MOVEL     'S'           VC(823)                        *RIC.SEDE-EXPORT DPD
174100000303E   2C                   ENDIF
174200000228     C*
1743009807101    C                   ENDIF                                                  *SOLO SEQ. 3 -------
174400961128     C*
174500961128     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
174600970203     C*    SE UNA VOCE E' DA CALCOLARE MA NON E' STATA CARICATA PERCHE' NON
174700970203     C*    VALIDA ALLA DATA, LA DISATTIVO
1748009702111    C                   DO        NVOCA         N
174900970207     C                   Z-ADD     VOCA(N)       V
1750009611282    C     VC(V)         IFEQ      'S'                                          *VOCE DA CALCOLARE
1751009702033    C     VDDV(V)       IFEQ      *ZEROS                                       *DECORRENZA
175200970203     C                   MOVEL     'N'           VC(V)                          *DISATTIVO
1753009611283    C                   ENDIF
1754009611282    C                   ENDIF
1755009611281    C                   ENDDO
175600961128     C*
175700961128     C                   ENDSR
175800961128     C*------------------------------------------------------------------------*
175900961128     C* VALVOC - VALORIZZA LE VOCI VALIDE
176000961128     C*------------------------------------------------------------------------*
176100961128     C     VALVOC        BEGSR
176200970131     C*
176300970203     C* SEGUENTI VOCI DA NON CALCOLARE PERCHE' UTILIZZATE NEL FORMULONE
176400970203     C                   MOVEL     'N'           VC(270)
176500040609     C                   MOVEL     'N'           VC(305)
176600970203     C                   MOVEL     'N'           VC(300)
176700970203     C                   MOVEL     'N'           VC(900)
176800100422     C                   MOVEL     'N'           VC(816)
176900100422     C                   MOVEL     'N'           VC(817)
177000100422     C                   MOVEL     'N'           VC(818)
177100100422     C                   MOVEL     'N'           VC(821)
177200100422     C                   MOVEL     'N'           VC(810)
177300100422     C                   MOVEL     'N'           VC(811)
177400100422     C                   MOVEL     'N'           VC(812)
177500100422     C                   MOVEL     'N'           VC(819)
177600100422     C                   MOVEL     'N'           VC(813)
177700100422     C                   MOVEL     'N'           VC(814)
177800100422     C                   MOVEL     'N'           VC(815)
177900100422     C                   MOVEL     'N'           VC(820)
178000961128     C*
178100961128     C* CICLO PER TUTTE LE VOCI 'DA CALCOLARE'
178200970206     C                   Z-ADD     *ZEROS        VIM                            *SCHIERA IMP.DA SCR.
178300970207     C                   DO        NVOCA         N                 3 0
178400970207     C                   Z-ADD     VOCA(N)       V
178500970206     C     VIMP(V)       IFGT      *ZEROS
178600970207     C     VS(V)         IFGE      WSEQ
178700970206     C                   Z-ADD     *ZEROS        VIMP(V)
178800970206     C                   ELSE
178900970206     C                   Z-ADD     VIMP(V)       VIM(V)
179000970206     C                   ENDIF
179100970206     C                   ENDIF
179200970207     C     VS(V)         IFEQ      WSEQ
179300961128     C     VC(V)         IFEQ      'S'                                          *VOCE DA CALC.
179400961128     C                   Z-ADD     V             WVOC
179500961128     C                   Z-ADD     VPRG(V)       WPRG
179600000306     C                   MOVEL     VTIV(V)       WTIV
179700961128     C* CERCA TARIFFA
179800011017     C                   EXSR      CERTAR
179900961128     C*
180000961128     C* SE NON C'E' ERRORE (ES. MANCA TARIFFA)
180100961128     C* E IL MANCA TARIFFA ERA DA SEGNALARE
180200961128     C     $ERR          IFEQ      'N'
180300961128     C     WMSGMT        ANDEQ     'S'                                          *MSG  MANCA TARIFFA
180400961128     C*
180500961128     C* SE TIPO VALORIZZAZIONE = Q=QUANTITA  OPPURE P=PERCENTUALE
180600961128     C* RECUPERO LA QTA O IL VALORE DI RIFERIMENTO
180700011025     C                   Z-ADD     *ZEROS        WQTA             23 8
180800961128     C     VVRV(V)       IFNE      *BLANKS
180900961128     C                   Z-ADD     1             I
181000961128     C     VVRV(V)       LOOKUP    C2R(I)                                 98
181100961128     C     *IN98         IFEQ      '0'
181200961128     C                   MOVEL     MSG(19)       $MSG                           *TIPO TARIF. NON TRO
181300961128     C                   EXSR      SCRERR
181400961128     C                   ELSE
181500961128     C*
181600961128     C* SE E' UNA TARIFFA DOVE E' STATO CALCOLATO IL PESO TASSABILE
181700961128     C* (GIA' CALCOLATO NELLA RICERCA DELLO SCAGLIONE) PRENDO QTA
181800961128     C* UTILIZZATA PER LA RICERCA DELLO SCAGLIONE
181900961128     C* ALTRIMENTI PRENDO QTA DELLA SCHIERA
182000961128     C     VSPT(V)       IFNE      *BLANKS                                      *C'E' SVIL.PESO T
182100961128     C     WQTAPT        ANDGT     *ZEROS                                       *QTA PT > 0
182200961128     C                   Z-ADD     WQTAPT        WQTA                           *QTA PESO TAS.RIC.TA
182300961128     C                   ELSE                                                   *NON C'E' SVIL.PESO
182400961128     C                   Z-ADD     Q2R(I)        WQTA                           *QTA RIFERIM.
182500961128     C                   ENDIF
182600040811     C*
182700040811     C* SE E' UNA TARIFFA A BANCALI IMPOSTO LA QUANTITÀ A SECONDA CHE
182800040811     C* STO CALCOLANDO BANCALI BARTOLINI O BANCALI BNB
182900040811     C     VB(V)         IFEQ      'B'                                          *C'E' SVIL.PESO T
183000040811     C                   Z-ADD     ETCNBA        WQTA                           *QTA PESO TAS.RIC.TA
183100040811     C                   END                                                    *NON C'E' SVIL.PESO
183200040811     C     VB(V)         IFEQ      'O'                                          *C'E' SVIL.PESO T
183300040811     C                   Z-ADD     ETCNBN        WQTA                           *QTA RIFERIM.
183400040811     C                   ENDIF
183500040811     C                   MOVEL     T2R(I)        WTTQ              1            *TIPO TARIFFA QTA
183600961128     C                   ENDIF
183700961128     C                   ENDIF
183800961128     C*
183900961128     C* CONTROLLO L'ARROTONDAMENTO
184000011017     C                   EXSR      ARROTO                                       *ARROTONDA QTA
184100961128     C*
184200011025     C                   Z-ADD     *ZEROS        WVVOCE           23 8          *IMPORTO VOCE
184300961128     C*
184400961128     C* SE TIPO VOCE V=VOCE NORMALE
184500961128     C* IN BASE AL TIPO TARIFFA CALCOLA IMPORTO A QTA, FISSO, PERCENTUALE
184600961128     C     VTVO(V)       IFEQ      'V'                                          *VOCE
184700961128     C                   EXSR      CALVOC                                       *CALCOLA VOCI NORMAL
184800961128     C                   ENDIF
184900961128     C*
185000961128     C* SE TIPO VOCE M=MAGGIORAZIONE
185100961128     C* RECUPERO TUTTE LE VOCI DA MAGGIORARE E APPLICO LA PERCENTUALE
185200970204     C     VTVO(V)       IFEQ      'M'                                          *MAGGIORAZIONE
185300970205     C                   EXSR      CERMAG                                       *CALCOLA MAGGIORAZIO
185400970204     C                   ENDIF
185500961128     C*
185600961128     C                   ENDIF
185700961128     C                   ENDIF
185800970203     C                   ENDIF
185900961128     C                   ENDDO
186000961128     C*
186100961128     C                   ENDSR
186200961128     C*------------------------------------------------------------------------*
186300961128     C* CALVOC - CALCOLA LE VOCI NORMALI
186400961128     C*------------------------------------------------------------------------*
186500961128     C     CALVOC        BEGSR
186600961128     C*
186700961128     C* CALCOLO IN BASE AL TIPO TARIFFA
186800961128     C*
186900961128     C     WTTQ          IFEQ      'Q'                                          *TARIFFA PER QUANTIT
187000961128     C     WQTA          ANDNE     *ZEROS
187100961128     C     EVDITR        ANDNE     *ZEROS
187200961128     C     WQTA          MULT      EVDITR        WVVOCE
187300961128     C                   ENDIF
187400961128     C*
187500961128     C     WTTQ          IFEQ      'F'                                          *TARIF.IMPORTO FISSO
187600961128     C                   Z-ADD     EVDITR        WVVOCE
187700961128     C                   ENDIF
187800961128     C*
187900961128     C     WTTQ          IFEQ      'P'                                          *TARIFFA PER PERCENT
188000961128     C     WQTA          ANDNE     *ZEROS
188100961128     C     EVDPER        ANDNE     *ZEROS
188200961128     C     WQTA          DIV       100           WVVOCE
188300961128     C     WVVOCE        MULT      EVDPER        WVVOCE
188400961128     C                   ENDIF
188500961128     C*
188600961128     C* CONTROLLO IMPORTO MINIMO
188700961128     C     EVDIMI        IFNE      *ZEROS
188800961128     C     WVVOCE        ANDLT     EVDIMI
188900961128     C                   Z-ADD     EVDIMI        WVVOCE
189000961128     C                   ENDIF
189100961128     C     EVDIMA        IFNE      *ZEROS
189200961128     C     WVVOCE        ANDGT     EVDIMA
189300961128     C                   Z-ADD     EVDIMA        WVVOCE
189400961128     C                   ENDIF
189500961128     C*
189600961128     C* CONTROLLO SEGNO E CARICO IMPORTO NELLA SCHIERA
189700961128     C     EVDSEG        IFEQ      '+'                                          *SEGNO VOCE POSITIVO
189800970205     C                   Z-ADD     WVVOCE        VIMP(V)
189900970205     C                   Z-ADD     WVVOCE        VIM(V)
190000961128     C                   ELSE                                                   *SEGNO VOCE NEGATIVO
190100961128     C                   Z-SUB     WVVOCE        VIMP(V)
190200970205     C                   Z-SUB     WVVOCE        VIM(V)
190300961128     C                   ENDIF
190400961128     C*
190500961128     C                   ENDSR
190600961128     C*------------------------------------------------------------------------*
190700961128     C* ARROTO - ARROTONDA LA QUANTITA'
190800961128     C*          INPUT  : WQTA (DA ARROTONDARE)
190900961128     C*          OUTPUT : WQTA (ARROTONDATA)
191000961128     C*------------------------------------------------------------------------*
191100961128     C     ARROTO        BEGSR
191200011017     C*
191300011017     C* SE LA DIVISA DI CALCOLO DEL CE E' ITL => ESEGUO ARROTONDAMENTO
191400021128     C* ERRORACCIO: si parla di qtà anche NON valuta !!!
191500021128     C*----WDIVCE        IFEQ      'ITL'
191600961128     C*
191700961128     C* CONTROLLO SE ESISTE L'ARROTONDAMENTO
191800970227     C     WQTA          IFNE      *ZEROS
191900961128     C     VARL(V)       ANDNE     *ZEROS
192000961128     C     WQTA          IFLE      VARL(V)                                      *VERIFICO LIMITE ARR
192100011025     C                   Z-ADD     VARF(V)       WARR             23 8
192200961128     C                   ELSE
192300961128     C                   Z-ADD     VARO(V)       WARR
192400961128     C                   ENDIF
192500970227     C*
192600961128     C* CONTROLLO SE DATO GIA' ARRORONDATO
192700011025     C     WQTA          DIV       WARR          WINT             18 0          *ARROTONDO
192800011025     C                   MVR                     WT0155           21 8    99     * 99=ON --> RESTO=0
192900961128     C     *IN99         IFEQ      *OFF                                         *NON IMPORTA ARROT.
193000961128     C     WINT          MULT      WARR          WQTA
193100961128     C                   ADD       WARR          WQTA
193200961128     C                   ENDIF
193300970227     C                   ENDIF
193400961128     C*
193500021128     C*---               ENDIF
193600011017     C*
193700961128     C                   ENDSR
193800961128     C*------------------------------------------------------------------------*
193900961128     C* CALPT - CALCOLA IL PESO TASSABILE
194000961128     C*------------------------------------------------------------------------*
194100961128     C     CALPT         BEGSR
194200961128     C*
194300961128     C* SE ESISTE RAPPORTO PESO/VOLUME CALCOLO PESO TASSABILE
194400961128     C     EVDRPV        IFNE      *ZEROS
194500961128     C*
194600961128     C* CALCOLO PESO TASSABILE DAL VOLUME
194700961128     C     SVSC(S)       MULT      EVDRPV        WPTAS
194800961128     C     WPTAS         MULT      100           WPTAS
194900961128     C*
195000961128     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
195100961128     C     VSPT(V)       IFEQ      '1'
195200961128     C                   Z-ADD     WPTAS         WQTA
195300961128     C                   ENDIF
195400961128     C*
195500961128     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
195600961128     C     VSPT(V)       IFEQ      '2'
195700991008     C********** WPTAS     IFGT WQTA
195800991008     C     WPTAS         IFGT      SPSC(S)
195900961128     C                   Z-ADD     WPTAS         WQTA
196000961128     C                   ENDIF
196100961128     C                   ENDIF
196200961128     C*
196300961128     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
196400961128     C     VSPT(V)       IFEQ      '3'
196500991008     C********** WPTAS     IFLT WQTA
196600991008     C     WPTAS         IFLT      SPSC(S)
196700961128     C                   Z-ADD     WPTAS         WQTA
196800961128     C                   ENDIF
196900961128     C                   ENDIF
197000961128     C*
197100961128     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
197200011025     C                   Z-ADD     WQTA          WQTAPT           21 8          *PESO TASS.X CALC.CO
197300961128     C*
197400961128     C                   ENDIF
197500961128     C*
197600961128     C                   ENDSR
197700030429     C*------------------------------------------------------------------------*
197800030429     C*  CALPS  - CALCOLA IL PESO TASSABILE PER RICERCA SCAGLIONE
197900030429     C*------------------------------------------------------------------------*
198000030429     C     CALPS         BEGSR
198100030429     C*
198200030429     C* CALCOLO PESO TASSABILE DAL VOLUME
198300030429     C     SVSC(S)       MULT      VRPV(V)       WPTASR
198400030429     C     WPTASR        MULT      100           WPTASR
198500030429     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
198600030429     C     VSPS(V)       IFEQ      '1'
198700030429     C                   Z-ADD     WPTASR        WQTA
198800030429     C                   ENDIF
198900030429     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
199000030429     C     VSPS(V)       IFEQ      '2'
199100030429     C     WPTASR        IFGT      SPSC(S)
199200030429     C                   Z-ADD     WPTASR        WQTA
199300060504     c                   else
199400060504     C                   Z-ADD     SPSC(S)       WQTA
199500030429     C                   ENDIF
199600030429     C                   ENDIF
199700030429     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
199800030429     C     VSPS(V)       IFEQ      '3'
199900030429     C     WPTASR        IFLT      SPSC(S)
200000030429     C                   Z-ADD     WPTASR        WQTA
200100060504     c                   else
200200060504     C                   Z-ADD     SPSC(S)       WQTA
200300030429     C                   ENDIF
200400030429     C                   ENDIF
200500030429     C*
200600030429     C                   ENDSR
200700961128     C*------------------------------------------------------------------------*
200800961128     C* CERMAG - CERCA LE MAGGIORAZIONI
200900961128     C*------------------------------------------------------------------------*
201000961128     C     CERMAG        BEGSR
201100961128     C*
201200961128     C* SE PERCENTUALE DIVERSA DA ZERO NON CERCO NEMMENO LA MAGGIORAZIONE
201300970205     C     EVDPER        IFNE      *ZEROS                                       --- 1 -->
201400961128     C                   MOVEL     'N'           WMAGOK            1            *MAGG. NON TROVATA
201500961128     C*
201600961128     C* CONTROLLA SE MAGGIORAZIONI IN MEMORIA
201700961128     C                   Z-ADD     1             M                 6 0
201800961128     C     M             DOWLE     WULTMA                                       FINO ULTIMA MAGG.
201900970205     C     WVOC          IFEQ      MVOM(M)                                      *CONTR. CAMPI CHIAVE
202000970205     C     WPRG          ANDEQ     MPRM(M)
202100961128     C                   MOVEL     'S'           WMAGOK                         *MAGG. TROVATA
202200961128     C                   Z-ADD     MVOM(M)       EVMVOM
202300961128     C                   Z-ADD     MPRM(M)       EVMPRM
202400961128     C                   Z-ADD     MVOC(M)       EVMVOC
202500961128     C                   Z-ADD     MPRG(M)       EVMPRG
202600961128     C                   EXSR      CALMAG                                       CALCOLA MAGGIORAZION
202700961128     C                   ENDIF
202800961128     C                   ADD       1             M
202900961128     C                   ENDDO
203000961128     C*
203100961128     C* SE NON TROVATA IN MEMORIA E FINE SCHIERA LEGGO SUL FILE
203200961128     C     WMAGOK        IFEQ      'N'                                          --- 2 -->
203300050104     C     WULTMA        ANDGE     600
203400961128     C*
203500050104     C********           Z-ADD     WVOC          EVMVOC
203600050104     C                   Z-ADD     WVOC          EVMVOM
203700961128     C                   Z-ADD     WPRG          EVMPRM
203800961128     C                   Z-ADD     *ZEROS        EVMVOC
203900961128     C                   Z-ADD     *ZEROS        EVMPRG
204000961128     C     KEYEVM        SETLL     ECEVM01L                           99
204100961128     C  N99              READ      ECEVM01L                               99
204200961128     C     *IN99         IFEQ      '1'
204300961128     C                   MOVEL     MSG(22)       $MSG                           *MAGG. SENZA MAGG.
204400961128     C                   EXSR      SCRERR
204500961128     C                   END
204600961128     C     *IN99         DOWEQ     '0'
204700961128     C     EVMVOM        ANDEQ     WVOC
204800961128     C     EVMPRM        ANDEQ     WPRG
204900961128     C     EVMATB        IFEQ      *BLANKS
205000961128     C                   EXSR      CALMAG                                       *CALCOLA MAGG.
205100961128     C                   ENDIF
205200961128     C                   READ      ECEVM01L                               99
205300961128     C                   ENDDO
205400961128     C*
205500961128     C                   ENDIF                                                  <-- 2 ---
205600961128     C                   ENDIF                                                  <-- 1 ---
205700961128     C*
205800961128     C                   ENDSR
205900961128     C*------------------------------------------------------------------------*
206000961128     C* CALMAG - CALCOLA LA MAGGIORAZIONE
206100961128     C*------------------------------------------------------------------------*
206200961128     C     CALMAG        BEGSR
206300961128     C*
206400970205     C                   Z-ADD     EVMVOC        I
206500961128     C*
206600041105     C                   IF        VC(I) = 'S'
206700970206     C* TROVO LA % DELLA MAGGIORAZIONE (% DA CONSIDERARE DELLA % DELLA MAGG.)
206800970206     C     EVDPER        DIV       100           WPERMA
206900011025     C     WPERMA        MULT      VP(V)         WPERMA           23 8
207000970227     C*
207100970227     C* APPLICA LA PERCENTUALE
207200050121     C     VIM(I)        DIV       100           WVVOCE
207300970206     C     WVVOCE        MULT      WPERMA        WVVOCE
207400961128     C*
207500961128     C* CONTROLLO IMPORTO MINIMO
207600970205     C     EVDIMI        IFNE      *ZEROS
207700970205     C     WVVOCE        ANDLT     EVDIMI
207800970205     C                   Z-ADD     EVDIMI        WVVOCE
207900961128     C                   ENDIF
208000961128     C*
208100961128     C* CONTROLLO IMPORTO MASSIMO
208200970205     C     EVDIMA        IFNE      *ZEROS
208300970205     C     WVVOCE        ANDGT     EVDIMA
208400970205     C                   Z-ADD     EVDIMA        WVVOCE
208500961128     C                   ENDIF
208600961128     C*
208700961128     C* CONTROLLO SEGNO E AGGIUNGO O TOLGO IMPORTO NELLA SCHIERA
208800970205     C     EVDSEG        IFEQ      '+'                                          *SEGNO VOCE POSITIVO
208900050121     C     VIM(I)        ADD       WVVOCE        VIM(I)                         *IMPORTO VOCE
209000050121     C                   ELSE                                                   *SEGNO VOCE NEGATIVO
209100050121     C     VIM(I)        SUB       WVVOCE        VIM(I)
209200961128     C                   ENDIF
209300041105     C                   ENDIF
209400961128     C*
209500961128     C                   ENDSR
209600961128     C*------------------------------------------------------------------------*
209700961128     C* CERTAR - CERCA LA TARIFFA
209800961128     C*------------------------------------------------------------------------*
209900961128     C     CERTAR        BEGSR
210000961128     C*
210100970227     C* IMPOSTA CAMPI LAVORO
210200961128     C                   MOVEL     'N'           WFLTT             1            *TARIFFA TROVATA
210300961128     C                   MOVEL     *BLANKS       WRIF                           *RIF.TARIFFA
210400961128     C                   MOVEL     *BLANKS       WTT1              3            *TERM-TERM, X MSG
210500961128     C                   MOVEL     *BLANKS       WTT2              3            *TERM-TERM, X MSG
210600961128     C                   MOVEL     *BLANKS       WCTSM             2            *COD.TASS., X MSG
210700961128     C                   MOVEL     'S'           WMSGMT            1            *MESSAGG.MANCA TARIF
210800011025     C                   Z-ADD     *ZEROS        WPTAS            21 8          *PESO TASSABILE
210900030429     C                   Z-ADD     *ZEROS        WPTASR           21 8          *P.TASSABILE RICERCA
211000961128     C                   MOVEL     D00FIE        WIE               1
211100030429     C*
211200030429     C* calcola il peso tassabile da ricercare nello scaglione
211300030429     C     VSPS(V)       IFGT      *ZEROS
211400030429     C                   EXSR      CALPS
211500050309     C*????              Z-ADD     WPTASR        WQTA
211600030429     C                   ELSE
211700961128     C*
211800961128     C* CERCA LA QTA DA RICERCARE NELLO SCAGLIONE
211900961128     C                   Z-ADD     *ZEROS        WQTA
212000961128     C                   Z-ADD     1             I
212100961128     C     VTIV(V)       LOOKUP    C2R(I)                                 98
212200961128     C     *IN98         IFEQ      '0'
212300961128     C                   MOVEL     MSG(20)       $MSG                           *QTA X RIC.SC. NON T
212400961128     C                   EXSR      SCRERR
212500961128     C                   ELSE
212600961128     C                   Z-ADD     Q2R(I)        WQTA                           *QTA DA RICERCARE
212700961128     C                   END
212800030429     C*
212900030429     C                   ENDIF
213000961128     C*
213100961128     C* SE RICHIESTO, TENTATIVO PER CLIENTE
213200961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
213300961128     C     WIE           ANDEQ     'I'                                          *ITALIA
213400961128     C     VRKI(V)       ANDNE     *BLANKS                                      *RIF.CLI.ITALIA
213500961128     C                   MOVEL     VRKI(V)       WRIF
213600961128     C                   EXSR      CERCLI                                       *CERCA CLIENTE
213700961128     C                   ENDIF
213800961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
213900961128     C     WIE           ANDEQ     'E'                                          *ESTERO
214000961128     C     VRKE(V)       ANDNE     *BLANKS                                      *RIF.CLI.ESTERO
214100961128     C                   MOVEL     VRKE(V)       WRIF
214200961128     C                   EXSR      CERCLI                                       *CERCA CLIENTE
214300961128     C                   ENDIF
214400961128     C*
214500961128     C* SE RICHIESTO, TENTATIVO PER CODICE TASSAZIONE
214600961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
214700961128     C     WIE           ANDEQ     'I'                                          *ITALIA
214800961128     C     VRTI(V)       ANDNE     *BLANKS                                      *RIF.TAS.ITALIA
214900961128     C                   MOVEL     VRTI(V)       WRIF
215000961128     C                   EXSR      CERTAS                                       *CERCA TASSAZIONE
215100961128     C                   ENDIF
215200961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
215300961128     C     WIE           ANDEQ     'E'                                          *ESTERO
215400961128     C     VRTE(V)       ANDNE     *BLANKS                                      *RIF.TAS.ESTERO
215500961128     C                   MOVEL     VRTE(V)       WRIF
215600961128     C                   EXSR      CERTAS                                       *CERCA TASSAZIONE
215700961128     C                   ENDIF
215800961128     C*
215900961128     C* SE RICHIESTO, TENTATIVO PER CODICE FILIALE
216000961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
216100961128     C     WIE           ANDEQ     'I'                                          *ITALIA
216200961128     C     VRFI(V)       ANDNE     *BLANKS                                      *RIF.FIL.ITALIA
216300961128     C                   MOVEL     VRFI(V)       WRIF
216400961128     C                   EXSR      CERFIL                                       *CERCA FILIALE
216500961128     C                   ENDIF
216600961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
216700961128     C     WIE           ANDEQ     'E'                                          *ESTERO
216800961128     C     VRFE(V)       ANDNE     *BLANKS                                      *RIF.FIL.ESTERO
216900961128     C                   MOVEL     VRFE(V)       WRIF
217000961128     C                   EXSR      CERFIL                                       *CERCA FILIALE
217100961128     C                   ENDIF
217200961128     C*
217300961128     C* SE TARIFFA NON ANCORA TROVATA CERCA TARIFFA DI CARTELLO
217400961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
217500961128     C                   EXSR      CERCAR                                       *CERCA CARTELLO
217600961128     C                   ENDIF
217700961128     C*
217800961128     C* CONTROLLO MANCA TARIFFA
217900961128     C     WFLTT         IFEQ      'N'                                          --- 1 -->
218000961128     C     WMSGMT        IFEQ      'S'                                          *MESSAGG.MANCA TARIF
218100961128     C                   MOVEL     MSG(21)       DSMSG                          *MANCA TARIFFA
218200961128     C                   MOVE      WVOC          DSVOCE                         *COD.VOCE
218300961128     C                   MOVE      WTT1          DSTT1                          *TERM.1
218400961128     C                   MOVE      WTT2          DSTT2                          *TERM.2
218500961128     C                   MOVE      WCTSM         DSCTS                          *COD.TASS.
218600961128     C                   MOVEL     DSMSG         $MSG                           *MANCA TARIFFA
218700961128     C                   EXSR      SCRERR
218800961128     C                   ENDIF
218900961128     C                   ELSE
219000961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
219100961128     C                   END                                                    <-- 1 ---
219200961128     C*
219300961128     C                   ENDSR
219400961128     C*------------------------------------------------------------------------*
219500961128     C* CERCLI - CERCA LA TARIFFA PER CLIENTE
219600961128     C*------------------------------------------------------------------------*
219700961128     C     CERCLI        BEGSR
219800961128     C*
219900961128     C                   MOVEL     'K'           WTTV
220000961128     C                   Z-ADD     *ZEROS        WFT1
220100961128     C                   Z-ADD     *ZEROS        WFT2
220200961128     C                   Z-ADD     *ZEROS        WKSC
220300961128     C                   Z-ADD     *ZEROS        WCTR
220400961128     C                   MOVEL     *BLANKS       WCTS
220500961128     C*
220600021129     C                   Z-ADD     DEPKSC        WKSC
220700961128     C                   Z-ADD     D00CTR        WCTR
220800961128     C*
220900961128     C* LANCIA GESTIONE TARIFFE
221000961128     C                   EXSR      GESEVD
221100961128     C*
221200961128     C* SE TARIFFA BOLLA NON E' STATA TROVATA PROVO CON TARIFFA = 999 (TUTTE)
221300961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
221400021129     C                   Z-ADD     DEPKSC        WKSC
221500961128     C                   Z-ADD     999           WCTR
221600961128     C                   EXSR      GESEVD                                       *GEST. TARIFFE
221700961128     C                   ENDIF
221800961128     C*
221900961128     C                   ENDSR
222000961128     C*------------------------------------------------------------------------*
222100961128     C* CERTAS - CERCA LA TARIFFA PER CODICE TASSAZIONE
222200961128     C*------------------------------------------------------------------------*
222300961128     C     CERTAS        BEGSR
222400961128     C*
222500961128     C                   MOVEL     'T'           WTTV
222600961128     C                   Z-ADD     *ZEROS        WFT1
222700961128     C                   Z-ADD     *ZEROS        WFT2
222800961128     C                   Z-ADD     *ZEROS        WKSC
222900961128     C                   Z-ADD     *ZEROS        WCTR
223000961128     C                   MOVEL     *BLANKS       WCTS
223100961128     C     WRIF          IFEQ      'SP'
223200961128     C                   MOVEL     WCTP          WCTS
223300961128     C                   ENDIF
223400961128     C     WRIF          IFEQ      'SA'
223500961128     C                   MOVEL     TCTS(T)       WCTS
223600961128     C                   ENDIF
223700961128     C*
223800961128     C* LANCIA GESTIONE TARIFFE
223900961128     C                   EXSR      GESEVD
224000961128     C*
224100961128     C* CAMPI PER MESSAGGI
224200961128     C                   MOVE      WCTS          WCTSM
224300961128     C*
224400961128     C                   ENDSR
224500961128     C*------------------------------------------------------------------------*
224600961128     C* CERFIL - CERCA LA TARIFFA PER FILIALE/TERMINAL
224700961128     C*------------------------------------------------------------------------*
224800961128     C     CERFIL        BEGSR
224900961128     C*
225000961128     C                   MOVEL     'F'           WTTV
225100961128     C                   Z-ADD     *ZEROS        WFT1
225200961128     C                   Z-ADD     *ZEROS        WFT2
225300961128     C                   Z-ADD     *ZEROS        WKSC
225400961128     C                   Z-ADD     *ZEROS        WCTR
225500961128     C                   MOVEL     *BLANKS       WCTS
225600140115     C                   MOVEL     *BLANKS       Wnaz
225700140115     C                   MOVEL     *BLANKS       wcad
225800961128     C     WRIF          IFEQ      'LP'
225900961128     C                   Z-ADD     D00LNP        WFT1
226000961128     C                   ENDIF
226100961128     C     WRIF          IFEQ      'LT'
226200961128     C                   Z-ADD     D00LNP        WFT1
226300961128     C                   Z-ADD     WTFP          WFT2
226400961128     C                   ENDIF
226500961128     C     WRIF          IFEQ      'TP'
226600961128     C                   Z-ADD     WTFP          WFT1
226700961128     C                   ENDIF
226800961128     C     WRIF          IFEQ      'TT'
226900961128     C                   Z-ADD     WTFP          WFT1
227000961128     C                   Z-ADD     TTFA(T)       WFT2
227100961128     C                   ENDIF
227200961128     C     WRIF          IFEQ      'TA'
227300961128     C                   Z-ADD     TTFA(T)       WFT1
227400961128     C                   ENDIF
227500961128     C     WRIF          IFEQ      'TL'
227600961128     C                   Z-ADD     TTFA(T)       WFT1
227700961128     C                   Z-ADD     TLNA(T)       WFT2
227800961128     C                   ENDIF
227900961128     C     WRIF          IFEQ      'LA'
228000961128     C                   Z-ADD     TLNA(T)       WFT1
228100961128     C                   ENDIF
228200961128     C*
228300961128     C* LANCIA GESTIONE TARIFFE
228400961128     C                   EXSR      GESEVD
228500961128     C*
228600961128     C* CAMPI PER MESSAGGI
228700961128     C                   MOVE      WFT1          WTT1
228800961128     C                   MOVE      WFT2          WTT2
228900961128     C*
229000961128     C                   ENDSR
229100961128     C*------------------------------------------------------------------------*
229200961128     C* CERCAR - CERCA LA TARIFFA DI CARTELLO
229300961128     C*------------------------------------------------------------------------*
229400961128     C     CERCAR        BEGSR
229500961128     C*
229600961128     C                   MOVEL     'C'           WTTV
229700961128     C                   Z-ADD     *ZEROS        WFT1
229800961128     C                   Z-ADD     *ZEROS        WFT2
229900961128     C                   Z-ADD     *ZEROS        WKSC
230000961128     C                   Z-ADD     *ZEROS        WCTR
230100961128     C                   MOVEL     *BLANKS       WCTS
230200140115     C                   MOVEL     *BLANKS       Wnaz
230300140115     C                   MOVEL     *BLANKS       wcad
230400961128     C*
230500961128     C* LANCIA GESTIONE TARIFFE
230600961128     C                   EXSR      GESEVD
230700961128     C*
230800961128     C                   ENDSR
230900961128     C*------------------------------------------------------------------------*
231000961128     C* GESEVD - GESTIONE FILE ECEVD (TARIFFE)
231100961128     C*------------------------------------------------------------------------*
231200961128     C     GESEVD        BEGSR
231300961128     C*
231400961128     C* SE TIPO SVILUPPO PESO TASSABILE 2=MIGLIORE REALE/TASS. O 3=MINORE REA/TA
231500961128     C* CERCO IL RAPP PESO/VOL PRIMA DI ARROTONDARE
231600961128     C     VSPT(V)       IFEQ      '2'                                          --- 1 -->
231700961128     C     VSPT(V)       OREQ      '3'
231800961128     C*
231900961128     C* CERCA LO SCAGLIONE CON LA QTA DELLA BOLLA
232000961128     C                   EXSR      SETEVD
232100961128     C*
232200961128     C* SE IL RAPP. PESO/VOL MAGGIORE DI ZERO E VOLUME MAGGIORE DI ZERO
232300961128     C* CERCO SCAGLIONE CON PESO TASSABILE
232400961128     C     EVDRPV        IFNE      *ZEROS
232500961128     C     SVSC(S)       ANDNE     *ZEROS
232600961128     C*
232700961128     C* CALCOLO PESO TASSABILE
232800961128     C                   EXSR      CALPT                                        *CALCOLO PESO TASSAB
232900961128     C                   ENDIF
233000961128     C*
233100961128     C                   ENDIF                                                  <-- 1 ---
233200961128     C*
233300961128     C* CONTROLLA ARROTONDAMENTO
233400961128     C                   EXSR      ARROTO                                       *ARROTONDA QTA
233500961128     C*
233600961128     C* CERCA LO SCAGLIONE GIUSTO IN TARIFFA
233700961128     C                   EXSR      SETEVD
233800961128     C*
233900961128     C                   ENDSR
234000961128     C*------------------------------------------------------------------------*
234100961128     C* SETEVD - POSIZIONAMENTO E PRIMA LETTURA FILE TARIFFE
234200961128     C*------------------------------------------------------------------------*
234300961128     C     SETEVD        BEGSR
234400961128     C*
234500961128     C                   MOVEL     'N'           WSETLL            1
234600961128     C                   MOVEL     '0'           FINEVD            1
234700961128     C*
234800961128     C* SE TARIFFA DI CARTELLO CONTROLLO PRIMA SE CARICATA IN MEMORIA
234900961128IF  1C     WTTV          IFEQ      'C'
235000961128     C                   EXSR      CHKCAR
235100961128E   1C                   ENDIF
235200961128     C*
235300961128     C* SE TARIFFA DI FILIALE CONTROLLO PRIMA SE CARICATA IN MEMORIA
235400961128IF  1C     WTTV          IFEQ      'F'
235500961128     C                   EXSR      CHKFIL
235600961128E   1C                   ENDIF
235700961128     C*
235800961128     C* SE TARIFFA NON TROVATA IN MEMORIA LEGGO DAL FILE
235900961128IF  1C     WSETLL        IFEQ      'N'
236000961128     C*
236100961128     C* POSIZIONAMENTO E PRIMA LETTURA
236200961128     C                   Z-ADD     WVOC          EVDVOC
236300961128     C                   Z-ADD     WPRG          EVDPRG
236400961128     C                   MOVEL     WTTV          EVDTTV
236500961128     C                   Z-ADD     WFT1          EVDFT1
236600961128     C                   Z-ADD     WFT2          EVDFT2
236700961128     C                   Z-ADD     WKSC          EVDCLI
236800961128     C                   Z-ADD     WCTR          EVDCTR
236900961128     C                   MOVEL     WCTS          EVDCTS
237000140115     C                   movel     Wnaz          EVDnaz
237100140115     C                   movel(p)  wcad          EVDcad
237200961128     C                   Z-ADD     *ZEROS        EVDDDT
237300961128     C                   Z-ADD     *ZEROS        EVDSGI
237400140115     C     KEYEVD        SETLL     ECEVD06L                           99
237500961128     C*
237600961128IF  2C     *IN99         IFEQ      *ON
237700961128     C                   MOVEL     '1'           FINEVD
237800961128X   2C                   ELSE
237900961128     C                   EXSR      LETEVD
238000961128E   2C                   ENDIF
238100961128     C*
238200961128     C* CONTROLLO MANCA TARIFFA
238300961128IF  2C     WRECOK        IFEQ      'N'
238400050615     c                   clear                   ecevdds
238500961128X   2C                   ELSE
238600961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
238700961128E   2C                   ENDIF
238800961128     C*
238900961128E   1C                   ENDIF
239000961128     C*
239100961128     C                   ENDSR
239200961128     C*------------------------------------------------------------------------*
239300961128     C* LETEVD - LETTURA PROSSIMO RECORD TARIFFE
239400961128     C*------------------------------------------------------------------------*
239500961128     C     LETEVD        BEGSR
239600961128     C*
239700961128     C* LEGGO FINO A:
239800961128     C*     - FINE FILE O SUPERATO LIMITE FINALE
239900961128     C*     - TROVATO RECORD VALIDO
240000961128     C                   MOVEL     'N'           WRECOK
240100961128     C     FINEVD        DOWEQ     '0'                                          --- 1 -->
240200961128     C     WRECOK        ANDEQ     'N'
240300961128     C*
240400961128     C* LETTURE SUCCESSIVE ALLA PRIMA
240500140115     C                   READ      ECEVD06L                               99
240600961128     C*
240700961128     C     *IN99         IFEQ      '1'                                          *FINE FILE
240800961128     C     EVDVOC        ORNE      WVOC
240900961128     C     EVDPRG        ORNE      WPRG
241000961128     C     EVDTTV        ORNE      WTTV
241100961128     C     EVDFT1        ORNE      WFT1
241200961128     C     EVDFT2        ORNE      WFT2
241300961128     C     EVDCLI        ORNE      WKSC
241400961128     C     EVDCTR        ORNE      WCTR
241500961128     C     EVDCTS        ORNE      WCTS
241600140115     C     EVDCAd        ORNE      wcad
241700140115     C     EVDnaz        ORNE      Wnaz
241800961128     C                   MOVEL     '1'           FINEVD
241900961128     C                   ELSE
242000961128     C                   EXSR      CHKEVD
242100961128     C                   END
242200961128     C*
242300961128     C                   END                                                    <-- 1 ---
242400961128     C*
242500961128     C                   ENDSR
242600961128     C*------------------------------------------------------------------------*
242700961128     C* CHKEVD - CONTROLLA VALIDITA' DELLA TARIFFA
242800961128     C*------------------------------------------------------------------------*
242900961128     C     CHKEVD        BEGSR
243000961128     C*
243100961128     C                   MOVEL     'S'           WRECOK            1
243200961128     C* ESCLUDO SE ANNULLATO
243300961128     C     EVDATB        IFNE      *BLANKS
243400961128     C                   MOVEL     'N'           WRECOK
243500961128     C                   END
243600961128     C*
243700961128     C* ESCLUDO SE DATA SPEDIZIONE FUORI DALLA DECORR/SCAD.
243800961128     C     D00DTC        IFLT      EVDDDT
243900961128     C     D00DTC        ORGT      EVDDST
244000961128     C                   MOVEL     'N'           WRECOK
244100961128     C                   END
244200011019     C*
244300011019     C     WDIVCE        IFNE      WDIVAC
244400011019     C                   IF        WTIV = '21' OR
244500011019     C                             WTIV = '22' OR
244600011019     C                             WTIV = '23' OR
244700011019     C                             WTIV = '25'
244800011019     C                   EXSR      CNVEVDSG
244900011019     C                   ENDIF
245000011019     C                   EXSR      CNVEVD
245100011019     C                   ENDIF
245200961128     C*
245300961128     C* ESCLUDO SE QTA NON COMPRESA NELLO SCAGLIONE
245400000306IF  1C     WTIV          IFNE      *BLANKS                                      *SI RICERCA SCAGLION
245500011017     C*
245600011017     C     WQTA          IFLT      EVDSGI
245700961128     C     WQTA          ORGT      EVDSGF
245800961128     C                   MOVEL     'N'           WRECOK
245900011017     C                   ENDIF
246000000306E   1C                   ENDIF
246100961128     C*
246200961128     C                   ENDSR
246300970211     C*------------------------------------------------------------------------*
246400970211     C* PERFOR - DETERMINA PERCENTUALI PER FORMULA
246500970211     C*------------------------------------------------------------------------*
246600970211     C     PERFOR        BEGSR
246700970211     C*
246800040611     c                   do
246900970211     C* -----> CERCA LA VOCE
247000970211     C     D00TSP        IFEQ      'D'                                          *DISTRIBUZIONE
247100970211     C     ETCCLV        IFEQ      'A'                                          *CLASSE
247200100526     C                   Z-ADD     816           V                 6 0          *SEDE-DIST-CL.A
247300970211     C                   ENDIF
247400970211     C     ETCCLV        IFEQ      'B'
247500100526     C                   Z-ADD     817           V                              *SEDE-DIST-CL.B
247600970211     C                   ENDIF
247700970211     C     ETCCLV        IFEQ      'C'
247800100526     C                   Z-ADD     818           V                              *SEDE-DIST-CL.C
247900970211     C                   ENDIF
248000980709     C     ETCCLV        IFEQ      'T'
248100020213     C     ETCCLV        OREQ      'D'
248200100526     C                   Z-ADD     821           V                              *SEDE-DIST-CL.T
248300980709     C                   ENDIF
248400970211     C                   ENDIF
248500970211     C     D00TSP        IFEQ      'C'                                          *CORRIERE(NORMALE)
248600970211     C     ETCCLV        IFEQ      'A'                                          *CLASSE
248700100422     C                   Z-ADD     810           V                              *SEDE-NORM-CL.A
248800970211     C                   ENDIF
248900970211     C     ETCCLV        IFEQ      'B'
249000100422     C                   Z-ADD     811           V                              *SEDE-NORM-CL.B
249100970211     C                   ENDIF
249200970211     C     ETCCLV        IFEQ      'C'
249300100422     C                   Z-ADD     812           V                              *SEDE-NORM-CL.C
249400970211     C                   ENDIF
249500980709     C     ETCCLV        IFEQ      'T'
249600020213     C     ETCCLV        OREQ      'D'
249700100422     C                   Z-ADD     819           V                              *SEDE-NORM-CL.T
249800980709     C                   ENDIF
249900970211     C                   ENDIF
250000970211     C     D00TSP        IFEQ      'E'                                          *ESPRESSO
250100970211     C     ETCCLV        IFEQ      'A'                                          *CLASSE
250200100422     C                   Z-ADD     813           V                              *SEDE-ESPR-CL.A
250300970211     C                   ENDIF
250400970211     C     ETCCLV        IFEQ      'B'
250500100422     C                   Z-ADD     814           V                              *SEDE-ESPR-CL.B
250600970211     C                   ENDIF
250700970211     C     ETCCLV        IFEQ      'C'
250800100422     C                   Z-ADD     815           V                              *SEDE-ESPR-CL.C
250900970211     C                   ENDIF
251000980709     C     ETCCLV        IFEQ      'T'
251100020213     C     ETCCLV        OREQ      'D'
251200100422     C                   Z-ADD     820           V                              *SEDE-ESPR-CL.T
251300980709     C                   ENDIF
251400970211     C                   ENDIF
251500970211     C*
251600970211     C* -----> RECUPERA LA % DELLA VOCE
251700970211     C                   Z-ADD     V             WVOC
251800970221     C                   Z-ADD     VPRG(V)       WPRG
251900041109     C                   MOVEL     VTIV(V)       WTIV
252000970211     C                   EXSR      CERTAR
252100970211     C     $ERR          IFEQ      'S'
252200970211     C                   MOVEL     MSG(2)        $MSG                           *TARIF. NON TROV
252300970211     C                   EXSR      SCRERR
252400040611     c                   leave
252500970211     C                   ENDIF
252600011025     C                   Z-ADD     EVDPER        WPERRI           21 8          *% RICARICO SEDE
252700970211     C*
252800970211     C* RECUPERA LA % DEL RICARICO COMMERCIALE
252900980710     C                   Z-ADD     270           V
253000980710     C                   Z-ADD     270           WVOC
253100970221     C                   Z-ADD     VPRG(V)       WPRG
253200041109     C                   MOVEL     VTIV(V)       WTIV
253300970211     C                   EXSR      CERTAR
253400970211     C     $ERR          IFEQ      'S'
253500970211     C                   MOVEL     MSG(17)       $MSG                           *TARIF. NON TROV
253600970211     C                   EXSR      SCRERR
253700040611     c                   leave
253800970211     C                   ENDIF
253900011025     C                   Z-ADD     EVDPER        WPERCO           21 8          *% RICARICO COMM.LE
254000040609     C* RECUPERA LA % DEL FINANZIAM.TARIFFE IMPORT DPD solo se franco
254100040609     c                   clear                   wperft
254200040614     C                   IF        vc(305)='S'
254300040609     C                   Z-ADD     305           V
254400040609     C                   Z-ADD     305           WVOC
254500040609     C                   Z-ADD     VPRG(V)       WPRG
254600041109     C                   MOVEL     VTIV(V)       WTIV
254700040609     C                   EXSR      CERTAR
254800040609     C     $ERR          IFEQ      'S'
254900040609     C                   MOVEL     MSG(29)       $MSG                           *TARIF. NON TROV
255000040609     C                   EXSR      SCRERR
255100040611     c                   leave
255200040609     C                   ENDIF
255300040609     C                   Z-ADD     EVDPER        WPERFT           21 8          *% RICARICO COMM.LE
255400040609     c                   end
255500970211     C*
255600970211     C* RECUPERA LA % DEL RICARICO PAGAMENTO
255700970211     C                   Z-ADD     300           V
255800970211     C                   Z-ADD     300           WVOC
255900970221     C                   Z-ADD     VPRG(V)       WPRG
256000041109     C                   MOVEL     VTIV(V)       WTIV
256100970211     C                   EXSR      CERTAR
256200970211     C     $ERR          IFEQ      'S'
256300970211     C                   MOVEL     MSG(23)       $MSG                           *TARIF. NON TROV
256400970211     C                   EXSR      SCRERR
256500040611     c                   leave
256600970211     C                   ENDIF
256700011025     C                   Z-ADD     EVDPER        WPERPA           21 8          *% RICARICO PAGAM.
256800970227     C*---
256900970227     C* F O R M U L A
257000970227     C*---
257100970227     C* PC = % COMMERCIALE
257200970227     C* PP = % PAGAMENTO
257300970227     C* PR = % RICARICO DELTA
257400040609     C* FT = % FINANZIAM.TARIFFE export DPD franco
257500970211     C*
257600040609     C* 100 ( PC + PP + PR + FT) + PR ( PC + PP + FT)
257700040609     C* ---------------------------------------------  = % RICARICHI
257800040609     C* 100 ( 100 - PC - PP -FT) - PR ( PC + PP + FT)
257900970211     C*
258000970211     C* CALCOLO DENOMINATORE
258100011025     C     WPERCO        ADD       WPERPA        W0153W           23 8
258200970211     C                   ADD       WPERRI        W0153W
258300040609     C                   ADD       WPERft        W0153W
258400011025     C     100           MULT      W0153W        W0153A           23 8
258500970211     C*
258600970211     C     WPERCO        ADD       WPERPA        W0153W
258700040609     C                   ADD       WPERft        W0153W
258800011025     C     WPERRI        MULT      W0153W        W0153B           23 8
258900970211     C*
259000011025     C     W0153A        ADD       W0153B        WDENOM           23 8
259100970211     C*
259200970211     C* CALCOLO DIVISORE
259300970211     C     100           SUB       WPERCO        W0153W
259400040609     C                   SUB       WPERPA        W0153W
259500040609     C                   SUB       WPERft        W0153W
259600970211     C     100           MULT      W0153W        W0153A
259700970211     C*
259800970211     C     WPERCO        ADD       WPERPA        W0153W
259900040609     C                   ADD       WPERft        W0153W
260000970211     C     WPERRI        MULT      W0153W        W0153B
260100970211     C*
260200011025     C     W0153A        SUB       W0153B        WDIVIS           23 8
260300970211     C*
260400970211     C* CALCOLO TOTALE
260500970211     C     WDENOM        DIV       WDIVIS        W0153W
260600011025     C     W0153W        MULT      100           WPERTO           23 8
260700970211     C*
260800040611     c                   enddo
260900970211     C                   ENDSR
261000970211     C*------------------------------------------------------------------------*
261100970225     C* CALTOT - CALCOLA RICARICO SEDE + RAGGR. DELTA
261200970211     C*------------------------------------------------------------------------*
261300970211     C     CALTOT        BEGSR
261400970211     C*
261500970211     C* CICLO PER TUTTE LE VOCI X TOT.VOCI + RAGGR.DELTA
261600970211     C                   Z-ADD     *ZEROS        IRDL                           *SCHIERA RAGGR.DELTA
261700970211     C                   DO        NVOCA         N
2618009702111    C                   Z-ADD     VOCA(N)       V
2619009702112    C     VIM(V)        IFGT      *ZEROS
262000970211     C*
262100970211     C* APPLICA LA % DA CONSIDERARE (NON PER LE MAGGIORAZIONI)
262200970211     C     VP(V)         IFLT      100
262300970211     C     VTVO(V)       ANDNE     'M'
262400970211     C     VIM(V)        DIV       100           WVVOCE
262500970211     C     WVVOCE        MULT      VP(V)         VIM(V)
262600970211     C                   ENDIF
262700970211     C*
262800970213     C* SE LA VOCE DEVE ESSERE INGLOBATA (NEL PORTO) FORZO RAGGR.DELTA = 001
262900970211     C     VI(V)         IFEQ      'S'
263000970211     C                   Z-ADD     001           VRDL(V)
263100970211     C                   ENDIF
263200970211     C*
263300970227     C* RAGGRUPPAMENTO DELTA
263400970211     C                   Z-ADD     VRDL(V)       R                 6 0          *INDICE RAGGR.DELTA
263500970211     C                   ADD       VIM(V)        IRDL(R)                        *IMP.RAGGR.DELTA
263600970211     C*
2637009702112    C                   ENDIF
2638009702111    C                   ENDDO
263900970430     C*
264000970430     C* CALCOLA IMPORTO RICARICHI E LI AGGIUNGE AI TOTALI
264100970623IF  1C     D00TLA        IFEQ      'P'
264200970430     C*
264300011025     C                   Z-ADD     *ZEROS        WCODEL           21 8          *VALORE COMP.DELTA
264400970910     C* SE DA PORTO, AGGIUNGE LA % RICARICHI TOT. A TUTTI I RAGGRUPPAM. DELTA
264500970910     C                   DO        50            R
264600970910     C     IRDL(R)       IFGT      *ZEROS
264700970910     C     IRDL(R)       DIV       100           WVVOCE
264800970430     C     WVVOCE        MULT      WPERTO        WVVOCE                         *% RICARICHI TOTALI
264900970910     C                   ADD       WVVOCE        IRDL(R)                        *AGG.VALORE AL RAGGR
265000970910     C                   ADD       WVVOCE        WCODEL                         *AGG.ALLA COMP.DELTA
265100970910     C                   ENDIF
265200970910     C                   ENDDO
265300970910     C*
265400970910     C* IMPORTO % RICARICHI TOTALI (SEDE+PAG+COM)
265500970910     C                   Z-ADD     900           V                              *VOCE PER COMP.DELTA
265600970910     C                   Z-ADD     WCODEL        VIM(V)                         *IMPORTO VOCE
265700970430     C*
265800970430X   1C                   ELSE
265900970430     C*
266000011025     C                   Z-ADD     *ZEROS        WCOMPA           21 8          *COMP.PAGAM.
266100011025     C                   Z-ADD     *ZEROS        WCOMCO           21 8          *COMP.COMM.LE
266200011025     C                   Z-ADD     *ZEROS        WCOMRI           21 8          *COMP.SEDE
266300040610     C                   Z-ADD     *ZEROS        WCOMFT           21 8          *COMP.SEDE
266400970430     C* SE DA DELTA, RECUPERA L'IMPORTO RICAVI SCAGLIONE/CODICE TASSAZIONE DAL
266500970430     C* WORKFILE E APPLICA LE 3 PERCENTUALI PAGAMENTO/COMMERCIALE/SEDE
266600980909     C******************** MOVELUSER      ETWUSR           *UTENTE
266700980909     C                   MOVEL     JOB           ETWUSR                         *UTENTE
266800091123     C                   MOVEL     'S'           ETWDSF                         *SEDE/FIL
266900970430     C                   MOVEL     D00CTO        ETWCTO                         *TARIF/OFF
267000970430     C                   Z-ADD     D00KSC        ETWKSC                         *CLIENTE
267100970430     C                   Z-ADD     D00CTR        ETWCTR                         *TARIFFA
267200970430     C                   Z-ADD     D00PRG        ETWPRG                         *PROGRESSIVO
267300970430     C                   Z-ADD     SSGL(S)       ETWSGL                         *SCAGLIONE SUP.
267400970430     C                   MOVEL     TCTS(T)       ETWCTS                         *COD.TASSAZ.
267500010926     C     KEYETX        SETLL     TEETW01L
267600010926     C     KEYETX        READE     TEETW01L                               99
267700970430DO  2C     *IN99         DOWEQ     *OFF
267800970910     C                   Z-ADD     ETWRDL        R
267900970910     C* CALC. LA %RICARICO COMMERCIALE A L'AGG. AL RAGGR. DELTA
268000970910     C     ETWRIC        DIV       100           WVVOCE
268100970910     C     WVVOCE        MULT      WPERCO        WVVOCE                         *%COMM.LE
268200970910     C                   ADD       WVVOCE        IRDL(R)
268300970910     C                   ADD       WVVOCE        WCOMCO                         *AGG.ALLA COMP.COMML
268400970910     C* APPLICA LA %RICARICO PAGAMENTO AL RICAVO REALE
268500970910     C     ETWRIC        DIV       100           WVVOCE
268600970910     C     WVVOCE        MULT      WPERPA        WVVOCE                         *%PAGAMENTO
268700970910     C                   ADD       WVVOCE        IRDL(R)
268800970910     C                   ADD       WVVOCE        WCOMPA                         *AGG.ALLA COMP.PAGAM
268900040610     C* APPLICA LA %FINANZIAM.TARIFFE IMPORT DPD al ricavo reale
269000040610     C     ETWRIC        DIV       100           WVVOCE
269100040610     C     WVVOCE        MULT      WPERFT        WVVOCE                         *%PAGAMENTO
269200040610     C                   ADD       WVVOCE        IRDL(R)
269300040610     C                   ADD       WVVOCE        WCOMFT                         *AGG.ALLA COMP.PAGAM
269400010926     C     KEYETX        READE     TEETW01L                               99
269500970430E   2C                   ENDDO
269600970910     C*
269700970910     C* AGGIUNGE LA % RICARICHI SEDE A TUTTI I RAGGRUPPAM. DELTA
269800970910     C                   DO        50            R
269900970910     C     IRDL(R)       IFGT      *ZEROS
270000970910     C     IRDL(R)       DIV       100           WVVOCE
270100970910     C     WVVOCE        MULT      WPERRI        WVVOCE                         *% RICARICHI SEDE
270200970910     C                   ADD       WVVOCE        IRDL(R)                        *AGG.VALORE AL RAGGR
270300970910     C                   ADD       WVVOCE        WCOMRI                         *AGG.ALLA COMP.SEDE
270400970910     C                   ENDIF
270500970910     C                   ENDDO
270600970910     C*
270700970910     C* IMPORTO % RICARICHI NEL DELTA
270800980710     C                   Z-ADD     270           V                              *VOCE PER COMP.COMML
270900980710     C                   Z-ADD     WCOMCO        VIM(V)                         *IMPORTO VOCE
271000040610     C                   Z-ADD     305           V                              *VOCE PER COMP.COMML
271100040610     C                   Z-ADD     WCOMft        VIM(V)                         *IMPORTO VOCE
271200970910     C                   Z-ADD     300           V                              *VOCE PER COMP.PAGAM
271300970910     C                   Z-ADD     WCOMPA        VIM(V)                         *IMPORTO VOCE
271400970910     C                   Z-ADD     900           V                              *VOCE PER COMP.SEDE
271500970910     C                   Z-ADD     WCOMRI        VIM(V)                         *IMPORTO VOCE
271600970430     C*
271700970430E   1C                   ENDIF
271800970430     C*
271900970430     C* IMPORTO % RICARICHI NEL DELTA
272000970910     C**                   Z-ADD900       V                *VOCE PER COMP.DELTA
272100970910     C**                   Z-ADDWCODEL    VIM,V            *IMPORTO VOCE
272200970910     C****                 Z-ADDVRDL,V    R                *INDICE RAGGR.DELTA
272300970910     C****                 ADD  WVVOCE    IRDL,R           *IMP.RAGGR.DELTA
272400970211     C*
272500970401     C* SOLO PER LANCIO X CALC.PORTO: CALC. INOLTRO,DELTA RICHIESTO,TAR.FINITA
2726009705070    C*££        D00TLA    IFEQ 'P'                        *LANCIO X PORTO
272700970317     C*
272800970317     C* SE INOLTRO DA INGLOBARE CALCOLA IMP. E LO TOGLIE DAL RAGGR."PORTO"
272900970317     C***********ETCSIN    IFEQ 'N'                        *SCORPORO INOLTRO ?
2730009705071    C*££        ETCSIN    IFEQ 'S'                        *INGLOBA INOLTRO ?
273100970507     C*££        SINL,S    ANDGT*ZEROS                     *IMPORTO INOLTRO
273200970507     C*££        SINL,S    DIV  100       WVVOCE
273300970507     C*££        WVVOCE    MULT ETCPPV    WVVOCE           *IMP.INOLTRO
273400970319     C***********WVVOCE    IFLE IRDL,1
273500970319     C*********************SUB  WVVOCE    IRDL,1           *IMP.RAGGR.DELTA PORT
273600970507     C*££                  ADD  WVVOCE    IRDL,1           *IMP.RAGGR.DELTA PORT
273700970319     C*********************ELSE
273800970319     C*********************Z-ADD*ZEROS    IRDL,1
273900970319     C*********************ENDIF
2740009705071    C*££                  ENDIF
274100970211     C*
274200970211     C* CALCOLA % DELTA RICHIESTO SOLO PER LANCIO X CALC.PORTO
2743009705071    C*$$        ETCPDE    IFGT *ZEROS                     * % DELTA RICH.GENER.
274400970507     C*$$        SPDE,S    ORGT *ZEROS                     * % DELTA RICH.SCAGL.
274500970211     C*
274600970402     C* SOMMA DELTA GENERALE E DELTA DELLO SCAGLIONE
274700970507     C*$$        ETCSDE    IFEQ '-'
274800970507     C*$$        ETCPDE    MULT -1        WGPDE  155       *DELTA GEN.CON SEGNO
274900970507     C*$$                  ELSE
275000970507     C*$$                  Z-ADDETCPDE    WGPDE
275100000705     C*$$                  ENDIF
275200970507     C*$$        SSDE,S    IFEQ '-'
275300970507     C*$$        SPDE,S    MULT -1        WSPDE  155       *DELTA SCA.CON SEGNO
275400970507     C*$$                  ELSE
275500970507     C*$$                  Z-ADDSPDE,S    WSPDE
275600970507     C*$$                  ENDIF
275700970507     C*$$        WGPDE     ADD  WSPDE     WTPDE  155       *DELTA TOT.CON SEGNO
275800970507     C*$$        WTPDE     IFLT 0
275900970507     C*$$        WTPDE     MULT -1        WTPDE
276000970507     C*$$                  MOVEL'-'       WTSDE   1
276100970507     C*$$                  ELSE
276200970507     C*$$                  MOVEL'+'       WTSDE
276300970507     C*$$                  ENDIF
276400970402     C*
276500970507     C*$$                  XFOOTIRDL      WVVOCE
276600970507     C*$$        WVVOCE    DIV  100       WVVOCE
276700970507     C*$$        WVVOCE    MULT WTPDE     WVVOCE           * % DELTA RICH.
276800970211     C*
276900970227     C* APPLICA DELTA RICHIESTO AL RAGGRUPPAMENTO DELTA
2770009705072    C*$$                  DO   50        R
2771009705073    C*$$        IRDL,R    IFGT *ZEROS
2772009705074    C*$$        WTSDE     IFEQ '+'                        *SEGNO DELTA RICH.
277300970507     C*$$        100       SUB  WTPDE     WPDE   155
277400970507     C*$$        100       MULT IRDL,R    WVVOCE
277500970507     C*$$        WVVOCE    DIV  WPDE      IRDL,R
2776009705074    C*$$                  ELSE                            *SEGNO NEGATIVO
277700970507     C*$$        100       ADD  WTPDE     WPDE
277800970507     C*$$        100       MULT IRDL,R    WVVOCE
277900970507     C*$$        WVVOCE    DIV  WPDE      IRDL,R
2780009705074    C*$$                  ENDIF
2781009705073    C*$$                  ENDIF
2782009705072    C*$$                  ENDDO
278300970211     C*
2784009705071    C*$$                  ENDIF
278500970401     C*
278600970401     C* CONTROLLA TARIFFA FINITA SOLO SE SCAGLIONE MAGGIORE QTA TAR.FINITA
278700970401     C* APPLICA TARIFFA FINITA SOLO AL PORTO (INDICE=1)
2788009704231    C*******    SSGL,S    IFGT D00ATA
2789009705071    C*££        SPSC,S    IFGT D00ATA
279000970401     C*
279100970507     C*££                  MOVELD00CTR    UNOCTR  10
279200970401     C*
2793009705072    C*££        UNOCTR    IFEQ 1                          *TARIFFA A COLLI
279400970507     C*££        IRDL,1    DIV  SCSC,S    WVVOCE
279500970507     C*££                  ELSE                            *TARIFFA KG/QLI
279600970507     C*££        IRDL,1    DIV  SPSC,S    WVVOCE
279700970507     C*££                  ENDIF
279800970401     C*
2799009705072    C*££        UNOCTR    IFEQ 0                          *TARIFFA A QLI
280000970507     C*££        WVVOCE    MULT 100       IRDL,1
2801009705072    C*££                  ELSE                            *TARIFFA KG/COLLI
280200970507     C*££                  Z-ADDWVVOCE    IRDL,1
2803009705072    C*££                  ENDIF
280400970401     C*
2805009705071    C*££                  ENDIF
2806009705070    C*££                  ENDIF
280700970211     C*
280800970211     C                   ENDSR
280900970211     C*------------------------------------------------------------------------*
281000970211     C* SCRETW - SCRIVE SU FILE DI LAVORO LE COMPETENZE
281100970211     C*------------------------------------------------------------------------*
281200970211     C     SCRETW        BEGSR
281300970211     C*
281400970227     C* SCRIVE UN RECORD PER OGNI RAGGRUPPAMENTO DELTA
281500970211     C                   DO        50            R
281600970211     C     IRDL(R)       IFGT      *ZEROS
281700970211     C     R             OREQ      1
281800980909     C******************** MOVELUSER      ETWUSR           *UTENTE
281900980909     C                   MOVEL     JOB           ETWUSR                         *UTENTE
282000091123     C                   MOVEL     'S'           ETWDSF                         *SEDE/FIL
282100970211     C                   MOVEL     D00CTO        ETWCTO                         *TARIF/OFF
282200970211     C                   Z-ADD     D00KSC        ETWKSC                         *CLIENTE
282300970211     C                   Z-ADD     D00CTR        ETWCTR                         *TARIFFA
282400970211     C                   Z-ADD     D00PRG        ETWPRG                         *PROGRESSIVO
282500970211     C                   Z-ADD     SSGL(S)       ETWSGL                         *SCAGLIONE SUP.
282600970211     C                   MOVEL     TCTS(T)       ETWCTS                         *COD.TASSAZ.
282700970211     C                   Z-ADD     R             ETWRDL                         *RAGGR.DELTA
282800970211     C                   MOVEL     TRCT(T)       ETWRCT                         *RAGGR.C.TASS.
282900970211     C*
283000011025     C     TPPE(T)       DIV       100           WPERC            21 8
283100970211     C     WPERC         MULT      SPPE(S)       ETWPPE                         *% PESO
283200970211     C*
283300970211     C                   MOVEL     TRAP(T)       ETWRAP                         *REGIONE APP.
283400970211     C                   MOVEL     TNAR(T)       ETWNAR                         *NAZIONE APP.
283500010926     C     KEYETW        CHAIN     TEETW01L                           99
283600970211     C     *IN99         IFEQ      '0'                                          *ESISTE GIA'
283700970211     C                   ADD       IRDL(R)       ETWCOM                         *COMPET.
283800970317     C     D00TLA        IFEQ      'P'                                          *LANCIO X PORTO
283900970307     C                   Z-ADD     *ZEROS        ETWRIC
284000970317     C                   ENDIF
284100010926     C                   UPDATE    TEETW000
284200970211     C                   ELSE                                                   *NON ESISTE
284300970211     C                   Z-ADD     IRDL(R)       ETWCOM                         *COMPET.
284400970307     C                   Z-ADD     *ZEROS        ETWRIC
284500010926     C                   WRITE     TEETW000
284600970211     C                   ENDIF
284700970211     C                   ENDIF
284800970211     C                   ENDDO
284900970211     C*
285000970211     C                   ENDSR
285100970211     C*------------------------------------------------------------------------*
285200970211     C* SCREWC - SCRIVE SU FILE DI LAVORO IL DETTAGLIO DELLE VOCI (SE DEBUG)
285300970211     C*------------------------------------------------------------------------*
285400970211     C     SCREWC        BEGSR
285500970211     C*
285600970227     C* SCRIVE UN RECORD PER OGNI VOCE
285700970211     C                   DO        NVOCA         N
285800970211     C                   Z-ADD     VOCA(N)       V
285900970211     C     VIM(V)        IFGT      *ZEROS
286000980909     C******************** MOVELUSER      EWCUSR           *UTENTE
286100980909     C                   MOVEL     JOB           EWCUSR                         *UTENTE
286200091123     C                   MOVEL     'S'           EWCDSF                         *SEDE/FIL
286300980708     C                   MOVEL     D00CTO        EWCCTO                         *TARIF/OFF
286400970211     C                   Z-ADD     D00KSC        EWCKSC                         *CLIENTE
286500970211     C                   Z-ADD     D00CTR        EWCCTR                         *TARIFFA
286600970211     C                   Z-ADD     D00PRG        EWCPRG                         *PROGRESSIVO
286700970211     C                   MOVEL     TCTS(T)       EWCCTS                         *COD.TASSAZ.
286800970211     C                   Z-ADD     SSGL(S)       EWCSGL                         *SCAGLIONE SUP.
286900970211     C                   Z-ADD     V             EWCVOC                         *VOCE
287000970211     C                   Z-ADD     VIM(V)        EWCIMP                         *IMPORTO
287100970211     C                   Z-ADD     VRDL(V)       EWCRDL                         *RAGGR.DELTA
287200010926     C                   WRITE     TEETWC00
287300970211     C                   ENDIF
287400970211     C                   ENDDO
287500970211     C*
287600970211     C                   ENDSR
287700961128     C*------------------------------------------------------------------------*
287800961128     C* CHKCAR - CONTROLLA SE TARIFFA DI CARTELLO CARICATA IN MEMORIA
287900961128     C*------------------------------------------------------------------------*
288000961128     C     CHKCAR        BEGSR
288100961128     C*
288200961128     C* CONTROLLA ESISTENZA TARIFFA DI CARTELLO IN LINEA FRA QUELLE CARICATE
288300961128     C                   Z-ADD     1             I
288400961128DO  1C     I             DOWLE     WULTCA                                       FINO ULTIMA TAR.CART
288500961128     C     WSETLL        ANDEQ     'N'                                          CARTELLO NON TROVATA
288600961128     C*
288700961128     C* CONTROLLI DI VALIDITA'
288800961128IF  2C     WVOC          IFEQ      CVOC(I)                                      *COD. VOCE
288900961128     C*
289000961128IF  3C     WPRG          IFEQ      CPRG(I)                                      *ALTRI CAMPI CHIAVE
289100961128     C     WFT1          ANDEQ     CFT1(I)
289200961128     C     WFT2          ANDEQ     CFT2(I)
289300961128     C     WKSC          ANDEQ     CCLI(I)
289400961128     C     WCTR          ANDEQ     CCTR(I)
289500961128     C     WCTS          ANDEQ     CCTS(I)
289600140115     C     Wnaz          ANDEQ     Cnaz(I)
289700140115     C     wcad          ANDEQ     Ccad(I)
289800961128     C*
289900961128IF  4C     D00DTC        IFGE      CDDT(I)                                      *DECORRENZA/SCADENZA
290000961128     C     D00DTC        ANDLE     CDST(I)
290100961128     C*
290200000306IF  5C*---       WQTA      IFGE CSGI,I                     *SCAGLIONE
290300000306     C*---       WQTA      ANDLECSGF,I
290400000306     C*
290500000306IF  5C     WTIV          IFEQ      *BLANKS                                      *NO RICERCA SCAGLION
290600000306     C     WTIV          ORNE      *BLANKS                                      *SI RICERCA SCAGLION
290700000306     C     WQTA          ANDGE     CSGI(I)                                      *SCAGLIONE
290800000306     C     WQTA          ANDLE     CSGF(I)
290900961128     C*
291000961128     C* CARTELLO TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
291100961128     C                   MOVEL     'S'           WSETLL
291200961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
291300961128     C*
291400961128     C                   Z-ADD     CVOC(I)       EVDVOC
291500961128     C                   Z-ADD     CPRG(I)       EVDPRG
291600961128     C                   MOVEL     'C'           EVDTTV
291700961128     C                   Z-ADD     CFT1(I)       EVDFT1
291800961128     C                   Z-ADD     CFT2(I)       EVDFT2
291900961128     C                   Z-ADD     CCLI(I)       EVDCLI
292000961128     C                   Z-ADD     CCTR(I)       EVDCTR
292100961128     C                   MOVEL     CCTS(I)       EVDCTS
292200140115     C                   movel     Ccad(I)       EVDcad
292300140115     C                   movel     Cnaz(I)       EVDnaz
292400961128     C                   Z-ADD     CDDT(I)       EVDDDT
292500961128     C                   Z-ADD     CDST(I)       EVDDST
292600961128     C                   Z-ADD     CSGI(I)       EVDSGI
292700961128     C                   Z-ADD     CSGF(I)       EVDSGF
292800961128     C                   Z-ADD     CITR(I)       EVDITR
292900961128     C                   Z-ADD     CIMI(I)       EVDIMI
293000961128     C                   Z-ADD     CIMA(I)       EVDIMA
293100961128     C                   MOVEL     CSEG(I)       EVDSEG
293200961128     C                   Z-ADD     CPER(I)       EVDPER
293300961128     C                   Z-ADD     CRPV(I)       EVDRPV
293400961128     C*
293500961128E   5C                   ENDIF
293600961128E   4C                   ENDIF
293700961128E   3C                   ENDIF
293800961128E   2C                   ENDIF
293900961128     C*
294000961128     C                   ADD       1             I
294100961128E   1C                   ENDDO
294200961128     C*
294300961128     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
294400961128     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
294500961128IF  1C     WSETLL        IFEQ      'N'                                          *NON TROVATA IN MEM.
294600021112     C     WULTCA        ANDLT     300                                          *NON FINE SCHIERA
294700961128     C                   MOVEL     'S'           WSETLL                         *NON LEGGERE PIU'
294800961128E   1C                   ENDIF
294900961128     C*
295000961128     C                   ENDSR
295100961128     C*------------------------------------------------------------------------*
295200011025     C* CARFIL - CARICA TUTTE LE TARIFFE DELLA FILIALE
295300961128     C*------------------------------------------------------------------------*
295400961128     C     CARFIL        BEGSR
295500961128     C*
295600961128     C* POSIZIONAMENTO E PRIMA LETTURA
295700961128     C     D00LNP        SETLL     ECEVD03L
295800961128     C                   READ      ECEVD03L                               99
295900961128     C*
296000961128     C* FINO A FINE FILE
296100961128     C                   Z-ADD     *ZEROS        I                 6 0
296200961128DO  1C     *IN99         DOWEQ     *OFF
296300961128     C     EVDFT1        ANDEQ     D00LNP                                       *FILIALE DEL CLIENTE
296400961128IF  3C     EVDATB        IFEQ      *BLANKS                                      *NON ANNULLATA
296500050103     C     D00DTC        ANDGE     EVDDDT                                       *MAGG. DECORRENZA
296600050103     C     D00DTC        ANDLE     EVDDST                                       *MIN. SCADENZA
296700011025     C*
296800011025     C* LEGGO LA RELATIVA TESTATA VOCE X EFFETTUARE ALCUNE CONSIDERAZIONI
296900011025     C     KEYEVT2       CHAIN     ECEVT01L
297000011025     C                   IF        %found(ECEVT01L)
297100011025     C     WDIVCE        IFNE      WDIVAC
297200011025     C                   IF        EVTTIV = '21' OR
297300011025     C                             EVTTIV = '22' OR
297400011025     C                             EVTTIV = '23' OR
297500011025     C                             EVTTIV = '25'
297600011025     C                   EXSR      CNVEVDSG
297700011025     C                   ENDIF
297800011025     C                   EXSR      CNVEVD
297900011025     C                   ENDIF
298000011025     C*
298100961128     C                   ADD       1             I
298200961128     C                   Z-ADD     EVDVOC        FVOC(I)
298300961128     C                   Z-ADD     EVDPRG        FPRG(I)
298400961128     C                   Z-ADD     EVDFT1        FFT1(I)
298500961128     C                   Z-ADD     EVDFT2        FFT2(I)
298600961128     C                   Z-ADD     EVDCLI        FCLI(I)
298700961128     C                   Z-ADD     EVDCTR        FCTR(I)
298800961128     C                   MOVEL     EVDCTS        FCTS(I)
298900140115     C                   movel     EVDcad        Fcad(I)
299000140115     C                   movel     EVDnaz        Fnaz(I)
299100961128     C                   Z-ADD     EVDDDT        FDDT(I)
299200961128     C                   Z-ADD     EVDDST        FDST(I)
299300961128     C                   Z-ADD     EVDSGI        FSGI(I)
299400961128     C                   Z-ADD     EVDSGF        FSGF(I)
299500961128     C                   Z-ADD     EVDITR        FITR(I)
299600961128     C                   Z-ADD     EVDIMI        FIMI(I)
299700961128     C                   Z-ADD     EVDIMA        FIMA(I)
299800961128     C                   MOVEL     EVDSEG        FSEG(I)
299900961128     C                   Z-ADD     EVDPER        FPER(I)
300000961128     C                   Z-ADD     EVDRPV        FRPV(I)
300100961128E   2C                   ENDIF
300200011025     C                   ENDIF
300300961128     C*
300400961128     C                   READ      ECEVD03L                               99
300500961128E   1C                   ENDDO
300600961128     C*
300700961128     C                   Z-ADD     I             WULTTF            6 0          *ULT.TARIF.FILIALE
300800961128     C*
300900961128     C*
301000961128     C                   ENDSR
301100961128     C*------------------------------------------------------------------------*
301200961128     C* CHKFIL - CONTROLLA SE TARIFFA FILIALE GIA' IN MEMORIA
301300961128     C*------------------------------------------------------------------------*
301400961128     C     CHKFIL        BEGSR
301500961128     C*
301600961128     C* CONTROLLA ESISTENZA TARIFFA FILIALE FRA QUELLE CARICATE
301700961128     C                   Z-ADD     1             I
301800961128DO  1C     I             DOWLE     WULTTF                                       *FINO ULTIMA TAR.FIL
301900961128     C     WSETLL        ANDEQ     'N'                                          *TARIFFA NON TROVATA
302000961128     C*
302100961128     C* CONTROLLI DI VALIDITA'
302200961128IF  2C     WVOC          IFEQ      FVOC(I)                                      *COD.VOCE
302300961128     C*
302400961128IF  3C     WPRG          IFEQ      FPRG(I)                                      *ALTRI CAMPI CHIAVE
302500961128     C     WFT1          ANDEQ     FFT1(I)
302600961128     C     WFT2          ANDEQ     FFT2(I)
302700961128     C     WKSC          ANDEQ     FCLI(I)
302800961128     C     WCTR          ANDEQ     FCTR(I)
302900961128     C     WCTS          ANDEQ     FCTS(I)
303000140115     C     Wnaz          ANDEQ     Fnaz(I)
303100140115     C     wcad          ANDEQ     Fcad(I)
303200961128     C*
303300961128IF  4C     D00DTC        IFGE      FDDT(I)                                      *DECORRENZA/SCADENZA
303400961128     C     D00DTC        ANDLE     FDST(I)
303500961128     C*
303600000306IF  5C*---       WQTA      IFGE FSGI,I                     *SCAGLIONE
303700000306     C*---       WQTA      ANDLEFSGF,I
303800000306     C*
303900000306IF  5C     WTIV          IFEQ      *BLANKS                                      *NO RICERCA SCAGLION
304000000306     C     WTIV          ORNE      *BLANKS                                      *SI RICERCA SCAGLION
304100000306     C     WQTA          ANDGE     FSGI(I)                                      *SCAGLIONE
304200000306     C     WQTA          ANDLE     FSGF(I)
304300961128     C*
304400961128     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
304500961128     C                   MOVEL     'S'           WSETLL                         *NON LEGGERE PIU'
304600961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
304700961128     C*
304800961128     C                   Z-ADD     FVOC(I)       EVDVOC
304900961128     C                   Z-ADD     FPRG(I)       EVDPRG
305000961128     C                   MOVEL     'F'           EVDTTV
305100961128     C                   Z-ADD     FFT1(I)       EVDFT1
305200961128     C                   Z-ADD     FFT2(I)       EVDFT2
305300961128     C                   Z-ADD     FCLI(I)       EVDCLI
305400961128     C                   Z-ADD     FCTR(I)       EVDCTR
305500961128     C                   MOVEL     FCTS(I)       EVDCTS
305600140115     C                   movel     Fcad(I)       EVDcad
305700140115     C                   movel     Fnaz(I)       EVDnaz
305800961128     C                   Z-ADD     FDDT(I)       EVDDDT
305900961128     C                   Z-ADD     FDST(I)       EVDDST
306000961128     C                   Z-ADD     FSGI(I)       EVDSGI
306100961128     C                   Z-ADD     FSGF(I)       EVDSGF
306200961128     C                   Z-ADD     FITR(I)       EVDITR
306300961128     C                   Z-ADD     FIMI(I)       EVDIMI
306400961128     C                   Z-ADD     FIMA(I)       EVDIMA
306500961128     C                   MOVEL     FSEG(I)       EVDSEG
306600961128     C                   Z-ADD     FPER(I)       EVDPER
306700961128     C                   Z-ADD     FRPV(I)       EVDRPV
306800961128     C*
306900961128E   5C                   ENDIF
307000961128E   4C                   ENDIF
307100961128E   3C                   ENDIF
307200961128E   2C                   ENDIF
307300961128     C*
307400961128     C                   ADD       1             I
307500961128E   1C                   ENDDO
307600961128     C*
307700961128     C                   ENDSR
307800961128     C*------------------------------------------------------------------------*
307900011025     C* CARCAR - CARICA LE TARIFFE DI CARTELLO
308000961128     C*------------------------------------------------------------------------*
308100961128     C     CARCAR        BEGSR
308200961128     C*
308300961128     C* POSIZIONAMENTO E PRIMA LETTURA
308400961128     C                   MOVEL     'C'           EVDTTV                         *TARIFFE DI CARTELLO
308500961128     C     EVDTTV        SETLL     ECEVD04L
308600961128     C                   READ      ECEVD04L                               99
308700961128     C*
308800961128     C* FINO A FINE FILE E TARIFFE DI CARTELLO
308900961128     C                   Z-ADD     *ZEROS        I
309000961128DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
309100961128     C     EVDTTV        ANDEQ     'C'                                          *TARIFFE DI CARTELLO
309200021112     C     I             ANDLT     300                                          *FINE SCHIERA
309300961128IF  2C     EVDATB        IFEQ      *BLANKS                                      *NON ANNULLATA
309400050103     C     D00DTC        ANDGE     EVDDDT                                       *MAGG. DECORRENZA
309500050103     C     D00DTC        ANDLE     EVDDST                                       *MIN. SCADENZA
309600011025     C*
309700011025     C* LEGGO LA RELATIVA TESTATA VOCE X EFFETTUARE ALCUNE CONSIDERAZIONI
309800011025     C     KEYEVT2       CHAIN     ECEVT01L
309900011025     C                   IF        %found(ECEVT01L)
310000011025     C     WDIVCE        IFNE      WDIVAC
310100011025     C                   IF        EVTTIV = '21' OR
310200011025     C                             EVTTIV = '22' OR
310300011025     C                             EVTTIV = '23' OR
310400011025     C                             EVTTIV = '25'
310500011025     C                   EXSR      CNVEVDSG
310600011025     C                   ENDIF
310700011025     C                   EXSR      CNVEVD
310800011025     C                   ENDIF
310900011025     C*
311000961128     C                   ADD       1             I
311100961128     C                   Z-ADD     EVDVOC        CVOC(I)
311200961128     C                   Z-ADD     EVDPRG        CPRG(I)
311300961128     C                   Z-ADD     EVDFT1        CFT1(I)
311400961128     C                   Z-ADD     EVDFT2        CFT2(I)
311500961128     C                   Z-ADD     EVDCLI        CCLI(I)
311600961128     C                   Z-ADD     EVDCTR        CCTR(I)
311700961128     C                   MOVEL     EVDCTS        CCTS(I)
311800140115     C                   movel     EVDcad        Ccad(I)
311900140115     C                   movel     EVDnaz        Cnaz(I)
312000961128     C                   Z-ADD     EVDDDT        CDDT(I)
312100961128     C                   Z-ADD     EVDDST        CDST(I)
312200961128     C                   Z-ADD     EVDSGI        CSGI(I)
312300961128     C                   Z-ADD     EVDSGF        CSGF(I)
312400961128     C                   Z-ADD     EVDITR        CITR(I)
312500961128     C                   Z-ADD     EVDIMI        CIMI(I)
312600961128     C                   Z-ADD     EVDIMA        CIMA(I)
312700961128     C                   MOVEL     EVDSEG        CSEG(I)
312800961128     C                   Z-ADD     EVDPER        CPER(I)
312900961128     C                   Z-ADD     EVDRPV        CRPV(I)
313000961128E   2C                   ENDIF
313100011025     C                   ENDIF
313200961128     C*
313300961128     C                   READ      ECEVD04L                               99
313400961128E   1C                   ENDDO
313500961128     C*
313600961128     C                   Z-ADD     I             WULTCA            6 0          *ULT.TARIFFA CART.
313700961128     C*
313800961128     C                   ENDSR
313900961128     C*------------------------------------------------------------------------*
314000961128     C* CARMAG - CARICA LE MAGGIORAZIONI
314100961128     C*------------------------------------------------------------------------*
314200961128     C     CARMAG        BEGSR
314300961128     C*
314400961128     C* POSIZIONAMENTO E PRIMA LETTURA
314500961128     C     *LOVAL        SETLL     ECEVM01L
314600961128     C                   READ      ECEVM01L                               99
314700961128     C*
314800961128     C* FINO A FINE FILE E MAGGIORAZIONI
314900961128     C                   Z-ADD     *ZEROS        I
315000961128DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
315100050104     C     I             ANDLT     600                                          *FINE SCHIERA
315200961128IF  2C     EVMATB        IFEQ      *BLANKS                                      *NON ANNULLATA
315300050103     C     KEYEVT3       CHAIN     ECEVT01L
315400050103     C                   IF        %found(ECEVT01L) and EVTATB = *blanks
3155000501033    C                             and D00DTC >= EVTDDV and                     *NON E' ANNULLATO
315600050103     C                             D00DTC <= EVTDSV                             *MAGG. DECORRENZA
315700961128     C                   ADD       1             I
315800961128     C                   Z-ADD     EVMVOM        MVOM(I)
315900961128     C                   Z-ADD     EVMPRM        MPRM(I)
316000961128     C                   Z-ADD     EVMVOC        MVOC(I)
316100961128     C                   Z-ADD     EVMPRG        MPRG(I)
316200961128E   2C                   ENDIF
316300050103E   3C                   ENDIF
316400961128     C*
316500961128     C                   READ      ECEVM01L                               99
316600961128E   1C                   ENDDO
316700961128     C*
316800961128     C                   Z-ADD     I             WULTMA            6 0          *ULT.MAGGIORAZIONE
316900961128     C*
317000961128     C                   ENDSR
317100961128     C*------------------------------------------------------------------------*
317200961128     C* CARTAB - CARICA LE TABELLE
317300961128     C*------------------------------------------------------------------------*
317400961128     C     CARTAB        BEGSR
317500961128     C*
317600961128     C* CARICA TABELLA -2R- RIFERIMENTO VOCE
317700961128     C     $ERR          IFEQ      'N'
317800961128     C                   Z-ADD     0             I
317900961128     C                   Z-ADD     CODUT         KTBUTE
318000961128     C                   MOVEL     '2R'          KTBCOD
318100961128     C                   MOVEL     *BLANKS       KTBKEY
318200961128     C     KEYTAB        SETLL     TABEL00F                           99
318300961128     C  N99              READ      TABEL00F                               99
318400961128     C     *IN99         IFEQ      '1'
318500961128     C                   MOVEL     MSG(3)        $MSG
318600961128     C                   EXSR      SCRERR
318700961128     C                   END
318800961128     C     *IN99         DOWEQ     '0'
318900961128     C     TBLCOD        ANDEQ     '2R'
319000961128     C     TBLFLG        IFEQ      *BLANKS
319100961128     C                   ADD       1             I
319200961128     C                   MOVEL     TBLKEY        C2R(I)
319300961128     C                   MOVEL     TBLUNI        DS2R
319400961128     C                   MOVEL     §2RTVA        T2R(I)                         *TIPO VALORIZZ. Q,V,
319500961128     C                   END
319600961128     C                   READ      TABEL00F                               99
319700961128     C                   END
319800961128     C                   END
319900010801     C*
320000010801     C* CARICA TABELLA -1B-  CODICI TRATTAMENTO MERCE
320100010801IF  1C     $ERR          IFEQ      'N'
320200010801     C                   Z-ADD     0             I
320300010801     C                   Z-ADD     CODUT         KTBUTE
320400010801     C                   MOVEL     '1B'          KTBCOD
320500010801     C     KEYTAB        SETLL     TABEL00F                           99
320600010801     C  N99              READ      TABEL00F                               99
320700010801IF  2C     *IN99         IFEQ      '1'
320800010801     C                   MOVEL     MSG(4)        $MSG
320900010801     C                   EXSR      SCRERR
321000010801E   2C                   ENDIF
321100010801DO  2C     *IN99         DOWEQ     '0'
321200010801     C     TBLCOD        ANDEQ     '1B'
321300010801IF  3C     TBLFLG        IFEQ      *BLANKS
321400010801     C                   ADD       1             I
321500010801     C                   MOVEL     TBLKEY        CTM(I)                         *CODICE TRATTAMENTO
321600010801     C                   MOVEL     TBLUNI        DS1B
321700010801     C                   MOVEL     §1BF12        CF12(I)                        *FLAG "DISCK C"
321800010801E   3C                   ENDIF
321900010801     C                   READ      TABEL00F                               99
322000010801E   2C                   ENDDO
322100010801E   1C                   ENDIF
322200971211     C*
322300971211     C* CARICA FLAG ITALIA/ESTERO DALL'ORGANIGRAMMA
322400971211     C     $ERR          IFEQ      'N'
322500971211     C     *LOVAL        SETLL     AZORG01L
322600971211     C                   READ      AZORG01L                               98
322700971211DO  1C     *IN98         DOWEQ     *OFF
322800971211IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
322900971211     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
323000971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
323100980710     C                   MOVEL     ORGDIT        DIT(F)                         *FLAG BAR/SDI
323200000217     C                   MOVEL     ORGDE3        OG143
323300020729     C                   IF        §OGNTW = 'DPD'
323400020729     C                   MOVEL     'S'           DPD(F)                         *FLAG FILIALE DPD
323500020729     C                   ENDIF
323600020503     C                   MOVEL     §OGNTW        NTW(F)
323700020729     C*
323800020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
323900020729     C                   CLEAR                   DNTW
324000020729     C*
324100020729     C                   MOVEL(P)  'NTW'         KTECOD
324200020729     C                   MOVEL(P)  §OGNTW        KTEKE1
324300020729     C     KEYTBE        CHAIN     TNTBE01L
324400020729     C                   IF        %found(TNTBE01L)
324500020729     C                   MOVEL     TBEUNI        DNTW
324600020729     C                   ENDIF
324700020729     C*
324800020729     C                   MOVEL     §NTWFIE       FL1(F)
325200971211E   2C                   ENDIF
325300971211     C                   READ      AZORG01L                               98
325400971211E   1C                   ENDDO
325500971211     C                   END
325600961128     C*
325700961128     C                   ENDSR
325800011008     C*------------------------------------------------------------------------*
325900011008     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
326000011008     C*------------------------------------------------------------------------*
326100011008     C     CARGEDCE      BEGSR
326200011017     C*
326300011017     C                   MOVEL     *BLANKS       WDIVCE            3
326400011008     C*
326500011008     C                   CLEAR                   DSGEDCE
326600011008     C*
326700011008     C                   MOVEL(P)  'GED'         KTECOD
326800011008     C                   MOVEL(P)  'CE'          KTEKE1
326900011008     C     KEYTBE        CHAIN     TNTBE01L
327000011008     C                   IF        %found(TNTBE01L)
327100011008     C                   MOVEL     TBEUNI        DSGEDCE
327200011008     C                   ENDIF
327300011008     C*
327400011008     C                   IF        §GEDCEDAT > D00DTC
327500011026     C                   EVAL      WDIVCE = §GEDCEDIP
327600011017     C                   ELSE
327700011017     C                   EVAL      WDIVCE = §GEDCEDIV
327800011008     C                   ENDIF
327900011008     C*
328000011008     C                   ENDSR
328100011017     C*------------------------------------------------------------------------*
328200011017     C* CARGEDAC - CARICAMENTO TABELLA "GED" TIPO RECORD "AC" E CONSIDERAZIONI SU DIVISE
328300011017     C*------------------------------------------------------------------------*
328400011017     C     CARGEDAC      BEGSR
328500011017     C*
328600011017     C                   MOVEL     *BLANKS       WDIVAC            3
328700011017     C*
328800011017     C                   CLEAR                   DGEDAC
328900011017     C*
329000011017     C                   MOVEL(P)  'GED'         KTECOD
329100011017     C                   MOVEL(P)  'AC'          KTEKE1
329200011017     C     KEYTBE        CHAIN     TNTBE01L
329300011017     C                   IF        %found(TNTBE01L)
329400011017     C                   MOVEL     TBEUNI        DGEDAC
329500011017     C                   ENDIF
329600011017     C*
329700011017     C                   IF        §GEDACDAT > D00DTC
329800011017     C                   EVAL      WDIVAC = §GEDACDIP
329900011017     C                   ELSE
330000011017     C                   EVAL      WDIVAC = §GEDACDID
330100011017     C                   ENDIF
330200011017     C*
330300011017     C                   ENDSR
330400011008     C*------------------------------------------------------------------------*
330500011008     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
330600011008     C*------------------------------------------------------------------------*
330700011008     C     CNVEUR        BEGSR
330800011008     C*
330900011008     C* AZZERA VARIABILI DI LAVORO
331000011008     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
331100011008     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
331200011008     C*
331300011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
331400011026     C*                  IF        WDIVCE = 'ITL'
331500011026     C*                  EVAL      NDEC = '0'
331600011026     C*                  ELSE
331700011026     C*                  EVAL      NDEC = '3'
331800011026     C*                  ENDIF
331900011026     C                   EVAL      NDEC = '9'
332000011008     C*
332100011008IF  1C     WIMI          IFNE      *ZEROS
332200011008     C                   CLEAR                   YEUR
332300011008     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
332400011008     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
332500011023     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
332600011008     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
332700011008     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
332800011008     C                   CALL      'YEURCO'
332900011008     C                   PARM                    YEUR
333000011008IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
333100011008     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
333200011008X   2C                   ELSE                                                   *SI ERRORI
333300011008     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
333400011008E   2C                   ENDIF
333500011008E   1C                   ENDIF
333600011008     C*
333700011008     C                   ENDSR
333800011008     C*------------------------------------------------------------------------*
333900011008     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
334000011008     C*------------------------------------------------------------------------*
334100011008     C     CNVETC        BEGSR
334200011017     C*
334300011008     C* ARROTONDAMENTO
334400011008     C                   Z-ADD     ETCIAR        WIMI             30 9
334500011008     C                   MOVEL     D00DIV        WDVI              3
334600011008     C                   EXSR      CNVEUR
334700011008     C                   Z-ADD     WIMO          ETCIAR
334800011008     C* CONTRASSEGNO
334900011008     C                   Z-ADD     ETCCAS        WIMI             30 9
335000011008     C                   MOVEL     D00DIV        WDVI              3
335100011008     C                   EXSR      CNVEUR
335200011008     C                   Z-ADD     WIMO          ETCCAS
335300011008     C* ASSICURAZIONE
335400011008     C                   Z-ADD     ETCASC        WIMI             30 9
335500011008     C                   MOVEL     D00DIV        WDVI              3
335600011008     C                   EXSR      CNVEUR
335700011008     C                   Z-ADD     WIMO          ETCASC
335800011008     C*
335900011008     C                   ENDSR
336000011008     C*------------------------------------------------------------------------*
336100011008     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
336200011008     C*------------------------------------------------------------------------*
336300011008     C     CNVETS        BEGSR
336400011017     C*
336500011008     C* INOLTRO
336600011008     C                   Z-ADD     ETSINL        WIMI             30 9
336700011008     C                   MOVEL     D00DIV        WDVI              3
336800011008     C                   EXSR      CNVEUR
336900011008     C                   Z-ADD     WIMO          ETSINL
337000011008     C*
337100011008     C                   ENDSR
337200011017     C*------------------------------------------------------------------------*
337300011017     C* CNVEVDSG - CONVERTE I CAMPI SCAGLIONI DI ECEVD IN DIVISA DI CALCOLO DEL CE
337400011017     C*------------------------------------------------------------------------*
337500011017     C     CNVEVDSG      BEGSR
337600011017     C*
337700011017     C* SCAGLIONE INIZIALE
337800011017     C                   Z-ADD     EVDSGI        WIMI             30 9
337900011017     C                   MOVEL     WDIVAC        WDVI              3
338000011017     C                   EXSR      CNVEUR
338100011017     C                   Z-ADD     WIMO          EVDSGI
338200011017     C* SCAGLIONE FINALE
338300011017     C                   Z-ADD     EVDSGF        WIMI             30 9
338400011017     C                   MOVEL     WDIVAC        WDVI              3
338500011017     C                   EXSR      CNVEUR
338600011017     C                   Z-ADD     WIMO          EVDSGF
338700011017     C*
338800011017     C                   ENDSR
338900011017     C*------------------------------------------------------------------------*
339000011017     C* CNVEVD - CONVERTE I CAMPI NON SCAGLIONI DI ECEVD IN DIVISA DI CALCOLO DEL CE
339100011017     C*------------------------------------------------------------------------*
339200011017     C     CNVEVD        BEGSR
339300011017     C*
339400011017     C* IMPORTO UNITARIO TARIFFA
339500011017     C                   Z-ADD     EVDITR        WIMI             30 9
339600011017     C                   MOVEL     WDIVAC        WDVI              3
339700011017     C                   EXSR      CNVEUR
339800011017     C                   Z-ADD     WIMO          EVDITR
339900011017     C* IMPORTO MINIMO VOCE
340000011017     C                   Z-ADD     EVDIMI        WIMI             30 9
340100011017     C                   MOVEL     WDIVAC        WDVI              3
340200011017     C                   EXSR      CNVEUR
340300011017     C                   Z-ADD     WIMO          EVDIMI
340400011017     C* IMPORTO MASSIMO VOCE
340500011017     C                   Z-ADD     EVDIMA        WIMI             30 9
340600011017     C                   MOVEL     WDIVAC        WDVI              3
340700011017     C                   EXSR      CNVEUR
340800011017     C                   Z-ADD     WIMO          EVDIMA
340900011017     C*
341000011017     C                   ENDSR
341100961128     C*------------------------------------------------------------------------*
341200961128     C* SUBDAT - RECUPERARE DATA E ORA ATTUALI
341300961128     C*------------------------------------------------------------------------*
341400961128     C     SUBDAT        BEGSR
341500961128     C*
341600961128     C                   TIME                    DTTIME
341700961128     C                   Z-ADD     DTANN         DSANN
341800961128     C                   Z-ADD     DTMES         DSMES
341900961128     C                   Z-ADD     DTGIO         DSGIO
342000961128     C                   Z-ADD     DTSEC         DSSEC
342100961128     C*
342200961128     C                   ENDSR
342300961128     C*------------------------------------------------------------------------*
342400961128     C* SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
342500961128     C*------------------------------------------------------------------------*
342600961128     C     SCRERR        BEGSR
342700961128     C*
342800961128     C                   MOVEL     'S'           $ERR
342900961128     C                   MOVEL     $MSG          D00MSG
343000961128     C                   MOVEL     1             D00ERR
343100961128     C*
343200961128     C                   ENDSR
343300970213     C*------------------------------------------------------------------------*
343400970213     C* CANCWF - CANCELLA WORKFILE
343500970213     C*------------------------------------------------------------------------*
343600970213     C     CANCWF        BEGSR
343700970213     C*
343800010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETW00F
343900970318IF  1C     D00TLA        IFEQ      'P'                                          *LANCIO X PORTO
344000980909     C******************** MOVELUSER      ETWUSR
344100980909     C                   MOVEL     JOB           ETWUSR
344200010926     C     KEYETW        SETLL     TEETW01L                           99
344300010926     C  N99              READ      TEETW01L                               99
344400970318DO  2C     *IN99         DOWEQ     *OFF
344500980909     C********** USER      ANDEQETWUSR
344600980909     C     JOB           ANDEQ     ETWUSR
344700010926     C                   DELETE    TEETW000
344800010926     C                   READ      TEETW01L                               99
344900970318E   2C                   ENDDO
345000970318E   1C                   ENDIF
345100970213     C*
345200010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETWC0F
345300980909     C******************** MOVELUSER      EWCUSR
345400980909     C                   MOVEL     JOB           EWCUSR
345500010926     C     KEYEWC        SETLL     TEETWC1L                           99
345600010926     C  N99              READ      TEETWC1L                               99
345700970213DO  1C     *IN99         DOWEQ     *OFF
345800980909IF  2C********** USER      ANDEQEWCUSR
345900980909IF  2C     JOB           ANDEQ     EWCUSR
346000010926     C                   DELETE    TEETWC00
346100010926     C                   READ      TEETWC1L                               99
346200970213E   1C                   ENDDO
346300970213     C*
346400970213     C                   ENDSR
346500961128     C*------------------------------------------------------------------------*
346600961128     C* RUTINZ - ROUTINE INIZIALE
346700961128     C*------------------------------------------------------------------------*
346800961128     C     RUTINZ        BEGSR
346900970227     C*--------------------
347000970227     C* RICEVIMENTO PARAMETRI
347100970227     C*--------------------
347200961128     C     *ENTRY        PLIST
347300961128     C                   PARM                    KPJBA
347400961128     C                   PARM                    DSTE00
347500970227     C*--------------------
347600970227     C* CHIAVI DI LETTURA
347700970227     C*--------------------
347800970910     C* LETTURA AZORG01L - COMPLETA
347900970910     C     KEYORG        KLIST
348000970910     C                   KFLD                    KORFIL
348100970227     C* LETTURA TABEL00F - COMPLETA
348200961128     C     KEYTAB        KLIST
348300961128     C                   KFLD                    KTBUTE
348400961128     C                   KFLD                    KTBCOD
348500961128     C                   KFLD                    KTBKEY
348600970227     C* LETTURA ECEVT01L - COMPLETA
348700961128     C     KEYEVT        KLIST
348800961128     C                   KFLD                    EVTVOC
348900961128     C                   KFLD                    EVTPRG
349000011025     C* LETTURA ECEVT01L - COMPLETA
349100011025     C     KEYEVT2       KLIST
349200011025     C                   KFLD                    EVDVOC
349300011025     C                   KFLD                    EVDPRG
349400050103     C* LETTURA ECEVT01L - COMPLETA
349500050103     C     KEYEVT3       KLIST
349600050103     C                   KFLD                    EVMVOC
349700050103     C                   KFLD                    EVMPRG
349800140115     C* LETTURA ECEVD06L - COMPLETA
349900961128     C     KEYEVD        KLIST
350000961128     C                   KFLD                    EVDVOC
350100961128     C                   KFLD                    EVDPRG
350200961128     C                   KFLD                    EVDTTV
350300961128     C                   KFLD                    EVDFT1
350400961128     C                   KFLD                    EVDFT2
350500961128     C                   KFLD                    EVDCLI
350600961128     C                   KFLD                    EVDCTR
350700961128     C                   KFLD                    EVDCTS
350800140115     C                   KFLD                    EVDnaz
350900140115     C                   KFLD                    EVDCAd
351000961128     C                   KFLD                    EVDDDT
351100961128     C                   KFLD                    EVDSGI
351200970227     C* LETTURA ECEVD02L - COMPLETA
351300961128     C     KEYEV2        KLIST
351400961128     C                   KFLD                    EVDCLI
351500970227     C* LETTURA ECEVM01L - COMPLETA
351600961128     C     KEYEVM        KLIST
351700961128     C                   KFLD                    EVMVOM
351800961128     C                   KFLD                    EVMPRM
351900961128     C                   KFLD                    EVMVOC
352000961128     C                   KFLD                    EVMPRG
352100011017     C* LETTURA TEETT01L - PARZIALE
352200961128     C     KE2ETT        KLIST
352300980708     C                   KFLD                    KTTDSF                         *SEDE/FIL
352400970117     C                   KFLD                    KTTCTO                         *TARIF/OFF
352500961128     C                   KFLD                    KTTKSC                         *CLIENTE
352600961128     C                   KFLD                    KTTCTR                         *TARIFFA
352700961128     C                   KFLD                    KTTPRG                         *PROGRESSIVO
352800961128     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
352900961128     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
353000011017     C* LETTURA TEETT01L - COMPLETA
353100961128     C     KEYETT        KLIST
353200980708     C                   KFLD                    KTTDSF                         *SEDE/FIL
353300970117     C                   KFLD                    KTTCTO                         *TARIF/OFF
353400961128     C                   KFLD                    KTTKSC                         *CLIENTE
353500961128     C                   KFLD                    KTTCTR                         *TARIFFA
353600961128     C                   KFLD                    KTTPRG                         *PROGRESSIVO
353700961128     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
353800961128     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
353900961128     C                   KFLD                    KTTCTS                         *COD.TASS.
354000010926     C* LETTURA TEETC01L - COMPLETA
354100961128     C     KEYETC        KLIST
354200980708     C                   KFLD                    KTCDSF                         *SEDE/FIL
354300970117     C                   KFLD                    KTCCTO                         *TARIF/OFF
354400961128     C                   KFLD                    KTCKSC                         *CLIENTE
354500961128     C                   KFLD                    KTCCTR                         *TARIFFA
354600961128     C                   KFLD                    KTCPRG                         *PROGRESSIVO
354700961128     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
354800010926     C* LETTURA TEETS01L - PARZIALE
354900961128     C     KE2ETS        KLIST
355000980708     C                   KFLD                    KTSDSF                         *SEDE/FIL
355100970117     C                   KFLD                    KTSCTO                         *TARIF/OFF
355200961128     C                   KFLD                    KTSKSC                         *CLIENTE
355300961128     C                   KFLD                    KTSCTR                         *TARIFFA
355400961128     C                   KFLD                    KTSPRG                         *PROGRESSIVO
355500961128     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
355600010926     C* LETTURA TEETS01L - COMPLETA
355700961128     C     KEYETS        KLIST
355800980708     C                   KFLD                    KTSDSF                         *SEDE/FIL
355900970117     C                   KFLD                    KTSCTO                         *TARIF/OFF
356000961128     C                   KFLD                    KTSKSC                         *CLIENTE
356100961128     C                   KFLD                    KTSCTR                         *TARIFFA
356200961128     C                   KFLD                    KTSPRG                         *PROGRESSIVO
356300961128     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
356400961128     C                   KFLD                    KTSSGL                         *SCAGLIONE
356500010926     C* LETTURA TEETW01L - COMPLETA
356600961128     C     KEYETW        KLIST
356700970214     C                   KFLD                    ETWUSR                         UTENTE
356800980708     C                   KFLD                    ETWDSF                         *SEDE/FIL
356900970117     C                   KFLD                    ETWCTO                         *TARIF/OFF
357000961128     C                   KFLD                    ETWKSC                         *CLIENTE
357100961128     C                   KFLD                    ETWCTR                         *TARIFFA
357200961128     C                   KFLD                    ETWPRG                         *PROGRESSIVO
357300961128     C                   KFLD                    ETWSGL                         *SCAGL.
357400961128     C                   KFLD                    ETWCTS                         *C.TASS.
357500961128     C                   KFLD                    ETWRDL                         *RAGGR.DELTA
357600010926     C* LETTURA TEETW01L - PARZIALE
357700970430     C     KEYETX        KLIST
357800970430     C                   KFLD                    ETWUSR                         UTENTE
357900980708     C                   KFLD                    ETWDSF                         *SEDE/FIL
358000970430     C                   KFLD                    ETWCTO                         *TARIFFA/OFFERTA
358100970430     C                   KFLD                    ETWKSC                         *CLIENTE
358200970430     C                   KFLD                    ETWCTR                         *TARIFFA
358300970430     C                   KFLD                    ETWPRG                         *PROGRESSIVO
358400970430     C                   KFLD                    ETWSGL                         *SCAGLIONE
358500970430     C                   KFLD                    ETWCTS                         *CODICE TASSAZIONE
358600010926     C* LETTURA TEETWC1L - PARZIALE
358700970213     C     KEYEWC        KLIST
358800970214     C                   KFLD                    EWCUSR                         *UTENTE
358900011008     C* LETTURA TNTBE01L - PARZIALE
359000011008     C     KEYTBE        KLIST
359100011008     C                   KFLD                    KTECOD                         *UTENTE
359200011008     C                   KFLD                    KTEKE1                         *CODICE
359300970227     C*--------------------
359400970227     C* VARIABILI RIFERITE AL DATA BASE
359500970227     C*--------------------
359600970910     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
359700961128     C     *LIKE         DEFINE    TBLKUT        KTBUTE
359800961128     C     *LIKE         DEFINE    TBLCOD        KTBCOD
359900961128     C     *LIKE         DEFINE    TBLKEY        KTBKEY
360000961128     C     *LIKE         DEFINE    EVTARF        WVAL
360100961128     C     *LIKE         DEFINE    EVTVOC        WVOC
360200961128     C     *LIKE         DEFINE    EVTPRG        WPRG
360300000306     C     *LIKE         DEFINE    EVTTIV        WTIV
360400961128     C     *LIKE         DEFINE    EVTRFI        WRIF
360500961128     C     *LIKE         DEFINE    EVDTTV        WTTV
360600961128     C     *LIKE         DEFINE    EVDFT1        WFT1
360700961128     C     *LIKE         DEFINE    EVDFT2        WFT2
360800961128     C     *LIKE         DEFINE    EVDCLI        WKSC
360900961128     C     *LIKE         DEFINE    EVDCTR        WCTR
361000961128     C     *LIKE         DEFINE    EVDCTS        WCTS
361100140115     C     *LIKE         DEFINE    EVDnaz        Wnaz
361200140115     C     *LIKE         DEFINE    EVDCAd        wcad
361300961128     C     *LIKE         DEFINE    EVTDDV        WSPDAT
361400961128     C     *LIKE         DEFINE    ETCLNP        WTFP
361500970910     C     *LIKE         DEFINE    EVDCTS        WCTP
361600021129     C     *LIKE         DEFINE    D00KSC        DEPKSC                         *DEPOSITI
361700011017     C     *LIKE         DEFINE    ETTKSC        KTTKSC                         *TEETT00F
361800980708     C     *LIKE         DEFINE    ETTDSF        KTTDSF
361900970117     C     *LIKE         DEFINE    ETTCTO        KTTCTO
362000961128     C     *LIKE         DEFINE    ETTCTR        KTTCTR
362100961128     C     *LIKE         DEFINE    ETTPRG        KTTPRG
362200961128     C     *LIKE         DEFINE    ETTLNP        KTTLNP
362300961128     C     *LIKE         DEFINE    ETTFIE        KTTFIE
362400961128     C     *LIKE         DEFINE    ETTCTS        KTTCTS
362500010926     C     *LIKE         DEFINE    ETCKSC        KTCKSC                         *TEETC00F
362600980708     C     *LIKE         DEFINE    ETCDSF        KTCDSF
362700970117     C     *LIKE         DEFINE    ETCCTO        KTCCTO
362800961128     C     *LIKE         DEFINE    ETCCTR        KTCCTR
362900961128     C     *LIKE         DEFINE    ETCPRG        KTCPRG
363000961128     C     *LIKE         DEFINE    ETCLNP        KTCLNP
363100961128     C     *LIKE         DEFINE    ETSKSC        KTSKSC                         *TNESC00F
363200980708     C     *LIKE         DEFINE    ETSDSF        KTSDSF
363300970117     C     *LIKE         DEFINE    ETSCTO        KTSCTO
363400961128     C     *LIKE         DEFINE    ETSCTR        KTSCTR
363500961128     C     *LIKE         DEFINE    ETSPRG        KTSPRG
363600961128     C     *LIKE         DEFINE    ETSLNP        KTSLNP
363700961128     C     *LIKE         DEFINE    ETSSGL        KTSSGL
363800011008     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
363900011008     C     *LIKE         DEFINE    TBEKE1        KTEKE1
364000011017     C*--------------------
364100011017     C* VARIABILI NON RIFERITE AL DATA BASE
364200011017     C*--------------------
364300961128     C*
364400961128     C* CANCELLA EVENTUALI RECORD NEI FILE DI LAVORO
364500970203     C                   Z-ADD     80            LENGH            15 5
364600010926     C   99
364700010926     CANN99              CALL      'QCAEXEC'                                    *TEETW00F
364800970203     C                   PARM                    CMD(1)
364900970203     C                   PARM                    LENGH
365000010926     C   99
365100010926     CANN99              CALL      'QCAEXEC'                                    *TEETWC0F
365200970203     C                   PARM                    CMD(2)
365300970203     C                   PARM                    LENGH
365400961128     C*
365500961128     C* IMPOSTO CAMPI DI GESTIONE PGM
365600961128     C                   MOVEL     'N'           $ERR              1
365700961128     C                   MOVEL     *BLANKS       $MSG             80
365800961128     C*
365900961128     C* RECUPERO IL CODICE DELL'AZIENDA
366000961128     C                   Z-ADD     1             CODUT             1 0
366100021129     C*
366200021129     C* SE OFFERTA, RECUPERA IL CODICE CLIENTE DALLA VISITA (se già associato)
366300091123     c                   if        %subst(d00op0: 1: 1)='O'
366400091123     c                   clear                   tnte04ds
366500091123     C                   movel     D00dsf        d04dsf
366600091123     C                   Z-ADD     D00KSC        d04nrv
366700091123     c                   call      'TNTE04R'
366800091123     c                   parm                    kpjba
366900091126     c                   parm                    tnte04ds
367000091123     C                   Z-ADD     d04KSC        DEPKSC                         *CLIENTE DELLA VISIT
367100021129X   1C                   ELSE                                                   *TARIFFA
367200021129     C                   Z-ADD     D00KSC        DEPKSC                          -CLIENTE TARIFFA
367300021129E   1C                   ENDIF
367400961128     C*
367500961128     C* CARICA LE TABELLE NECESSARIE AL PGM
367600970213     C                   EXSR      CANCWF                                       *CANCELLA WORKFILE
367700961128     C                   EXSR      CARTAB                                       *TABELLE
367800011010     C                   EXSR      CARGEDCE                                     *TABELLE NUOVE
367900011017     C                   EXSR      CARGEDAC                                     *TABELLE NUOVE
368000961128     C                   EXSR      CARCAR                                       *TARIFFE DI CARTELLO
368100961128     C                   EXSR      CARMAG                                       *CARICA MAGGIORAZION
368200961128     C                   EXSR      CARFIL                                       *CARICA TARIFFE FIL
368300961128     C*
368400961128     C                   ENDSR
368500961128     O*------------------------------------------------------------------------*
368600961128** CMD - COMANDI CLP
368700010926CLRPFM FILE(TEETW00F)
368800010926CLRPFM FILE(TEETWC0F)
368900961128
369000961128
369100961128
369200961128** MSG - MESSAGGI
369300961128TNTE10R- Linea partenza in DS non trovata. Avv.CED.                       LPNT 1
369400961128TNTE10R- Tariffa ricarico sede non trovata.                               TRSN 2
369500961128TNTE10R- Tabella '2R' non trovata. Avvertire CED.                         TB2R 3
369600010801TNTE10R- Tabella '1B' non trovata. Avvertire CED.                         TB1B 4
369700961128TNTE10R- Tipo tariffa '01' (colli) non trovata.                           TT01 5
369800961128TNTE10R- Tipo tariffa '02' (n.spedizioni) non trovata.                    TT02 6
369900961128TNTE10R- Tipo tariffa '03' (chilogrammi) non trovata.                     TT03 7
370000961128TNTE10R- Tipo tariffa '04' (volume) non trovata.                          TT04 8
370100961128TNTE10R- Tipo tariffa '09' (stop ritiro) non trovata.                     TT09 9
370200000228TNTE10R- Tipo tariffa '12' (giorni giacenza) non trovata.                 TT1210
370300000228TNTE10R- Tipo tariffa '11' (chilogrammi x gg gicenza) non trovata.        TT1111
370400961128TNTE10R- Tipo tariffa '22' (contrassegno) non trovata.                    TT2212
370500961128TNTE10R- Tipo tariffa '23' (val merce assicur.) non trovata.              TT2313
370600961128TNTE10R- Tipo tariffa '24' (n.giorni pagamento) non trovata.              TT2414
370700961128TNTE10R- Tipo tariffa '25' (imp.comp.sede) non trovata.                   TT2515
370800961128TNTE10R- Tipo tariffa '10' (stop consegna) non trovata.                   TT1016
370900961128TNTE10R- Tariffa ricarico commerciale non trovata.                        TRCN17
371000961128TNTE10R- Voce non trovata.                                                VNTR18
371100961128TNTE10R- Tipo tariffa della voce non trovata in schiera.                  TTNT19
371200961128TNTE10R- Qta x ricerca scaglione non trovata.                             QRSN20
371300961128TNTE10R- Manca tariffa.Voce xxx.999 999                                   MATA21
371400961128TNTE10R- Voce di maggiorazione senza voci da maggiorare.                  VMSV22
371500961128TNTE10R- Tariffa ricarico pagamento non trovata.                          TRPN23
371600961128TNTE10R- Dati tipo del cliente/tariffa non trovati.                       DTCN24
371700961128TNTE10R- Scaglioni del cliente/tariffa non trovati.                       SCCN25
371800961128TNTE10R- Cod. tassazione del cliente/tariffa non trovati.                 CTCN26
371900961128TNTE10R- Cod. tassazione non trovato.                                     CTNT27
372000030428TNTE10R- Tipo tariffa '06' (bancali) non trovata.                         TT0628
372100040611TNTE10R- Tariffa finanziam.export franco DPD non trovata                  TRFT29
372200030428TNTE10R- 29                                                                   30
