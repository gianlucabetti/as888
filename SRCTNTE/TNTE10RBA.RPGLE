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
002200140930     FTEETT11L  IF   E           K DISK    prefix(£)
002300010926     FTEETW01L  UF A E           K DISK
002400010926     FTEETWC1L  UF A E           K DISK
002500961128     FTABEL00F  IF   E           K DISK
002600011008     FTNTBE01L  IF   E           K DISK
002700961128     FAZORG01L  IF   E           K DISK
002800961128     D*------------------------------------------------------------------------*
002900961128     D* SCHIERE
003000961128     D*------------------------------------------------------------------------*
003100141001     d totalects       s             21  8
003200141001     d comodo          s             21  8
003300961128     D*-------------------
003400961128     D* COMANDI
003500961128     D*-------------------
003600961128     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003700961128     D*-------------------
003800961128     D* MESSAGGI
003900961128     D*-------------------
004000961128     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
004100961128     D*-------------------
004200970204     D* VOCI (INDICE=CODICE VOCE)
004300961128     D*-------------------
004400040524     D VOCA            S              3  0 DIM(300)                             *COD.VOCE CARICATA
004500961128     D VPRG            S              3  0 DIM(999)                             *PROGRESSIVO VOCE
004600961128     D VTVO            S              1    DIM(999)                             *TIPO VOCE
004700961128     D VDDV            S              8  0 DIM(999)                             *DATA DECORRENZA VOC
004800961128     D VDSV            S              8  0 DIM(999)                             *DATA SCADENZA VOCE
004900961128     D VVRV            S              2    DIM(999)                             *TIPO TARIFFA VOCE '
005000961128     D VTIV            S              2    DIM(999)                             *TIPO RICERCA SCAGL'
005100961128     D VSPT            S              1    DIM(999)                             *SVILUPPO PESO TASSA
005200961128     D VCMP            S              3  0 DIM(999)                             *COD.COMPETENZA
005300961128     D VRFI            S              2    DIM(999)                             *RIF.TARIF.FIL.ITA
005400961128     D VRFE            S              2    DIM(999)                             *RIF.TARIF.FIL.EST
005500961128     D VRTI            S              2    DIM(999)                             *RIF.TARIF.TASS.ITA
005600961128     D VRTE            S              2    DIM(999)                             *RIF.TARIF.TASS.EST
005700961128     D VRPI            S              2    DIM(999)                             *RIF.TARIF.CAP.ITA
005800961128     D VRPE            S              2    DIM(999)                             *RIF.TARIF.CAP.EST
005900961128     D VRKI            S              2    DIM(999)                             *RIF.TARIF.CLIENTE.I
006000961128     D VRKE            S              2    DIM(999)                             *RIF.TARIF.CLIENTE.E
006100011025     D VARF            S             21  8 DIM(999)                             *ARROTONDAM. FINO LI
006200011025     D VARL            S             21  8 DIM(999)                             *ARROTONDAM. LIMITE
006300011025     D VARO            S             21  8 DIM(999)                             *ARROTONDAM. OLTRE L
006400011025     D VIMP            S             21  8 DIM(999)                             *IMPORTO CALCOLATO V
006500011025     D VIM             S             21  8 DIM(999)                             *IMPORTO DA SCR. SU
006600030428     D VRPV            S              3  1 DIM(999)
006700030428     D VSPS            S              1    DIM(999)
006800961128     D VC              S              1    DIM(999)                             *VOCE DA CALCOLARE S
006900961128     D VV              S              1    DIM(999)                             *VOCE VALIDA S/N
007000000215     D*
007100970220     D VP              S              9  5 DIM(999)                             *% DA CONSIDERARE
007200961128     D VI              S              1    DIM(999)                             *INGLOBA NEL PORTO S
007300040811     D vb              S              1    DIM(999)                             *INGLOBA NEL PORTO S
007400970207     D VS              S              1  0 DIM(999)                             *SEQUENZA CALCOLO
007500961128     D VRDL            S              3  0 DIM(999)                             *RAGGRUPPAMENTO DELT
007600961128     D*------------------
007700961128     D* IMPORTI RAGGRUPPAMENTI DELTA
007800961128     D*------------------
007900011025     D IRDL            S             21  8 DIM(50)
008000961128     D*-------------------
008100961128     D* SCAGLIONI
008200961128     D*-------------------
008300011025     D SSGL            S             21  8 DIM(10)                              *SCAGLIONE SUPERIORE
008400961128     D SPSC            S              7  1 DIM(10)                              *PESO RIF. CALCOLO
008500961128     D SCSC            S              7  0 DIM(10)                              *COLLI RIF. CALCOLO
008600961128     D SVSC            S              7  3 DIM(10)                              *VOLUME RIF. CALCOLO
008700961128     D SPPE            S              4  1 DIM(10)                              *% PESO
008800011025     D SINL            S             21  8 DIM(10)                              *IMPORTO INOLTRO
008900970402     D SSDE            S              1    DIM(10)                              *SEGNO DELTA
009000970402     D SPDE            S              4  1 DIM(10)                              *% DELTA
009100970317     D SRPV            S              3  1 DIM(10)                              *RAPP. PESO/VOL.
009200961128     D*-------------------
009300961128     D* CODICI TASSAZIONE
009400961128     D*-------------------
009500050103     D TCTS            S              2    DIM(600)                             *CODICE
009600050103     D TRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
009700050103     D TPPE            S              4  1 DIM(600)                             *% PESO
009800050103     D TLNA            S              3  0 DIM(600)                             *LINEA ARRIVO
009900050103     D TTFA            S              3  0 DIM(600)                             *TERMINAL ARRIVO
010000050103     D TRAP            S              2    DIM(600)                             *REGIONE APPARTENENZ
010100050103     D TNAR            S              2    DIM(600)                             *NAZIONE APPARTENENZ
010200961128     D*-------------------
010300961128     D* TARIFFE DI CARTELLO
010400961128     D*-------------------
010500021112     D CVOC            S              3  0 DIM(300)                             CODICE TARIFFA
010600021112     D CPRG            S              3  0 DIM(300)                             *PROGRESSIVO TARIFFA
010700021112     D CFT1            S              3  0 DIM(300)                             *FILIALE ... PARTENZ
010800021112     D CFT2            S              3  0 DIM(300)                             *FILIALE ... ARRIVO
010900021112     D CCLI            S              7  0 DIM(300)                             *CLIENTE
011000021112     D CCTR            S              3  0 DIM(300)                             *TARIFFA
011100021112     D CCTS            S              2    DIM(300)                             *CODICE TASSAZIONE
011200140115     D Ccad            S              9    DIM(300)
011300140115     D Cnaz            S              3    DIM(300)
011400021112     D CDDT            S              8  0 DIM(300)                             *DATA DECORRENZA
011500021112     D CDST            S              8  0 DIM(300)                             *DATA SCADENZA
011600021112     D CSGI            S             21  8 DIM(300)                             *SCAGLIONE INFERIORE
011700021112     D CSGF            S             21  8 DIM(300)                             *SCAGLIONE SUPERIORE
011800021112     D CITR            S             21  8 DIM(300)                             *IMPORTO UNITARIO
011900021112     D CIMI            S             21  8 DIM(300)                             *IMPORTO MINIMO TARI
012000021112     D CIMA            S             21  8 DIM(300)                             *IMPORTO MINIMO TARI
012100021112     D CSEG            S              1    DIM(300)                             *SEGNO IMPORTO TARIF
012200021112     D CPER            S              5  2 DIM(300)                             *PERCENTUALE DA APPL
012300021112     D CRPV            S              3  1 DIM(300)                             *RAPPORTO PESO/VOLUM
012400961128     D*-------------------
012500961128     D* TARIFFE DELL'ULTIMA FILIALE DEL CLIENTE
012600961128     D*-------------------
012700021112     D FVOC            S              3  0 DIM(600)                             CODICE TARIFFA
012800021112     D FPRG            S              3  0 DIM(600)                             *PROGRESSIVO TARIFFA
012900021112     D FFT1            S              3  0 DIM(600)                             *FILIALE ... PARTENZ
013000021112     D FFT2            S              3  0 DIM(600)                             *FILIALE ... ARRIVO
013100021112     D FCLI            S              7  0 DIM(600)                             *CLIENTE
013200021112     D FCTR            S              3  0 DIM(600)                             *TARIFFA
013300021112     D FCTS            S              2    DIM(600)                             *CODICE TASSAZIONE
013400140115     D Fcad            S              9    DIM(600)
013500140115     D Fnaz            S              3    DIM(600)
013600021112     D FDDT            S              8  0 DIM(600)                             *DATA DECORRENZA
013700021112     D FDST            S              8  0 DIM(600)                             *DATA SCADENZA
013800021112     D FSGI            S             21  8 DIM(600)                             *SCAGLIONE INFERIORE
013900021112     D FSGF            S             21  8 DIM(600)                             *SCAGLIONE SUPERIORE
014000021112     D FITR            S             21  8 DIM(600)                             *IMPORTO UNITARIO
014100021112     D FIMI            S             21  8 DIM(600)                             *IMPORTO MINIMO TARI
014200021112     D FIMA            S             21  8 DIM(600)                             *IMPORTO MINIMO TARI
014300021112     D FSEG            S              1    DIM(600)                             *SEGNO IMPORTO TARIF
014400021112     D FPER            S              5  2 DIM(600)                             *PERCENTUALE DA APPL
014500021112     D FRPV            S              3  1 DIM(600)                             *RAPPORTO PESO/VOLUM
014600961128     D*-------------------
014700961128     D* MAGGIORAZIONI
014800961128     D*-------------------
014900050104     D MVOM            S              3  0 DIM(600)                             *CODICE VOCE MAGG.
015000050104     D MPRM            S              3  0 DIM(600)                             *PROGRESSIVO VOCE MA
015100050104     D MVOC            S              3  0 DIM(600)                             *CODICE VOCE DA MAGG
015200050104     D MPRG            S              3  0 DIM(600)                             *PROGRESSIVO VOCE DA
015300140930     c*
015400140930     D scad            DS                  OCCURS(500) INZ
015500140930     D  naz                    1   1800
015600140930     D                                     DIM(600)                             *N°SPED.SCAGLIONE
015700140930     D  cad                 1801   7200
015800140930     D                                     DIM(600)                             *PESO
015900140930     D  inc                 7201  10200P 5
016000140930     D                                     DIM(600)                             *PESO
016100140930     D*-------------------
016200140930     D* nazione
016300140930     D*-------------------
016400140930     D d15             S              3    DIM(1000)
016500140930     D ecf             S              1  0 DIM(1000)
016600140930     D pcf             S              1  0 DIM(1000)
016700961128     D*-------------------
016800961128     D* TIPI TARIFFA
016900961128     D*-------------------
017000961128     D C2R             S              2    DIM(100)                             *TIPO TARIFFA
017100961128     D T2R             S              1    DIM(100)                             *TIPO VALORIZ. Q,V,F
017200011025     D Q2R             S             21  8 DIM(100)                             *QUANTITA'
017300971211     D*****************************************************************
017400971211     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
017500971211     D FL1             S              1    DIM(999)                             *FLAG ITALIA/ESTERO
017600980710     D DIT             S              3    DIM(999)                             *FLAG BAR/SDI
017700000215     D DPD             S              1    DIM(999)                             *FLAG FILIALE DPD (S
017800020503     D NTW             S              3    DIM(999)                             *NETWORK
017900010801     D CTS             S              2    DIM(999)                             *C.TASSAZIONE FILIAL
018000010801     D*-------------------
018100010801     D* TRATTAMENTO MERCE
018200010801     D*-------------------
018300010801     D CTM             S              2    DIM(100)                             *CODCICE
018400010801     D CF12            S              1    DIM(100)                             *FLAG "DISK C"
018500961128     D*------------------------------------------------------------------------*
018600961128     D* INPUT
018700961128     D*------------------------------------------------------------------------*
018800091123     D tnte04ds      E DS
018900140115     D ecevdds       E DS                  extname(ecevd06l)
019000030428     D*-------------------
019100030428     D* DS LETTURA CAMPO EVTXXX
019200030428     D*-------------------
019300030428     D DEVTXXX       E DS
019400961128     D*-------------------
019500961128     D* DS TIPI TARIFFA
019600961128     D*-------------------
019700961128     D DS2R          E DS
019800140930     D*-------------------
019900140930     D* DS ESTERNA TABELLA 15 (CODICI nazioni)
020000140930     D*-------------------
020100140930     D DS15          E DS
020200961128     D*-------------------
020300961128     D* DS CODICI TASSAZIONE
020400961128     D*-------------------
020500961128     D DSCT          E DS
020600000215     D*-------------------
020700000215     D* DS ESTERNA DESCRIZIONE 43 ORGANIGRAMMA
020800000215     D*-------------------
020900000215     D OG143         E DS
021000010801     D*-------------------
021100010801     D* DS ESTERNA TABELLA 7O (CODICI TRATTAMENTO MERCE)
021200010801     D*-------------------
021300010801     D DS1B          E DS
021301150326     D*-------------------
021302150326     D Detcflo       E DS
021400961128     D*-------------------
021500961128     D* DS SCOMPOSIZIONE DATA IN ANNO/MESE/GIONRO
021600961128     D*-------------------
021700961128     D DS§DAT          DS
021800961128     D  DSSEC                  1      2  0
021900961128     D  DSANN                  3      4  0
022000961128     D  DSAAS                  1      4  0
022100961128     D  DSMES                  5      6  0
022200961128     D  DSGIO                  7      8  0
022300961128     D  DSMGS                  5      8  0
022400961128     D  DSDAT6                 3      8  0
022500961128     D  DSDAT8                 1      8  0
022600961128     D DS§TIM          DS
022700961128     D  DTORA                  1      2  0
022800961128     D  DTMIN                  3      4  0
022900961128     D  DTSCN                  5      6  0
023000961128     D  DTGIO                  7      8  0
023100961128     D  DTMES                  9     10  0
023200961128     D  DTSEC                 11     12  0
023300961128     D  DTANN                 13     14  0
023400961128     D  DTORA6                 1      6  0
023500961128     D  DTTIME                 1     14  0
023600961128     D*-------------------
023700961128     D* DS MESSAGGIO MANCA TARIFFA
023800961128     D*-------------------
023900961128     D                 DS
024000961128     D  DSMSG                  1     80
024100961128     D  DSVOCE                29     31
024200961128     D  DSTT1                 33     35
024300961128     D  DSTT2                 37     39
024400970210     D  DSCTE                 41     42
024500970210     D  DSNAR                 61     63
024600970210     D  DSCAP                 65     74
024700970210     D  DSCTS                 70     71
024800961128     D  DSDER                  1     74
024900961128     D  DSCER                 75     78
025000970910     D*------------------
025100970910     D* DS "TISI95R" - CONTROLLO CAP
025200970910     D*------------------
025300970910     D DSSI95        E DS                  EXTNAME(TISI95DS)
025400970910     D*------------------
025500970910     D* DS "FNLV55R" - REPERIMENTO TERMINALs
025600970910     D*------------------
025700970910     D DSLV55        E DS                  EXTNAME(FNLV55DS)
025800961128     D*-------------------
025900961128     D* DS ESTERNA IN INPUT AL PGM
026000961128     D*-------------------
026100961128     D DSTE00        E DS                  EXTNAME(TNTE00DS)
026200970214     D*-------------------
026300970214     D* INFDS - INFORMAZIONI PROGRAMMA
026400970214     D*-------------------
026500970213     D                SDS
026600980909     D  JOB                  244    253
026700970213     D  USER                 254    263
026800961128     D*-------------------
026900961128     D* ARCHITETTURA
027000961128     D*-------------------
027100961128     D KPJBA         E DS
027200011008     D*------------------
027300011008     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
027400011008     D*------------------
027500011008     D DSGEDCE       E DS
027600011017     D*------------------
027700011017     D* DS LETTURA TABELLA "GED" TIPO RECORD "AC"
027800011017     D*------------------
027900011017     D DGEDAC        E DS
028000011008     D*-------------------
028100011008     D* CONVERSIONI IMPORTI
028200011008     D*-------------------
028300011008     D YEUR          E DS                  EXTNAME(YEURCODS)
028400020729     D*------------------
028500020729     D* DS PER LETTURA TABELLA "NTW"
028600020729     D*------------------
028700020729     D DNTW          E DS
028800961128     C*------------------------------------------------------------------------*
028900961128     C* MAIN LINE
029000961128     C*------------------------------------------------------------------------*
029100961128     C*
029200961128     C* ESEGUE ROUTINE INIZIALE
029300961128     C                   EXSR      RUTINZ
029400961128     C*
029500961128     C* SE ERRORE RUTINZ ESCO PGM
029600961128     C     $ERR          IFEQ      'N'
029700970910     C*
029800970910     C* RECUPERA INSTRADAMENTO -PARTENZA-
029900970910     C     $ERR          IFEQ      'N'
030000970910     C                   EXSR      REPINP
030100970910     C                   ENDIF
030200970131     C*
030300970131     C* IMPOSTA LA SEQUENZA IN TUTTE LE VOCI
030400970131     C     $ERR          IFEQ      'N'
030500970131     C                   EXSR      IMPSEQ
030600970131     C                   END
030700961128     C*
030800961128     C* CARICA DATI TIPO
030900961128     C     $ERR          IFEQ      'N'
031000961128     C                   EXSR      CARETC
031100961128     C                   END
031200961128     C*
031300961128     C* CARICA COD.TASSAZIONE
031400961128     C     $ERR          IFEQ      'N'
031500961128     C                   EXSR      CARETT
031600961128     C                   END
031700961128     C*
031800961128     C* CARICA SCAGLIONI
031900961128     C     $ERR          IFEQ      'N'
032000961128     C                   EXSR      CARETS
032100961128     C                   END
032200961128     C*
032300961128     C* CALCOLA VALORI TABELLA '2R' (QTA TIPO TARIFFA) PER SEQ. 1
032400961128     C     $ERR          IFEQ      'N'
032500961128     C                   EXSR      CAL2R1
032600961128     C                   END
032700961128     C*
032800970207     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE E CONTROLLA VALIDITA
032900961128     C     $ERR          IFEQ      'N'
033000961128     C                   EXSR      CHKVOC
033100961128     C                   END
033200961128     C*
033300961128     C* VALORIZZA LE VOCI DA CALCOLARE
033400961128     C     $ERR          IFEQ      'N'
033500961128     C                   Z-ADD     1             WSEQ              6 0
033600961128     C                   EXSR      VALVOC
033700961128     C                   END
033800961128     C*
033900970225     C* CICLO PER TUTTI GLI SCAGLIONI ----------------------------------------->
0340009611282    C                   DO        WULTSC        S                 6 0
0341009611283    C     SSGL(S)       IFNE      *ZEROS
034200980903     C*
034300980903     C* CERCA LA LINEA ARRIVO E IL TEMINAL ARRIVO PER OGNI COD.TASSAZ. UTILIZZANDO
034400980903     C* ANCHE IL PESO DELLO SCAGLIONE (SE PESO > XX CONSEGNA SDI)
034500980903     C     $ERR          IFEQ      'N'
034600980903     C                   EXSR      CERLNA
034700980903     C                   END
034800961128     C*
034900961128     C* CALCOLA VALORI TABELLA '2R' (QTA TIPO TARIFFA) PER SEQ. 2
035000961128     C     $ERR          IFEQ      'N'
035100961128     C                   EXSR      CAL2R2
035200961128     C                   END
035300961128     C*
035400970207     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE E CONTROLLA VALIDITA
035500961128     C     $ERR          IFEQ      'N'
035600961128     C                   EXSR      CHKVOC
035700961128     C                   END
035800961128     C*
035900961128     C* VALORIZZA LE VOCI DA CALCOLARE
036000961128     C     $ERR          IFEQ      'N'
036100961128     C                   Z-ADD     2             WSEQ
036200961128     C                   EXSR      VALVOC
036300961128     C                   END
036400961128     C*
036500970225     C* CICLO PER TUTTI I CODICI TASSAZIONE ARRIVO ---------------------------->
0366009611284    C                   DO        WULTCT        T                 6 0
0367009611285    C     TCTS(T)       IFNE      *BLANKS
036800961128     C*
036900970207     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE E CONTROLLA VALIDITA
037000961128     C     $ERR          IFEQ      'N'
037100961128     C                   EXSR      CHKVOC
037200961128     C                   END
037300000704     C*
037400000704     C* DETERMINA PERCENTUALI PER FORMULA (% ESPRESSO,COMM.LE,PAGAM.)
037500000704     C     $ERR          IFEQ      'N'
037600000704     C                   EXSR      PERFOR
037700000704     C                   END
037800961128     C*
037900961128     C* VALORIZZA LE VOCI DA CALCOLARE
038000961128     C     $ERR          IFEQ      'N'
038100961128     C                   Z-ADD     3             WSEQ
038200961128     C                   EXSR      VALVOC
038300961128     C                   END
038400961128     C*
038500970206     C* CALCOLA FORMULA RICARICHI + RAGGUPPAMENTO DELTA
038600961128     C     $ERR          IFEQ      'N'
038700961128     C                   EXSR      CALTOT
038800961128     C                   END
038900961128     C*
039000011017     C* SCRIVE IL FILE DI LAVORO TEETW
039100961128     C     $ERR          IFEQ      'N'
039200961128     C                   EXSR      SCRETW
039300961128     C                   END
039400961128     C*
039500010926     C* SCRIVE IL FILE DI LAVORO TEETWC SE LANCIO IN DEBUG
039600961128     C     $ERR          IFEQ      'N'
039700970207     C     D00DBG        ANDEQ     'S'
039800970207     C                   EXSR      SCREWC
039900970207     C                   END
040000961128     C*
0401009611285    C                   ENDIF                                                  *C.TASS=0
0402009611284    C                   ENDDO                                                  *SEQ. 3
040300961128     C*
0404009611283    C                   ENDIF                                                  *SCAGL=0
0405009611282    C                   ENDDO                                                  *SEQ. 2
040600961128     C*
0407009611281    C                   ENDIF                                                  *IF ERR
040800961128     C*
040900970910     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
041000970910     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
041100970910     C                   MOVEL     'C'           I95TLA
041200970910     C                   CALL      'TISI95R'
041300970910     C                   PARM                    DSSI95
041400970910     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
041500970910     C                   MOVEL     'C'           D55TLA
041600970910     C                   CALL      'FNLV55R'
041700970910     C                   PARM                    DSLV55
041800961128     C                   SETON                                        LR
041900970910     C*------------------------------------------------------------------------*
042000970910     C* REPINP - RECUPERA INTRADAMENTO -PARTENZA-
042100970910     C*------------------------------------------------------------------------*
042200970910     C     REPINP        BEGSR
042300970910     C*
042400970910     C* TERMINAL PARTENZA
042500970910     C                   CLEAR                   DSLV55
042600970910     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
042700970910     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
042800970910     C                   Z-ADD     D00LNP        D55LIN                         *LINEA
042900970910     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
043000970910     C                   CALL      'FNLV55R'
043100970910     C                   PARM                    DSLV55
043200970910IF  1C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
043300970910     C                   MOVEL     D55MSG        $MSG
043400970910     C                   EXSR      SCRERR                                       *IMPOSTA ERRORE
043500970910X   1C                   ELSE                                                   *TROVATO
043600970910     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
043700970910E   1C                   ENDIF
043800970910     C*
043900970910     C* CODICE TASSAZIONE PARTENZA
044000970910     C                   Z-ADD     D00LNP        KORFIL
044100970910     C     KEYORG        CHAIN     AZORG01L                           98
044200970910IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATO
044300970910     C                   MOVEL     MSG(1)        $MSG
044400970910     C                   EXSR      SCRERR                                       *IMPOSTA ERRORE
044500970910X   1C                   ELSE                                                   *TROVATA
044600970910     C                   MOVEL     ORGCTS        WCTP                           *CTS MITTENTE
044700970910E   1C                   ENDIF
044800970910     C*
044900970910     C                   ENDSR
045000970211     C*------------------------------------------------------------------------*
045100970211     C* IMPSEQ -IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
045200970211     C*------------------------------------------------------------------------*
045300970211     C     IMPSEQ        BEGSR
045400970211     C*
045500970211     C* LEGGO TUTTO IL FILE TESTATE VOCI E PER TUTTE LE VOCI VALIDE
045600970211     C* ALLA DATA IN INPUT IMPOSTO LA SEQUENZA
045700970211     C                   Z-ADD     *ZEROS        I
045800970211     C                   Z-ADD     *ZEROS        EVTVOC
045900970211     C                   Z-ADD     *ZEROS        EVTPRG
046000970211     C     KEYEVT        SETLL     ECEVT01L                           99
046100970211     C  N99              READ      ECEVT01L                               99
0462009702111    C     *IN99         DOWEQ     '0'                                          *FINE FILE
0463009702112    C     EVTATB        IFEQ      *BLANKS                                      *NON E' ANNULLATO
046400970211     C     D00DTC        ANDGE     EVTDDV                                       *MAGG. DECORRENZA
046500970211     C     D00DTC        ANDLE     EVTDSV                                       *MIN. SCADENZA
046600970211     C                   ADD       1             I
046700970211     C                   Z-ADD     EVTVOC        VOCA(I)
046800970211     C                   Z-ADD     EVTVOC        V
046900970211     C                   Z-ADD     EVTPRG        VPRG(V)
047000970211     C                   MOVEL     EVTTVO        VTVO(V)
047100970211     C                   Z-ADD     EVTDDV        VDDV(V)
047200970211     C                   Z-ADD     EVTDSV        VDSV(V)
047300970211     C                   MOVEL     EVTVRV        VVRV(V)
047400970211     C                   MOVEL     EVTTIV        VTIV(V)
047500970211     C                   MOVEL     EVTSPT        VSPT(V)
047600970211     C                   Z-ADD     EVTCMP        VCMP(V)
047700970211     C                   MOVEL     EVTRFI        VRFI(V)
047800970211     C                   MOVEL     EVTRFE        VRFE(V)
047900970211     C                   MOVEL     EVTRTI        VRTI(V)
048000970211     C                   MOVEL     EVTRTE        VRTE(V)
048100970211     C                   MOVEL     EVTRPI        VRPI(V)
048200970211     C                   MOVEL     EVTRPE        VRPE(V)
048300970211     C                   MOVEL     EVTRKI        VRKI(V)
048400970211     C                   MOVEL     EVTRKE        VRKE(V)
048500970211     C                   Z-ADD     EVTARF        VARF(V)
048600970211     C                   Z-ADD     EVTARL        VARL(V)
048700970211     C                   Z-ADD     EVTARO        VARO(V)
048800970211     C                   Z-ADD     EVTRDL        VRDL(V)                        *RAGGR.DELTA
048900030428     C                   MOVEL     EVTXXX        DEVTXXX
049000030428     C                   Z-ADD     EVTRPV        VRPV(V)
049100030428     C                   MOVEL     EVTSPS        VSPS(V)
049200970211     C                   EXSR      SEQVOC                                       *DETERMINA SEQ.CALC.
0493009702112    C                   ENDIF
049400970211     C                   READ      ECEVT01L                               99
0495009702111    C                   ENDDO
049600970211     C*
049700970211     C* MEMORIZZA N.ELEMENTI SCHIERA VOCI CARICATE
049800970211     C                   Z-ADD     I             NVOCA             3 0          *N.ELEM. VOCI CAR.
049900970211     C*
050000970211     C* X LE VOCI FORMULONE (SEDE COMM.+PAGAM.) E DELTA FORZO SEQ. 3
050100970211     C* PERCHE SONO CALCOLATE PER OGNI C.TASS. E PER OGNI SCAGL.
050200980710     C                   Z-ADD     3             VS(270)                        *UFF.COMM. BART
050300040609     C                   Z-ADD     3             VS(305)                        *UFF.COMM. BART
050400970211     C                   Z-ADD     3             VS(300)                        *INCASSO E RISCHI FI
050500970211     C                   Z-ADD     3             VS(900)                        *DELTA
050600970211     C*
050700990422     C* X LE VOCI ATTIVATE IN BASE ALLA LINEA DI ARR. FORZO SEQ.3
050800990422     C* LE VOCI DIPENDENTI DAI COD.TASSAZIONE SONO GIA' FORZATE IN SEQVOC
050900990422     C* LE MAGGIORAZIONI NON IMPORTA METTERLE PERCHE' SONO GIA' FORZATE IN SEQVOC
051000970211     C                   Z-ADD     3             VS(71)                         *UFF.PARTENZA EXPORT
051100000228     C                   Z-ADD     3             VS(74)                         *UFF.PARTENZA EXPORT
051200010801     C                   Z-ADD     3             VS(275)                        *ASSISTENZA IMP/EXP
051300010801     C                   Z-ADD     3             VS(276)                        *ASSISTENZA IMP/EXP
051400040610     C                   Z-ADD     3             VS(288)                        *UFF.PARTENZA EXPORT
051500030429     C                   Z-ADD     3             VS(166)                        *MONOCOLLO GERMANIA
051600040610     C                   Z-ADD     3             VS(170)                        *MONOCOLLO GERMANIA
051700970211     C                   Z-ADD     3             VS(171)                        *ARR.EXPORT (X SPED.
051800970211     C                   Z-ADD     3             VS(172)                        *ARR.EXPORT (A PESO)
051900130529     C                   Z-ADD     3             VS(173)                        *ARR.EXPORT (A PESO)
052000000228     C                   Z-ADD     3             VS(175)                        *ARR.EXPORT DPD (A C
052100030428     C                   Z-ADD     3             VS(176)                        *ARR.EXPORT DPD (A P
052200030430     C                   Z-ADD     3             VS(231)                        *TRAINI DEFL. ESTERO
052300000228     C                   Z-ADD     3             VS(233)                        *TRAINI DEFL. ESTERO
052400970211     C                   Z-ADD     3             VS(114)                        *SCARICO DEFL.EXPORT
052500970211     C                   Z-ADD     3             VS(124)                        *SMISTAM.DEFL.EXPORT
052600970211     C                   Z-ADD     3             VS(152)                        *CARICO DEFL.EXPORT
052700970211     C                   Z-ADD     3             VS(230)                        *TRAINI DEFLUENZA
052800970211     C                   Z-ADD     3             VS(111)                        *SCARICO DEFLUENZA
052900970211     C                   Z-ADD     3             VS(121)                        *SMISTAM.DEFLUENZA
053000970211     C                   Z-ADD     3             VS(151)                        *CARICO DEFLUENZA
053100010112     C                   Z-ADD     3             VS(161)                        *CARRIAG.CONS.NEW
053200010112     C                   Z-ADD     3             VS(177)                        *STOP CONS.NEW
053300000228     C                   Z-ADD     3             VS(251)                        *C/AS UFF.ARR
053400000228     C                   Z-ADD     3             VS(252)                        *C/AS PADR.ARR
053500000228     C                   Z-ADD     3             VS(256)                        *C/AS ARR.EXP (A VAL
053600000228     C                   Z-ADD     3             VS(257)                        *C/AS ARR.EXP (A %)
053700000228     C                   Z-ADD     3             VS(280)                        *C/AS ASSIST.EXP
053800100422     C                   Z-ADD     3             VS(810)                        *SEDE-CORR-CL.A
053900100422     C                   Z-ADD     3             VS(811)                        *SEDE-CORR-CL.B
054000100422     C                   Z-ADD     3             VS(812)                        *SEDE-CORR-CL.C
054100100422     C                   Z-ADD     3             VS(819)                        *SEDE-CORR-CL.T
054200100422     C                   Z-ADD     3             VS(813)                        *SEDE-ESPR-CL.A
054300100422     C                   Z-ADD     3             VS(814)                        *SEDE-ESPR-CL.B
054400100422     C                   Z-ADD     3             VS(815)                        *SEDE-ESPR-CL.C
054500100422     C                   Z-ADD     3             VS(820)                        *SEDE-ESPR-CL.T
054600100422     C                   Z-ADD     3             VS(816)                        *SEDE-DIST-CL.A
054700100422     C                   Z-ADD     3             VS(817)                        *SEDE-DIST-CL.B
054800100422     C                   Z-ADD     3             VS(818)                        *SEDE-DIST-CL.C
054900100422     C                   Z-ADD     3             VS(821)                        *SEDE-DIST-CL.T
055000100422     C                   Z-ADD     3             VS(822)                        *SEDE 88/99
055100100422     C                   Z-ADD     3             VS(823)                        *SEDE-EXP.DPD
055200011030     C                   Z-ADD     3             VS(278)                        *CONSEGNA ISOLA
055300011030     C                   Z-ADD     3             VS(279)                        *RITIRI   ISOLA
055400040621     C                   Z-ADD     3             VS(285)                        *RITIRI   ISOLA
055500051107     C                   Z-ADD     3             VS(323)                        *RITIRI   ISOLA
055600051107     C                   Z-ADD     3             VS(327)                        *RITIRI   ISOLA
055700051107     C                   Z-ADD     3             VS(328)                        *RITIRI   ISOLA
055800070522     C                   Z-ADD     3             VS(331)                        *RITIRI   ISOLA
055900070523     C                   Z-ADD     3             VS(268)                        *RITIRI   ISOLA
056000081031     C                   Z-ADD     3             VS(340)                        *RITIRI   ISOLA
056100090122     C                   Z-ADD     3             VS(332)                        *ARR.EXPORT (X SPED.
056200980710     C*
056300970211     C                   ENDSR
056400970211     C*------------------------------------------------------------------------*
056500970211     C* SEQVOC - DETERMINA LA SEQUENZA DI CALCOLO DI UNA VOCE
056600970211     C*           1 = VOCI NON DIPENDENTI DA SCAGLIONI O CT/LN/TER ARRIVO
056700970211     C*           2 = VOCI NON DIPENDENTI DA CT/LN/TER ARRIVO MA DIPENDENTI
056800970211     C*               DA SCAGLIONI
056900970211     C*           3 = VOCI DIPENDENTI DA SCAGLIONI E CT/LN/TER ARRIVO
057000970211     C*------------------------------------------------------------------------*
057100970211     C     SEQVOC        BEGSR
057200970211     C*
057300970211     C                   Z-ADD     EVTVOC        V
057400970211     C                   Z-ADD     1             VS(V)
057500970211     C* CONTROLLO SE DIPENDENTE DAL TIPO TARIFFA O DAGLI SCAGLIONI
057600970211     C* CAMPI: TIPO TARIFFA VOCE O TIPO RICERCA SCAGLIONI
057700970211     C     EVTVRV        IFEQ      '00'                                         *RIF.VOCE=QLI
057800970211     C     EVTVRV        OREQ      '01'                                         *RIF.VOCE=COLLI
057900970211     C     EVTVRV        OREQ      '03'                                         *RIF.VOCE=KG
058000970211     C     EVTVRV        OREQ      '04'                                         *RIF.VOCE=VOLUME
058100030429     C     EVTVRV        OREQ      '05'                                         *RIF.VOCE=KG TASSAB
058200000228     C     EVTVRV        OREQ      '11'                                         *RIF.VOCE=KG X GG GI
058300970211     C     EVTTIV        OREQ      '00'                                         *RIC.SCAG=QLI
058400970211     C     EVTTIV        OREQ      '01'                                         *RIC.SCAG=COLLI
058500970211     C     EVTTIV        OREQ      '03'                                         *RIC.SCAG=KG
058600970211     C     EVTTIV        OREQ      '04'                                         *RIC.SCAG=VOLUME
058700030429     C     EVTTIV        OREQ      '05'                                         *RIC.SCAG=KG TASSAB
058800000228     C     EVTTIV        OREQ      '11'                                         *RIC.SCAG=KG X GG GI
058900970211     C                   Z-ADD     2             VS(V)
059000970211     C                   ENDIF
059100970211     C*
059200970211     C* CONTROLLO SE DIPENDENTE DA DATI DI ARRIVO
059300970211     C* CAMPI: RIFERIMENTO TARIFFA FIL/C.TASS/CAP
059400970211     C* RIFERIMENTO TARIFFA FILIALE ITALIA
059500970211     C     EVTRFI        IFEQ      'LA'                                         *LINEA ARR.
059600970211     C     EVTRFI        OREQ      'TA'                                         *TERM. ARR.
059700970211     C     EVTRFI        OREQ      'TL'                                         *DEFL.
059800970211     C     EVTRFI        OREQ      'TT'                                         *TRAINO
059900970211     C     EVTRFI        OREQ      'T1'                                         *TRANSITO 1
060000970211     C     EVTRFI        OREQ      'T2'                                         *TRANSITO 2
060100040608     C     EVTRFI        OREQ      'T3'                                         *TRANSITO 2
060200040608     C     EVTRFI        OREQ      'T4'                                         *TRANSITO 2
060300040608     C     EVTRFI        OREQ      'T5'                                         *TRANSITO 2
060400970211     C*
060500970211     C* RIFERIMENTO TARIFFA FILIALE ESTERO
060600970211     C     EVTRFE        OREQ      'LA'                                         *LINEA ARR.
060700970211     C     EVTRFE        OREQ      'TA'                                         *TERM. ARR.
060800970211     C     EVTRFE        OREQ      'TL'                                         *DEFL.
060900970211     C     EVTRFE        OREQ      'TT'                                         *TRAINO
061000970211     C     EVTRFE        OREQ      'T1'                                         *TRANSITO 1
061100970211     C     EVTRFE        OREQ      'T2'                                         *TRANSITO 2
061200040608     C     EVTRFE        OREQ      'T3'                                         *TRANSITO 2
061300040608     C     EVTRFE        OREQ      'T4'                                         *TRANSITO 2
061400040608     C     EVTRFE        OREQ      'T5'                                         *TRANSITO 2
061500970211     C*
061600970211     C* RIFERIMENTO TARIFFA C.TASSAZ. ITALIA
061700970211     C     EVTRTI        OREQ      'SA'                                         *C.TASS. ARRIVO
061800970211     C*
061900970211     C* RIFERIMENTO TARIFFA C.TASSAZ. ESTERO
062000970211     C     EVTRTE        OREQ      'SA'                                         *C.TASS. ARRIVO
062100970211     C*
062200970211     C* RIFERIMENTO TARIFFA CAP ITALIA
062300970211     C     EVTRTI        OREQ      'PA'                                         *CAP ARRIVO
062400970211     C*
062500970211     C* RIFERIMENTO TARIFFA CAP ESTERO
062600970211     C     EVTRTE        OREQ      'PA'                                         *CAP ARRIVO
062700970211     C*
062800970211     C* SE E' UNA MAGGIORAZIONE
062900970211     C     EVTTVO        OREQ      'M'                                          *MAGGIORAZIONE
063000970211     C                   Z-ADD     3             VS(V)
063100970211     C                   ENDIF
063200970211     C*
063300970211     C                   ENDSR
063400970211     C*------------------------------------------------------------------------*
063500970211     C* CARETC - CARICA DATI TIPO
063600970211     C*------------------------------------------------------------------------*
063700970211     C     CARETC        BEGSR
063800970211     C*
063900091123     C                   MOVEL     'S'           KTCDSF                         *SEDE/FIL
064000970211     C                   MOVEL     D00CTO        KTCCTO                         *TARIF/OFF
064100970211     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE
064200970211     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
064300970211     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
064400970211     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
064500011008     C     KEYETC        CHAIN     TEETC01L                           99
064600970211     C     *IN99         IFEQ      *ON
064700970211     C                   MOVEL     MSG(24)       $MSG
064800970211     C                   EXSR      SCRERR
064900011008     C                   ELSE
065000011017     C     D00DIV        IFNE      WDIVCE
065100011008     C                   EXSR      CNVETC
065200011008     C                   ENDIF
065300011008     C                   ENDIF
065400970211     C*
065500970211     C                   ENDSR
065600970211     C*------------------------------------------------------------------------*
065700970211     C* CARETT - CARICA I CODICI TASSAZIONE
065800970211     C*------------------------------------------------------------------------*
065900970211     C     CARETT        BEGSR
066000970211     C*
066100970211     C                   Z-ADD     *ZEROS        I
066200091123     C                   MOVEL     'S'           KTTDSF                         *SEDE/FIL
066300970211     C                   MOVEL     D00CTO        KTTCTO                         *TARIF/OFF
066400970211     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE
066500970211     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
066600970211     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
066700970211     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
066800970211     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
066900011017     C     KE2ETT        SETLL     TEETT01L                           98
067000011017     C  N98              READ      TEETT01L                               98
067100970211     C     *IN98         IFEQ      '1'
067200970211     C                   MOVEL     MSG(26)       $MSG
067300970211     C                   EXSR      SCRERR
067400970211     C                   END
067500970211     C*
067600970211DO 1 C     *IN98         DOWEQ     '0'
067700970211     C     $ERR          ANDEQ     'N'
067800091123     C     ETTDSF        ANDEQ     'S'
067900980708     C     ETTCTO        ANDEQ     D00CTO
068000970211     C     ETTKSC        ANDEQ     D00KSC
068100970211     C     ETTCTR        ANDEQ     D00CTR
068200970211     C     ETTPRG        ANDEQ     D00PRG
068300970211     C     ETTLNP        ANDEQ     D00LNP
068400970211     C     ETTFIE        ANDEQ     D00FIE
068500970211IF 2 C     ETTATB        IFEQ      *BLANKS
068600970211     C     ETTRCT        ANDNE     *BLANKS
068700970211     C                   ADD       1             I
068800970211     C                   MOVEL     ETTCTS        TCTS(I)                        *COD.TASS.
068900970211     C                   MOVEL     ETTRCT        TRCT(I)                        *RAGGR. COD. TASS.
069000970211     C                   Z-ADD     ETTPPE        TPPE(I)                        *% PESO
069100140930     c* carico la ds multipla dei relativi cap
069200140930     c                   if        d00fie = 'E'
069300140930     C                   EXSR      CARETT10
069400140930     c                   end
069500970211     C*
069600970211     C* RECUPERA LINEA ARRIVO E TERMINAL ARRIVO
069700970211     C                   Z-ADD     CODUT         KTBUTE
069800970211     C                   MOVEL     'CT'          KTBCOD
069900970211     C                   MOVEL     ETTCTS        KTBKEY
070000970211     C     KEYTAB        CHAIN     TABEL00F                           99
070100970211     C     *IN99         IFEQ      *ON
070200970211     C                   MOVEL     MSG(27)       DSMSG                          *C.TASS. NON TROVATO
070300970211     C                   MOVEL     ETTCTS        DSCTE                          *C.TASS.
070400970211     C                   MOVEL     DSMSG         $MSG                           *MANCA TARIFFA
070500970211     C                   EXSR      SCRERR
070600970211     C                   END
070700970211IF 3 C     $ERR          IFEQ      'N'
070800970211     C                   MOVEL     TBLUNI        DSCT
070900970211     C*
071000970211     C                   MOVEL     §CTRAP        TRAP(I)                        *REGIONE APPARTENENZ
071100970211     C     §CTNAR        IFEQ      *BLANKS
071200970211     C                   MOVEL     'IT'          TNAR(I)                        *NAZIONE APPARTENENZ
071300970211     C                   ELSE
071400970211     C                   MOVEL     'EE'          TNAR(I)
071500970211     C                   ENDIF
071600970211E  3 C                   ENDIF
071700970211E  2 C                   ENDIF
071800011017     C                   READ      TEETT01L                               98
071900970211E  1 C                   ENDDO
072000970211     C                   Z-ADD     I             WULTCT            5 0
072100970211     C*
072200970211     C                   ENDSR
072300140930     C*------------------------------------------------------------------------*
072400140930     C* CARETT10 - CARICA I CODICI TASSAZIONE - CAP
072500140930     C*------------------------------------------------------------------------*
072600140930     C     CARETT10      BEGSR
072700140930     C                   Z-ADD     d00lnp        F
072800140930     c                   eval      kttntw = NTW(F)
072900141001     c                   eval      kttcts = ettcts
073000141001     c                   clear                   ii                4 0
073100140930     C*
073200140930     c     i             occur     scad
073300140930     C     KEyETT        SETLL     TEETT11L
073400140930     c                   do        *hival
073500140930     C     keyett        READe     TEETT11L
073600140930     C                   IF        %eof(teett11l)
073700140930     C                   leave
073800140930     C                   END
073900140930     C*
074000140930IF 2 C                   IF        £ettatb <> *BLANKS or £ettnsp = 0
074100140930     c                   iter
074200140930     c                   end
074300141001     c                   add       1             ii
074400141001     C                   MOVEL     £ettnaz       naz(Ii)                        *COD.TASS.
074500141001     C                   MOVEL     £ettcad       cad(Ii)                        *RAGGR. COD. TASS.
074600141001     C                   Z-ADD     £ETTinc       inc(Ii)                        *% PESO
074700140930E  1 C                   ENDDO
074800140930     C*
074900140930     C                   ENDSR
075000980903     C*------------------------------------------------------------------------*
075100980903     C* CERLNA - CERCA LINEA ARRIVO E TERMINAL ARRIVO PER OGNI COD.TASSAZIONE
075200980903     C*------------------------------------------------------------------------*
075300980903     C     CERLNA        BEGSR
075400980903     C*
075500980903     C* CICLO PER TUTTI I CODICI TASSAZIONE ARRIVO
075600980903DO  1C                   DO        WULTCT        I
075700980903     C*
075800980903     C* RECUPERA LINEA ARRIVO E TERMINAL ARRIVO
075900980903     C                   Z-ADD     CODUT         KTBUTE
076000980903     C                   MOVEL     'CT'          KTBCOD
076100980903     C                   MOVEL     TCTS(I)       KTBKEY
076200980903     C     KEYTAB        CHAIN     TABEL00F                           99
076300980903     C     *IN99         IFEQ      *ON
076400980903     C                   MOVEL     MSG(27)       DSMSG                          *C.TASS. NON TROVATO
076500980903     C                   MOVEL     TCTS(I)       DSCTE                          *C.TASS.
076600980903     C                   MOVEL     DSMSG         $MSG                           *MANCA TARIFFA
076700980903     C                   EXSR      SCRERR
076800980903     C                   END
076900980903IF  2C     $ERR          IFEQ      'N'
077000980903     C                   MOVEL     TBLUNI        DSCT
077100980903     C*
077200980903     C* LINEA ARRIVO
077300980903     C                   CLEAR                   DSSI95
077400980903     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
077500980903     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
077600980903     C                   MOVEL     §CTNAR        I95NAR                         *NAZIONE
077700980903     C                   MOVEL     §CTCAP        I95CAP                         *CAP
077800980903     C                   Z-ADD     SPSC(S)       I95LKG                         *PESO
077900980903     C                   Z-ADD     SVSC(S)       I95LMC                         *VOLUME
078000980903     C                   Z-ADD     WTFP          I95TFP                         *TERMINAL PARTENZA
078100980903     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
078200980903     C                   Z-ADD     D00DTC        I95DAT                         *DATA RIFERIMENTO
078300980903     C                   CALL      'TISI95R'
078400980903     C                   PARM                    DSSI95
078500980903IF  3C     O95ERR        IFNE      *BLANKS                                      *NON TROVATO
078600980903     C                   MOVEL     O95MSG        $MSG
078700980903     C                   EXSR      SCRERR
078800980903X   3C                   ELSE                                                   *TROVATO
078900980903     C                   Z-ADD     O95LNA        TLNA(I)                        *LINEA ARRIVO
079000980903     C* TERMINAL ARRIVO
079100980903     C                   CLEAR                   DSLV55
079200980903     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
079300980903     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
079400980903     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
079500980903     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
079600980903     C                   Z-ADD     D00LNP        D55LNP                         *LINEA PARTENZA
079700980903     C                   CALL      'FNLV55R'
079800980903     C                   PARM                    DSLV55
079900980903IF  4C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
080000980903     C                   MOVEL     D55MSG        $MSG
080100980903     C                   EXSR      SCRERR
080200980903X   4C                   ELSE                                                   *TROVATO
080300980903     C                   Z-ADD     D55TFA        TTFA(I)                        *TERMINAL ARRIVO
080400980903E   4C                   ENDIF
080500980903E   3C                   ENDIF
080600980903E   2C                   ENDIF
080700980903E   1C                   ENDDO
080800980903     C*
080900980903     C                   ENDSR
081000970211     C*------------------------------------------------------------------------*
081100970211     C* CARETS - CARICA GLI SCAGLIONI
081200970211     C*------------------------------------------------------------------------*
081300970211     C     CARETS        BEGSR
081400970211     C*
081500970211     C                   Z-ADD     *ZEROS        I
081600091123     C                   MOVEL     'S'           KTSDSF                         *SEDE/FIL
081700970211     C                   MOVEL     D00CTO        KTSCTO                         *TARIF/OFF
081800970211     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE
081900970211     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
082000970211     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
082100970211     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
082200010926     C     KE2ETS        SETLL     TEETS01L                           99
082300010926     C  N99              READ      TEETS01L                               99
082400970211     C     *IN99         IFEQ      '1'
082500970211     C                   MOVEL     MSG(25)       $MSG
082600970211     C                   EXSR      SCRERR
082700970211     C                   END
0828009702111    C     *IN99         DOWEQ     '0'
082900091123     C     ETSDSF        ANDEQ     'S'
083000980708     C     ETSCTO        ANDEQ     D00CTO
083100970211     C     ETSKSC        ANDEQ     D00KSC
083200970211     C     ETSCTR        ANDEQ     D00CTR
083300970211     C     ETSPRG        ANDEQ     D00PRG
083400970211     C     ETSLNP        ANDEQ     D00LNP
0835009702112    C     ETSATB        IFEQ      *BLANKS
083600011017     C     D00DIV        IFNE      WDIVCE
083700011008     C                   EXSR      CNVETS
083800011008     C                   ENDIF
083900970211     C                   ADD       1             I
084000970211     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPER.
084100970211     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO RIF. CALC.
084200970211     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI RIF. CALC.
084300970211     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOL. RIF. CALC.
084400970211     C                   Z-ADD     ETSPPE        SPPE(I)                        *PERC. PESO
084500970317     C                   Z-ADD     ETSINL        SINL(I)                        *IMPORTO INOLTRO
084600970402     C                   MOVEL     ETSSDE        SSDE(I)                        *SEGNO DELTA
084700970402     C                   Z-ADD     ETSPDE        SPDE(I)                        *% DELTA
084800970317     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPP. PESO/VOL.
0849009702112    C                   END
085000010926     C                   READ      TEETS01L                               99
0851009702111    C                   END
085200970211     C                   Z-ADD     I             WULTSC            5 0
085300970211     C*
085400970211     C                   ENDSR
085500961128     C*------------------------------------------------------------------------*
085600961128     C* CAL2R1 - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA) X SEQUENZA 1
085700961128     C*------------------------------------------------------------------------*
085800961128     C     CAL2R1        BEGSR
085900961128     C*
086000961128     C* TIPO -02- SPEDIZIONI --------------------------------------------------*
086100961128     C                   Z-ADD     1             I
086200961128     C     '02'          LOOKUP    C2R(I)                                 98
086300961128     C     *IN98         IFEQ      '0'
086400961128     C                   MOVEL     MSG(6)        $MSG                           *TIPO TARIF.02 NON T
086500961128     C                   EXSR      SCRERR
086600961128     C                   ELSE
086700961128     C                   Z-ADD     1             Q2R(I)                         *UNO
086800961128     C                   ENDIF
086900961128     C*
087000040811     C* TIPO -06- BANCALI -----------------------------------------------------*
087100040802     C                   Z-ADD     1             I
087200040802     C     '06'          LOOKUP    C2R(I)                                 98
087300040802     C     *IN98         IFEQ      '0'
087400040802     C                   MOVEL     MSG(28)       $MSG                           *TIPO TARIF.02 NON T
087500040802     C                   EXSR      SCRERR
087600040811     C                   ELSE
087700040811     C                   Z-ADD     1             Q2R(I)                         *UNO
087800040802     C                   ENDIF
087900040802     C*
088000961128     C* TIPO -09- STOP RITIRO -------------------------------------------------*
088100961128     C                   Z-ADD     1             I
088200961128     C     '09'          LOOKUP    C2R(I)                                 98
088300961128IF  1C     *IN98         IFEQ      '0'
088400961128     C                   MOVEL     MSG(9)        $MSG                           *TIPO TARIF.09 NON T
088500961128     C                   EXSR      SCRERR
088600961128X   1C                   ELSE
088700961128     C                   Z-ADD     1             Q2R(I)
088800961128     C     ETCSPR        IFGT      1
088900961128     C     1             DIV       ETCSPR        WNSTOP           12 6          *1 / N. DI STOP
089000961128     C     WNSTOP        IFLT      0,001
089100961128     C                   Z-ADD     0,001         Q2R(I)
089200961128     C                   ELSE
089300961128     C                   Z-ADD     WNSTOP        Q2R(I)
089400961128     C                   ENDIF
089500961128     C                   ENDIF
089600961128E   1C                   ENDIF
089700961128     C*
089800961128     C* TIPO -10- STOP CONSEGNA -----------------------------------------------*
089900961128     C                   Z-ADD     1             I
090000961128     C     '10'          LOOKUP    C2R(I)                                 98
090100961128IF  1C     *IN98         IFEQ      '0'
090200961128     C                   MOVEL     MSG(16)       $MSG                           *TIPO TARIF.10 NON T
090300961128     C                   EXSR      SCRERR
090400961128X   1C                   ELSE
090500961128     C                   Z-ADD     1             Q2R(I)
090600961128     C     ETCSPC        IFGT      1
090700961128     C     1             DIV       ETCSPC        WNSTOP           12 6          *1 / N. DI STOP
090800961128     C     WNSTOP        IFLT      0,001
090900961128     C                   Z-ADD     0,001         Q2R(I)
091000961128     C                   ELSE
091100961128     C                   Z-ADD     WNSTOP        Q2R(I)
091200961128     C                   ENDIF
091300961128     C                   ENDIF
091400961128E   1C                   ENDIF
091500961128     C*
091600961128     C* TIPO -22- CONTRASEGNO -------------------------------------------------*
091700961128     C                   Z-ADD     1             I
091800961128     C     '22'          LOOKUP    C2R(I)                                 98
091900961128     C     *IN98         IFEQ      '0'
092000961128     C                   MOVEL     MSG(12)       $MSG                           *TIPO TARIF.22 NON T
092100961128     C                   EXSR      SCRERR
092200961128     C                   ELSE
092300961128     C                   Z-ADD     ETCCAS        Q2R(I)                         *IMPORTO CONTRASSEGN
092400961128     C                   ENDIF
092500961128     C*
092600961128     C* TIPO -23- VALORE MERCE ASSICURATA -------------------------------------*
092700961128     C                   Z-ADD     1             I
092800961128     C     '23'          LOOKUP    C2R(I)                                 98
092900961128     C     *IN98         IFEQ      '0'
093000961128     C                   MOVEL     MSG(13)       $MSG                           *TIPO TARIF.23 NON T
093100961128     C                   EXSR      SCRERR
093200961128     C                   ELSE
093300961128     C                   Z-ADD     ETCASC        Q2R(I)                         *VAL MERCE ASSIC.
093400961128     C                   ENDIF
093500961128     C*
093600961128     C* TIPO -24- NUMERO DI GIORNI PAGAMENTO ----------------------------------*
093700961128     C                   Z-ADD     1             I
093800961128     C     '24'          LOOKUP    C2R(I)                                 98
093900961128     C     *IN98         IFEQ      '0'
094000961128     C                   MOVEL     MSG(14)       $MSG                           *TIPO TARIF.24 NON T
094100961128     C                   EXSR      SCRERR
094200961128     C                   ELSE
094300961128     C                   Z-ADD     ETCGGP        Q2R(I)                         *GIORNI PAGAM.
094400961128     C                   ENDIF
094500000228     C*
094600000228     C* TIPO -12- NUMERO DI GIORNI GIACENZA -----------------------------------*
094700000228     C                   Z-ADD     1             I
094800000228     C     '12'          LOOKUP    C2R(I)                                 98
094900000228     C     *IN98         IFEQ      '0'
095000000228     C                   MOVEL     MSG(10)       $MSG                           *TIPO TARIF.12 NON T
095100000228     C                   EXSR      SCRERR
095200000228     C                   ELSE
095300000228     C     ETCGGG        IFEQ      *ZEROS
095400000228     C                   Z-ADD     1             Q2R(I)                         *GIORNI GIACENZA
095500000228     C                   ELSE
095600000228     C                   Z-ADD     ETCGGG        Q2R(I)                         *GIORNI GIACENZA
095700000228     C                   ENDIF
095800000228     C                   ENDIF
095900961128     C*
096000961128     C                   ENDSR
096100961128     C*------------------------------------------------------------------------*
096200961128     C* CAL2R2 - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA) X SEQUENZA 2
096300961128     C*------------------------------------------------------------------------*
096400961128     C     CAL2R2        BEGSR
096500961128     C*
096600961128     C* TIPO -01- COLLI -------------------------------------------------------*
096700961128     C                   Z-ADD     1             I
096800961128     C     '01'          LOOKUP    C2R(I)                                 98
096900961128     C     *IN98         IFEQ      '0'
097000961128     C                   MOVEL     MSG(5)        $MSG                           *TIPO TARIF.01 NON T
097100961128     C                   EXSR      SCRERR
097200961128     C                   ELSE
097300961128     C                   Z-ADD     SCSC(S)       Q2R(I)                         *NUMERO COLLI
097400961128     C                   ENDIF
097500961128     C*
097600961128     C* TIPO -03- CHILOGRAMMI -------------------------------------------------*
097700961128     C                   Z-ADD     1             I
097800961128     C     '03'          LOOKUP    C2R(I)                                 98
097900961128     C     *IN98         IFEQ      '0'
098000961128     C                   MOVEL     MSG(7)        $MSG                           *TIPO TARIF.03 NON T
098100961128     C                   EXSR      SCRERR
098200961128     C                   ELSE
098300961128     C                   Z-ADD     SPSC(S)       Q2R(I)                         *PESO KG.
098400961128     C                   ENDIF
098500961128     C*
098600961128     C* TIPO -04- VOLUME ------------------------------------------------------*
098700961128     C                   Z-ADD     1             I
098800961128     C     '04'          LOOKUP    C2R(I)                                 98
098900961128     C     *IN98         IFEQ      '0'
099000961128     C                   MOVEL     MSG(8)        $MSG                           *TIPO TARIF.04 NON T
099100961128     C                   EXSR      SCRERR
099200961128     C                   ELSE
099300961128     C                   Z-ADD     SVSC(S)       Q2R(I)                         *VOLUME
099400961128     C                   ENDIF
099500040809     C*
099600000228     C* TIPO -11- CHILOGRAMMI PER GIORNI GIACENZA -----------------------------*
099700000228     C                   Z-ADD     1             I
099800000228     C     '11'          LOOKUP    C2R(I)                                 98
099900000228     C     *IN98         IFEQ      '0'
100000000228     C                   MOVEL     MSG(11)       $MSG                           *TIPO TARIF.11 NON T
100100000228     C                   EXSR      SCRERR
100200000228     C                   ELSE
100300000228     C     ETCGGG        IFEQ      *ZEROS
100400011025     C                   Z-ADD     1             WT0155           21 8
100500000228     C                   ELSE
100600000228     C                   Z-ADD     ETCGGG        WT0155
100700000228     C                   ENDIF
100800000228     C     SPSC(S)       MULT      WT0155        Q2R(I)
100900000228     C                   ENDIF
101000961128     C*
101100961128     C                   ENDSR
101200961128     C*------------------------------------------------------------------------*
101300970207     C* CHKVOC - DETERMINA LE VOCI DA CALCOLARE E CONTROLLA VALIDITA'
101400961128     C*------------------------------------------------------------------------*
101500961128     C     CHKVOC        BEGSR
101600970207     C*
101700970207     C* IMPOSTA A NO TUTTI I FLAG 'DA CALCOLARE' DELLE VOCI
101800040811     C* LA SCHIERA VB MI SERVE AD INDIVIDUARE SE BANCALI BARTOLINI O BANCALI
101900040811     C* BNB
102000040811     C                   CLEAR                   VB                             *VOCE DA CALC.
102100040811     C                   MOVEL     'N'           VC                             *VOCE DA CALC.
102200970207     C                   Z-ADD     100           VP                             * % DA CONSIDERARE
102300970207     C                   MOVEL     'S'           VI                             *INGLOBA S/N
102400970207     C*
102500970207     C* IMPOSTA A SI TUTTE LE VOCI SEMPRE PRESENTI
102600970207     C                   MOVEL     'S'           VC(10)                         *STOP RITIRO
102700970207     C                   MOVEL     'S'           VC(50)                         *SMISTAMENTO PART.
102800970207     C                   MOVEL     'S'           VC(60)                         *CARICO-SP.PART.-IMP
102900970207     C                   MOVEL     'S'           VC(70)                         *UFFICIO PART.
103000970207     C                   MOVEL     'S'           VC(80)                         *SPESE VARIE PART. 1
103100970207     C                   MOVEL     'S'           VC(81)                         *SPESE VARIE PART. 2
103200970207     C                   MOVEL     'S'           VC(110)                        *SCARICO-SP.ARR-IMA
103300970207     C                   MOVEL     'S'           VC(120)                        *SMISTAMENTO ARR.
103400970207     C                   MOVEL     'S'           VC(130)                        *SPUNTA USCITA
103500970207     C                   MOVEL     'S'           VC(180)                        *UFFICIO ARRIVI
103600970207     C                   MOVEL     'S'           VC(190)                        *SPESE VARIE ARR. 1
103700970207     C                   MOVEL     'S'           VC(191)                        *SPESE VARIE ARR. 2
103800970207     C                   MOVEL     'S'           VC(192)                        *SCORTE NA/PA A SPED
103900061207     C                   MOVEL     'S'           VC(193)                        *SCORTE NA/PA A SPED
104000021028     C                   MOVEL     'S'           VC(194)                        *SCORTE NA/PA A KG.
104100970207     C                   MOVEL     'S'           VC(300)                        *PAGAMENTO
104200980717     C                   MOVEL     'S'           VC(320)                        *SERV.SEDE(A SPED.)
104300000215     C                   MOVEL     'S'           VC(321)                        *SERV.SEDE(A PESO)
104400000215     C                   MOVEL     'S'           VC(322)                        *SERV.SEDE(A COLLO)
104500090909     C     D00DTC        IFGT      20090900
104600090909     C                   MOVEL     'S'           VC(112)
104700090909     C                   MOVEL     'S'           VC(122)
104800090909     C                   MOVEL     'S'           VC(137)
104900090909     c                   end
105000050926     C* CONTROLLO SE ESISTE =======> MERCE DI VALORE
105100050926     C     D00DTC        IFGT      20051000
105200050926     c     etcval        andgt     0
105300050926     C                   MOVEL     'S'           VC(19)
105400050926     C                   Z-ADD     etcval        VP(19)                         * % DA CONSIDER.
105500050926     C                   MOVEL     'S'           VC(21)
105600050926     C                   Z-ADD     etcval        VP(21)                         * % DA CONSIDER.
105700050926     C                   ENDIF
105800000721     C*
105900970207     C                   MOVEL     'S'           VC(900)                        *DELTA
106000041213     C* =============> adeguamento tariffariorio 2005
106100041213     C     D00DTC        IFGT      20050100
106200091123     C     D00DTC        andlt     20100100
106300041213     C                   MOVEL     'S'           VC(600)
106400041213     C                   MOVEL     'S'           VC(601)
106500041213     C                   MOVEL     'S'           VC(602)
106600041213     C                   MOVEL     'S'           VC(603)
106700041213     C                   MOVEL     'S'           VC(604)
106800040608     C                   ENDIF
106900061207     c* attivo sempre le maggiorazioni di adeguamento tariffario anno 2007
107000061207     C     D00DTC        IFGT      20070100
107100091123     C     D00DTC        andlt     20100100
107200061207     C                   MOVEL     'S'           VC(605)
107300061207     C                   MOVEL     'S'           VC(606)
107400061207     c                   end
107500091123     C                   IF        d00dtc > 20090400   and
107600091123     C                             d00dtc < 20100100
107700090908     C                   MOVEL     'S'           VC(607)
107800090908     c                   end
107900091123     C                   IF        d00dtc > 20090900 and
108000091123     C                             d00dtc < 20100100
108100090908     C                   MOVEL     'S'           VC(608)
108200090908     c                   end
108300041213     C* =============> immagine digitalizzata
108400041213     C     D00DTC        IFGT      20040700
108500041213     C                   MOVEL     'S'           VC(188)                        *CARICO CONSEGNA
108600041213     C                   ENDIF
108700021105     c*
108800021105     C* =============> ETICHETTATURA e SCARICO e PICKING e CARICO, FINO al 2003
108900040609     c* escludeva la 140 e 150 (carico aut.) erroneamente
109000021105     C     D00DTC        IFLT      20030000
109100021105     C                   MOVEL     'S'           VC(30)                         *SCARICO RITIRO
109200021105     C                   MOVEL     'S'           VC(40)                         *ETICHETTATURA
109300040609     C                   ENDIF
109400021105     C                   MOVEL     'S'           VC(140)                        *PICKING
109500021105     C                   MOVEL     'S'           VC(150)                        *CARICO CONSEGNA
109600040609     C*                  ENDIF
109700021105     c*
109800021105     C* =============> ETICHETTATURA e SCARICO, DOPO il 2002
109900021105IF  1C     D00DTC        IFGT      20030000
110000021105     C*
110100021105IF  2C     ETCDKC        IFNE      'S'                                          *NO procedura disc C
110200021105IF  3C     ETCIX1        IFEQ      *BLANKS                                      *autotrasportatore
110300021105     C                   MOVEL     'S'           VC(40)                         *ETICHETTATURA
110400021105     C                   MOVEL     'S'           VC(57)                         *CONTR.ENTRATA MERCI
110500021105X   3C                   ELSE                                                   *NO autotrasportator
110600021105IF  4C     ETCIX1        IFEQ      'C'                                          *cooperativa
110700021105     C                   MOVEL     'S'           VC(42)                         *ETICHETTATURA
110800021105E   4C                   ENDIF
110900021105E   3C                   ENDIF
111000021105E   2C                   ENDIF
111100021105     c*
111200021105IF  2C     ETCDKC        IFEQ      'S'                                          *SI procedura disc C
111300021105IF  3C     ETCIX1        IFEQ      *BLANKS                                      *autotrasportatore
111400030919IF  4C     D00DTC        IFLT      20030922
111500030919     C                   MOVEL     'S'           VC(43)
111600030919X   4c                   ELSE
111700030919IF  5C     ETCPCM        IFGT      *ZEROS                                       *merce a magazzino
111800030918     C                   MOVEL     'S'           VC(44)                         *PREPAR.ETIC.DISC C
111900030919X   5c                   ELSE                                                   *NO merce a magazz.
112000021105     C                   MOVEL     'S'           VC(43)                         *PREPAR.ETIC.DISC C
112100030919E   5C                   ENDIF
112200030919E   4C                   ENDIF
112300030919E   3C                   ENDIF
112400021105E   2C                   ENDIF
112500021105     C*
112600021105IF  2C     ETCIX2        IFEQ      *BLANKS                                      *autotrasportatore
112700021105     C                   MOVEL     'S'           VC(30)                         *SCARICO RITIRO
112800021105X   2C                   ELSE                                                   *cooperativa
112900021105     C                   MOVEL     'S'           VC(33)                         *SCARICO RITIRO
113000021105E   2C                   ENDIF
113100021105     C*
113200021105E   1C                   ENDIF
113300021105     C*
113400021105     C* CONTROLLO SE ESISTE =======> DISK 'A' E DISK 'B'
113500021105     C     ETCBOL        IFEQ      'B'
113600021105     C                   MOVEL     'S'           VC(510)                        *DISK 'B'
113700021105     C                   MOVEL     'S'           VC(511)                        *DISK 'A/B'
113800021105     C                   MOVEL     'S'           VC(55)                         *QUADRAT.DISK B
113900021105     C                   ENDIF
114000021105     C     ETCBOL        IFEQ      'A'
114100021105     C                   MOVEL     'S'           VC(511)                        *DISK 'A/B'
114200021105     C                   ENDIF
114300021105     C*
114400021105     C* CONTROLLA SE ESISTE =======> DISCK 'C'
114500021105     C     ETCDKC        IFEQ      'S'
114600021105     C                   MOVEL     'S'           VC(56)
114700021105     C                   ENDIF
114800050121     C* CONTROLLO SE ESISTE =======> CONSEGNA FUORI MISURA
114900050121     c     ETCPCF        IFGT      0
115000050121     C                   MOVEL     'S'           VC(475)
115100050121     C                   Z-ADD     ETCPCF        VP(475)                        * % DA CONSIDER.
115200050121     C                   MOVEL     ETCIFU        VI(475)                        *INGLOBA S/N
115300050121     C                   ENDIF
115400040802     C* CONTROLLO SE ESISTE =======> DESTINATARIO DISAGIATO
115500040802     c     etcdes        ifgt      0
115600040802     C                   MOVEL     'S'           VC(534)
115700040802     C                   Z-ADD     etcdes        VP(534)                        * % DA CONSIDER.
115800050805     C                   MOVEL     ETCIde        VI(534)                        *INGLOBA S/N
115900040802     C                   MOVEL     'S'           VC(535)
116000040802     C                   Z-ADD     etcdes        VP(535)                        * % DA CONSIDER.
116100050805     C                   MOVEL     ETCIde        VI(535)                        *INGLOBA S/N
116200040802     C                   ENDIF
116300040809     C* CONTROLLO SE ESISTE =======> BANCALI BAN
116400040802     c     etcnba        ifgt      0
116401150326IF  4C     D00DTC        andlt     20150323
116500040802     C                   MOVEL     'S'           VC(17)
116600040811     C                   MOVEL     'B'           VB(17)
116700040824     C                   Z-ADD     etcban        VP(17)
116800050805     C                   MOVEL     ETCIba        VI(17)                         *INGLOBA S/N
116900040802     C                   MOVEL     'S'           VC(119)
117000040811     C                   MOVEL     'B'           VB(119)
117100040824     C                   Z-ADD     etcban        VP(119)
117200050805     C                   MOVEL     ETCIba        VI(119)                        *INGLOBA S/N
117300040802     C                   MOVEL     'S'           VC(142)
117400040811     C                   MOVEL     'B'           VB(142)
117500040824     C                   Z-ADD     etcban        VP(142)
117600050805     C                   MOVEL     ETCIba        VI(142)                        *INGLOBA S/N
117700040802     C                   MOVEL     'S'           VC(186)
117800040811     C                   MOVEL     'B'           VB(186)
117900040824     C                   Z-ADD     etcban        VP(186)
118000050805     C                   MOVEL     ETCIba        VI(186)                        *INGLOBA S/N
118100040802     C                   MOVEL     'S'           VC(213)
118200040811     C                   MOVEL     'B'           VB(213)
118300040824     C                   Z-ADD     etcban        VP(213)
118400050805     C                   MOVEL     ETCIba        VI(213)                        *INGLOBA S/N
118500040802     C                   ENDIF
118501150326     c*
118502150326     c     etcnba        ifgt      0
118503150326IF  4C     D00DTC        andge     20150323
118504150326     c                   movel     etcflo        detcflo
118505150326     c* se non impostato forzo 01
118506150326     c                   if        §ETCTBA = '  '
118507150326     C                   MOVEL     '01'          §etctba
118508150326     c                   end
118509150326     c* eur
118510150326     c                   if        §ETCTBA = '01'
118511150326     C                   MOVEL     'S'           VC(342)
118512150326     C                   MOVEL     'B'           VB(342)
118513150326     C                   Z-ADD     etcban        VP(342)
118514150326     C                   MOVEL     ETCIba        VI(342)                        *INGLOBA S/N
118515150326cc   c                   end
118516150326     C* epal
118517150326     c                   if        §ETCTBA = '02'
118518150326     C                   MOVEL     'S'           VC(343)
118519150326     C                   MOVEL     'B'           VB(343)
118520150326     C                   Z-ADD     etcban        VP(343)
118521150326     C                   MOVEL     ETCIba        VI(343)                        *INGLOBA S/N
118522150326     c                   end
118523150326     C* leggeri
118524150326     c                   if        §ETCTBA = '03'
118525150326     C                   MOVEL     'S'           VC(344)
118526150326     C                   MOVEL     'B'           VB(344)
118527150326     C                   Z-ADD     etcban        VP(344)
118528150326     C                   MOVEL     ETCIba        VI(344)                        *INGLOBA S/N
118529150326     c                   end
118530150326     C                   ENDIF
118600040811     c* BANCALI oltre 20 colli non imbancalate
118700040811     c     etcbnb        ifgt      0
118701150326IF  4C     D00DTC        iflt      20150323
118702150326IF  4C     D00DTC        orge      20150323
118703150326     C     etcbol        andeq     ' '
118800040811     C                   MOVEL     'S'           VC(34)
118900040811     C                   MOVEL     'O'           VB(34)
119000040811     C                   Z-ADD     ETCBNB        VP(34)
119100040811     C                   Z-ADD     2             VS(34)
119200040811     C                   ENDIF
119201150326     C                   ENDIF
119300050805     C* CONTROLLA SE ESISTE =======> packing list
119400050808IF  1C     etcpkl        IFeq      'S'
119500050805     C     D00DTC        andgt     20050900
119600050805     C                   MOVEL     'S'           VC(189)
119700050805E   1C                   ENDIF
119800000215     C*
119900970207     C* CONTROLLO SE ESISTE =======> AFFLUENZA
120000970207     C*    LINEA PARTENZA DIVERSA DAL TERMINAL PARTENZA
120100970207     C     D00LNP        IFNE      WTFP
120200970207     C                   MOVEL     'S'           VC(220)                        *TRAINI AFFLUENZA
120300970207     C                   MOVEL     'S'           VC(31)                         *SCARICO AFFLUENZA
120400970207     C                   MOVEL     'S'           VC(51)                         *SMISTAM.AFFLUENZA
120500970207     C                   MOVEL     'S'           VC(61)                         *CARICO AFFLUENZA
120600050926     c* merce di valore
120700050926IF  1C     etcval        IFgt      0
120800050926IF  0C     d00dtc        ANDGT     20051000
120900050926     C                   MOVEL     'S'           VC(20)
121000050926     c                   Z-ADD     etcval        VP(20)                         * % DA CONSIDER.
121100050926     c                   end
121200970207     C                   ENDIF
121300970207     C*
121400970207     C* CONTROLLO SE ESISTE =======> GIACENZA
121500970207     C     ETCPGI        IFGT      *ZEROS
121600970207     C                   MOVEL     'S'           VC(260)                        *GIACENZA UFF.PART.
121700970207     C                   Z-ADD     ETCPGI        VP(260)                        * % DA CONSIDER.
121800970207     C                   MOVEL     ETCIGI        VI(260)                        *INGLOBA S/N
121900970207     C                   MOVEL     'S'           VC(261)                        *GIACENZA UFF.ARR.
122000970207     C                   Z-ADD     ETCPGI        VP(261)                        * % DA CONSIDER.
122100970207     C                   MOVEL     ETCIGI        VI(261)                        *INGLOBA S/N
122200970207     C                   MOVEL     'S'           VC(262)                        *GIACENZA MAG.ARR.
122300970207     C                   Z-ADD     ETCPGI        VP(262)                        * % DA CONSIDER.
122400970207     C                   MOVEL     ETCIGI        VI(262)                        *INGLOBA S/N
122500970207     C                   ENDIF
122600970207     C*
122700060314     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE AC PLUS
122800060314     C                   MOVE      D00KSC        KSCIS             4 0
1229000603141    C     KSCIS         IFEQ      8888
123000970226     C     KSCIS         OREQ      9999
123100060314     C     ETCPAC        IFGT      *ZEROS
123200970226     C                   MOVEL     'S'           VC(291)                        *ASSIC. X CONTO 88/9
123300970226     C                   Z-ADD     ETCPAC        VP(291)                        * % DA CONSIDER.
123400970226     C                   MOVEL     ETCIAS        VI(291)                        *INGLOBA S/N
123500060314     c                   end
123600060314x1   C                   ELSE
1237000603142    C     ETCPAC        IFGT      *ZEROS
123800970207     C                   MOVEL     'S'           VC(290)                        *ASSIC. PER CONTO
123900970207     C                   Z-ADD     ETCPAC        VP(290)                        * % DA CONSIDER.
124000970207     C                   MOVEL     ETCIAS        VI(290)                        *INGLOBA S/N
1241000603143    C                   if        d00dtc > 20050800                            *FINO al 2002
124200051110     C                   MOVEL     'S'           VC(292)
124300051110     C                   Z-ADD     ETCPAC        VP(292)                        * % DA CONSIDER.
124400051110     C                   MOVEL     ETCIAS        VI(292)                        *INGLOBA S/N
1245000603143    c                   end
1246009702262    C                   ENDIF
124700970207     C*
124800970207     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE RCV
124900060316     c* la competenza x rcv viene sempre attribuita x rcv o x ac base
125000970207     C     ETCPAR        IFGT      *ZEROS
125100060316     C     ETCxx1        orGT      *ZEROS
125200100224     C* attivo nuova voce 286 da febbraio 2010 solo per
125300100224     C* Estero, la voce 281 da febbraio sarà solo                      venta
125400100224     C* per Italia
125500100224     c                   if        d00dtc > 20100200  and
125600100224     C                             d00fie = 'E'
125700100224     C                   MOVEL     'S'           VC(286)
125800100224     c                   if        etcpar > 0
125900100224     C                   Z-ADD     ETCPAR        VP(286)                        * % DA CONSIDER.
126000100224     c                   else
126100100224     C                   Z-ADD     ETCxx1        VP(286)                        * % DA CONSIDER.
126200100224     c                   end
126300100224     C                   MOVEL     'S'           VI(286)                        *INGLOBA S/N
126400100224     c                   else
126500970207     C                   MOVEL     'S'           VC(281)                        *ASSIC. RCV DA 06/96
126600060316     c                   if        etcpar > 0
126700970207     C                   Z-ADD     ETCPAR        VP(281)                        * % DA CONSIDER.
126800060316     c                   else
126900060316     C                   Z-ADD     ETCxx1        VP(281)                        * % DA CONSIDER.
127000060316     c                   end
127100060314     c* x rcv inglobo sempre nel porto
127200060314     C                   MOVEL     'S'           VI(281)                        *INGLOBA S/N
127300970207     C                   ENDIF
127400100224     C                   ENDIF
127500060314     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE AC base
127600060314     C                   if        d00dtc > 20060300 and etcxx1 > 0             *FINO al 2002
127700060314     c* x ac base non inglobo nel porto
127800060314     c* cliente top o direzionale
127900060314     c                   select
128000060314     c                   when      etcclv = 'T' or etcclv = 'D'
128100060314     C                   MOVEL     'S'           VC(293)
128200060314     C                   Z-ADD     ETCxx1        VP(293)                        * % DA CONSIDER.
128300060314     C                   MOVEL     'N'           VI(293)                        *INGLOBA S/N
128400060314     c* cliente A
128500060314     c                   when      etcclv = 'A'
128600060314     C                   MOVEL     'S'           VC(294)
128700060314     C                   Z-ADD     ETCxx1        VP(294)                        * % DA CONSIDER.
128800060314     C                   MOVEL     'N'           VI(294)                        *INGLOBA S/N
128900060314     c* cliente B o C
129000060314     c                   when      etcclv = 'B' or etcclv = 'C'
129100060314     C                   MOVEL     'S'           VC(295)
129200060314     C                   Z-ADD     ETCxx1        VP(295)                        * % DA CONSIDER.
129300060314     C                   MOVEL     'N'           VI(295)                        *INGLOBA S/N
129400060314     C                   endsl
129500060314     C                   endif
1296000603141    C                   ENDIF
129700970207     C*
129800970207     C* CONTROLLO SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
129900970207     C     ETCPCA        IFGT      *ZEROS
130000131120     C*    ETCPCS        IFGT      *ZEROS                                       *CONS. SUPERMERCATI
130100131120     C*                  ELSE
130200021028     C                   MOVEL     'S'           VC(240)                        *CONS. SU APPUNT.
130300021028     C                   Z-ADD     ETCPCA        VP(240)                        * % DA CONSIDER.
130400021028     C                   MOVEL     ETCIAP        VI(240)                        *INGLOBA S/N
130500131120     C*                  ENDIF
130600021028     C                   ENDIF
130700970207     C*
130800970207     C* CONTROLLO SE ESISTE =======> CONSEGNA AI SUPERMERCATI
130900970207     C     ETCPCS        IFGT      *ZEROS
131000970207     C                   MOVEL     'S'           VC(242)                        *CONS. AI SUPERMERCA
131100970207     C                   Z-ADD     ETCPCS        VP(242)                        * % DA CONSIDER.
131200970207     C                   MOVEL     ETCISU        VI(242)                        *INGLOBA S/N
131300980121     C                   MOVEL     'S'           VC(247)                        *CONS. AI SUPERMERCA
131400980121     C                   Z-ADD     ETCPCS        VP(247)                        * % DA CONSIDER.
131500980121     C                   MOVEL     ETCISU        VI(247)                        *INGLOBA S/N
131600970207     C                   ENDIF
131700970207     C*
131800970207     C* CONTROLLO SE ESISTE =======> CONSEGNA AI PIANI
131900970207     C     ETCPCP        IFGT      *ZEROS
132000970207     C                   MOVEL     'S'           VC(241)                        *CONS. AI PIANI
132100970207     C                   Z-ADD     ETCPCP        VP(241)                        * % DA CONSIDER.
132200970207     C                   MOVEL     ETCIPI        VI(241)                        *INGLOBA S/N
132300970207     C                   ENDIF
132400970207     C*
132500970207     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
132600970207     C     ETCCAS        IFGT      *ZEROS
132700060224     c**** mittente
132800970207     C     ETCPAM        IFGT      *ZEROS                                       *MITTENTE
132900060224     c******* non clienti informatizzati
133000060224     C                   IF        (etcbol <> 'A' and
133100060224     C                             etcbol <> 'B' and
133200060224     C                             etcdkc <> 'S' and d00dtc > 20060200) or
133300060224IF  2C                             d00dtc < 20060200
133400060224     C                   MOVEL     'S'           VC(255)
133500060224     C                   Z-ADD     ETCPAM        VP(255)                        * % DA CONSIDER.
133600060224     C                   MOVEL     ETCICA        VI(255)                        *INGLOBA S/N
133700060224     c                   end
133800060224     c******* clienti informatizzati
133900060224     C                   IF        (etcbol = 'A' or
134000060224     C                             etcbol = 'B' or
134100060224     C                             etcdkc = 'S' ) and d00dtc > 20060200
134200060224     C                   MOVEL     'S'           VC(258)
134300060224     C                   Z-ADD     ETCPAM        VP(258)                        * % DA CONSIDER.
134400060224     C                   MOVEL     ETCICA        VI(258)                        *INGLOBA S/N
134500060224     c                   end
134600970207     C                   MOVEL     'S'           VC(254)                        *CONTRASS.MITTENTE
134700970207     C                   Z-ADD     ETCPAM        VP(254)                        * % DA CONSIDER.
134800970207     C                   MOVEL     ETCICA        VI(254)                        *INGLOBA S/N
134900970207     C                   ENDIF
135000060224     c**** bartolini
135100970226     C     ETCPAB        IFGT      *ZEROS                                       *BARTOLINI
135200060224     c******* non clienti informatizzati
135300060224     C                   IF        (etcbol <> 'A' and
135400060224     C                             etcbol <> 'B' and
135500060224     C                             etcdkc <> 'S' and d00dtc > 20060200) or
135600060224IF  2C                             d00dtc < 20060200
135700060224     C                   MOVEL     'S'           VC(250)
135800060224     C                   Z-ADD     ETCPAB        VP(250)                        * % DA CONSIDER.
135900060224     C                   MOVEL     ETCICA        VI(250)                        *INGLOBA S/N
136000060224     c                   end
136100970207     C                   MOVEL     'S'           VC(253)                        *CONTRASS.SEDE
136200970207     C                   Z-ADD     ETCPAB        VP(253)                        * % DA CONSIDER.
136300970207     C                   MOVEL     ETCICA        VI(253)                        *INGLOBA S/N
136400970207     C                   ENDIF
136500970207     C                   ENDIF
136600970207     C*
136700970207     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
136800011030     C                   Z-ADD     *ZEROS        N63               6 3
136900011030     C     ETCPFD        IFGT      *ZEROS
137000011030     c     ETCCIS        ORGT      *ZEROS
137100011030     C     ETCPFD        ADD       ETCCIS        N63
137200011030     C     N63           IFGT      100
137300011030     C                   Z-ADD     100           N63
137400011030     C                   ENDIF
137500011030     C                   MOVEL     'S'           VC(532)                        *FERMO DEPOSITO
137600011030     C                   Z-ADD     N63           VP(532)                        * % DA CONSIDER.
137700011030     C                   ENDIF
137800021028     C*
137900021028     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
138000021028     C     ETCPFD        IFGT      *ZEROS
138100021028     C                   MOVEL     'S'           VC(184)                        *FERMO DEPOSITO
138200021028     C                   Z-ADD     ETCPFD        VP(184)                        * % DA CONSIDER.
138300021028E   2C                   ENDIF
138400970207     C*
138500011009     C* CONTROLLO SE ESISTE =======> MERCE A MAGAZZINO
138600011030     C                   Z-ADD     *ZEROS        N63
138700011030     C     ETCPCM        IFGT      *ZEROS
138800011030     C     ETCSAM        ORGT      *ZEROS
138900011030     C     ETCPCM        ADD       ETCSAM        N63
139000011030     C     N63           IFGT      100
139100011030     C                   Z-ADD     100           N63
139200011030     C                   ENDIF
139300011030     C                   MOVEL     'S'           VC(521)                        *CONS. A MAGAZZINO N
139400011030     C                   Z-ADD     N63           VP(521)                        * % DA CONSIDER.
139500011030     C                   ENDIF
139600970207     C*
139700010801     C* CONTROLLO SE ESISTE =======> COMPETENZA SEDE -CLIENTE CODIFICATO-
139800010801     C*     SE NON E' CLIENTE 8888 O 9999
139900010801     C                   MOVE      D00KSC        KSCIS             4 0
140000010801     C     KSCIS         IFNE      8888
140100010801     C     KSCIS         ANDNE     9999
140200980910     C     ETCESP        IFGT      *ZEROS
140300010109     C                   MOVEL     'S'           VC(533)                        *ESPRESSO NEW
140400010109     C                   Z-ADD     ETCESP        VP(533)                        * % DA CONSIDER.
140500010109     C                   MOVEL     ETCIES        VI(533)                        *INGLOBA S/N
140600001206     C                   ENDIF
140700010801     C                   ENDIF
140800970429     C*
140900970429     C* CONTROLLO SE ESISTE =======> DDT (DOCUM.TRASP.DA CONS. AL DEST.)
141000970429     C     ETCPCD        IFGT      *ZEROS
141100970429     C                   MOVEL     'S'           VC(72)                         *UFF.PART DDT
141200970429     C                   Z-ADD     ETCPCD        VP(72)                         * % DA CONSIDER.
141300970429     C                   MOVEL     ETCIDD        VI(72)                         *INGLOBA S/N
141400970429     C                   MOVEL     'S'           VC(182)                        *UFF.ARR. DDT
141500970429     C                   Z-ADD     ETCPCD        VP(182)                        * % DA CONSIDER.
141600970429     C                   MOVEL     ETCIDD        VI(182)                        *INGLOBA S/N
141700970429     C                   ENDIF
141800970729     C*
141900970729     C* CONTROLLO SE ESISTE =======> LOCALITA' DISAGIATE
142000970729     C     ETCPCL        IFGT      *ZEROS
142100970729     C                   MOVEL     'S'           VC(246)                        *LOC.DISAG.
142200970729     C                   Z-ADD     ETCPCL        VP(246)                        * % DA CONSIDER.
142300970729     C                   MOVEL     ETCILO        VI(246)                        *INGLOBA S/N
142400970729     C                   ENDIF
142500980202     C*
142600020912     C* CONTROLLO SE ESISTE =======>
142700980710     C                   MOVEL     'S'           VC(270)                        *SEDE COMM.LE   BART
142800000721     C                   MOVEL     'S'           VC(592)                        *MAGG.FEDERAL EXPRES
142900000721     C                   MOVEL     'S'           VC(274)                        *ASSIST.CLIENTI FEDE
143000010109     C                   MOVEL     'S'           VC(523)                        *CCM OLIWEB NEW
143100010109     C                   MOVEL     'S'           VC(594)                        *MAGG.FEDEX NEW
143200021031     C                   MOVEL     'S'           VC(595)                        *MAGG.FEDEX NEW 89
143300980710     C*
143400980710     C* CONTROLLO SE ESISTE =======> TRAINI LINEA
143500001206     C                   MOVEL     'S'           VC(212)                        *TRAINI LINEA
143600981023     C*
143700981023     C* CONTROLLO SE ESISTE =======> CARRIAGGIO RITIRO BARTOLINI/SDI
143800001206     C                   MOVEL     'S'           VC(23)                         *CARRIAGGIO RITIRO N
143900990422     C*
144000990422     C* CONTROLLO SE ESISTE =======> PREPAGATI
144100990422     C*    SE CLIENTE 8888
144200990422     C                   MOVE      D00KSC        KSCIS             4 0
144300990422     C     KSCIS         IFEQ      8888
144400990422     C                   MOVEL     'S'           VC(266)                        *PREPAGATI
144500990422     C                   ENDIF
144600980710     C*
144700980710     C*
144800980710     C* SOLO SE GIA' ENTRATI NEL LOOP DEGLI SCAGLIONI -----------------------------
1449009807101    C     S             IFGT      *ZEROS
145000980710     C     SPSC(S)       ANDGT     *ZEROS
145100000228     C*
145200000228     C* CONTROLLO SE ESISTE =======> GIACENZA
145300000228     C     ETCPGI        IFGT      *ZEROS
145400000228     C     SPSC(S)       IFLE      100                                          *PESO RIFERIMENTO <1
145500000228     C                   MOVEL     'S'           VC(267)                        *GIACENZA £ X GG
145600000228     C                   Z-ADD     ETCPGI        VP(267)                        * % DA CONSIDER.
145700000228     C                   MOVEL     ETCIGI        VI(267)                        *INGLOBA S/N
145800000228     C                   ELSE
145900000228     C                   MOVEL     'S'           VC(265)                        *GIACENZA £ X KG/GG
146000000228     C                   Z-ADD     ETCPGI        VP(265)                        * % DA CONSIDER.
146100000228     C                   MOVEL     ETCIGI        VI(265)                        *INGLOBA S/N
146200000228     C                   ENDIF
146300000228     C                   ENDIF
146400980710     C*
146500980710     C                   ENDIF                                                  *SOLO SEQ. 2 -------
146600980202     C*
146700970207     C*
146800980710     C* SOLO SE GIA' ENTRATI NEL LOOP DEI COD.TASSAZ. -----------------------------
146900990422     C* ATT.NE !! SE SI INSERISCE UNA VOCE IN QUESTO PARAGRAFO, FORZARE SEQ=3 NELLA
147000990422     C* SUBROUTINE IMPSEQ
1471009712151    C     T             IFGT      *ZEROS
147200971215     C     TCTS(T)       ANDNE     *BLANKS
147300111003     C                   Z-ADD     TLNA(T)       F
147400111003      * ADEGUAMENTO "MOMENTANEO" TARIFFE EEX
147500111003      * partenza euroexpress, FRANCO
147600111003     c     NTW(F)        IFEQ      'EEX'
147700111003     C     etcfra        andeq     100
147800111003     C     D00DTC        andgt     20110900
147900111003     C                   MOVEL     'S'           VC(549)
148000111003     C                   ENDIF
148100081031      * arrivo euroexpress, franco, loc.disagiate
148200081031      * la 341 non è stata attivata perchè sono i resi e non i
148300081031      * ritiri, inoltre la % loc. disagiate nella creazione dati
148400081031      * tipo è solo per le consegne
148500081031     c     NTW(F)        IFEQ      'EEX'
148600081031     C     ETCFRA        andeq     100
148700081031     C     ETCPCL        ANDGT     0
148800081031     C     D00DTC        andgt     20081026
148900081031     C                   MOVEL     'S'           VC(340)
149000081031     C                   ENDIF
149100970207     C*
149200970207     C* CONTROLLO SE ESISTE =======> ESTERO - EXPORT
149300970207     C*    LINEA ARRIVO   > 300
149400971211     C                   Z-ADD     TLNA(T)       F
149500971211     C     FL1(F)        IFEQ      'E'
149600000217     C*
149700000217     C     DPD(F)        IFEQ      'S'                                          *LINEA ARRIVO DPD
149800000217     C                   MOVEL     'S'           VC(74)                         *UFF.PART.EXPORT DPD
149900010801     C                   MOVEL     'S'           VC(276)                        *ASSIST.IMP/EXP DPD
150000040609     c* attivo la 305 FINANZIAM.TARIFFE IMPORT DPD solo se franco e lna=DPD
150100040609     c* no nel caso di assegnati import
150200040611     c                   if        etcfra = 100 and
150300040611     C                             d00dtc > 20040700                            *FINO al 2002
150400040609     C                   MOVEL     'S'           VC(305)                        *MAGG.FEDEX NEW 89
150500040609     c                   end
150600040611     c* attivo la 288 COMP.INOLTRI ISOLE ESTERE DPD                    =DPD
150700040610     c                   if        etccis > 0
150800040610     C                   MOVEL     'S'           VC(288)                        *MAGG.FEDEX NEW 89
150900040610     C                   Z-ADD     etccis        VP(288)                        * % DA CONSIDER.
151000040610     c                   end
151100000217     C                   ELSE
151200000217     C                   MOVEL     'S'           VC(71)                         *UFF.PARTENZA EXPORT
151300020503     C     NTW(F)        IFNE      'FED'
151400010801     C                   MOVEL     'S'           VC(275)                        *ASSISTENZA IMP/EXP
151500020503     C                   MOVEL     'S'           VC(555)
151600030527     C                   MOVEL     'S'           VC(556)
151700020503     C                   ENDIF
151800010109     C                   ENDIF
151900010109     C                   MOVEL     'S'           VC(543)                        *AZZ.COMP.ARR.ITA NE
152000000217     C     DPD(F)        IFEQ      'S'                                          *LINEA ARRIVO DPD
152100000217     C                   MOVEL     'S'           VC(175)                        *ARR.EXP.DPD (COLLO)
152200000217     C                   ELSE                                                   *LINEA ARR. NON DPD
152300020503     C     NTW(F)        IFNE      'FED'
152400090122     C     D00DTC        IFGT      20090000                                     *FINO al 2002
152500090122     C                   MOVEL     'S'           VC(332)                        *ARR.EXPORT (X SPED.
152600090122     C                   END
152700091019     C     D00DTC        IFGT      20030000                                     *FINO al 2002
152800050524     c                   movel     tcts(t)       wdcts             1
152900030429     C     SCSC(S)       IFEQ      1
153000030429     C     SPSC(S)       ANDLE     30
153100050524     C*    TLNA(T)       ANDEQ     310
153200070523     C     wdcts         ANDEQ     'D'
153300030429     C     ETCCAS        ANDEQ     *ZEROS
153400070523     C     SCSC(S)       oreq      1
153500070523     C     SPSC(S)       ANDLE     30
153600070523     C     tcts(t)       ANDEQ     'W4'
153700070523     C     ETCCAS        ANDEQ     *ZEROS
153800030429     C                   MOVEL     'S'           VC(166)
153900040611     c                   if        etccis > 0  and
154000040611     C                             d00dtc > 20040100                            *FINO al 2002
154100040610     c* non memorizzo la schiera se ingloba al porto VI(170) xchè nella
154200040610     c* voce c'é il raggruppamento = 001 bisognerebbe forzare un rag.
154300040610     c* delta fittizio se chiesto di non inglobarlo al porto
154400040610     C                   MOVEL     'S'           VC(170)
154500040610     C                   Z-ADD     etccis        VP(170)                        * % DA CONSIDER.
154600040610     c                   end
154700030429     C                   ELSE
154800030429     C                   MOVEL     'S'           VC(171)
154900030429     C                   MOVEL     'S'           VC(172)
155000130529IF  5C                   IF        d00dtc > 20130600
155100130529     C                   MOVEL     'S'           VC(173)
155200130529     c                   end
155300130529IF  5C                   IF        d00dtc > 20051106
155400130529     C                   MOVEL     'S'           VC(323)
155500051107     C                   MOVEL     'S'           VC(327)
155600051107     C                   MOVEL     'S'           VC(328)
155700051107     c                   end
155800070522IF  5C                   IF        d00dtc > 20070100
155900070522     C                   MOVEL     'S'           VC(331)
156000070522     C                   MOVEL     'S'           VC(552)
156100070522     c                   end
156200040610     c* 547 MAGG.AUTOSTRADA AUSTRIA ARRIVO
156300040611     C                   if        d00dtc > 20040100                            *FINO al 2002
156400040610     C                   MOVEL     'S'           VC(547)
156500040610     c* 545 MAGGIORAZ.CONSEGNE ISOLE GERM E CONSEGNA ISOLE
156600040610     c     etccis        ifne      0
156700040610     C                   MOVEL     'S'           VC(545)
156800040610     C                   Z-ADD     etccis        VP(545)                        * % DA CONSIDER.
156900040610     c                   end
157000040611     c                   end
157100030429     C                   ENDIF
157200030428     c                   ENDIF
157300020503     C                   ENDIF
157400000217     C                   ENDIF
157500970207     C                   ENDIF
157600970207     C*
157700970207     C* CONTROLLO SE ESISTE =======> DEFLUENZA
157800970207     C*    LINEA ARRIVO DIVERSA DAL TERMINAL ARRIVO
157900971211     C                   Z-ADD     TLNA(T)       F
158000970207     C     TLNA(T)       IFNE      TTFA(T)
158100971211     C     FL1(F)        IFEQ      'E'                                          *ESTERO
158200970207     C                   MOVEL     'S'           VC(114)                        *SCARICO DEFL.EXPORT
158300970207     C                   MOVEL     'S'           VC(124)                        *SMISTAM.DEFL.EXPORT
158400970207     C                   MOVEL     'S'           VC(152)                        *CARICO DEFL.EXPORT
158500000217     C     DPD(F)        IFEQ      'S'                                          *LINEA ARRIVO DPD
158600000217     C                   MOVEL     'S'           VC(233)                        *TRAINI DEFL.EST.DPD
158700000217     C                   ELSE                                                   *LINEA ARRIVO NON DP
158800001206     C                   MOVEL     'S'           VC(231)                        *TRAINI DEFL.EST.
158900000217     C                   ENDIF
159000970207     C                   ELSE                                                   *ITALIA
159100970207     C                   MOVEL     'S'           VC(230)                        *TRAINI DEFLUENZA
159200970207     C                   MOVEL     'S'           VC(111)                        *SCARICO DEFLUENZA
159300970207     C                   MOVEL     'S'           VC(121)                        *SMISTAM.DEFLUENZA
159400970207     C                   MOVEL     'S'           VC(151)                        *CARICO DEFLUENZA
159500110905IF  0C     d00dtc        ifge      20110829
159600110905     C                   MOVEL     'S'           VC(548)
159700110905     c                   end
159800090909IF  0C     d00dtc        ifgt      20090900
159900090909     C                   MOVEL     'S'           VC(113)
160000090909     C                   MOVEL     'S'           VC(125)
160100090909     C                   MOVEL     'S'           VC(154)
160200090909     c                   end
160300050926     c* merce di valore
160400050926IF  1C     etcval        IFgt      0
160500050926IF  0C     d00dtc        ANDGT     20051000
160600050926     C                   MOVEL     'S'           VC(22)
160700050926     c                   Z-ADD     etcval        VP(22)                         * % DA CONSIDER.
160800050926     c                   end
160900970207     C                   ENDIF
161000970207     C                   ENDIF
161100970207     C*
161200970207     C* CONTROLLO SE ESISTE =======> LOCALI
161300980710     C*    LINEA DI PART. = LINEA DI ARR. = TERM.PART. = TERM.ARR.
161400970207     C     D00LNP        IFEQ      TLNA(T)
161500980710     C     D00LNP        ANDEQ     TTFA(T)
161600980710     C     D00LNP        ANDEQ     WTFP
161700970207     C                   MOVEL     'S'           VC(500)                        *LOCALI 1
161800970207     C                   ENDIF
161900980710     C*
162000980710     C* CONTROLLO SE ESISTE =======> AFFLUENZA/DEFLUENZA SU STESSO TERMINAL
162100980710     C*    SE TERM.PART. = TERM.ARR. E LINEA ARR. <> LINEA PART.
162200980710     C     WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
162300980710     C     D00LNP        ANDNE     WTFP                                         *SEC.LIVELLO PART.
162400980710     C     TLNA(T)       ANDNE     TTFA(T)                                      *SEC.LIVELLO ARR.
162500980710     C                   MOVEL     'S'           VC(560)                        *AZZ.DOPPIA AFL/DEFL
162600980710     C                   ENDIF
162700980710     C*
162800980710     C* CONTROLLO SE ESISTE =======> LINEA PART. = TERM. EXPORT
162900980710     C*    SE TERM.PART. = TERM.ARR. = LINEA PART. E EXPORT
163000980710     C                   Z-ADD     TLNA(T)       F
163100980710     C     WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
163200980710     C     D00LNP        ANDEQ     WTFP                                         *PART. DA TERMINAL
163300980710     C     FL1(F)        ANDEQ     'E'                                          *EXPORT
163400980710     C                   MOVEL     'S'           VC(561)                        *AZZ.SCARICO AFFL.
163500980710     C                   ENDIF
163600980826     C*
163700980826     C* CONTROLLO SE ESISTE =======> LINEA PART. E' 2°LIV. TERM. EXPORT
163800980826     C*    SE TERM.PART.=TERM.ARR. E LINEA PART.<>TERM.PART. E EXPORT
163900980826     C                   Z-ADD     TLNA(T)       F
164000980826     C     WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
164100980826     C     D00LNP        ANDNE     WTFP                                         *2°LIV. DEL T.P.
164200980826     C     FL1(F)        ANDEQ     'E'                                          *EXPORT
164300980826     C                   MOVEL     'S'           VC(570)                        *AZZ.COMP.MAG.EXP.
164400980826     C                   ENDIF
164500040611     C* SOLO SE franco, EEX, e giacenza -------------------------------------------
164600040611     C                   Z-ADD     TLNA(T)       F
164700040611     C     ntw(F)        IFEQ      'EEX'
164800040611     C     etcfra        andeq     100
164900040611     C     etcpgi        andgt     0
165000040611     C                   MOVEL     'S'           VC(590)                        *CONS. A MAGAZZINO N
165100040611     C                   Z-ADD     etcpgi        VP(590)                        * % DA CONSIDER.
165200070522     C                   IF        d00dtc > 20070100
165300070522     C                   MOVEL     'S'           VC(268)
165400070523     C                   Z-ADD     etcpgi        VP(268)                        * % DA CONSIDER.
165500070523     C                   MOVEL     ETCIGI        VI(268)                        *INGLOBA S/N
165600070522     C                   MOVEL     'S'           VC(553)
165700070522     C                   Z-ADD     etcpgi        VP(553)                        * % DA CONSIDER.
165800070522     C                   ENDIF
165900040611     C                   ENDIF
166000980826     C*
166100980826     C* CONTROLLO SE ESISTE =======> LINEA ARR. E' 2°LIV. TERM. IMPORT
166200980826     C*    SE TERM.PART.=TERM.ARR. E LINEA ARR.<>TERM.ARR. E IMPORT
166300040608     c* asterisco l'attivazione della mag. 571 xchè la lnp non può essere
166400040608     c* estero
166500040608     C*                  Z-ADD     D00LNP        F
166600040608     C*    WTFP          IFEQ      TTFA(T)                                      *STESSO TERMINAL
166700040608     C*    TLNA(T)       ANDNE     TTFA(T)                                      *2°LIV. DEL T.A.
166800040608     C*    FL1(F)        ANDEQ     'E'                                          *IMPORT
166900040608     C*                  MOVEL     'S'           VC(571)                        *AZZ.COMP.MAG.IMP.
167000040608     C*                  ENDIF
167100981023     C*
167200981023     C* CONTROLLO SE ESISTE =======> CARRIAGGIO/STOP CONSEGNA BARTOLINI/SDI
167300010112     C                   MOVEL     'S'           VC(161)                        *CARRIAG.CONS.NEW
167400010112     C                   MOVEL     'S'           VC(177)                        *STOP DI CONS.NEW
167500011009     C*
167600011009     C* CONTROLLO SE ESISTE =======> CONSEGNA ISOLA
167700011030     C     ETCCIS        IFGT      *ZEROS
167800131120     C*    ETCPFD        ANDEQ     *ZEROS
167900020104     C                   Z-ADD     TLNA(T)       F
168000020104     C     FL1(F)        IFEQ      'E'                                          *LINEA ARRIVO ESTERO
168100020104     C                   ELSE                                                   *LINEA ARRITO ITALIA
168200011030     C                   MOVEL     'S'           VC(278)                        *CONSEGNA ISOLA
168300011030     C                   Z-ADD     ETCCIS        VP(278)                        * % DA CONSIDERARE
168400011030     C                   MOVEL     ETCISO        VI(278)                        *INGLOBA S/N
168500020104     C                   ENDIF
168600020104     C                   ENDIF
168700011030     C*
168800011009     C* CONTROLLO SE ESISTE =======> RITIRO ISOLA
168900011030     C     ETCSAM        IFGT      *ZEROS
169000131120     C*    ETCPFD        ANDEQ     *ZEROS
169100011030     C                   MOVEL     'S'           VC(279)                        *RITIRO ISOLA
169200011030     C                   Z-ADD     ETCSAM        VP(279)                        * % DA CONSIDERARE
169300011030     C                   MOVEL     ETCINR        VI(279)                        *INGLOBA S/N
169400040621     C                   MOVEL     'S'           VC(285)                        *RITIRO ISOLA
169500040621     C                   Z-ADD     ETCSAM        VP(285)                        * % DA CONSIDERARE
169600040621     C                   MOVEL     ETCINR        VI(285)                        *INGLOBA S/N
169700011030     C                   ENDIF
169800000228     C*
169900000228     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
170000000228     C     ETCCAS        IFGT      *ZEROS
170100000228     C                   Z-ADD     TLNA(T)       F
170200000228     C     FL1(F)        IFEQ      'E'                                          *ESTERO
170300000228     C                   MOVEL     'S'           VC(256)                        *C/ASS.ARR.EXP (A VA
170400000228     C     ETCPAB        ADD       ETCPAM        VP(256)                        * % DA CONSIDER.
170500000228     C                   MOVEL     ETCICA        VI(256)                        *INGLOBA S/N
170600000228     C                   MOVEL     'S'           VC(257)                        *C/ASS.ARR.EXP.(A %)
170700000228     C     ETCPAB        ADD       ETCPAM        VP(257)                        * % DA CONSIDER.
170800000228     C                   MOVEL     ETCICA        VI(257)                        *INGLOBA S/N
170900000228     C                   MOVEL     'S'           VC(280)                        *C/ASS ASSIST.EXP.
171000000228     C     ETCPAB        ADD       ETCPAM        VP(280)                        * % DA CONSIDER.
171100000228     C                   MOVEL     ETCICA        VI(280)                        *INGLOBA S/N
171200000228     C                   ELSE                                                   *ITALIA
171300000228     C                   MOVEL     'S'           VC(251)                        *CONTRASS.UFF.ARR.
171400000228     C     ETCPAB        ADD       ETCPAM        VP(251)                        * % DA CONSIDER.
171500000228     C                   MOVEL     ETCICA        VI(251)                        *INGLOBA S/N
171600000228     C                   MOVEL     'S'           VC(252)                        *CONTRASS.PADRON.ARR
171700000228     C     ETCPAB        ADD       ETCPAM        VP(252)                        * % DA CONSIDER.
171800000228     C                   MOVEL     ETCICA        VI(252)                        *INGLOBA S/N
171900000228     C                   ENDIF
172000000228     C                   ENDIF
172100000302     C*
172200000302     C* CONTROLLO SE ESISTE =======> COMPETENZA SEDE
172300000303     C*    SE LINEA PARTENZA E LINEA ARRIVO <> DPD
172400010801     C                   Z-ADD     D00LNP        F
172500010801     C                   Z-ADD     TLNA(T)       F1                4 0
172600010801IF  2C     DPD(F)        IFNE      'S'                                          *L.PARTENZA NON DPD
172700010801     C     DPD(F1)       ANDNE     'S'                                          *L.ARRIVO   NON DPD
172800010801     C*    SE CLIENTE 8888 O 9999
172900010801     C                   MOVE      D00KSC        KSCIS             4 0
173000010801IF  3C     KSCIS         IFEQ      8888
173100010801     C     KSCIS         OREQ      9999
173200100422     C                   MOVEL     'S'           VC(822)                        *RICARICO SEDE 88/99
173300010801E   3C                   ENDIF
173400010801     C*    SE NON E' CLIENTE 8888 O 9999
173500010801IF  3C     KSCIS         IFNE      8888
173600010801     C     KSCIS         ANDNE     9999
173700000303IF  4C     D00TSP        IFEQ      'D'
173800000303IF  5C     ETCCLV        IFEQ      'A'                                          *CLASSE
173900100422     C                   MOVEL     'S'           VC(816)                        *SEDE-DIST-CL.A
174000000303E   5C                   ENDIF
174100000303IF  5C     ETCCLV        IFEQ      'B'                                          *CLASSE
174200100422     C                   MOVEL     'S'           VC(817)                        *SEDE-DIST-CL.B
174300000303E   5C                   ENDIF
174400000303IF  5C     ETCCLV        IFEQ      'C'
174500100422     C                   MOVEL     'S'           VC(818)                        *SEDE-DIST-CL.C
174600000303E   5C                   ENDIF
174700000303IF  5C     ETCCLV        IFEQ      'T'
174800020213     C     ETCCLV        OREQ      'D'
174900100422     C                   MOVEL     'S'           VC(821)                        *SEDE-DIST-CL.T
175000000303E   5C                   ENDIF
175100000303E   4C                   ENDIF
175200000303     C*
175300000303IF  4C     D00TSP        IFEQ      'C'                                          *CORRIERE(NORMALE)
175400000303IF  5C     ETCCLV        IFEQ      'A'                                          *CLASSE
175500100422     C                   MOVEL     'S'           VC(810)                        *SEDE-CORR-CL.A
175600000303E   5C                   ENDIF
175700000303IF  5C     ETCCLV        IFEQ      'B'
175800100422     C                   MOVEL     'S'           VC(811)                        *SEDE-CORR-CL.B
175900000303E   5C                   ENDIF
176000000303IF  5C     ETCCLV        IFEQ      'C'
176100100422     C                   MOVEL     'S'           VC(812)                        *SEDE-CORR-CL.C
176200000303E   5C                   ENDIF
176300000303IF  5C     ETCCLV        IFEQ      'T'
176400020213     C     ETCCLV        OREQ      'D'
176500100422     C                   MOVEL     'S'           VC(819)                        *SEDE-CORR-CL.T
176600000303E   5C                   ENDIF
176700000303E   4C                   ENDIF
176800000303     C*
176900000303IF  4C     D00TSP        IFEQ      'E'                                          *ESPRESSO
177000000303IF  5C     ETCCLV        IFEQ      'A'                                          *CLASSE
177100100422     C                   MOVEL     'S'           VC(813)                        *SEDE-ESPR-CL.A
177200000303E   5C                   ENDIF
177300000303IF  5C     ETCCLV        IFEQ      'B'
177400100422     C                   MOVEL     'S'           VC(814)                        *SEDE-ESPR-CL.B
177500000303E   5C                   ENDIF
177600000303IF  5C     ETCCLV        IFEQ      'C'
177700100422     C                   MOVEL     'S'           VC(815)                        *SEDE-ESPR-CL.C
177800000303E   5C                   ENDIF
177900000303IF  5C     ETCCLV        IFEQ      'T'
178000020213     C     ETCCLV        OREQ      'D'
178100100422     C                   MOVEL     'S'           VC(820)                        *SEDE-ESPR-CL.T
178200000303E   5C                   ENDIF
178300000303E   4C                   ENDIF
178400000303E   3C                   ENDIF
178500000303E   2C                   ENDIF
178600000303     C* CONTROLLO SE ESISTE =======> RICARICO SEDE EXPORT DPD
178700040609     C*    FRANCHI EXPORT ( ASSEGNATI IMPORT non può essere xchè la lnp = I)
178800000303     C                   Z-ADD     TLNA(T)       F
178900000303IF  2C     DPD(F)        IFEQ      'S'                                          *L.ARRIVO DPD
179000040609     c* non è d'applicare se la % franco = 0 e %assegnato = 100
179100040609     c     etcfra        andeq     100
179200100422     C                   MOVEL     'S'           VC(823)                        *RIC.SEDE-EXPORT DPD
179300000303E   2C                   ENDIF
179400000228     C*
1795009807101    C                   ENDIF                                                  *SOLO SEQ. 3 -------
179600961128     C*
179700961128     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
179800970203     C*    SE UNA VOCE E' DA CALCOLARE MA NON E' STATA CARICATA PERCHE' NON
179900970203     C*    VALIDA ALLA DATA, LA DISATTIVO
1800009702111    C                   DO        NVOCA         N
180100970207     C                   Z-ADD     VOCA(N)       V
1802009611282    C     VC(V)         IFEQ      'S'                                          *VOCE DA CALCOLARE
1803009702033    C     VDDV(V)       IFEQ      *ZEROS                                       *DECORRENZA
180400970203     C                   MOVEL     'N'           VC(V)                          *DISATTIVO
1805009611283    C                   ENDIF
1806009611282    C                   ENDIF
1807009611281    C                   ENDDO
180800961128     C*
180900961128     C                   ENDSR
181000961128     C*------------------------------------------------------------------------*
181100961128     C* VALVOC - VALORIZZA LE VOCI VALIDE
181200961128     C*------------------------------------------------------------------------*
181300961128     C     VALVOC        BEGSR
181400970131     C*
181500970203     C* SEGUENTI VOCI DA NON CALCOLARE PERCHE' UTILIZZATE NEL FORMULONE
181600970203     C                   MOVEL     'N'           VC(270)
181700040609     C                   MOVEL     'N'           VC(305)
181800970203     C                   MOVEL     'N'           VC(300)
181900970203     C                   MOVEL     'N'           VC(900)
182000100422     C                   MOVEL     'N'           VC(816)
182100100422     C                   MOVEL     'N'           VC(817)
182200100422     C                   MOVEL     'N'           VC(818)
182300100422     C                   MOVEL     'N'           VC(821)
182400100422     C                   MOVEL     'N'           VC(810)
182500100422     C                   MOVEL     'N'           VC(811)
182600100422     C                   MOVEL     'N'           VC(812)
182700100422     C                   MOVEL     'N'           VC(819)
182800100422     C                   MOVEL     'N'           VC(813)
182900100422     C                   MOVEL     'N'           VC(814)
183000100422     C                   MOVEL     'N'           VC(815)
183100100422     C                   MOVEL     'N'           VC(820)
183200961128     C*
183300961128     C* CICLO PER TUTTE LE VOCI 'DA CALCOLARE'
183400970206     C                   Z-ADD     *ZEROS        VIM                            *SCHIERA IMP.DA SCR.
183500970207     C                   DO        NVOCA         N                 3 0
183600970207     C                   Z-ADD     VOCA(N)       V
183700970206     C     VIMP(V)       IFGT      *ZEROS
183800970207     C     VS(V)         IFGE      WSEQ
183900970206     C                   Z-ADD     *ZEROS        VIMP(V)
184000970206     C                   ELSE
184100970206     C                   Z-ADD     VIMP(V)       VIM(V)
184200970206     C                   ENDIF
184300970206     C                   ENDIF
184400970207     C     VS(V)         IFEQ      WSEQ
184500961128     C     VC(V)         IFEQ      'S'                                          *VOCE DA CALC.
184600961128     C                   Z-ADD     V             WVOC
184700961128     C                   Z-ADD     VPRG(V)       WPRG
184800000306     C                   MOVEL     VTIV(V)       WTIV
184900961128     C* CERCA TARIFFA
185000011017     C                   EXSR      CERTAR
185100961128     C*
185200961128     C* SE NON C'E' ERRORE (ES. MANCA TARIFFA)
185300961128     C* E IL MANCA TARIFFA ERA DA SEGNALARE
185400961128     C     $ERR          IFEQ      'N'
185500961128     C     WMSGMT        ANDEQ     'S'                                          *MSG  MANCA TARIFFA
185600961128     C*
185700961128     C* SE TIPO VALORIZZAZIONE = Q=QUANTITA  OPPURE P=PERCENTUALE
185800961128     C* RECUPERO LA QTA O IL VALORE DI RIFERIMENTO
185900011025     C                   Z-ADD     *ZEROS        WQTA             23 8
186000961128     C     VVRV(V)       IFNE      *BLANKS
186100961128     C                   Z-ADD     1             I
186200961128     C     VVRV(V)       LOOKUP    C2R(I)                                 98
186300961128     C     *IN98         IFEQ      '0'
186400961128     C                   MOVEL     MSG(19)       $MSG                           *TIPO TARIF. NON TRO
186500961128     C                   EXSR      SCRERR
186600961128     C                   ELSE
186700961128     C*
186800961128     C* SE E' UNA TARIFFA DOVE E' STATO CALCOLATO IL PESO TASSABILE
186900961128     C* (GIA' CALCOLATO NELLA RICERCA DELLO SCAGLIONE) PRENDO QTA
187000961128     C* UTILIZZATA PER LA RICERCA DELLO SCAGLIONE
187100961128     C* ALTRIMENTI PRENDO QTA DELLA SCHIERA
187200961128     C     VSPT(V)       IFNE      *BLANKS                                      *C'E' SVIL.PESO T
187300961128     C     WQTAPT        ANDGT     *ZEROS                                       *QTA PT > 0
187400961128     C                   Z-ADD     WQTAPT        WQTA                           *QTA PESO TAS.RIC.TA
187500961128     C                   ELSE                                                   *NON C'E' SVIL.PESO
187600961128     C                   Z-ADD     Q2R(I)        WQTA                           *QTA RIFERIM.
187700961128     C                   ENDIF
187800040811     C*
187900040811     C* SE E' UNA TARIFFA A BANCALI IMPOSTO LA QUANTITÀ A SECONDA CHE
188000040811     C* STO CALCOLANDO BANCALI BARTOLINI O BANCALI BNB
188100040811     C     VB(V)         IFEQ      'B'                                          *C'E' SVIL.PESO T
188200040811     C                   Z-ADD     ETCNBA        WQTA                           *QTA PESO TAS.RIC.TA
188300040811     C                   END                                                    *NON C'E' SVIL.PESO
188400040811     C     VB(V)         IFEQ      'O'                                          *C'E' SVIL.PESO T
188500040811     C                   Z-ADD     ETCNBN        WQTA                           *QTA RIFERIM.
188600040811     C                   ENDIF
188700040811     C                   MOVEL     T2R(I)        WTTQ              1            *TIPO TARIFFA QTA
188800961128     C                   ENDIF
188900961128     C                   ENDIF
189000961128     C*
189100961128     C* CONTROLLO L'ARROTONDAMENTO
189200011017     C                   EXSR      ARROTO                                       *ARROTONDA QTA
189300961128     C*
189400011025     C                   Z-ADD     *ZEROS        WVVOCE           23 8          *IMPORTO VOCE
189500961128     C*
189600961128     C* SE TIPO VOCE V=VOCE NORMALE
189700961128     C* IN BASE AL TIPO TARIFFA CALCOLA IMPORTO A QTA, FISSO, PERCENTUALE
189800140930     C                   CLEAR                   FLGCAP            1
189900961128     C     VTVO(V)       IFEQ      'V'                                          *VOCE
190000961128     C                   EXSR      CALVOC                                       *CALCOLA VOCI NORMAL
190100961128     C                   ENDIF
190200141001     C* VERIFICO SE PRESENTI TARIFFE PER codice tassazione e CAP
190300141001     c                   if        wseq = 3
190400140930     C                   EXSR      ETT10IMP
190500141001     c                   end
190600961128     C*
190700961128     C* SE TIPO VOCE M=MAGGIORAZIONE
190800961128     C* RECUPERO TUTTE LE VOCI DA MAGGIORARE E APPLICO LA PERCENTUALE
190900970204     C     VTVO(V)       IFEQ      'M'                                          *MAGGIORAZIONE
191000970205     C                   EXSR      CERMAG                                       *CALCOLA MAGGIORAZIO
191100970204     C                   ENDIF
191200961128     C*
191300961128     C                   ENDIF
191400961128     C                   ENDIF
191500970203     C                   ENDIF
191600961128     C                   ENDDO
191700961128     C*
191800961128     C                   ENDSR
191900961128     C*------------------------------------------------------------------------*
192000961128     C* CALVOC - CALCOLA LE VOCI NORMALI
192100961128     C*------------------------------------------------------------------------*
192200961128     C     CALVOC        BEGSR
192300961128     C*
192400961128     C* CALCOLO IN BASE AL TIPO TARIFFA
192500961128     C*
192600961128     C     WTTQ          IFEQ      'Q'                                          *TARIFFA PER QUANTIT
192700961128     C     WQTA          ANDNE     *ZEROS
192800961128     C     EVDITR        ANDNE     *ZEROS
192900961128     C     WQTA          MULT      EVDITR        WVVOCE
193000961128     C                   ENDIF
193100961128     C*
193200961128     C     WTTQ          IFEQ      'F'                                          *TARIF.IMPORTO FISSO
193300961128     C                   Z-ADD     EVDITR        WVVOCE
193400961128     C                   ENDIF
193500961128     C*
193600961128     C     WTTQ          IFEQ      'P'                                          *TARIFFA PER PERCENT
193700961128     C     WQTA          ANDNE     *ZEROS
193800961128     C     EVDPER        ANDNE     *ZEROS
193900961128     C     WQTA          DIV       100           WVVOCE
194000961128     C     WVVOCE        MULT      EVDPER        WVVOCE
194100961128     C                   ENDIF
194200961128     C*
194300961128     C* CONTROLLO IMPORTO MINIMO
194400961128     C     EVDIMI        IFNE      *ZEROS
194500961128     C     WVVOCE        ANDLT     EVDIMI
194600961128     C                   Z-ADD     EVDIMI        WVVOCE
194700961128     C                   ENDIF
194800961128     C     EVDIMA        IFNE      *ZEROS
194900961128     C     WVVOCE        ANDGT     EVDIMA
195000961128     C                   Z-ADD     EVDIMA        WVVOCE
195100961128     C                   ENDIF
195200961128     C*
195300961128     C* CONTROLLO SEGNO E CARICO IMPORTO NELLA SCHIERA
195400961128     C     EVDSEG        IFEQ      '+'                                          *SEGNO VOCE POSITIVO
195500141001     C* no SE RICHIAMATO DALLA SUBROUTINE DEI CAP
195600141001     C                   IF        FLGCAP = *blanks
195700970205     C                   Z-ADD     WVVOCE        VIMP(V)
195800970205     C                   Z-ADD     WVVOCE        VIM(V)
195900140930     C                   END
196000961128     C                   ELSE                                                   *SEGNO VOCE NEGATIVO
196100141001     C* no SE RICHIAMATO DALLA SUBROUTINE DEI CAP
196200141001     C                   IF        FLGCAP = *blanks
196300961128     C                   Z-SUB     WVVOCE        VIMP(V)
196400970205     C                   Z-SUB     WVVOCE        VIM(V)
196500961128     C                   ENDIF
196600140930     C                   ENDIF
196700961128     C*
196800961128     C                   ENDSR
196900961128     C*------------------------------------------------------------------------*
197000961128     C* ARROTO - ARROTONDA LA QUANTITA'
197100961128     C*          INPUT  : WQTA (DA ARROTONDARE)
197200961128     C*          OUTPUT : WQTA (ARROTONDATA)
197300961128     C*------------------------------------------------------------------------*
197400961128     C     ARROTO        BEGSR
197500011017     C*
197600011017     C* SE LA DIVISA DI CALCOLO DEL CE E' ITL => ESEGUO ARROTONDAMENTO
197700021128     C* ERRORACCIO: si parla di qtà anche NON valuta !!!
197800021128     C*----WDIVCE        IFEQ      'ITL'
197900961128     C*
198000961128     C* CONTROLLO SE ESISTE L'ARROTONDAMENTO
198100970227     C     WQTA          IFNE      *ZEROS
198200961128     C     VARL(V)       ANDNE     *ZEROS
198300961128     C     WQTA          IFLE      VARL(V)                                      *VERIFICO LIMITE ARR
198400011025     C                   Z-ADD     VARF(V)       WARR             23 8
198500961128     C                   ELSE
198600961128     C                   Z-ADD     VARO(V)       WARR
198700961128     C                   ENDIF
198800970227     C*
198900961128     C* CONTROLLO SE DATO GIA' ARRORONDATO
199000011025     C     WQTA          DIV       WARR          WINT             18 0          *ARROTONDO
199100011025     C                   MVR                     WT0155           21 8    99     * 99=ON --> RESTO=0
199200961128     C     *IN99         IFEQ      *OFF                                         *NON IMPORTA ARROT.
199300961128     C     WINT          MULT      WARR          WQTA
199400961128     C                   ADD       WARR          WQTA
199500961128     C                   ENDIF
199600970227     C                   ENDIF
199700961128     C*
199800021128     C*---               ENDIF
199900011017     C*
200000961128     C                   ENDSR
200100961128     C*------------------------------------------------------------------------*
200200961128     C* CALPT - CALCOLA IL PESO TASSABILE
200300961128     C*------------------------------------------------------------------------*
200400961128     C     CALPT         BEGSR
200500961128     C*
200600961128     C* SE ESISTE RAPPORTO PESO/VOLUME CALCOLO PESO TASSABILE
200700961128     C     EVDRPV        IFNE      *ZEROS
200800961128     C*
200900961128     C* CALCOLO PESO TASSABILE DAL VOLUME
201000961128     C     SVSC(S)       MULT      EVDRPV        WPTAS
201100961128     C     WPTAS         MULT      100           WPTAS
201200961128     C*
201300961128     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
201400961128     C     VSPT(V)       IFEQ      '1'
201500961128     C                   Z-ADD     WPTAS         WQTA
201600961128     C                   ENDIF
201700961128     C*
201800961128     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
201900961128     C     VSPT(V)       IFEQ      '2'
202000991008     C********** WPTAS     IFGT WQTA
202100991008     C     WPTAS         IFGT      SPSC(S)
202200961128     C                   Z-ADD     WPTAS         WQTA
202300961128     C                   ENDIF
202400961128     C                   ENDIF
202500961128     C*
202600961128     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
202700961128     C     VSPT(V)       IFEQ      '3'
202800991008     C********** WPTAS     IFLT WQTA
202900991008     C     WPTAS         IFLT      SPSC(S)
203000961128     C                   Z-ADD     WPTAS         WQTA
203100961128     C                   ENDIF
203200961128     C                   ENDIF
203300961128     C*
203400961128     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
203500011025     C                   Z-ADD     WQTA          WQTAPT           21 8          *PESO TASS.X CALC.CO
203600961128     C*
203700961128     C                   ENDIF
203800961128     C*
203900961128     C                   ENDSR
204000030429     C*------------------------------------------------------------------------*
204100030429     C*  CALPS  - CALCOLA IL PESO TASSABILE PER RICERCA SCAGLIONE
204200030429     C*------------------------------------------------------------------------*
204300030429     C     CALPS         BEGSR
204400030429     C*
204500030429     C* CALCOLO PESO TASSABILE DAL VOLUME
204600030429     C     SVSC(S)       MULT      VRPV(V)       WPTASR
204700030429     C     WPTASR        MULT      100           WPTASR
204800030429     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
204900030429     C     VSPS(V)       IFEQ      '1'
205000030429     C                   Z-ADD     WPTASR        WQTA
205100030429     C                   ENDIF
205200030429     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
205300030429     C     VSPS(V)       IFEQ      '2'
205400030429     C     WPTASR        IFGT      SPSC(S)
205500030429     C                   Z-ADD     WPTASR        WQTA
205600060504     c                   else
205700060504     C                   Z-ADD     SPSC(S)       WQTA
205800030429     C                   ENDIF
205900030429     C                   ENDIF
206000030429     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
206100030429     C     VSPS(V)       IFEQ      '3'
206200030429     C     WPTASR        IFLT      SPSC(S)
206300030429     C                   Z-ADD     WPTASR        WQTA
206400060504     c                   else
206500060504     C                   Z-ADD     SPSC(S)       WQTA
206600030429     C                   ENDIF
206700030429     C                   ENDIF
206800030429     C*
206900030429     C                   ENDSR
207000961128     C*------------------------------------------------------------------------*
207100961128     C* CERMAG - CERCA LE MAGGIORAZIONI
207200961128     C*------------------------------------------------------------------------*
207300961128     C     CERMAG        BEGSR
207400961128     C*
207500961128     C* SE PERCENTUALE DIVERSA DA ZERO NON CERCO NEMMENO LA MAGGIORAZIONE
207600970205     C     EVDPER        IFNE      *ZEROS                                       --- 1 -->
207700961128     C                   MOVEL     'N'           WMAGOK            1            *MAGG. NON TROVATA
207800961128     C*
207900961128     C* CONTROLLA SE MAGGIORAZIONI IN MEMORIA
208000961128     C                   Z-ADD     1             M                 6 0
208100961128     C     M             DOWLE     WULTMA                                       FINO ULTIMA MAGG.
208200970205     C     WVOC          IFEQ      MVOM(M)                                      *CONTR. CAMPI CHIAVE
208300970205     C     WPRG          ANDEQ     MPRM(M)
208400961128     C                   MOVEL     'S'           WMAGOK                         *MAGG. TROVATA
208500961128     C                   Z-ADD     MVOM(M)       EVMVOM
208600961128     C                   Z-ADD     MPRM(M)       EVMPRM
208700961128     C                   Z-ADD     MVOC(M)       EVMVOC
208800961128     C                   Z-ADD     MPRG(M)       EVMPRG
208900961128     C                   EXSR      CALMAG                                       CALCOLA MAGGIORAZION
209000961128     C                   ENDIF
209100961128     C                   ADD       1             M
209200961128     C                   ENDDO
209300961128     C*
209400961128     C* SE NON TROVATA IN MEMORIA E FINE SCHIERA LEGGO SUL FILE
209500961128     C     WMAGOK        IFEQ      'N'                                          --- 2 -->
209600050104     C     WULTMA        ANDGE     600
209700961128     C*
209800050104     C********           Z-ADD     WVOC          EVMVOC
209900050104     C                   Z-ADD     WVOC          EVMVOM
210000961128     C                   Z-ADD     WPRG          EVMPRM
210100961128     C                   Z-ADD     *ZEROS        EVMVOC
210200961128     C                   Z-ADD     *ZEROS        EVMPRG
210300961128     C     KEYEVM        SETLL     ECEVM01L                           99
210400961128     C  N99              READ      ECEVM01L                               99
210500961128     C     *IN99         IFEQ      '1'
210600961128     C                   MOVEL     MSG(22)       $MSG                           *MAGG. SENZA MAGG.
210700961128     C                   EXSR      SCRERR
210800961128     C                   END
210900961128     C     *IN99         DOWEQ     '0'
211000961128     C     EVMVOM        ANDEQ     WVOC
211100961128     C     EVMPRM        ANDEQ     WPRG
211200961128     C     EVMATB        IFEQ      *BLANKS
211300961128     C                   EXSR      CALMAG                                       *CALCOLA MAGG.
211400961128     C                   ENDIF
211500961128     C                   READ      ECEVM01L                               99
211600961128     C                   ENDDO
211700961128     C*
211800961128     C                   ENDIF                                                  <-- 2 ---
211900961128     C                   ENDIF                                                  <-- 1 ---
212000961128     C*
212100961128     C                   ENDSR
212200961128     C*------------------------------------------------------------------------*
212300961128     C* CALMAG - CALCOLA LA MAGGIORAZIONE
212400961128     C*------------------------------------------------------------------------*
212500961128     C     CALMAG        BEGSR
212600961128     C*
212700970205     C                   Z-ADD     EVMVOC        I
212800961128     C*
212900041105     C                   IF        VC(I) = 'S'
213000970206     C* TROVO LA % DELLA MAGGIORAZIONE (% DA CONSIDERARE DELLA % DELLA MAGG.)
213100970206     C     EVDPER        DIV       100           WPERMA
213200011025     C     WPERMA        MULT      VP(V)         WPERMA           23 8
213300970227     C*
213400970227     C* APPLICA LA PERCENTUALE
213500050121     C     VIM(I)        DIV       100           WVVOCE
213600970206     C     WVVOCE        MULT      WPERMA        WVVOCE
213700961128     C*
213800961128     C* CONTROLLO IMPORTO MINIMO
213900970205     C     EVDIMI        IFNE      *ZEROS
214000970205     C     WVVOCE        ANDLT     EVDIMI
214100970205     C                   Z-ADD     EVDIMI        WVVOCE
214200961128     C                   ENDIF
214300961128     C*
214400961128     C* CONTROLLO IMPORTO MASSIMO
214500970205     C     EVDIMA        IFNE      *ZEROS
214600970205     C     WVVOCE        ANDGT     EVDIMA
214700970205     C                   Z-ADD     EVDIMA        WVVOCE
214800961128     C                   ENDIF
214900961128     C*
215000961128     C* CONTROLLO SEGNO E AGGIUNGO O TOLGO IMPORTO NELLA SCHIERA
215100970205     C     EVDSEG        IFEQ      '+'                                          *SEGNO VOCE POSITIVO
215200050121     C     VIM(I)        ADD       WVVOCE        VIM(I)                         *IMPORTO VOCE
215300050121     C                   ELSE                                                   *SEGNO VOCE NEGATIVO
215400050121     C     VIM(I)        SUB       WVVOCE        VIM(I)
215500961128     C                   ENDIF
215600041105     C                   ENDIF
215700961128     C*
215800961128     C                   ENDSR
215900961128     C*------------------------------------------------------------------------*
216000961128     C* CERTAR - CERCA LA TARIFFA
216100961128     C*------------------------------------------------------------------------*
216200961128     C     CERTAR        BEGSR
216300961128     C*
216400970227     C* IMPOSTA CAMPI LAVORO
216500961128     C                   MOVEL     'N'           WFLTT             1            *TARIFFA TROVATA
216600961128     C                   MOVEL     *BLANKS       WRIF                           *RIF.TARIFFA
216700961128     C                   MOVEL     *BLANKS       WTT1              3            *TERM-TERM, X MSG
216800961128     C                   MOVEL     *BLANKS       WTT2              3            *TERM-TERM, X MSG
216900961128     C                   MOVEL     *BLANKS       WCTSM             2            *COD.TASS., X MSG
217000961128     C                   MOVEL     'S'           WMSGMT            1            *MESSAGG.MANCA TARIF
217100011025     C                   Z-ADD     *ZEROS        WPTAS            21 8          *PESO TASSABILE
217200030429     C                   Z-ADD     *ZEROS        WPTASR           21 8          *P.TASSABILE RICERCA
217300961128     C                   MOVEL     D00FIE        WIE               1
217400030429     C*
217500030429     C* calcola il peso tassabile da ricercare nello scaglione
217600030429     C     VSPS(V)       IFGT      *ZEROS
217700030429     C                   EXSR      CALPS
217800050309     C*????              Z-ADD     WPTASR        WQTA
217900030429     C                   ELSE
218000961128     C*
218100961128     C* CERCA LA QTA DA RICERCARE NELLO SCAGLIONE
218200961128     C                   Z-ADD     *ZEROS        WQTA
218300961128     C                   Z-ADD     1             I
218400961128     C     VTIV(V)       LOOKUP    C2R(I)                                 98
218500961128     C     *IN98         IFEQ      '0'
218600961128     C                   MOVEL     MSG(20)       $MSG                           *QTA X RIC.SC. NON T
218700961128     C                   EXSR      SCRERR
218800961128     C                   ELSE
218900961128     C                   Z-ADD     Q2R(I)        WQTA                           *QTA DA RICERCARE
219000961128     C                   END
219100030429     C*
219200030429     C                   ENDIF
219300961128     C*
219400961128     C* SE RICHIESTO, TENTATIVO PER CLIENTE
219500961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
219600961128     C     WIE           ANDEQ     'I'                                          *ITALIA
219700961128     C     VRKI(V)       ANDNE     *BLANKS                                      *RIF.CLI.ITALIA
219800961128     C                   MOVEL     VRKI(V)       WRIF
219900961128     C                   EXSR      CERCLI                                       *CERCA CLIENTE
220000961128     C                   ENDIF
220100961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
220200961128     C     WIE           ANDEQ     'E'                                          *ESTERO
220300961128     C     VRKE(V)       ANDNE     *BLANKS                                      *RIF.CLI.ESTERO
220400961128     C                   MOVEL     VRKE(V)       WRIF
220500961128     C                   EXSR      CERCLI                                       *CERCA CLIENTE
220600961128     C                   ENDIF
220700961128     C*
220800961128     C* SE RICHIESTO, TENTATIVO PER CODICE TASSAZIONE
220900961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
221000961128     C     WIE           ANDEQ     'I'                                          *ITALIA
221100961128     C     VRTI(V)       ANDNE     *BLANKS                                      *RIF.TAS.ITALIA
221200961128     C                   MOVEL     VRTI(V)       WRIF
221300961128     C                   EXSR      CERTAS                                       *CERCA TASSAZIONE
221400961128     C                   ENDIF
221500961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
221600961128     C     WIE           ANDEQ     'E'                                          *ESTERO
221700961128     C     VRTE(V)       ANDNE     *BLANKS                                      *RIF.TAS.ESTERO
221800961128     C                   MOVEL     VRTE(V)       WRIF
221900961128     C                   EXSR      CERTAS                                       *CERCA TASSAZIONE
222000961128     C                   ENDIF
222100961128     C*
222200961128     C* SE RICHIESTO, TENTATIVO PER CODICE FILIALE
222300961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
222400961128     C     WIE           ANDEQ     'I'                                          *ITALIA
222500961128     C     VRFI(V)       ANDNE     *BLANKS                                      *RIF.FIL.ITALIA
222600961128     C                   MOVEL     VRFI(V)       WRIF
222700961128     C                   EXSR      CERFIL                                       *CERCA FILIALE
222800961128     C                   ENDIF
222900961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
223000961128     C     WIE           ANDEQ     'E'                                          *ESTERO
223100961128     C     VRFE(V)       ANDNE     *BLANKS                                      *RIF.FIL.ESTERO
223200961128     C                   MOVEL     VRFE(V)       WRIF
223300961128     C                   EXSR      CERFIL                                       *CERCA FILIALE
223400961128     C                   ENDIF
223500961128     C*
223600961128     C* SE TARIFFA NON ANCORA TROVATA CERCA TARIFFA DI CARTELLO
223700961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
223800961128     C                   EXSR      CERCAR                                       *CERCA CARTELLO
223900961128     C                   ENDIF
224000961128     C*
224100961128     C* CONTROLLO MANCA TARIFFA
224200961128     C     WFLTT         IFEQ      'N'                                          --- 1 -->
224300961128     C     WMSGMT        IFEQ      'S'                                          *MESSAGG.MANCA TARIF
224400961128     C                   MOVEL     MSG(21)       DSMSG                          *MANCA TARIFFA
224500961128     C                   MOVE      WVOC          DSVOCE                         *COD.VOCE
224600961128     C                   MOVE      WTT1          DSTT1                          *TERM.1
224700961128     C                   MOVE      WTT2          DSTT2                          *TERM.2
224800961128     C                   MOVE      WCTSM         DSCTS                          *COD.TASS.
224900961128     C                   MOVEL     DSMSG         $MSG                           *MANCA TARIFFA
225000961128     C                   EXSR      SCRERR
225100961128     C                   ENDIF
225200961128     C                   ELSE
225300961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
225400961128     C                   END                                                    <-- 1 ---
225500961128     C*
225600961128     C                   ENDSR
225700961128     C*------------------------------------------------------------------------*
225800961128     C* CERCLI - CERCA LA TARIFFA PER CLIENTE
225900961128     C*------------------------------------------------------------------------*
226000961128     C     CERCLI        BEGSR
226100961128     C*
226200961128     C                   MOVEL     'K'           WTTV
226300961128     C                   Z-ADD     *ZEROS        WFT1
226400961128     C                   Z-ADD     *ZEROS        WFT2
226500961128     C                   Z-ADD     *ZEROS        WKSC
226600961128     C                   Z-ADD     *ZEROS        WCTR
226700961128     C                   MOVEL     *BLANKS       WCTS
226800961128     C*
226900021129     C                   Z-ADD     DEPKSC        WKSC
227000961128     C                   Z-ADD     D00CTR        WCTR
227100961128     C*
227200961128     C* LANCIA GESTIONE TARIFFE
227300961128     C                   EXSR      GESEVD
227400961128     C*
227500961128     C* SE TARIFFA BOLLA NON E' STATA TROVATA PROVO CON TARIFFA = 999 (TUTTE)
227600961128     C     WFLTT         IFEQ      'N'                                          *TARIFFA NON TROVATA
227700021129     C                   Z-ADD     DEPKSC        WKSC
227800961128     C                   Z-ADD     999           WCTR
227900961128     C                   EXSR      GESEVD                                       *GEST. TARIFFE
228000961128     C                   ENDIF
228100961128     C*
228200961128     C                   ENDSR
228300961128     C*------------------------------------------------------------------------*
228400961128     C* CERTAS - CERCA LA TARIFFA PER CODICE TASSAZIONE
228500961128     C*------------------------------------------------------------------------*
228600961128     C     CERTAS        BEGSR
228700961128     C*
228800961128     C                   MOVEL     'T'           WTTV
228900961128     C                   Z-ADD     *ZEROS        WFT1
229000961128     C                   Z-ADD     *ZEROS        WFT2
229100961128     C                   Z-ADD     *ZEROS        WKSC
229200961128     C                   Z-ADD     *ZEROS        WCTR
229300961128     C                   MOVEL     *BLANKS       WCTS
229400961128     C     WRIF          IFEQ      'SP'
229500961128     C                   MOVEL     WCTP          WCTS
229600961128     C                   ENDIF
229700961128     C     WRIF          IFEQ      'SA'
229800961128     C                   MOVEL     TCTS(T)       WCTS
229900961128     C                   ENDIF
230000961128     C*
230100961128     C* LANCIA GESTIONE TARIFFE
230200961128     C                   EXSR      GESEVD
230300961128     C*
230400961128     C* CAMPI PER MESSAGGI
230500961128     C                   MOVE      WCTS          WCTSM
230600961128     C*
230700961128     C                   ENDSR
230800961128     C*------------------------------------------------------------------------*
230900961128     C* CERFIL - CERCA LA TARIFFA PER FILIALE/TERMINAL
231000961128     C*------------------------------------------------------------------------*
231100961128     C     CERFIL        BEGSR
231200961128     C*
231300961128     C                   MOVEL     'F'           WTTV
231400961128     C                   Z-ADD     *ZEROS        WFT1
231500961128     C                   Z-ADD     *ZEROS        WFT2
231600961128     C                   Z-ADD     *ZEROS        WKSC
231700961128     C                   Z-ADD     *ZEROS        WCTR
231800961128     C                   MOVEL     *BLANKS       WCTS
231900140115     C                   MOVEL     *BLANKS       Wnaz
232000140115     C                   MOVEL     *BLANKS       wcad
232100961128     C     WRIF          IFEQ      'LP'
232200961128     C                   Z-ADD     D00LNP        WFT1
232300961128     C                   ENDIF
232400961128     C     WRIF          IFEQ      'LT'
232500961128     C                   Z-ADD     D00LNP        WFT1
232600961128     C                   Z-ADD     WTFP          WFT2
232700961128     C                   ENDIF
232800961128     C     WRIF          IFEQ      'TP'
232900961128     C                   Z-ADD     WTFP          WFT1
233000961128     C                   ENDIF
233100961128     C     WRIF          IFEQ      'TT'
233200961128     C                   Z-ADD     WTFP          WFT1
233300961128     C                   Z-ADD     TTFA(T)       WFT2
233400961128     C                   ENDIF
233500961128     C     WRIF          IFEQ      'TA'
233600961128     C                   Z-ADD     TTFA(T)       WFT1
233700961128     C                   ENDIF
233800961128     C     WRIF          IFEQ      'TL'
233900961128     C                   Z-ADD     TTFA(T)       WFT1
234000961128     C                   Z-ADD     TLNA(T)       WFT2
234100961128     C                   ENDIF
234200961128     C     WRIF          IFEQ      'LA'
234300961128     C                   Z-ADD     TLNA(T)       WFT1
234400961128     C                   ENDIF
234500961128     C*
234600961128     C* LANCIA GESTIONE TARIFFE
234700961128     C                   EXSR      GESEVD
234800961128     C*
234900961128     C* CAMPI PER MESSAGGI
235000961128     C                   MOVE      WFT1          WTT1
235100961128     C                   MOVE      WFT2          WTT2
235200961128     C*
235300961128     C                   ENDSR
235400961128     C*------------------------------------------------------------------------*
235500961128     C* CERCAR - CERCA LA TARIFFA DI CARTELLO
235600961128     C*------------------------------------------------------------------------*
235700961128     C     CERCAR        BEGSR
235800961128     C*
235900961128     C                   MOVEL     'C'           WTTV
236000961128     C                   Z-ADD     *ZEROS        WFT1
236100961128     C                   Z-ADD     *ZEROS        WFT2
236200961128     C                   Z-ADD     *ZEROS        WKSC
236300961128     C                   Z-ADD     *ZEROS        WCTR
236400961128     C                   MOVEL     *BLANKS       WCTS
236500140115     C                   MOVEL     *BLANKS       Wnaz
236600140115     C                   MOVEL     *BLANKS       wcad
236700961128     C*
236800961128     C* LANCIA GESTIONE TARIFFE
236900961128     C                   EXSR      GESEVD
237000961128     C*
237100961128     C                   ENDSR
237200140930     C*------------------------------------------------------------------------*
237300140930     C* SE RICHIESTO, TENTATIVO PER CAP
237400140930     C*------------------------------------------------------------------------*
237500140930     C     ETT10IMP      BEGSR
237600140930     C*
237700140930     C                   CLEAR                   TOTALECTS
237800140930     C                   EVAL      FLGCAP = '1'
237900140930     C*
238000140930     C     WIE           IFEQ      'I'
238100140930     C     VRPI(V)       ANDNE     *BLANKS
238200140930     C                   MOVEL     VRPI(V)       WRIF
238300140930     C                   EXSR      CERCAP
238400140930     C                   ENDIF
238500140930     C     WIE           IFEQ      'E'
238600140930     C     VRPE(V)       ANDNE     *BLANKS
238700140930     C                   MOVEL     VRPE(V)       WRIF
238800140930     C                   EXSR      CERCAP
238900140930     C                   ENDIF
239000140930     C                   ENDSR
239100140930     C*------------------------------------------------------------------------*
239200140930     C*? CERCAP - CERCA LA TARIFFA PER CAP
239300140930     C*------------------------------------------------------------------------*
239400140930     C     CERCAP        BEGSR
239500140930     C*
239600140930     C                   MOVEL     'P'           WTTV
239700140930     C                   Z-ADD     *ZEROS        WFT1
239800140930     C                   Z-ADD     *ZEROS        WFT2
239900140930     C                   Z-ADD     *ZEROS        WKSC
240000140930     C                   Z-ADD     *ZEROS        WCTR
240100140930     C                   MOVEL     *BLANKS       WCTS
240200140930     C                   MOVEL     *BLANKS       wcad
240300140930     C                   MOVEL     *BLANKS       Wnaz
240400140930     C* PER OGNI CODICE TASSAZIONE
240500140930     c     t             occur     scad
240600140930     c                   do        600           c                 3 0
240700140930     c                   if        naz(c) = *blank
240800140930     c                   leave
240900140930     c                   end
241000141001     C                   MOVEL     'N'           WFLTT             1            *TARIFFA TROVATA
241100140930     c* cap completo
241200140930     C     WRIF          IFEQ      'PA'
241300140930     C                   MOVEl     naz(c)        Wnaz
241400140930     C                   MOVEl     cad(c)        wcad
241500140930     C* LANCIA GESTIONE TARIFFE
241600140930     C                   EXSR      GESEVD
241700140930     C                   ENDIF
241800140930     c* cap fittizio
241900141001     C                   if        (wfltt = 'N' and
242000141001     c                             wrif = 'PA') or wrif = 'PP'
242100140930     C                   MOVEl     naz(c)        Wnaz
242200140930     c     wnaz          lookup    d15(i)                                 88
242300140930     c                   if        *in88
242400140930     C                   eval      wcad = %subst(cad(c): pcf(i): ecf(i))
242500140930     C                   ENDIF
242600140930     C                   EXSR      GESEVD
242700140930     C                   ENDIF
242800140930     C* SE TROVO TARIFFA LA CARICO NELLA DS MULTIPLA
242900140930     C                   if        wfltt = 'S'
243000140930     C                   EXSR      CALVOC
243100141001     C                   EVAL      comodo = WVVOCE * INC(C) /100
243200141001     c                   else
243300141001     C                   EVAL      comodo = vimp(v) * INC(C) /100
243400140930     C                   END
243500141001     C                   ADD       comodo        TOTALECTS
243600140930     C                   ENDDO
243700140930     C* RIPORTO L'IMPORTO ALLA % DEL CODICE TASSAZIONE
243800140930     C                   IF        TOTALECTS <> 0
243900141001     C                   EVAL      VIMP(V) = TOTALECTS * 100 / TPPE(T)
244000141001     C                   EVAL      VIM(V)  = TOTALECTS * 100 / TPPE(T)
244100140930     C                   END
244200140930     C*
244300140930     C                   ENDSR
244400961128     C*------------------------------------------------------------------------*
244500961128     C* GESEVD - GESTIONE FILE ECEVD (TARIFFE)
244600961128     C*------------------------------------------------------------------------*
244700961128     C     GESEVD        BEGSR
244800961128     C*
244900961128     C* SE TIPO SVILUPPO PESO TASSABILE 2=MIGLIORE REALE/TASS. O 3=MINORE REA/TA
245000961128     C* CERCO IL RAPP PESO/VOL PRIMA DI ARROTONDARE
245100961128     C     VSPT(V)       IFEQ      '2'                                          --- 1 -->
245200961128     C     VSPT(V)       OREQ      '3'
245300961128     C*
245400961128     C* CERCA LO SCAGLIONE CON LA QTA DELLA BOLLA
245500961128     C                   EXSR      SETEVD
245600961128     C*
245700961128     C* SE IL RAPP. PESO/VOL MAGGIORE DI ZERO E VOLUME MAGGIORE DI ZERO
245800961128     C* CERCO SCAGLIONE CON PESO TASSABILE
245900961128     C     EVDRPV        IFNE      *ZEROS
246000961128     C     SVSC(S)       ANDNE     *ZEROS
246100961128     C*
246200961128     C* CALCOLO PESO TASSABILE
246300961128     C                   EXSR      CALPT                                        *CALCOLO PESO TASSAB
246400961128     C                   ENDIF
246500961128     C*
246600961128     C                   ENDIF                                                  <-- 1 ---
246700961128     C*
246800961128     C* CONTROLLA ARROTONDAMENTO
246900961128     C                   EXSR      ARROTO                                       *ARROTONDA QTA
247000961128     C*
247100961128     C* CERCA LO SCAGLIONE GIUSTO IN TARIFFA
247200961128     C                   EXSR      SETEVD
247300961128     C*
247400961128     C                   ENDSR
247500961128     C*------------------------------------------------------------------------*
247600961128     C* SETEVD - POSIZIONAMENTO E PRIMA LETTURA FILE TARIFFE
247700961128     C*------------------------------------------------------------------------*
247800961128     C     SETEVD        BEGSR
247900961128     C*
248000961128     C                   MOVEL     'N'           WSETLL            1
248100961128     C                   MOVEL     '0'           FINEVD            1
248200961128     C*
248300961128     C* SE TARIFFA DI CARTELLO CONTROLLO PRIMA SE CARICATA IN MEMORIA
248400961128IF  1C     WTTV          IFEQ      'C'
248500961128     C                   EXSR      CHKCAR
248600961128E   1C                   ENDIF
248700961128     C*
248800961128     C* SE TARIFFA DI FILIALE CONTROLLO PRIMA SE CARICATA IN MEMORIA
248900961128IF  1C     WTTV          IFEQ      'F'
249000961128     C                   EXSR      CHKFIL
249100961128E   1C                   ENDIF
249200961128     C*
249300961128     C* SE TARIFFA NON TROVATA IN MEMORIA LEGGO DAL FILE
249400961128IF  1C     WSETLL        IFEQ      'N'
249500961128     C*
249600961128     C* POSIZIONAMENTO E PRIMA LETTURA
249700961128     C                   Z-ADD     WVOC          EVDVOC
249800961128     C                   Z-ADD     WPRG          EVDPRG
249900961128     C                   MOVEL     WTTV          EVDTTV
250000961128     C                   Z-ADD     WFT1          EVDFT1
250100961128     C                   Z-ADD     WFT2          EVDFT2
250200961128     C                   Z-ADD     WKSC          EVDCLI
250300961128     C                   Z-ADD     WCTR          EVDCTR
250400961128     C                   MOVEL     WCTS          EVDCTS
250500140115     C                   movel     Wnaz          EVDnaz
250600140115     C                   movel(p)  wcad          EVDcad
250700961128     C                   Z-ADD     *ZEROS        EVDDDT
250800961128     C                   Z-ADD     *ZEROS        EVDSGI
250900140115     C     KEYEVD        SETLL     ECEVD06L                           99
251000961128     C*
251100961128IF  2C     *IN99         IFEQ      *ON
251200961128     C                   MOVEL     '1'           FINEVD
251300961128X   2C                   ELSE
251400961128     C                   EXSR      LETEVD
251500961128E   2C                   ENDIF
251600961128     C*
251700961128     C* CONTROLLO MANCA TARIFFA
251800961128IF  2C     WRECOK        IFEQ      'N'
251900050615     c                   clear                   ecevdds
252000961128X   2C                   ELSE
252100961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
252200961128E   2C                   ENDIF
252300961128     C*
252400961128E   1C                   ENDIF
252500961128     C*
252600961128     C                   ENDSR
252700961128     C*------------------------------------------------------------------------*
252800961128     C* LETEVD - LETTURA PROSSIMO RECORD TARIFFE
252900961128     C*------------------------------------------------------------------------*
253000961128     C     LETEVD        BEGSR
253100961128     C*
253200961128     C* LEGGO FINO A:
253300961128     C*     - FINE FILE O SUPERATO LIMITE FINALE
253400961128     C*     - TROVATO RECORD VALIDO
253500961128     C                   MOVEL     'N'           WRECOK
253600961128     C     FINEVD        DOWEQ     '0'                                          --- 1 -->
253700961128     C     WRECOK        ANDEQ     'N'
253800961128     C*
253900961128     C* LETTURE SUCCESSIVE ALLA PRIMA
254000140115     C                   READ      ECEVD06L                               99
254100961128     C*
254200961128     C     *IN99         IFEQ      '1'                                          *FINE FILE
254300961128     C     EVDVOC        ORNE      WVOC
254400961128     C     EVDPRG        ORNE      WPRG
254500961128     C     EVDTTV        ORNE      WTTV
254600961128     C     EVDFT1        ORNE      WFT1
254700961128     C     EVDFT2        ORNE      WFT2
254800961128     C     EVDCLI        ORNE      WKSC
254900961128     C     EVDCTR        ORNE      WCTR
255000961128     C     EVDCTS        ORNE      WCTS
255100140115     C     EVDCAd        ORNE      wcad
255200140115     C     EVDnaz        ORNE      Wnaz
255300961128     C                   MOVEL     '1'           FINEVD
255400961128     C                   ELSE
255500961128     C                   EXSR      CHKEVD
255600961128     C                   END
255700961128     C*
255800961128     C                   END                                                    <-- 1 ---
255900961128     C*
256000961128     C                   ENDSR
256100961128     C*------------------------------------------------------------------------*
256200961128     C* CHKEVD - CONTROLLA VALIDITA' DELLA TARIFFA
256300961128     C*------------------------------------------------------------------------*
256400961128     C     CHKEVD        BEGSR
256500961128     C*
256600961128     C                   MOVEL     'S'           WRECOK            1
256700961128     C* ESCLUDO SE ANNULLATO
256800961128     C     EVDATB        IFNE      *BLANKS
256900961128     C                   MOVEL     'N'           WRECOK
257000961128     C                   END
257100961128     C*
257200961128     C* ESCLUDO SE DATA SPEDIZIONE FUORI DALLA DECORR/SCAD.
257300961128     C     D00DTC        IFLT      EVDDDT
257400961128     C     D00DTC        ORGT      EVDDST
257500961128     C                   MOVEL     'N'           WRECOK
257600961128     C                   END
257700011019     C*
257800011019     C     WDIVCE        IFNE      WDIVAC
257900011019     C                   IF        WTIV = '21' OR
258000011019     C                             WTIV = '22' OR
258100011019     C                             WTIV = '23' OR
258200011019     C                             WTIV = '25'
258300011019     C                   EXSR      CNVEVDSG
258400011019     C                   ENDIF
258500011019     C                   EXSR      CNVEVD
258600011019     C                   ENDIF
258700961128     C*
258800961128     C* ESCLUDO SE QTA NON COMPRESA NELLO SCAGLIONE
258900000306IF  1C     WTIV          IFNE      *BLANKS                                      *SI RICERCA SCAGLION
259000011017     C*
259100011017     C     WQTA          IFLT      EVDSGI
259200961128     C     WQTA          ORGT      EVDSGF
259300961128     C                   MOVEL     'N'           WRECOK
259400011017     C                   ENDIF
259500000306E   1C                   ENDIF
259600961128     C*
259700961128     C                   ENDSR
259800970211     C*------------------------------------------------------------------------*
259900970211     C* PERFOR - DETERMINA PERCENTUALI PER FORMULA
260000970211     C*------------------------------------------------------------------------*
260100970211     C     PERFOR        BEGSR
260200970211     C*
260300040611     c                   do
260400970211     C* -----> CERCA LA VOCE
260500970211     C     D00TSP        IFEQ      'D'                                          *DISTRIBUZIONE
260600970211     C     ETCCLV        IFEQ      'A'                                          *CLASSE
260700100526     C                   Z-ADD     816           V                 6 0          *SEDE-DIST-CL.A
260800970211     C                   ENDIF
260900970211     C     ETCCLV        IFEQ      'B'
261000100526     C                   Z-ADD     817           V                              *SEDE-DIST-CL.B
261100970211     C                   ENDIF
261200970211     C     ETCCLV        IFEQ      'C'
261300100526     C                   Z-ADD     818           V                              *SEDE-DIST-CL.C
261400970211     C                   ENDIF
261500980709     C     ETCCLV        IFEQ      'T'
261600020213     C     ETCCLV        OREQ      'D'
261700100526     C                   Z-ADD     821           V                              *SEDE-DIST-CL.T
261800980709     C                   ENDIF
261900970211     C                   ENDIF
262000970211     C     D00TSP        IFEQ      'C'                                          *CORRIERE(NORMALE)
262100970211     C     ETCCLV        IFEQ      'A'                                          *CLASSE
262200100422     C                   Z-ADD     810           V                              *SEDE-NORM-CL.A
262300970211     C                   ENDIF
262400970211     C     ETCCLV        IFEQ      'B'
262500100422     C                   Z-ADD     811           V                              *SEDE-NORM-CL.B
262600970211     C                   ENDIF
262700970211     C     ETCCLV        IFEQ      'C'
262800100422     C                   Z-ADD     812           V                              *SEDE-NORM-CL.C
262900970211     C                   ENDIF
263000980709     C     ETCCLV        IFEQ      'T'
263100020213     C     ETCCLV        OREQ      'D'
263200100422     C                   Z-ADD     819           V                              *SEDE-NORM-CL.T
263300980709     C                   ENDIF
263400970211     C                   ENDIF
263500970211     C     D00TSP        IFEQ      'E'                                          *ESPRESSO
263600970211     C     ETCCLV        IFEQ      'A'                                          *CLASSE
263700100422     C                   Z-ADD     813           V                              *SEDE-ESPR-CL.A
263800970211     C                   ENDIF
263900970211     C     ETCCLV        IFEQ      'B'
264000100422     C                   Z-ADD     814           V                              *SEDE-ESPR-CL.B
264100970211     C                   ENDIF
264200970211     C     ETCCLV        IFEQ      'C'
264300100422     C                   Z-ADD     815           V                              *SEDE-ESPR-CL.C
264400970211     C                   ENDIF
264500980709     C     ETCCLV        IFEQ      'T'
264600020213     C     ETCCLV        OREQ      'D'
264700100422     C                   Z-ADD     820           V                              *SEDE-ESPR-CL.T
264800980709     C                   ENDIF
264900970211     C                   ENDIF
265000970211     C*
265100970211     C* -----> RECUPERA LA % DELLA VOCE
265200970211     C                   Z-ADD     V             WVOC
265300970221     C                   Z-ADD     VPRG(V)       WPRG
265400041109     C                   MOVEL     VTIV(V)       WTIV
265500970211     C                   EXSR      CERTAR
265600970211     C     $ERR          IFEQ      'S'
265700970211     C                   MOVEL     MSG(2)        $MSG                           *TARIF. NON TROV
265800970211     C                   EXSR      SCRERR
265900040611     c                   leave
266000970211     C                   ENDIF
266100011025     C                   Z-ADD     EVDPER        WPERRI           21 8          *% RICARICO SEDE
266200970211     C*
266300970211     C* RECUPERA LA % DEL RICARICO COMMERCIALE
266400980710     C                   Z-ADD     270           V
266500980710     C                   Z-ADD     270           WVOC
266600970221     C                   Z-ADD     VPRG(V)       WPRG
266700041109     C                   MOVEL     VTIV(V)       WTIV
266800970211     C                   EXSR      CERTAR
266900970211     C     $ERR          IFEQ      'S'
267000970211     C                   MOVEL     MSG(17)       $MSG                           *TARIF. NON TROV
267100970211     C                   EXSR      SCRERR
267200040611     c                   leave
267300970211     C                   ENDIF
267400011025     C                   Z-ADD     EVDPER        WPERCO           21 8          *% RICARICO COMM.LE
267500040609     C* RECUPERA LA % DEL FINANZIAM.TARIFFE IMPORT DPD solo se franco
267600040609     c                   clear                   wperft
267700040614     C                   IF        vc(305)='S'
267800040609     C                   Z-ADD     305           V
267900040609     C                   Z-ADD     305           WVOC
268000040609     C                   Z-ADD     VPRG(V)       WPRG
268100041109     C                   MOVEL     VTIV(V)       WTIV
268200040609     C                   EXSR      CERTAR
268300040609     C     $ERR          IFEQ      'S'
268400040609     C                   MOVEL     MSG(29)       $MSG                           *TARIF. NON TROV
268500040609     C                   EXSR      SCRERR
268600040611     c                   leave
268700040609     C                   ENDIF
268800040609     C                   Z-ADD     EVDPER        WPERFT           21 8          *% RICARICO COMM.LE
268900040609     c                   end
269000970211     C*
269100970211     C* RECUPERA LA % DEL RICARICO PAGAMENTO
269200970211     C                   Z-ADD     300           V
269300970211     C                   Z-ADD     300           WVOC
269400970221     C                   Z-ADD     VPRG(V)       WPRG
269500041109     C                   MOVEL     VTIV(V)       WTIV
269600970211     C                   EXSR      CERTAR
269700970211     C     $ERR          IFEQ      'S'
269800970211     C                   MOVEL     MSG(23)       $MSG                           *TARIF. NON TROV
269900970211     C                   EXSR      SCRERR
270000040611     c                   leave
270100970211     C                   ENDIF
270200011025     C                   Z-ADD     EVDPER        WPERPA           21 8          *% RICARICO PAGAM.
270300970227     C*---
270400970227     C* F O R M U L A
270500970227     C*---
270600970227     C* PC = % COMMERCIALE
270700970227     C* PP = % PAGAMENTO
270800970227     C* PR = % RICARICO DELTA
270900040609     C* FT = % FINANZIAM.TARIFFE export DPD franco
271000970211     C*
271100040609     C* 100 ( PC + PP + PR + FT) + PR ( PC + PP + FT)
271200040609     C* ---------------------------------------------  = % RICARICHI
271300040609     C* 100 ( 100 - PC - PP -FT) - PR ( PC + PP + FT)
271400970211     C*
271500970211     C* CALCOLO DENOMINATORE
271600011025     C     WPERCO        ADD       WPERPA        W0153W           23 8
271700970211     C                   ADD       WPERRI        W0153W
271800040609     C                   ADD       WPERft        W0153W
271900011025     C     100           MULT      W0153W        W0153A           23 8
272000970211     C*
272100970211     C     WPERCO        ADD       WPERPA        W0153W
272200040609     C                   ADD       WPERft        W0153W
272300011025     C     WPERRI        MULT      W0153W        W0153B           23 8
272400970211     C*
272500011025     C     W0153A        ADD       W0153B        WDENOM           23 8
272600970211     C*
272700970211     C* CALCOLO DIVISORE
272800970211     C     100           SUB       WPERCO        W0153W
272900040609     C                   SUB       WPERPA        W0153W
273000040609     C                   SUB       WPERft        W0153W
273100970211     C     100           MULT      W0153W        W0153A
273200970211     C*
273300970211     C     WPERCO        ADD       WPERPA        W0153W
273400040609     C                   ADD       WPERft        W0153W
273500970211     C     WPERRI        MULT      W0153W        W0153B
273600970211     C*
273700011025     C     W0153A        SUB       W0153B        WDIVIS           23 8
273800970211     C*
273900970211     C* CALCOLO TOTALE
274000970211     C     WDENOM        DIV       WDIVIS        W0153W
274100011025     C     W0153W        MULT      100           WPERTO           23 8
274200970211     C*
274300040611     c                   enddo
274400970211     C                   ENDSR
274500970211     C*------------------------------------------------------------------------*
274600970225     C* CALTOT - CALCOLA RICARICO SEDE + RAGGR. DELTA
274700970211     C*------------------------------------------------------------------------*
274800970211     C     CALTOT        BEGSR
274900970211     C*
275000970211     C* CICLO PER TUTTE LE VOCI X TOT.VOCI + RAGGR.DELTA
275100970211     C                   Z-ADD     *ZEROS        IRDL                           *SCHIERA RAGGR.DELTA
275200970211     C                   DO        NVOCA         N
2753009702111    C                   Z-ADD     VOCA(N)       V
2754009702112    C     VIM(V)        IFGT      *ZEROS
275500970211     C*
275600970211     C* APPLICA LA % DA CONSIDERARE (NON PER LE MAGGIORAZIONI)
275700970211     C     VP(V)         IFLT      100
275800970211     C     VTVO(V)       ANDNE     'M'
275900970211     C     VIM(V)        DIV       100           WVVOCE
276000970211     C     WVVOCE        MULT      VP(V)         VIM(V)
276100970211     C                   ENDIF
276200970211     C*
276300970213     C* SE LA VOCE DEVE ESSERE INGLOBATA (NEL PORTO) FORZO RAGGR.DELTA = 001
276400970211     C     VI(V)         IFEQ      'S'
276500970211     C                   Z-ADD     001           VRDL(V)
276600970211     C                   ENDIF
276700970211     C*
276800970227     C* RAGGRUPPAMENTO DELTA
276900970211     C                   Z-ADD     VRDL(V)       R                 6 0          *INDICE RAGGR.DELTA
277000970211     C                   ADD       VIM(V)        IRDL(R)                        *IMP.RAGGR.DELTA
277100970211     C*
2772009702112    C                   ENDIF
2773009702111    C                   ENDDO
277400970430     C*
277500970430     C* CALCOLA IMPORTO RICARICHI E LI AGGIUNGE AI TOTALI
277600970623IF  1C     D00TLA        IFEQ      'P'
277700970430     C*
277800011025     C                   Z-ADD     *ZEROS        WCODEL           21 8          *VALORE COMP.DELTA
277900970910     C* SE DA PORTO, AGGIUNGE LA % RICARICHI TOT. A TUTTI I RAGGRUPPAM. DELTA
278000970910     C                   DO        50            R
278100970910     C     IRDL(R)       IFGT      *ZEROS
278200970910     C     IRDL(R)       DIV       100           WVVOCE
278300970430     C     WVVOCE        MULT      WPERTO        WVVOCE                         *% RICARICHI TOTALI
278400970910     C                   ADD       WVVOCE        IRDL(R)                        *AGG.VALORE AL RAGGR
278500970910     C                   ADD       WVVOCE        WCODEL                         *AGG.ALLA COMP.DELTA
278600970910     C                   ENDIF
278700970910     C                   ENDDO
278800970910     C*
278900970910     C* IMPORTO % RICARICHI TOTALI (SEDE+PAG+COM)
279000970910     C                   Z-ADD     900           V                              *VOCE PER COMP.DELTA
279100970910     C                   Z-ADD     WCODEL        VIM(V)                         *IMPORTO VOCE
279200970430     C*
279300970430X   1C                   ELSE
279400970430     C*
279500011025     C                   Z-ADD     *ZEROS        WCOMPA           21 8          *COMP.PAGAM.
279600011025     C                   Z-ADD     *ZEROS        WCOMCO           21 8          *COMP.COMM.LE
279700011025     C                   Z-ADD     *ZEROS        WCOMRI           21 8          *COMP.SEDE
279800040610     C                   Z-ADD     *ZEROS        WCOMFT           21 8          *COMP.SEDE
279900970430     C* SE DA DELTA, RECUPERA L'IMPORTO RICAVI SCAGLIONE/CODICE TASSAZIONE DAL
280000970430     C* WORKFILE E APPLICA LE 3 PERCENTUALI PAGAMENTO/COMMERCIALE/SEDE
280100980909     C******************** MOVELUSER      ETWUSR           *UTENTE
280200980909     C                   MOVEL     JOB           ETWUSR                         *UTENTE
280300091123     C                   MOVEL     'S'           ETWDSF                         *SEDE/FIL
280400970430     C                   MOVEL     D00CTO        ETWCTO                         *TARIF/OFF
280500970430     C                   Z-ADD     D00KSC        ETWKSC                         *CLIENTE
280600970430     C                   Z-ADD     D00CTR        ETWCTR                         *TARIFFA
280700970430     C                   Z-ADD     D00PRG        ETWPRG                         *PROGRESSIVO
280800970430     C                   Z-ADD     SSGL(S)       ETWSGL                         *SCAGLIONE SUP.
280900970430     C                   MOVEL     TCTS(T)       ETWCTS                         *COD.TASSAZ.
281000010926     C     KEYETX        SETLL     TEETW01L
281100010926     C     KEYETX        READE     TEETW01L                               99
281200970430DO  2C     *IN99         DOWEQ     *OFF
281300970910     C                   Z-ADD     ETWRDL        R
281400970910     C* CALC. LA %RICARICO COMMERCIALE A L'AGG. AL RAGGR. DELTA
281500970910     C     ETWRIC        DIV       100           WVVOCE
281600970910     C     WVVOCE        MULT      WPERCO        WVVOCE                         *%COMM.LE
281700970910     C                   ADD       WVVOCE        IRDL(R)
281800970910     C                   ADD       WVVOCE        WCOMCO                         *AGG.ALLA COMP.COMML
281900970910     C* APPLICA LA %RICARICO PAGAMENTO AL RICAVO REALE
282000970910     C     ETWRIC        DIV       100           WVVOCE
282100970910     C     WVVOCE        MULT      WPERPA        WVVOCE                         *%PAGAMENTO
282200970910     C                   ADD       WVVOCE        IRDL(R)
282300970910     C                   ADD       WVVOCE        WCOMPA                         *AGG.ALLA COMP.PAGAM
282400040610     C* APPLICA LA %FINANZIAM.TARIFFE IMPORT DPD al ricavo reale
282500040610     C     ETWRIC        DIV       100           WVVOCE
282600040610     C     WVVOCE        MULT      WPERFT        WVVOCE                         *%PAGAMENTO
282700040610     C                   ADD       WVVOCE        IRDL(R)
282800040610     C                   ADD       WVVOCE        WCOMFT                         *AGG.ALLA COMP.PAGAM
282900010926     C     KEYETX        READE     TEETW01L                               99
283000970430E   2C                   ENDDO
283100970910     C*
283200970910     C* AGGIUNGE LA % RICARICHI SEDE A TUTTI I RAGGRUPPAM. DELTA
283300970910     C                   DO        50            R
283400970910     C     IRDL(R)       IFGT      *ZEROS
283500970910     C     IRDL(R)       DIV       100           WVVOCE
283600970910     C     WVVOCE        MULT      WPERRI        WVVOCE                         *% RICARICHI SEDE
283700970910     C                   ADD       WVVOCE        IRDL(R)                        *AGG.VALORE AL RAGGR
283800970910     C                   ADD       WVVOCE        WCOMRI                         *AGG.ALLA COMP.SEDE
283900970910     C                   ENDIF
284000970910     C                   ENDDO
284100970910     C*
284200970910     C* IMPORTO % RICARICHI NEL DELTA
284300980710     C                   Z-ADD     270           V                              *VOCE PER COMP.COMML
284400980710     C                   Z-ADD     WCOMCO        VIM(V)                         *IMPORTO VOCE
284500040610     C                   Z-ADD     305           V                              *VOCE PER COMP.COMML
284600040610     C                   Z-ADD     WCOMft        VIM(V)                         *IMPORTO VOCE
284700970910     C                   Z-ADD     300           V                              *VOCE PER COMP.PAGAM
284800970910     C                   Z-ADD     WCOMPA        VIM(V)                         *IMPORTO VOCE
284900970910     C                   Z-ADD     900           V                              *VOCE PER COMP.SEDE
285000970910     C                   Z-ADD     WCOMRI        VIM(V)                         *IMPORTO VOCE
285100970430     C*
285200970430E   1C                   ENDIF
285300970430     C*
285400970430     C* IMPORTO % RICARICHI NEL DELTA
285500970910     C**                   Z-ADD900       V                *VOCE PER COMP.DELTA
285600970910     C**                   Z-ADDWCODEL    VIM,V            *IMPORTO VOCE
285700970910     C****                 Z-ADDVRDL,V    R                *INDICE RAGGR.DELTA
285800970910     C****                 ADD  WVVOCE    IRDL,R           *IMP.RAGGR.DELTA
285900970211     C*
286000970401     C* SOLO PER LANCIO X CALC.PORTO: CALC. INOLTRO,DELTA RICHIESTO,TAR.FINITA
2861009705070    C*££        D00TLA    IFEQ 'P'                        *LANCIO X PORTO
286200970317     C*
286300970317     C* SE INOLTRO DA INGLOBARE CALCOLA IMP. E LO TOGLIE DAL RAGGR."PORTO"
286400970317     C***********ETCSIN    IFEQ 'N'                        *SCORPORO INOLTRO ?
2865009705071    C*££        ETCSIN    IFEQ 'S'                        *INGLOBA INOLTRO ?
286600970507     C*££        SINL,S    ANDGT*ZEROS                     *IMPORTO INOLTRO
286700970507     C*££        SINL,S    DIV  100       WVVOCE
286800970507     C*££        WVVOCE    MULT ETCPPV    WVVOCE           *IMP.INOLTRO
286900970319     C***********WVVOCE    IFLE IRDL,1
287000970319     C*********************SUB  WVVOCE    IRDL,1           *IMP.RAGGR.DELTA PORT
287100970507     C*££                  ADD  WVVOCE    IRDL,1           *IMP.RAGGR.DELTA PORT
287200970319     C*********************ELSE
287300970319     C*********************Z-ADD*ZEROS    IRDL,1
287400970319     C*********************ENDIF
2875009705071    C*££                  ENDIF
287600970211     C*
287700970211     C* CALCOLA % DELTA RICHIESTO SOLO PER LANCIO X CALC.PORTO
2878009705071    C*$$        ETCPDE    IFGT *ZEROS                     * % DELTA RICH.GENER.
287900970507     C*$$        SPDE,S    ORGT *ZEROS                     * % DELTA RICH.SCAGL.
288000970211     C*
288100970402     C* SOMMA DELTA GENERALE E DELTA DELLO SCAGLIONE
288200970507     C*$$        ETCSDE    IFEQ '-'
288300970507     C*$$        ETCPDE    MULT -1        WGPDE  155       *DELTA GEN.CON SEGNO
288400970507     C*$$                  ELSE
288500970507     C*$$                  Z-ADDETCPDE    WGPDE
288600000705     C*$$                  ENDIF
288700970507     C*$$        SSDE,S    IFEQ '-'
288800970507     C*$$        SPDE,S    MULT -1        WSPDE  155       *DELTA SCA.CON SEGNO
288900970507     C*$$                  ELSE
289000970507     C*$$                  Z-ADDSPDE,S    WSPDE
289100970507     C*$$                  ENDIF
289200970507     C*$$        WGPDE     ADD  WSPDE     WTPDE  155       *DELTA TOT.CON SEGNO
289300970507     C*$$        WTPDE     IFLT 0
289400970507     C*$$        WTPDE     MULT -1        WTPDE
289500970507     C*$$                  MOVEL'-'       WTSDE   1
289600970507     C*$$                  ELSE
289700970507     C*$$                  MOVEL'+'       WTSDE
289800970507     C*$$                  ENDIF
289900970402     C*
290000970507     C*$$                  XFOOTIRDL      WVVOCE
290100970507     C*$$        WVVOCE    DIV  100       WVVOCE
290200970507     C*$$        WVVOCE    MULT WTPDE     WVVOCE           * % DELTA RICH.
290300970211     C*
290400970227     C* APPLICA DELTA RICHIESTO AL RAGGRUPPAMENTO DELTA
2905009705072    C*$$                  DO   50        R
2906009705073    C*$$        IRDL,R    IFGT *ZEROS
2907009705074    C*$$        WTSDE     IFEQ '+'                        *SEGNO DELTA RICH.
290800970507     C*$$        100       SUB  WTPDE     WPDE   155
290900970507     C*$$        100       MULT IRDL,R    WVVOCE
291000970507     C*$$        WVVOCE    DIV  WPDE      IRDL,R
2911009705074    C*$$                  ELSE                            *SEGNO NEGATIVO
291200970507     C*$$        100       ADD  WTPDE     WPDE
291300970507     C*$$        100       MULT IRDL,R    WVVOCE
291400970507     C*$$        WVVOCE    DIV  WPDE      IRDL,R
2915009705074    C*$$                  ENDIF
2916009705073    C*$$                  ENDIF
2917009705072    C*$$                  ENDDO
291800970211     C*
2919009705071    C*$$                  ENDIF
292000970401     C*
292100970401     C* CONTROLLA TARIFFA FINITA SOLO SE SCAGLIONE MAGGIORE QTA TAR.FINITA
292200970401     C* APPLICA TARIFFA FINITA SOLO AL PORTO (INDICE=1)
2923009704231    C*******    SSGL,S    IFGT D00ATA
2924009705071    C*££        SPSC,S    IFGT D00ATA
292500970401     C*
292600970507     C*££                  MOVELD00CTR    UNOCTR  10
292700970401     C*
2928009705072    C*££        UNOCTR    IFEQ 1                          *TARIFFA A COLLI
292900970507     C*££        IRDL,1    DIV  SCSC,S    WVVOCE
293000970507     C*££                  ELSE                            *TARIFFA KG/QLI
293100970507     C*££        IRDL,1    DIV  SPSC,S    WVVOCE
293200970507     C*££                  ENDIF
293300970401     C*
2934009705072    C*££        UNOCTR    IFEQ 0                          *TARIFFA A QLI
293500970507     C*££        WVVOCE    MULT 100       IRDL,1
2936009705072    C*££                  ELSE                            *TARIFFA KG/COLLI
293700970507     C*££                  Z-ADDWVVOCE    IRDL,1
2938009705072    C*££                  ENDIF
293900970401     C*
2940009705071    C*££                  ENDIF
2941009705070    C*££                  ENDIF
294200970211     C*
294300970211     C                   ENDSR
294400970211     C*------------------------------------------------------------------------*
294500970211     C* SCRETW - SCRIVE SU FILE DI LAVORO LE COMPETENZE
294600970211     C*------------------------------------------------------------------------*
294700970211     C     SCRETW        BEGSR
294800970211     C*
294900970227     C* SCRIVE UN RECORD PER OGNI RAGGRUPPAMENTO DELTA
295000970211     C                   DO        50            R
295100970211     C     IRDL(R)       IFGT      *ZEROS
295200970211     C     R             OREQ      1
295300980909     C******************** MOVELUSER      ETWUSR           *UTENTE
295400980909     C                   MOVEL     JOB           ETWUSR                         *UTENTE
295500091123     C                   MOVEL     'S'           ETWDSF                         *SEDE/FIL
295600970211     C                   MOVEL     D00CTO        ETWCTO                         *TARIF/OFF
295700970211     C                   Z-ADD     D00KSC        ETWKSC                         *CLIENTE
295800970211     C                   Z-ADD     D00CTR        ETWCTR                         *TARIFFA
295900970211     C                   Z-ADD     D00PRG        ETWPRG                         *PROGRESSIVO
296000970211     C                   Z-ADD     SSGL(S)       ETWSGL                         *SCAGLIONE SUP.
296100970211     C                   MOVEL     TCTS(T)       ETWCTS                         *COD.TASSAZ.
296200970211     C                   Z-ADD     R             ETWRDL                         *RAGGR.DELTA
296300970211     C                   MOVEL     TRCT(T)       ETWRCT                         *RAGGR.C.TASS.
296400970211     C*
296500011025     C     TPPE(T)       DIV       100           WPERC            21 8
296600970211     C     WPERC         MULT      SPPE(S)       ETWPPE                         *% PESO
296700970211     C*
296800970211     C                   MOVEL     TRAP(T)       ETWRAP                         *REGIONE APP.
296900970211     C                   MOVEL     TNAR(T)       ETWNAR                         *NAZIONE APP.
297000010926     C     KEYETW        CHAIN     TEETW01L                           99
297100970211     C     *IN99         IFEQ      '0'                                          *ESISTE GIA'
297200970211     C                   ADD       IRDL(R)       ETWCOM                         *COMPET.
297300970317     C     D00TLA        IFEQ      'P'                                          *LANCIO X PORTO
297400970307     C                   Z-ADD     *ZEROS        ETWRIC
297500970317     C                   ENDIF
297600010926     C                   UPDATE    TEETW000
297700970211     C                   ELSE                                                   *NON ESISTE
297800970211     C                   Z-ADD     IRDL(R)       ETWCOM                         *COMPET.
297900970307     C                   Z-ADD     *ZEROS        ETWRIC
298000010926     C                   WRITE     TEETW000
298100970211     C                   ENDIF
298200970211     C                   ENDIF
298300970211     C                   ENDDO
298400970211     C*
298500970211     C                   ENDSR
298600970211     C*------------------------------------------------------------------------*
298700970211     C* SCREWC - SCRIVE SU FILE DI LAVORO IL DETTAGLIO DELLE VOCI (SE DEBUG)
298800970211     C*------------------------------------------------------------------------*
298900970211     C     SCREWC        BEGSR
299000970211     C*
299100970227     C* SCRIVE UN RECORD PER OGNI VOCE
299200970211     C                   DO        NVOCA         N
299300970211     C                   Z-ADD     VOCA(N)       V
299400970211     C     VIM(V)        IFGT      *ZEROS
299500980909     C******************** MOVELUSER      EWCUSR           *UTENTE
299600980909     C                   MOVEL     JOB           EWCUSR                         *UTENTE
299700091123     C                   MOVEL     'S'           EWCDSF                         *SEDE/FIL
299800980708     C                   MOVEL     D00CTO        EWCCTO                         *TARIF/OFF
299900970211     C                   Z-ADD     D00KSC        EWCKSC                         *CLIENTE
300000970211     C                   Z-ADD     D00CTR        EWCCTR                         *TARIFFA
300100970211     C                   Z-ADD     D00PRG        EWCPRG                         *PROGRESSIVO
300200970211     C                   MOVEL     TCTS(T)       EWCCTS                         *COD.TASSAZ.
300300970211     C                   Z-ADD     SSGL(S)       EWCSGL                         *SCAGLIONE SUP.
300400970211     C                   Z-ADD     V             EWCVOC                         *VOCE
300500970211     C                   Z-ADD     VIM(V)        EWCIMP                         *IMPORTO
300600970211     C                   Z-ADD     VRDL(V)       EWCRDL                         *RAGGR.DELTA
300700010926     C                   WRITE     TEETWC00
300800970211     C                   ENDIF
300900970211     C                   ENDDO
301000970211     C*
301100970211     C                   ENDSR
301200961128     C*------------------------------------------------------------------------*
301300961128     C* CHKCAR - CONTROLLA SE TARIFFA DI CARTELLO CARICATA IN MEMORIA
301400961128     C*------------------------------------------------------------------------*
301500961128     C     CHKCAR        BEGSR
301600961128     C*
301700961128     C* CONTROLLA ESISTENZA TARIFFA DI CARTELLO IN LINEA FRA QUELLE CARICATE
301800961128     C                   Z-ADD     1             I
301900961128DO  1C     I             DOWLE     WULTCA                                       FINO ULTIMA TAR.CART
302000961128     C     WSETLL        ANDEQ     'N'                                          CARTELLO NON TROVATA
302100961128     C*
302200961128     C* CONTROLLI DI VALIDITA'
302300961128IF  2C     WVOC          IFEQ      CVOC(I)                                      *COD. VOCE
302400961128     C*
302500961128IF  3C     WPRG          IFEQ      CPRG(I)                                      *ALTRI CAMPI CHIAVE
302600961128     C     WFT1          ANDEQ     CFT1(I)
302700961128     C     WFT2          ANDEQ     CFT2(I)
302800961128     C     WKSC          ANDEQ     CCLI(I)
302900961128     C     WCTR          ANDEQ     CCTR(I)
303000961128     C     WCTS          ANDEQ     CCTS(I)
303100140115     C     Wnaz          ANDEQ     Cnaz(I)
303200140115     C     wcad          ANDEQ     Ccad(I)
303300961128     C*
303400961128IF  4C     D00DTC        IFGE      CDDT(I)                                      *DECORRENZA/SCADENZA
303500961128     C     D00DTC        ANDLE     CDST(I)
303600961128     C*
303700000306IF  5C*---       WQTA      IFGE CSGI,I                     *SCAGLIONE
303800000306     C*---       WQTA      ANDLECSGF,I
303900000306     C*
304000000306IF  5C     WTIV          IFEQ      *BLANKS                                      *NO RICERCA SCAGLION
304100000306     C     WTIV          ORNE      *BLANKS                                      *SI RICERCA SCAGLION
304200000306     C     WQTA          ANDGE     CSGI(I)                                      *SCAGLIONE
304300000306     C     WQTA          ANDLE     CSGF(I)
304400961128     C*
304500961128     C* CARTELLO TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
304600961128     C                   MOVEL     'S'           WSETLL
304700961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
304800961128     C*
304900961128     C                   Z-ADD     CVOC(I)       EVDVOC
305000961128     C                   Z-ADD     CPRG(I)       EVDPRG
305100961128     C                   MOVEL     'C'           EVDTTV
305200961128     C                   Z-ADD     CFT1(I)       EVDFT1
305300961128     C                   Z-ADD     CFT2(I)       EVDFT2
305400961128     C                   Z-ADD     CCLI(I)       EVDCLI
305500961128     C                   Z-ADD     CCTR(I)       EVDCTR
305600961128     C                   MOVEL     CCTS(I)       EVDCTS
305700140115     C                   movel     Ccad(I)       EVDcad
305800140115     C                   movel     Cnaz(I)       EVDnaz
305900961128     C                   Z-ADD     CDDT(I)       EVDDDT
306000961128     C                   Z-ADD     CDST(I)       EVDDST
306100961128     C                   Z-ADD     CSGI(I)       EVDSGI
306200961128     C                   Z-ADD     CSGF(I)       EVDSGF
306300961128     C                   Z-ADD     CITR(I)       EVDITR
306400961128     C                   Z-ADD     CIMI(I)       EVDIMI
306500961128     C                   Z-ADD     CIMA(I)       EVDIMA
306600961128     C                   MOVEL     CSEG(I)       EVDSEG
306700961128     C                   Z-ADD     CPER(I)       EVDPER
306800961128     C                   Z-ADD     CRPV(I)       EVDRPV
306900961128     C*
307000961128E   5C                   ENDIF
307100961128E   4C                   ENDIF
307200961128E   3C                   ENDIF
307300961128E   2C                   ENDIF
307400961128     C*
307500961128     C                   ADD       1             I
307600961128E   1C                   ENDDO
307700961128     C*
307800961128     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
307900961128     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
308000961128IF  1C     WSETLL        IFEQ      'N'                                          *NON TROVATA IN MEM.
308100021112     C     WULTCA        ANDLT     300                                          *NON FINE SCHIERA
308200961128     C                   MOVEL     'S'           WSETLL                         *NON LEGGERE PIU'
308300961128E   1C                   ENDIF
308400961128     C*
308500961128     C                   ENDSR
308600961128     C*------------------------------------------------------------------------*
308700011025     C* CARFIL - CARICA TUTTE LE TARIFFE DELLA FILIALE
308800961128     C*------------------------------------------------------------------------*
308900961128     C     CARFIL        BEGSR
309000961128     C*
309100961128     C* POSIZIONAMENTO E PRIMA LETTURA
309200961128     C     D00LNP        SETLL     ECEVD03L
309300961128     C                   READ      ECEVD03L                               99
309400961128     C*
309500961128     C* FINO A FINE FILE
309600961128     C                   Z-ADD     *ZEROS        I                 6 0
309700961128DO  1C     *IN99         DOWEQ     *OFF
309800961128     C     EVDFT1        ANDEQ     D00LNP                                       *FILIALE DEL CLIENTE
309900961128IF  3C     EVDATB        IFEQ      *BLANKS                                      *NON ANNULLATA
310000050103     C     D00DTC        ANDGE     EVDDDT                                       *MAGG. DECORRENZA
310100050103     C     D00DTC        ANDLE     EVDDST                                       *MIN. SCADENZA
310200011025     C*
310300011025     C* LEGGO LA RELATIVA TESTATA VOCE X EFFETTUARE ALCUNE CONSIDERAZIONI
310400011025     C     KEYEVT2       CHAIN     ECEVT01L
310500011025     C                   IF        %found(ECEVT01L)
310600011025     C     WDIVCE        IFNE      WDIVAC
310700011025     C                   IF        EVTTIV = '21' OR
310800011025     C                             EVTTIV = '22' OR
310900011025     C                             EVTTIV = '23' OR
311000011025     C                             EVTTIV = '25'
311100011025     C                   EXSR      CNVEVDSG
311200011025     C                   ENDIF
311300011025     C                   EXSR      CNVEVD
311400011025     C                   ENDIF
311500011025     C*
311600961128     C                   ADD       1             I
311700961128     C                   Z-ADD     EVDVOC        FVOC(I)
311800961128     C                   Z-ADD     EVDPRG        FPRG(I)
311900961128     C                   Z-ADD     EVDFT1        FFT1(I)
312000961128     C                   Z-ADD     EVDFT2        FFT2(I)
312100961128     C                   Z-ADD     EVDCLI        FCLI(I)
312200961128     C                   Z-ADD     EVDCTR        FCTR(I)
312300961128     C                   MOVEL     EVDCTS        FCTS(I)
312400140115     C                   movel     EVDcad        Fcad(I)
312500140115     C                   movel     EVDnaz        Fnaz(I)
312600961128     C                   Z-ADD     EVDDDT        FDDT(I)
312700961128     C                   Z-ADD     EVDDST        FDST(I)
312800961128     C                   Z-ADD     EVDSGI        FSGI(I)
312900961128     C                   Z-ADD     EVDSGF        FSGF(I)
313000961128     C                   Z-ADD     EVDITR        FITR(I)
313100961128     C                   Z-ADD     EVDIMI        FIMI(I)
313200961128     C                   Z-ADD     EVDIMA        FIMA(I)
313300961128     C                   MOVEL     EVDSEG        FSEG(I)
313400961128     C                   Z-ADD     EVDPER        FPER(I)
313500961128     C                   Z-ADD     EVDRPV        FRPV(I)
313600961128E   2C                   ENDIF
313700011025     C                   ENDIF
313800961128     C*
313900961128     C                   READ      ECEVD03L                               99
314000961128E   1C                   ENDDO
314100961128     C*
314200961128     C                   Z-ADD     I             WULTTF            6 0          *ULT.TARIF.FILIALE
314300961128     C*
314400961128     C*
314500961128     C                   ENDSR
314600961128     C*------------------------------------------------------------------------*
314700961128     C* CHKFIL - CONTROLLA SE TARIFFA FILIALE GIA' IN MEMORIA
314800961128     C*------------------------------------------------------------------------*
314900961128     C     CHKFIL        BEGSR
315000961128     C*
315100961128     C* CONTROLLA ESISTENZA TARIFFA FILIALE FRA QUELLE CARICATE
315200961128     C                   Z-ADD     1             I
315300961128DO  1C     I             DOWLE     WULTTF                                       *FINO ULTIMA TAR.FIL
315400961128     C     WSETLL        ANDEQ     'N'                                          *TARIFFA NON TROVATA
315500961128     C*
315600961128     C* CONTROLLI DI VALIDITA'
315700961128IF  2C     WVOC          IFEQ      FVOC(I)                                      *COD.VOCE
315800961128     C*
315900961128IF  3C     WPRG          IFEQ      FPRG(I)                                      *ALTRI CAMPI CHIAVE
316000961128     C     WFT1          ANDEQ     FFT1(I)
316100961128     C     WFT2          ANDEQ     FFT2(I)
316200961128     C     WKSC          ANDEQ     FCLI(I)
316300961128     C     WCTR          ANDEQ     FCTR(I)
316400961128     C     WCTS          ANDEQ     FCTS(I)
316500140115     C     Wnaz          ANDEQ     Fnaz(I)
316600140115     C     wcad          ANDEQ     Fcad(I)
316700961128     C*
316800961128IF  4C     D00DTC        IFGE      FDDT(I)                                      *DECORRENZA/SCADENZA
316900961128     C     D00DTC        ANDLE     FDST(I)
317000961128     C*
317100000306IF  5C*---       WQTA      IFGE FSGI,I                     *SCAGLIONE
317200000306     C*---       WQTA      ANDLEFSGF,I
317300000306     C*
317400000306IF  5C     WTIV          IFEQ      *BLANKS                                      *NO RICERCA SCAGLION
317500000306     C     WTIV          ORNE      *BLANKS                                      *SI RICERCA SCAGLION
317600000306     C     WQTA          ANDGE     FSGI(I)                                      *SCAGLIONE
317700000306     C     WQTA          ANDLE     FSGF(I)
317800961128     C*
317900961128     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
318000961128     C                   MOVEL     'S'           WSETLL                         *NON LEGGERE PIU'
318100961128     C                   MOVEL     'S'           WFLTT                          *TARIFFA TROVATA
318200961128     C*
318300961128     C                   Z-ADD     FVOC(I)       EVDVOC
318400961128     C                   Z-ADD     FPRG(I)       EVDPRG
318500961128     C                   MOVEL     'F'           EVDTTV
318600961128     C                   Z-ADD     FFT1(I)       EVDFT1
318700961128     C                   Z-ADD     FFT2(I)       EVDFT2
318800961128     C                   Z-ADD     FCLI(I)       EVDCLI
318900961128     C                   Z-ADD     FCTR(I)       EVDCTR
319000961128     C                   MOVEL     FCTS(I)       EVDCTS
319100140115     C                   movel     Fcad(I)       EVDcad
319200140115     C                   movel     Fnaz(I)       EVDnaz
319300961128     C                   Z-ADD     FDDT(I)       EVDDDT
319400961128     C                   Z-ADD     FDST(I)       EVDDST
319500961128     C                   Z-ADD     FSGI(I)       EVDSGI
319600961128     C                   Z-ADD     FSGF(I)       EVDSGF
319700961128     C                   Z-ADD     FITR(I)       EVDITR
319800961128     C                   Z-ADD     FIMI(I)       EVDIMI
319900961128     C                   Z-ADD     FIMA(I)       EVDIMA
320000961128     C                   MOVEL     FSEG(I)       EVDSEG
320100961128     C                   Z-ADD     FPER(I)       EVDPER
320200961128     C                   Z-ADD     FRPV(I)       EVDRPV
320300961128     C*
320400961128E   5C                   ENDIF
320500961128E   4C                   ENDIF
320600961128E   3C                   ENDIF
320700961128E   2C                   ENDIF
320800961128     C*
320900961128     C                   ADD       1             I
321000961128E   1C                   ENDDO
321100961128     C*
321200961128     C                   ENDSR
321300961128     C*------------------------------------------------------------------------*
321400011025     C* CARCAR - CARICA LE TARIFFE DI CARTELLO
321500961128     C*------------------------------------------------------------------------*
321600961128     C     CARCAR        BEGSR
321700961128     C*
321800961128     C* POSIZIONAMENTO E PRIMA LETTURA
321900961128     C                   MOVEL     'C'           EVDTTV                         *TARIFFE DI CARTELLO
322000961128     C     EVDTTV        SETLL     ECEVD04L
322100961128     C                   READ      ECEVD04L                               99
322200961128     C*
322300961128     C* FINO A FINE FILE E TARIFFE DI CARTELLO
322400961128     C                   Z-ADD     *ZEROS        I
322500961128DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
322600961128     C     EVDTTV        ANDEQ     'C'                                          *TARIFFE DI CARTELLO
322700021112     C     I             ANDLT     300                                          *FINE SCHIERA
322800961128IF  2C     EVDATB        IFEQ      *BLANKS                                      *NON ANNULLATA
322900050103     C     D00DTC        ANDGE     EVDDDT                                       *MAGG. DECORRENZA
323000050103     C     D00DTC        ANDLE     EVDDST                                       *MIN. SCADENZA
323100011025     C*
323200011025     C* LEGGO LA RELATIVA TESTATA VOCE X EFFETTUARE ALCUNE CONSIDERAZIONI
323300011025     C     KEYEVT2       CHAIN     ECEVT01L
323400011025     C                   IF        %found(ECEVT01L)
323500011025     C     WDIVCE        IFNE      WDIVAC
323600011025     C                   IF        EVTTIV = '21' OR
323700011025     C                             EVTTIV = '22' OR
323800011025     C                             EVTTIV = '23' OR
323900011025     C                             EVTTIV = '25'
324000011025     C                   EXSR      CNVEVDSG
324100011025     C                   ENDIF
324200011025     C                   EXSR      CNVEVD
324300011025     C                   ENDIF
324400011025     C*
324500961128     C                   ADD       1             I
324600961128     C                   Z-ADD     EVDVOC        CVOC(I)
324700961128     C                   Z-ADD     EVDPRG        CPRG(I)
324800961128     C                   Z-ADD     EVDFT1        CFT1(I)
324900961128     C                   Z-ADD     EVDFT2        CFT2(I)
325000961128     C                   Z-ADD     EVDCLI        CCLI(I)
325100961128     C                   Z-ADD     EVDCTR        CCTR(I)
325200961128     C                   MOVEL     EVDCTS        CCTS(I)
325300140115     C                   movel     EVDcad        Ccad(I)
325400140115     C                   movel     EVDnaz        Cnaz(I)
325500961128     C                   Z-ADD     EVDDDT        CDDT(I)
325600961128     C                   Z-ADD     EVDDST        CDST(I)
325700961128     C                   Z-ADD     EVDSGI        CSGI(I)
325800961128     C                   Z-ADD     EVDSGF        CSGF(I)
325900961128     C                   Z-ADD     EVDITR        CITR(I)
326000961128     C                   Z-ADD     EVDIMI        CIMI(I)
326100961128     C                   Z-ADD     EVDIMA        CIMA(I)
326200961128     C                   MOVEL     EVDSEG        CSEG(I)
326300961128     C                   Z-ADD     EVDPER        CPER(I)
326400961128     C                   Z-ADD     EVDRPV        CRPV(I)
326500961128E   2C                   ENDIF
326600011025     C                   ENDIF
326700961128     C*
326800961128     C                   READ      ECEVD04L                               99
326900961128E   1C                   ENDDO
327000961128     C*
327100961128     C                   Z-ADD     I             WULTCA            6 0          *ULT.TARIFFA CART.
327200961128     C*
327300961128     C                   ENDSR
327400961128     C*------------------------------------------------------------------------*
327500961128     C* CARMAG - CARICA LE MAGGIORAZIONI
327600961128     C*------------------------------------------------------------------------*
327700961128     C     CARMAG        BEGSR
327800961128     C*
327900961128     C* POSIZIONAMENTO E PRIMA LETTURA
328000961128     C     *LOVAL        SETLL     ECEVM01L
328100961128     C                   READ      ECEVM01L                               99
328200961128     C*
328300961128     C* FINO A FINE FILE E MAGGIORAZIONI
328400961128     C                   Z-ADD     *ZEROS        I
328500961128DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
328600050104     C     I             ANDLT     600                                          *FINE SCHIERA
328700961128IF  2C     EVMATB        IFEQ      *BLANKS                                      *NON ANNULLATA
328800050103     C     KEYEVT3       CHAIN     ECEVT01L
328900050103     C                   IF        %found(ECEVT01L) and EVTATB = *blanks
3290000501033    C                             and D00DTC >= EVTDDV and                     *NON E' ANNULLATO
329100050103     C                             D00DTC <= EVTDSV                             *MAGG. DECORRENZA
329200961128     C                   ADD       1             I
329300961128     C                   Z-ADD     EVMVOM        MVOM(I)
329400961128     C                   Z-ADD     EVMPRM        MPRM(I)
329500961128     C                   Z-ADD     EVMVOC        MVOC(I)
329600961128     C                   Z-ADD     EVMPRG        MPRG(I)
329700961128E   2C                   ENDIF
329800050103E   3C                   ENDIF
329900961128     C*
330000961128     C                   READ      ECEVM01L                               99
330100961128E   1C                   ENDDO
330200961128     C*
330300961128     C                   Z-ADD     I             WULTMA            6 0          *ULT.MAGGIORAZIONE
330400961128     C*
330500961128     C                   ENDSR
330600961128     C*------------------------------------------------------------------------*
330700961128     C* CARTAB - CARICA LE TABELLE
330800961128     C*------------------------------------------------------------------------*
330900961128     C     CARTAB        BEGSR
331000961128     C*
331100961128     C* CARICA TABELLA -2R- RIFERIMENTO VOCE
331200961128     C     $ERR          IFEQ      'N'
331300961128     C                   Z-ADD     0             I
331400961128     C                   Z-ADD     CODUT         KTBUTE
331500961128     C                   MOVEL     '2R'          KTBCOD
331600961128     C                   MOVEL     *BLANKS       KTBKEY
331700961128     C     KEYTAB        SETLL     TABEL00F                           99
331800961128     C  N99              READ      TABEL00F                               99
331900961128     C     *IN99         IFEQ      '1'
332000961128     C                   MOVEL     MSG(3)        $MSG
332100961128     C                   EXSR      SCRERR
332200961128     C                   END
332300961128     C     *IN99         DOWEQ     '0'
332400961128     C     TBLCOD        ANDEQ     '2R'
332500961128     C     TBLFLG        IFEQ      *BLANKS
332600961128     C                   ADD       1             I
332700961128     C                   MOVEL     TBLKEY        C2R(I)
332800961128     C                   MOVEL     TBLUNI        DS2R
332900961128     C                   MOVEL     §2RTVA        T2R(I)                         *TIPO VALORIZZ. Q,V,
333000961128     C                   END
333100961128     C                   READ      TABEL00F                               99
333200961128     C                   END
333300961128     C                   END
333400010801     C*
333500010801     C* CARICA TABELLA -1B-  CODICI TRATTAMENTO MERCE
333600010801IF  1C     $ERR          IFEQ      'N'
333700010801     C                   Z-ADD     0             I
333800010801     C                   Z-ADD     CODUT         KTBUTE
333900010801     C                   MOVEL     '1B'          KTBCOD
334000010801     C     KEYTAB        SETLL     TABEL00F                           99
334100010801     C  N99              READ      TABEL00F                               99
334200010801IF  2C     *IN99         IFEQ      '1'
334300010801     C                   MOVEL     MSG(4)        $MSG
334400010801     C                   EXSR      SCRERR
334500010801E   2C                   ENDIF
334600010801DO  2C     *IN99         DOWEQ     '0'
334700010801     C     TBLCOD        ANDEQ     '1B'
334800010801IF  3C     TBLFLG        IFEQ      *BLANKS
334900010801     C                   ADD       1             I
335000010801     C                   MOVEL     TBLKEY        CTM(I)                         *CODICE TRATTAMENTO
335100010801     C                   MOVEL     TBLUNI        DS1B
335200010801     C                   MOVEL     §1BF12        CF12(I)                        *FLAG "DISCK C"
335300010801E   3C                   ENDIF
335400010801     C                   READ      TABEL00F                               99
335500010801E   2C                   ENDDO
335600010801E   1C                   ENDIF
335700140930     C*
335800140930     C* CARICA TABELLA -15-  nazioni per reperire cap fittizio
335900140930IF  1C     $ERR          IFEQ      'N'
336000140930     C                   Z-ADD     0             I
336100140930     C                   Z-ADD     CODUT         KTBUTE
336200140930     C                   MOVEL     '15'          KTBCOD
336300140930     C                   MOVEL     *BLANKS       KTBKEY
336400140930     C     KEYTAB        SETLL     TABEL00F                           99
336500140930     C  N99              READ      TABEL00F                               99
336600140930IF  2C     *IN99         IFEQ      '1'
336700140930     C                   MOVEL     MSG(28)       $MSG
336800140930     C                   EXSR      SCRERR
336900140930E   2C                   ENDIF
337000140930DO  2C     *IN99         DOWEQ     '0'
337100140930     C     TBLCOD        ANDEQ     '15'
337200140930IF  3C     TBLFLG        IFEQ      *BLANKS
337300140930     C                   ADD       1             I
337400140930     C                   MOVEL     TBLKEY        d15(I)
337500140930     C                   MOVEL     TBLUNI        DS15
337600140930     C                   MOVEL     §15pcf        pcf(I)
337700140930     C                   MOVEL     §15ecf        ecf(I)
337800140930E   3C                   ENDIF
337900140930     C                   READ      TABEL00F                               99
338000140930E   2C                   ENDDO
338100140930E   1C                   ENDIF
338200971211     C*
338300971211     C* CARICA FLAG ITALIA/ESTERO DALL'ORGANIGRAMMA
338400971211     C     $ERR          IFEQ      'N'
338500971211     C     *LOVAL        SETLL     AZORG01L
338600971211     C                   READ      AZORG01L                               98
338700971211DO  1C     *IN98         DOWEQ     *OFF
338800971211IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
338900971211     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
339000971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
339100980710     C                   MOVEL     ORGDIT        DIT(F)                         *FLAG BAR/SDI
339200000217     C                   MOVEL     ORGDE3        OG143
339300020729     C                   IF        §OGNTW = 'DPD'
339400020729     C                   MOVEL     'S'           DPD(F)                         *FLAG FILIALE DPD
339500020729     C                   ENDIF
339600020503     C                   MOVEL     §OGNTW        NTW(F)
339700020729     C*
339800020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
339900020729     C                   CLEAR                   DNTW
340000020729     C*
340100020729     C                   MOVEL(P)  'NTW'         KTECOD
340200020729     C                   MOVEL(P)  §OGNTW        KTEKE1
340300020729     C     KEYTBE        CHAIN     TNTBE01L
340400020729     C                   IF        %found(TNTBE01L)
340500020729     C                   MOVEL     TBEUNI        DNTW
340600020729     C                   ENDIF
340700020729     C*
340800020729     C                   MOVEL     §NTWFIE       FL1(F)
340900971211E   2C                   ENDIF
341000971211     C                   READ      AZORG01L                               98
341100971211E   1C                   ENDDO
341200971211     C                   END
341300961128     C*
341400961128     C                   ENDSR
341500011008     C*------------------------------------------------------------------------*
341600011008     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
341700011008     C*------------------------------------------------------------------------*
341800011008     C     CARGEDCE      BEGSR
341900011017     C*
342000011017     C                   MOVEL     *BLANKS       WDIVCE            3
342100011008     C*
342200011008     C                   CLEAR                   DSGEDCE
342300011008     C*
342400011008     C                   MOVEL(P)  'GED'         KTECOD
342500011008     C                   MOVEL(P)  'CE'          KTEKE1
342600011008     C     KEYTBE        CHAIN     TNTBE01L
342700011008     C                   IF        %found(TNTBE01L)
342800011008     C                   MOVEL     TBEUNI        DSGEDCE
342900011008     C                   ENDIF
343000011008     C*
343100011008     C                   IF        §GEDCEDAT > D00DTC
343200011026     C                   EVAL      WDIVCE = §GEDCEDIP
343300011017     C                   ELSE
343400011017     C                   EVAL      WDIVCE = §GEDCEDIV
343500011008     C                   ENDIF
343600011008     C*
343700011008     C                   ENDSR
343800011017     C*------------------------------------------------------------------------*
343900011017     C* CARGEDAC - CARICAMENTO TABELLA "GED" TIPO RECORD "AC" E CONSIDERAZIONI SU DIVISE
344000011017     C*------------------------------------------------------------------------*
344100011017     C     CARGEDAC      BEGSR
344200011017     C*
344300011017     C                   MOVEL     *BLANKS       WDIVAC            3
344400011017     C*
344500011017     C                   CLEAR                   DGEDAC
344600011017     C*
344700011017     C                   MOVEL(P)  'GED'         KTECOD
344800011017     C                   MOVEL(P)  'AC'          KTEKE1
344900011017     C     KEYTBE        CHAIN     TNTBE01L
345000011017     C                   IF        %found(TNTBE01L)
345100011017     C                   MOVEL     TBEUNI        DGEDAC
345200011017     C                   ENDIF
345300011017     C*
345400011017     C                   IF        §GEDACDAT > D00DTC
345500011017     C                   EVAL      WDIVAC = §GEDACDIP
345600011017     C                   ELSE
345700011017     C                   EVAL      WDIVAC = §GEDACDID
345800011017     C                   ENDIF
345900011017     C*
346000011017     C                   ENDSR
346100011008     C*------------------------------------------------------------------------*
346200011008     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
346300011008     C*------------------------------------------------------------------------*
346400011008     C     CNVEUR        BEGSR
346500011008     C*
346600011008     C* AZZERA VARIABILI DI LAVORO
346700011008     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
346800011008     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
346900011008     C*
347000011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
347100011026     C*                  IF        WDIVCE = 'ITL'
347200011026     C*                  EVAL      NDEC = '0'
347300011026     C*                  ELSE
347400011026     C*                  EVAL      NDEC = '3'
347500011026     C*                  ENDIF
347600011026     C                   EVAL      NDEC = '9'
347700011008     C*
347800011008IF  1C     WIMI          IFNE      *ZEROS
347900011008     C                   CLEAR                   YEUR
348000011008     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
348100011008     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
348200011023     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
348300011008     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
348400011008     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
348500011008     C                   CALL      'YEURCO'
348600011008     C                   PARM                    YEUR
348700011008IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
348800011008     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
348900011008X   2C                   ELSE                                                   *SI ERRORI
349000011008     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
349100011008E   2C                   ENDIF
349200011008E   1C                   ENDIF
349300011008     C*
349400011008     C                   ENDSR
349500011008     C*------------------------------------------------------------------------*
349600011008     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
349700011008     C*------------------------------------------------------------------------*
349800011008     C     CNVETC        BEGSR
349900011017     C*
350000011008     C* ARROTONDAMENTO
350100011008     C                   Z-ADD     ETCIAR        WIMI             30 9
350200011008     C                   MOVEL     D00DIV        WDVI              3
350300011008     C                   EXSR      CNVEUR
350400011008     C                   Z-ADD     WIMO          ETCIAR
350500011008     C* CONTRASSEGNO
350600011008     C                   Z-ADD     ETCCAS        WIMI             30 9
350700011008     C                   MOVEL     D00DIV        WDVI              3
350800011008     C                   EXSR      CNVEUR
350900011008     C                   Z-ADD     WIMO          ETCCAS
351000011008     C* ASSICURAZIONE
351100011008     C                   Z-ADD     ETCASC        WIMI             30 9
351200011008     C                   MOVEL     D00DIV        WDVI              3
351300011008     C                   EXSR      CNVEUR
351400011008     C                   Z-ADD     WIMO          ETCASC
351500011008     C*
351600011008     C                   ENDSR
351700011008     C*------------------------------------------------------------------------*
351800011008     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
351900011008     C*------------------------------------------------------------------------*
352000011008     C     CNVETS        BEGSR
352100011017     C*
352200011008     C* INOLTRO
352300011008     C                   Z-ADD     ETSINL        WIMI             30 9
352400011008     C                   MOVEL     D00DIV        WDVI              3
352500011008     C                   EXSR      CNVEUR
352600011008     C                   Z-ADD     WIMO          ETSINL
352700011008     C*
352800011008     C                   ENDSR
352900011017     C*------------------------------------------------------------------------*
353000011017     C* CNVEVDSG - CONVERTE I CAMPI SCAGLIONI DI ECEVD IN DIVISA DI CALCOLO DEL CE
353100011017     C*------------------------------------------------------------------------*
353200011017     C     CNVEVDSG      BEGSR
353300011017     C*
353400011017     C* SCAGLIONE INIZIALE
353500011017     C                   Z-ADD     EVDSGI        WIMI             30 9
353600011017     C                   MOVEL     WDIVAC        WDVI              3
353700011017     C                   EXSR      CNVEUR
353800011017     C                   Z-ADD     WIMO          EVDSGI
353900011017     C* SCAGLIONE FINALE
354000011017     C                   Z-ADD     EVDSGF        WIMI             30 9
354100011017     C                   MOVEL     WDIVAC        WDVI              3
354200011017     C                   EXSR      CNVEUR
354300011017     C                   Z-ADD     WIMO          EVDSGF
354400011017     C*
354500011017     C                   ENDSR
354600011017     C*------------------------------------------------------------------------*
354700011017     C* CNVEVD - CONVERTE I CAMPI NON SCAGLIONI DI ECEVD IN DIVISA DI CALCOLO DEL CE
354800011017     C*------------------------------------------------------------------------*
354900011017     C     CNVEVD        BEGSR
355000011017     C*
355100011017     C* IMPORTO UNITARIO TARIFFA
355200011017     C                   Z-ADD     EVDITR        WIMI             30 9
355300011017     C                   MOVEL     WDIVAC        WDVI              3
355400011017     C                   EXSR      CNVEUR
355500011017     C                   Z-ADD     WIMO          EVDITR
355600011017     C* IMPORTO MINIMO VOCE
355700011017     C                   Z-ADD     EVDIMI        WIMI             30 9
355800011017     C                   MOVEL     WDIVAC        WDVI              3
355900011017     C                   EXSR      CNVEUR
356000011017     C                   Z-ADD     WIMO          EVDIMI
356100011017     C* IMPORTO MASSIMO VOCE
356200011017     C                   Z-ADD     EVDIMA        WIMI             30 9
356300011017     C                   MOVEL     WDIVAC        WDVI              3
356400011017     C                   EXSR      CNVEUR
356500011017     C                   Z-ADD     WIMO          EVDIMA
356600011017     C*
356700011017     C                   ENDSR
356800961128     C*------------------------------------------------------------------------*
356900961128     C* SUBDAT - RECUPERARE DATA E ORA ATTUALI
357000961128     C*------------------------------------------------------------------------*
357100961128     C     SUBDAT        BEGSR
357200961128     C*
357300961128     C                   TIME                    DTTIME
357400961128     C                   Z-ADD     DTANN         DSANN
357500961128     C                   Z-ADD     DTMES         DSMES
357600961128     C                   Z-ADD     DTGIO         DSGIO
357700961128     C                   Z-ADD     DTSEC         DSSEC
357800961128     C*
357900961128     C                   ENDSR
358000961128     C*------------------------------------------------------------------------*
358100961128     C* SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
358200961128     C*------------------------------------------------------------------------*
358300961128     C     SCRERR        BEGSR
358400961128     C*
358500961128     C                   MOVEL     'S'           $ERR
358600961128     C                   MOVEL     $MSG          D00MSG
358700961128     C                   MOVEL     1             D00ERR
358800961128     C*
358900961128     C                   ENDSR
359000970213     C*------------------------------------------------------------------------*
359100970213     C* CANCWF - CANCELLA WORKFILE
359200970213     C*------------------------------------------------------------------------*
359300970213     C     CANCWF        BEGSR
359400970213     C*
359500010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETW00F
359600970318IF  1C     D00TLA        IFEQ      'P'                                          *LANCIO X PORTO
359700980909     C******************** MOVELUSER      ETWUSR
359800980909     C                   MOVEL     JOB           ETWUSR
359900010926     C     KEYETW        SETLL     TEETW01L                           99
360000010926     C  N99              READ      TEETW01L                               99
360100970318DO  2C     *IN99         DOWEQ     *OFF
360200980909     C********** USER      ANDEQETWUSR
360300980909     C     JOB           ANDEQ     ETWUSR
360400010926     C                   DELETE    TEETW000
360500010926     C                   READ      TEETW01L                               99
360600970318E   2C                   ENDDO
360700970318E   1C                   ENDIF
360800970213     C*
360900010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETWC0F
361000980909     C******************** MOVELUSER      EWCUSR
361100980909     C                   MOVEL     JOB           EWCUSR
361200010926     C     KEYEWC        SETLL     TEETWC1L                           99
361300010926     C  N99              READ      TEETWC1L                               99
361400970213DO  1C     *IN99         DOWEQ     *OFF
361500980909IF  2C********** USER      ANDEQEWCUSR
361600980909IF  2C     JOB           ANDEQ     EWCUSR
361700010926     C                   DELETE    TEETWC00
361800010926     C                   READ      TEETWC1L                               99
361900970213E   1C                   ENDDO
362000970213     C*
362100970213     C                   ENDSR
362200961128     C*------------------------------------------------------------------------*
362300961128     C* RUTINZ - ROUTINE INIZIALE
362400961128     C*------------------------------------------------------------------------*
362500961128     C     RUTINZ        BEGSR
362600970227     C*--------------------
362700970227     C* RICEVIMENTO PARAMETRI
362800970227     C*--------------------
362900961128     C     *ENTRY        PLIST
363000961128     C                   PARM                    KPJBA
363100961128     C                   PARM                    DSTE00
363200970227     C*--------------------
363300970227     C* CHIAVI DI LETTURA
363400970227     C*--------------------
363500970910     C* LETTURA AZORG01L - COMPLETA
363600970910     C     KEYORG        KLIST
363700970910     C                   KFLD                    KORFIL
363800970227     C* LETTURA TABEL00F - COMPLETA
363900961128     C     KEYTAB        KLIST
364000961128     C                   KFLD                    KTBUTE
364100961128     C                   KFLD                    KTBCOD
364200961128     C                   KFLD                    KTBKEY
364300970227     C* LETTURA ECEVT01L - COMPLETA
364400961128     C     KEYEVT        KLIST
364500961128     C                   KFLD                    EVTVOC
364600961128     C                   KFLD                    EVTPRG
364700011025     C* LETTURA ECEVT01L - COMPLETA
364800011025     C     KEYEVT2       KLIST
364900011025     C                   KFLD                    EVDVOC
365000011025     C                   KFLD                    EVDPRG
365100050103     C* LETTURA ECEVT01L - COMPLETA
365200050103     C     KEYEVT3       KLIST
365300050103     C                   KFLD                    EVMVOC
365400050103     C                   KFLD                    EVMPRG
365500140115     C* LETTURA ECEVD06L - COMPLETA
365600961128     C     KEYEVD        KLIST
365700961128     C                   KFLD                    EVDVOC
365800961128     C                   KFLD                    EVDPRG
365900961128     C                   KFLD                    EVDTTV
366000961128     C                   KFLD                    EVDFT1
366100961128     C                   KFLD                    EVDFT2
366200961128     C                   KFLD                    EVDCLI
366300961128     C                   KFLD                    EVDCTR
366400961128     C                   KFLD                    EVDCTS
366500140115     C                   KFLD                    EVDnaz
366600140115     C                   KFLD                    EVDCAd
366700961128     C                   KFLD                    EVDDDT
366800961128     C                   KFLD                    EVDSGI
366900970227     C* LETTURA ECEVD02L - COMPLETA
367000961128     C     KEYEV2        KLIST
367100961128     C                   KFLD                    EVDCLI
367200970227     C* LETTURA ECEVM01L - COMPLETA
367300961128     C     KEYEVM        KLIST
367400961128     C                   KFLD                    EVMVOM
367500961128     C                   KFLD                    EVMPRM
367600961128     C                   KFLD                    EVMVOC
367700961128     C                   KFLD                    EVMPRG
367800011017     C* LETTURA TEETT01L - PARZIALE
367900961128     C     KE2ETT        KLIST
368000980708     C                   KFLD                    KTTDSF                         *SEDE/FIL
368100970117     C                   KFLD                    KTTCTO                         *TARIF/OFF
368200961128     C                   KFLD                    KTTKSC                         *CLIENTE
368300961128     C                   KFLD                    KTTCTR                         *TARIFFA
368400961128     C                   KFLD                    KTTPRG                         *PROGRESSIVO
368500961128     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
368600961128     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
368700140930     C* LETTURA TEETT11L - COMPLETA
368800961128     C     KEYETT        KLIST
368900980708     C                   KFLD                    KTTDSF                         *SEDE/FIL
369000970117     C                   KFLD                    KTTCTO                         *TARIF/OFF
369100961128     C                   KFLD                    KTTKSC                         *CLIENTE
369200961128     C                   KFLD                    KTTCTR                         *TARIFFA
369300961128     C                   KFLD                    KTTPRG                         *PROGRESSIVO
369400961128     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
369500961128     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
369600140930     C                   KFLD                    KTTntw                         *COD.TASS.
369700140930     C                   KFLD                    KTTCTS                         *COD.TASS.
369800010926     C* LETTURA TEETC01L - COMPLETA
369900961128     C     KEYETC        KLIST
370000980708     C                   KFLD                    KTCDSF                         *SEDE/FIL
370100970117     C                   KFLD                    KTCCTO                         *TARIF/OFF
370200961128     C                   KFLD                    KTCKSC                         *CLIENTE
370300961128     C                   KFLD                    KTCCTR                         *TARIFFA
370400961128     C                   KFLD                    KTCPRG                         *PROGRESSIVO
370500961128     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
370600010926     C* LETTURA TEETS01L - PARZIALE
370700961128     C     KE2ETS        KLIST
370800980708     C                   KFLD                    KTSDSF                         *SEDE/FIL
370900970117     C                   KFLD                    KTSCTO                         *TARIF/OFF
371000961128     C                   KFLD                    KTSKSC                         *CLIENTE
371100961128     C                   KFLD                    KTSCTR                         *TARIFFA
371200961128     C                   KFLD                    KTSPRG                         *PROGRESSIVO
371300961128     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
371400010926     C* LETTURA TEETS01L - COMPLETA
371500961128     C     KEYETS        KLIST
371600980708     C                   KFLD                    KTSDSF                         *SEDE/FIL
371700970117     C                   KFLD                    KTSCTO                         *TARIF/OFF
371800961128     C                   KFLD                    KTSKSC                         *CLIENTE
371900961128     C                   KFLD                    KTSCTR                         *TARIFFA
372000961128     C                   KFLD                    KTSPRG                         *PROGRESSIVO
372100961128     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
372200961128     C                   KFLD                    KTSSGL                         *SCAGLIONE
372300010926     C* LETTURA TEETW01L - COMPLETA
372400961128     C     KEYETW        KLIST
372500970214     C                   KFLD                    ETWUSR                         UTENTE
372600980708     C                   KFLD                    ETWDSF                         *SEDE/FIL
372700970117     C                   KFLD                    ETWCTO                         *TARIF/OFF
372800961128     C                   KFLD                    ETWKSC                         *CLIENTE
372900961128     C                   KFLD                    ETWCTR                         *TARIFFA
373000961128     C                   KFLD                    ETWPRG                         *PROGRESSIVO
373100961128     C                   KFLD                    ETWSGL                         *SCAGL.
373200961128     C                   KFLD                    ETWCTS                         *C.TASS.
373300961128     C                   KFLD                    ETWRDL                         *RAGGR.DELTA
373400010926     C* LETTURA TEETW01L - PARZIALE
373500970430     C     KEYETX        KLIST
373600970430     C                   KFLD                    ETWUSR                         UTENTE
373700980708     C                   KFLD                    ETWDSF                         *SEDE/FIL
373800970430     C                   KFLD                    ETWCTO                         *TARIFFA/OFFERTA
373900970430     C                   KFLD                    ETWKSC                         *CLIENTE
374000970430     C                   KFLD                    ETWCTR                         *TARIFFA
374100970430     C                   KFLD                    ETWPRG                         *PROGRESSIVO
374200970430     C                   KFLD                    ETWSGL                         *SCAGLIONE
374300970430     C                   KFLD                    ETWCTS                         *CODICE TASSAZIONE
374400010926     C* LETTURA TEETWC1L - PARZIALE
374500970213     C     KEYEWC        KLIST
374600970214     C                   KFLD                    EWCUSR                         *UTENTE
374700011008     C* LETTURA TNTBE01L - PARZIALE
374800011008     C     KEYTBE        KLIST
374900011008     C                   KFLD                    KTECOD                         *UTENTE
375000011008     C                   KFLD                    KTEKE1                         *CODICE
375100970227     C*--------------------
375200970227     C* VARIABILI RIFERITE AL DATA BASE
375300970227     C*--------------------
375400970910     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
375500961128     C     *LIKE         DEFINE    TBLKUT        KTBUTE
375600961128     C     *LIKE         DEFINE    TBLCOD        KTBCOD
375700961128     C     *LIKE         DEFINE    TBLKEY        KTBKEY
375800961128     C     *LIKE         DEFINE    EVTARF        WVAL
375900961128     C     *LIKE         DEFINE    EVTVOC        WVOC
376000961128     C     *LIKE         DEFINE    EVTPRG        WPRG
376100000306     C     *LIKE         DEFINE    EVTTIV        WTIV
376200961128     C     *LIKE         DEFINE    EVTRFI        WRIF
376300961128     C     *LIKE         DEFINE    EVDTTV        WTTV
376400961128     C     *LIKE         DEFINE    EVDFT1        WFT1
376500961128     C     *LIKE         DEFINE    EVDFT2        WFT2
376600961128     C     *LIKE         DEFINE    EVDCLI        WKSC
376700961128     C     *LIKE         DEFINE    EVDCTR        WCTR
376800961128     C     *LIKE         DEFINE    EVDCTS        WCTS
376900140115     C     *LIKE         DEFINE    EVDnaz        Wnaz
377000140115     C     *LIKE         DEFINE    EVDCAd        wcad
377100961128     C     *LIKE         DEFINE    EVTDDV        WSPDAT
377200961128     C     *LIKE         DEFINE    ETCLNP        WTFP
377300970910     C     *LIKE         DEFINE    EVDCTS        WCTP
377400021129     C     *LIKE         DEFINE    D00KSC        DEPKSC                         *DEPOSITI
377500011017     C     *LIKE         DEFINE    ETTKSC        KTTKSC                         *TEETT00F
377600980708     C     *LIKE         DEFINE    ETTDSF        KTTDSF
377700970117     C     *LIKE         DEFINE    ETTCTO        KTTCTO
377800961128     C     *LIKE         DEFINE    ETTCTR        KTTCTR
377900961128     C     *LIKE         DEFINE    ETTPRG        KTTPRG
378000961128     C     *LIKE         DEFINE    ETTLNP        KTTLNP
378100961128     C     *LIKE         DEFINE    ETTFIE        KTTFIE
378200140930     C     *LIKE         DEFINE    £ettntw       KTTntw
378300961128     C     *LIKE         DEFINE    ETTCTS        KTTCTS
378400010926     C     *LIKE         DEFINE    ETCKSC        KTCKSC                         *TEETC00F
378500980708     C     *LIKE         DEFINE    ETCDSF        KTCDSF
378600970117     C     *LIKE         DEFINE    ETCCTO        KTCCTO
378700961128     C     *LIKE         DEFINE    ETCCTR        KTCCTR
378800961128     C     *LIKE         DEFINE    ETCPRG        KTCPRG
378900961128     C     *LIKE         DEFINE    ETCLNP        KTCLNP
379000961128     C     *LIKE         DEFINE    ETSKSC        KTSKSC                         *TNESC00F
379100980708     C     *LIKE         DEFINE    ETSDSF        KTSDSF
379200970117     C     *LIKE         DEFINE    ETSCTO        KTSCTO
379300961128     C     *LIKE         DEFINE    ETSCTR        KTSCTR
379400961128     C     *LIKE         DEFINE    ETSPRG        KTSPRG
379500961128     C     *LIKE         DEFINE    ETSLNP        KTSLNP
379600961128     C     *LIKE         DEFINE    ETSSGL        KTSSGL
379700011008     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
379800011008     C     *LIKE         DEFINE    TBEKE1        KTEKE1
379900011017     C*--------------------
380000011017     C* VARIABILI NON RIFERITE AL DATA BASE
380100011017     C*--------------------
380200961128     C*
380300961128     C* CANCELLA EVENTUALI RECORD NEI FILE DI LAVORO
380400970203     C                   Z-ADD     80            LENGH            15 5
380500010926     C   99
380600010926     CANN99              CALL      'QCAEXEC'                                    *TEETW00F
380700970203     C                   PARM                    CMD(1)
380800970203     C                   PARM                    LENGH
380900010926     C   99
381000010926     CANN99              CALL      'QCAEXEC'                                    *TEETWC0F
381100970203     C                   PARM                    CMD(2)
381200970203     C                   PARM                    LENGH
381300961128     C*
381400961128     C* IMPOSTO CAMPI DI GESTIONE PGM
381500961128     C                   MOVEL     'N'           $ERR              1
381600961128     C                   MOVEL     *BLANKS       $MSG             80
381700961128     C*
381800961128     C* RECUPERO IL CODICE DELL'AZIENDA
381900961128     C                   Z-ADD     1             CODUT             1 0
382000021129     C*
382100021129     C* SE OFFERTA, RECUPERA IL CODICE CLIENTE DALLA VISITA (se già associato)
382200091123     c                   if        %subst(d00op0: 1: 1)='O'
382300091123     c                   clear                   tnte04ds
382400091123     C                   movel     D00dsf        d04dsf
382500091123     C                   Z-ADD     D00KSC        d04nrv
382600091123     c                   call      'TNTE04R'
382700091123     c                   parm                    kpjba
382800091126     c                   parm                    tnte04ds
382900091123     C                   Z-ADD     d04KSC        DEPKSC                         *CLIENTE DELLA VISIT
383000021129X   1C                   ELSE                                                   *TARIFFA
383100021129     C                   Z-ADD     D00KSC        DEPKSC                          -CLIENTE TARIFFA
383200021129E   1C                   ENDIF
383300961128     C*
383400961128     C* CARICA LE TABELLE NECESSARIE AL PGM
383500970213     C                   EXSR      CANCWF                                       *CANCELLA WORKFILE
383600961128     C                   EXSR      CARTAB                                       *TABELLE
383700011010     C                   EXSR      CARGEDCE                                     *TABELLE NUOVE
383800011017     C                   EXSR      CARGEDAC                                     *TABELLE NUOVE
383900961128     C                   EXSR      CARCAR                                       *TARIFFE DI CARTELLO
384000961128     C                   EXSR      CARMAG                                       *CARICA MAGGIORAZION
384100961128     C                   EXSR      CARFIL                                       *CARICA TARIFFE FIL
384200961128     C*
384300961128     C                   ENDSR
384400961128     O*------------------------------------------------------------------------*
384500961128** CMD - COMANDI CLP
384600010926CLRPFM FILE(TEETW00F)
384700010926CLRPFM FILE(TEETWC0F)
384800961128
384900961128
385000961128
385100961128** MSG - MESSAGGI
385200961128TNTE10R- Linea partenza in DS non trovata. Avv.CED.                       LPNT 1
385300961128TNTE10R- Tariffa ricarico sede non trovata.                               TRSN 2
385400961128TNTE10R- Tabella '2R' non trovata. Avvertire CED.                         TB2R 3
385500010801TNTE10R- Tabella '1B' non trovata. Avvertire CED.                         TB1B 4
385600961128TNTE10R- Tipo tariffa '01' (colli) non trovata.                           TT01 5
385700961128TNTE10R- Tipo tariffa '02' (n.spedizioni) non trovata.                    TT02 6
385800961128TNTE10R- Tipo tariffa '03' (chilogrammi) non trovata.                     TT03 7
385900961128TNTE10R- Tipo tariffa '04' (volume) non trovata.                          TT04 8
386000961128TNTE10R- Tipo tariffa '09' (stop ritiro) non trovata.                     TT09 9
386100000228TNTE10R- Tipo tariffa '12' (giorni giacenza) non trovata.                 TT1210
386200000228TNTE10R- Tipo tariffa '11' (chilogrammi x gg gicenza) non trovata.        TT1111
386300961128TNTE10R- Tipo tariffa '22' (contrassegno) non trovata.                    TT2212
386400961128TNTE10R- Tipo tariffa '23' (val merce assicur.) non trovata.              TT2313
386500961128TNTE10R- Tipo tariffa '24' (n.giorni pagamento) non trovata.              TT2414
386600961128TNTE10R- Tipo tariffa '25' (imp.comp.sede) non trovata.                   TT2515
386700961128TNTE10R- Tipo tariffa '10' (stop consegna) non trovata.                   TT1016
386800961128TNTE10R- Tariffa ricarico commerciale non trovata.                        TRCN17
386900961128TNTE10R- Voce non trovata.                                                VNTR18
387000961128TNTE10R- Tipo tariffa della voce non trovata in schiera.                  TTNT19
387100961128TNTE10R- Qta x ricerca scaglione non trovata.                             QRSN20
387200961128TNTE10R- Manca tariffa.Voce xxx.999 999                                   MATA21
387300961128TNTE10R- Voce di maggiorazione senza voci da maggiorare.                  VMSV22
387400961128TNTE10R- Tariffa ricarico pagamento non trovata.                          TRPN23
387500961128TNTE10R- Dati tipo del cliente/tariffa non trovati.                       DTCN24
387600961128TNTE10R- Scaglioni del cliente/tariffa non trovati.                       SCCN25
387700961128TNTE10R- Cod. tassazione del cliente/tariffa non trovati.                 CTCN26
387800961128TNTE10R- Cod. tassazione non trovato.                                     CTNT27
387900030428TNTE10R- Tipo tariffa '06' (bancali) non trovata.                         TT0628
388000040611TNTE10R- Tariffa finanziam.export franco DPD non trovata                  TRFT29
388100030428TNTE10R- 29                                                                   30
