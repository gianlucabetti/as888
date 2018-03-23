000100000000     H*------------------------------------------------------------------------*
000200980122     H* SPALMA RETTIFICHE E NOTE DI CREDITO SUI SALDI CLIENTE DA CONTABILITA'
000300990603     H* NB: LANCIATO DALL'AGGIORNAMENTO SALDI CLIENTE DA CONTABILITA' - YCO902C
000400980122     H*     CHE A SUA VOLTA VIENE LANCIATO MENSILMENTE DAL SUPERLANCIO -TNSS01R
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000990609     FSISDO01L  IF   E           K DISK
001100000000     FTABEL00F  UF   E           K DISK
001200000000     FAZORG01L  IF   E           K DISK
001300990609     FSISDC01L  UF A E           K DISK
001400980119     D*------------------------------------------------------------------------*
001500980119     D* SCHIERE
001600980119     D*------------------------------------------------------------------------*
001700980119     D FIL             S              3  0 DIM(999)                             *FILIALI IN GESTIONE
001800000000     D*------------------------------------------------------------------------*
001900000000     D* INPUT
002000000000     D*------------------------------------------------------------------------*
002100010410     D DS5S          E DS
002200000000     D DS14          E DS
002300020805     D*------------------
002400020805     D* ARCHITETTURA
002500020805     D*------------------
002600020805     D KPJBA         E DS
002700000000     C*------------------------------------------------------------------------*
002800000000     C* MAIN LINE
002900000000     C*------------------------------------------------------------------------*
003000010308     C*
003100010308     C* NON FA NIENTE SE ERRONEAMENTE LANCIATO DA 'SDI'
003200020805IF  1C     KNSIF         IFEQ      'GAITRA201'
003300000000     C*
003400000000     C* POSIZIONAMENTO SUL FILE SALDI CLIENTI E PRIMA LETTURA
003500000000     C                   Z-ADD     *LOVAL        SDOKSC
003600990609     C     KEYSDO        SETLL     SISDO01L                           99        *FINE-ARCHIV
003700990609     C  N99              READ      SISDO01L                               99
003800000000     C*
003900000000     C* CICLO FINO A FINE FILE
004000000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
004100980119     C*
004200980119     C* CONTROLLA VALIDITA' RECORD
004300980119     C                   EXSR      CHKSDO
004400980119     C*
004500980119     C* ESEGUE L'AGGIORNAMENTO SOLO SE IL RECORD E' OK
004600980119     C     WRECOK        IFEQ      'S'                                          --- 2 -->
004700000000     C*
004800000000     C* AZZERA GLI IMPORTI SPALMATI
004900020110     C                   Z-ADD     *ZEROS        WINCSP
005000020110     C                   Z-ADD     *ZEROS        WIRRSP
005100000000     C*
005200000000     C* POSIZIONAMENTO SUL FILE SALDI CLIENTI REG/REG E PRIMA LETTURA
005300000000     C                   Z-ADD     SDOKSC        SDCKSC
005400000000     C                   Z-ADD     SDOANN        SDCANN
005500000000     C                   Z-ADD     SDOMES        SDCMES
005600990609     C     KEYSDC        SETLL     SISDC01L                           98        *FINE-ARCHIV
005700990609     C  N98              READ      SISDC01L                               98
005800000000     C*
005900000000     C* CICLO FINO A FINE FILE O FINE ANNO/MESE
006000000000     C     *IN98         DOWEQ     '0'                                          --- 4 -->
006100000000     C     SDCKSC        ANDEQ     SDOKSC
006200000000     C     SDCANN        ANDEQ     SDOANN
006300000000     C     SDCMES        ANDEQ     SDOMES
006400000000     C*
006500000000     C* CALCOLA IMPORTI DA SPALMARE
006600000000     C                   EXSR      CALCOL
006700000000     C*
006800000000     C* AGGIUNGE IMPORTO CALCOLATO AGLI IMPORTI SPALMATI
006900000000     C                   ADD       WIRR          WIRRSP
007000000000     C                   ADD       WINC          WINCSP
007100000000     C*
007200000000     C* SE GLI IMPORTI DA SPALMARE SONO UGUALI A QUELLI GIA' PRESENTI NEL REC.
007300000000     C* NON RISCRIVO NEMMENO IL RECORD
007400000000     C     SDCIRR        IFEQ      WIRR
007500000000     C     SDCINC        ANDEQ     WINC
007600000000     C                   ELSE
007700000000     C                   Z-ADD     WIRR          SDCIRR                         *RETT.RIC.
007800000000     C                   Z-ADD     WINC          SDCINC                         *NOTE ACCRED.
007900000000     C                   MOVEL     *BLANKS       SDCFTR                         *FLAG TRASM.
008000000000     C                   Z-ADD     *ZEROS        SDCDTR                         *DATA TRASM.
008100990609     C                   UPDATE    SISDC000                                     *RISCRIVE
008200000000     C                   END
008300000000     C*
008400000000     C* LEGGE IL CLIENTE REG/REG SUCCESSIVO
008500990609     C                   READ      SISDC01L                               98
008600000000     C                   ENDDO                                                  <-- 4 ---
008700000000     C*
008800020116     C* SE IL RESIDUO NON SPALMATO E' > 1 euro GENERO RECORD FASULLO IN SDC
008900020110     C     SDOIRR        SUB       WIRRSP        WIRRDI
009000020110     C     SDOINC        SUB       WINCSP        WINCDI
009100020110     C     WIRRDI        IFLT      -1                                           *ATT.NE-SONO NEGATIV
009200020110     C     WIRRDI        ORGT      1
009300020110     C     WINCDI        ORLT      -1
009400020110     C     WINCDI        ORGT      1
009500000000     C                   EXSR      CREFAS                                       *CREA RECORD FASULLO
009600000000     C                   ENDIF
009700000000     C                   END                                                    <-- 2 ---
009800000000     C*
009900000000     C* LEGGE IL CLIENTE SUCCESSIVO
010000990609     C                   READ      SISDO01L                               99
010100000000     C                   END                                                    <-- 1 ---
010200000000     C*
010300010410     C* AGGIORNA TABELLA 5S CON IL FLAG DI SPALMAMENTO EFFETTUATO
010400000000     C                   EXSR      AGGTAB
010500010308E   1C                   ENDIF
010600000000     C*
010700000000     C                   SETON                                        LR
010800980119     C*------------------------------------------------------------------------*
010900980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
011000980119     C*------------------------------------------------------------------------*
011100980119     C     CHKSDO        BEGSR
011200980119     C*
011300980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
011400980119     C*
011500980119     C* RECORD NON COMPRESO NELLE DATE RICHIESTE, ESCLUDE
011600980119IF  1C     SDOANN        IFGE      PARAAI
011700980119     C     SDOANN        ANDLE     PARAAF
011800980119X   1C                   ELSE
011900980119     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
012000980119E   1C                   ENDIF
012100980119     C*
012200980119     C* CLIENTE NON GESTITO DAL S.INFORMATIVO, ESCLUDE
012300980119     C                   SETOFF                                           28
012400980119     C                   MOVEL     SDOKSC        N3                3 0          *FILIALE
012500980119     C     N3            LOOKUP    FIL                                    28
012600980119IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
012700980119     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
012800980119E   1C                   ENDIF
012900980119     C*
013000980119     C                   ENDSR
013100000000     C*------------------------------------------------------------------------*
013200000000     C* CALCOL - CALCOLA IMPORTI DA SPALMARE
013300000000     C*------------------------------------------------------------------------*
013400000000     C     CALCOL        BEGSR
013500000000     C*
013600000000     C* AZZERA IMPORTI CALCOLO
013700020110     C                   Z-ADD     *ZEROS        WIRR
013800020110     C                   Z-ADD     *ZEROS        WINC
013900000000     C*
014000000000     C* CALCOLO RETT.RICAVI SE CI SONO
014100000000     C     SDOIRR        IFNE      *ZEROS
014200000000     C     SDOIRD        ANDNE     *ZEROS
014300000000     C     SDCIRD        ANDNE     *ZEROS
014400020110     C     SDOIRR        MULT      SDCIRD        WTM213           21 3
014500020110     C     WTM213        DIV(H)    SDOIRD        WIRR
014600000000     C                   END
014700000000     C*
014800000000     C* CALCOLO NOTE CRED.  SE CI SONO
014900000000     C     SDOINC        IFNE      *ZEROS
015000000000     C     SDOIRD        ANDNE     *ZEROS
015100000000     C     SDCIRD        ANDNE     *ZEROS
015200020110     C     SDOINC        MULT      SDCIRD        WTM213
015300020110     C     WTM213        DIV(H)    SDOIRD        WINC
015400000000     C                   END
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* AGGTAB - AGGIORNA TABELLA DATI SALDI
015900000000     C*------------------------------------------------------------------------*
016000000000     C     AGGTAB        BEGSR
016100000000     C*
016200000000     C                   Z-ADD     CODUT         KTBUTE
016300010410     C                   MOVEL     '5S'          KTBCOD
016400000000     C                   MOVEL     *BLANKS       KTBKEY
016500000000     C     KEYTAB        CHAIN     TABEL                              99
016600000000     C     *IN99         IFEQ      '0'                                          --- 1 -->
016700010410     C                   MOVEL     TBLUNI        DS5S
016800010410     C                   MOVEL     'S'           §5SFSC                         *CONTAB. SPALMATA S/
016900010410     C                   MOVEL     DS5S          TBLUNI
017000000000     C                   MOVEL     *BLANKS       TBLFTR                         *SFLEGGA X TRASMISS.
017100000000     C                   Z-ADD     *ZEROS        TBLDTR
017200000000     C                   UPDATE    TABEL
017300000000     C                   END                                                    <-- 1 ---
017400000000     C*
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700990609     C* CREFAS - CREA RECORD FASULLO IN SISDC (PER NC O RETTIF)
017800000000     C*------------------------------------------------------------------------*
017900000000     C     CREFAS        BEGSR
018000000000     C*
018100000000     C* CERCO LA REGIONE DEL CLIENTE
018200000000     C                   MOVEL     *BLANKS       WRREG             3 0
018300000000     C                   MOVEL     SDOKSC        WFIL3             3
018400000000     C                   MOVE      WFIL3         ORGFIL
018500000000     C     ORGFIL        CHAIN     AZORG01L                           99
018600000000     C     *IN99         IFEQ      '0'                                          --- 1 -->
018700000000     C*
018800000000     C* CERCO IL RAGGRUPPAMENTO REGIONE
018900000000     C                   Z-ADD     CODUT         KTBUTE
019000000000     C                   MOVEL     '14'          KTBCOD
019100000000     C                   MOVEL     ORGCRE        KTBKEY
019200000000     C     KEYTAB        CHAIN(N)  TABEL                              99
019300000000     C     *IN99         IFEQ      '0'                                          --- 2 -->
019400000000     C                   MOVEL     TBLUNI        DS14
019500020325     C* VERIFICO QULI RAGGRUPPAMENTI CONSIDERARE IN RELAZIONE ALL'ANNO DEL PERIODO DA ELABORARE
019600020325     C     SDOANN        IFLE      2001
019700020325     C                   MOVEL     §14CRR        WRREG
019800020325     C                   ELSE
019900020325     C                   MOVEL     §14CRN        WRREG
020000020325     C                   ENDIF
020100020325     C*
020200020325     C                   ENDIF                                                  <-- 2 ---
020300020325     C                   ENDIF                                                  <-- 1 ---
020400000000     C*
020500000000     C* IMPOSTO RECORD E SCRIVO
020600000000     C                   Z-ADD     SDOKSC        SDCKSC
020700000000     C                   Z-ADD     SDOANN        SDCANN
020800000000     C                   Z-ADD     SDOMES        SDCMES
020900000000     C                   Z-ADD     WRREG         SDCREP                         *REGIONE DI PARTENZA
021000000000     C                   Z-ADD     WRREG         SDCREA                         *REGIONE DI ARRIVO
021100000000     C                   MOVEL     '01'          SDCFPE                         *FASCIA DI PESO
021200000000     C                   MOVEL     'C'           SDCTSP                         *TIPO SERVIZIO
021300000000     C                   Z-ADD     *ZEROS        SDCCTR                         *TARIFFA
021400000000     C                   MOVEL     'F'           SDCTPO                         *FRNACO/ASSEGNATO
021500000000     C                   MOVEL     'N'           SDCRBL                         *RAGGRUPPAMENTO
021600000000     C                   MOVEL     *BLANKS       SDCTPR                         *TIPO PRODOTTO
021700990609     C     KEYSDX        CHAIN     SISDC01L                           99
021800000000     C     *IN99         IFEQ      '0'                                          --- 1 --> *ESISTE
021900020110     C     WIRRDI        IFLT      -1
022000020110     C     WIRRDI        ORGT      1
022100000000     C                   Z-ADD     WIRRDI        SDCIRR
022200000000     C                   ENDIF
022300020110     C     WINCDI        IFLT      -1
022400020110     C     WINCDI        ORGT      1
022500000000     C                   Z-ADD     WINCDI        SDCINC
022600000000     C                   ENDIF
022700990609     C                   UPDATE    SISDC000
022800000000     C                   ELSE                                                   <-- 1 --> *NON ESIST
022900000000     C                   Z-ADD     *ZEROS        SDCNSP
023000000000     C                   Z-ADD     *ZEROS        SDCPKG
023100020828     C                   Z-ADD     *ZEROS        SDCPCC
023200020828     C                   Z-ADD     *ZEROS        SDCPLS
023300000000     C                   Z-ADD     *ZEROS        SDCNCO
023400000000     C                   Z-ADD     *ZEROS        SDCVOL
023500000000     C                   Z-ADD     *ZEROS        SDCVCC
023600000000     C                   Z-ADD     *ZEROS        SDCVLS
023700000000     C                   Z-ADD     *ZEROS        SDCIRB
023800020110     C     WIRRDI        IFLT      -1
023900020110     C     WIRRDI        ORGT      1
024000000000     C                   Z-ADD     WIRRDI        SDCIRR
024100000000     C                   ENDIF
024200020110     C     WINCDI        IFLT      -1
024300020110     C     WINCDI        ORGT      1
024400000000     C                   Z-ADD     WINCDI        SDCINC
024500000000     C                   ENDIF
024600000000     C                   Z-ADD     *ZEROS        SDCIRP
024700000000     C                   Z-ADD     *ZEROS        SDCIRD
024800000000     C                   MOVEL     *BLANKS       SDCFTR
024900000000     C                   Z-ADD     *ZEROS        SDCDTR
025000990609     C                   WRITE     SISDC000
025100000000     C                   END                                                    <-- 1 ---
025200000000     C*
025300000000     C                   ENDSR
025400980119     C*------------------------------------------------------------------------*
025500980119     C* CARTAB - CARICA LE TABELLE OCCORRENTI
025600980119     C*------------------------------------------------------------------------*
025700980119     C     CARTAB        BEGSR
025800980119     C*
025900980119     C* CARICA LE FILIALI IN GESTIONE AL SISTEMA INFORMATIVO
026000980119     C                   Z-ADD     *ZEROS        I                 4 0
026100980119     C     *LOVAL        SETLL     AZORG01L
026200980119     C                   READ      AZORG01L                               99
026300980119DO  1C     *IN99         DOWEQ     *OFF
026400980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDE
026500980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
026600980119     C                   ADD       1             I
026700980119     C                   Z-ADD     ORGFIL        FIL(I)
026800980119E   3C                   ENDIF
026900980119E   2C                   ENDIF
027000980119     C                   READ      AZORG01L                               99
027100980119E   1C                   ENDDO
027200980119     C*
027300980119     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500000000     C* *INZSR - OPERAZIONI INIZIALI
027600000000     C*------------------------------------------------------------------------*
027700000000     C     *INZSR        BEGSR
027800000000     C*
027900000000     C* PARAMETRI IN INPUT AL PGM
028000000000     C     *ENTRY        PLIST
028100020805     C                   PARM                    KPJBA
028200990603     C                   PARM                    PARAAI            4 0          *ANNO INIZIO
028300000000     C                   PARM                    PARMMI            2 0          *MESE INIZIO
028400990603     C                   PARM                    PARAAF            4 0          *ANNO FINE
028500000000     C                   PARM                    PARMMF            2 0          *MESE FINE
028600000000     C*
028700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
028800000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
028900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
029000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
029100020110     C     *LIKE         DEFINE    SDOINC        WINCSP                         *SPALMATI
029200020110     C     *LIKE         DEFINE    SDOIRR        WIRRSP
029300020110     C     *LIKE         DEFINE    SDOIRR        WIRR                           *ATTRIBUITI
029400020110     C     *LIKE         DEFINE    SDOINC        WINC
029500020110     C     *LIKE         DEFINE    SDOIRR        WIRRDI                         *RESIDUI
029600020110     C     *LIKE         DEFINE    SDOINC        WINCDI
029700000000     C*
029800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
029900000000     C     KEYTAB        KLIST
030000000000     C                   KFLD                    KTBUTE
030100000000     C                   KFLD                    KTBCOD
030200000000     C                   KFLD                    KTBKEY
030300000000     C*
030400990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
030500000000     C     KEYSDO        KLIST
030600000000     C                   KFLD                    SDOKSC
030700000000     C                   KFLD                    SDOANN
030800000000     C                   KFLD                    SDOMES
030900000000     C*
031000990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
031100000000     C     KEYSDC        KLIST
031200000000     C                   KFLD                    SDCKSC
031300000000     C                   KFLD                    SDCANN
031400000000     C                   KFLD                    SDCMES
031500000000     C*
031600990609     C* CHIAVE LETTURA SISDC01L - COMPLETA
031700000000     C     KEYSDX        KLIST
031800000000     C                   KFLD                    SDCKSC
031900000000     C                   KFLD                    SDCANN
032000000000     C                   KFLD                    SDCMES
032100000000     C                   KFLD                    SDCREP
032200000000     C                   KFLD                    SDCREA
032300000000     C                   KFLD                    SDCFPE
032400000000     C                   KFLD                    SDCTSP
032500000000     C                   KFLD                    SDCCTR
032600000000     C                   KFLD                    SDCTPO
032700000000     C                   KFLD                    SDCRBL
032800000000     C                   KFLD                    SDCTPR
032900000000     C*
033000000000     C* VARIABILI DI CONTROLLO
033100000000     C                   MOVEL     '0'           FINL00            1
033200980119     C                   MOVEL     'N'           WRECOK            1
033300000000     C                   Z-ADD     1             CODUT             1 0
033400980119     C*
033500980119     C* CARICA LE TABELLE OCCORRENTI
033600980119     C                   EXSR      CARTAB
033700000000     C*
033800000000     C                   ENDSR
