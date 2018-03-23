000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - TRAINI - LANCIA LA RICHIESTA DI UN ALTRO GIORNO      *
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL06D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL06DVM:MSGULT)
001000000000     FTNTLT01L  UF A E           K DISK
001100000000     F                                     COMMIT
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI E FUNZIONI
001700000000     D*-------------------
001800000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001900000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000000000     D*-------------------
002100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200000000     D*-------------------
002300000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002400000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002500000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600000000     D*-------------------
002700000000     D* TABELLA DESCRIZIONI A VIDEO
002800000000     D*-------------------
002900000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000000000     D*-------------------
003100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003200000000     D*-------------------
003300000000     D KEYS            S              1    DIM(40)
003400000000     D*-------------------
003500000000     D* GIORNI DELLA SETTIMANA
003600000000     D*-------------------
003700000000     D GIO             S              1    DIM(8)                               *GIORNO ('1'÷'8')
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*-------------------
004200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300000000     D*-------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600000000     D*-------------------
004700000000     D* DS PER GESTIONE TASTI FUNZIONALI
004800000000     D*-------------------
004900000000     D DSKEYS        E DS
005000000000     D  KEYRIG                 1     40
005100000000     D*-------------------
005200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300000000     D*-------------------
005400000000     D                SDS
005500000000     D  USER                 254    263
005600000000     D*-------------------
005700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800000000     D*-------------------
005900000000     D DEVDS1          DS
006000000000     D  KEY                  369    369
006100000000     D  CURSOR               370    371
006200000000     D*---------------------
006300000000     D* ARCHITETTURA
006400000000     D*---------------------
006500000000     D KPJBA         E DS
006600000000     D*---------------------
006700000000     D* DS ESTERNA DI PROCEDURA
006800000000     D*---------------------
006900000000     D DS00          E DS                  EXTNAME(TNTL00DS)
007000000000     D*-------------------
007100000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007200000000     D*-------------------
007300000000     D DSMSG         E DS
007400020715
007500020715     d DdatiUte      e ds
007600020715     d AzuteDs       e ds                  ExtName(Azute00f)
007700020715     d Tibs34Ds      e ds                  Inz
007800020715
007900000000     C*------------------------------------------------------------------------*
008000000000     C* MAIN LINE
008100000000     C*------------------------------------------------------------------------*
008200000000     C*
008300000000     C                   WRITE     TL06DVT                                      *TESTATA
008400000000DO  1C     $FINE         DOUEQ     '1'
008500000000     C*
008600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008700000000IF  2C     $GES          IFEQ      'V1'
008800000000     C                   EXSR      GESV1
008900000000E   2C                   ENDIF
009000000000     C*
009100000000E   1C                   ENDDO
009200000000     C*
009300000000IF  1C     $F03          IFEQ      '1'
009400000000     C                   MOVEL     '1'           D00F03
009500000000E   1C                   ENDIF
009600000000     C* FINE PGM
009700000000     C                   MOVEL     DS00          KPJBU
009800000000     C                   SETON                                        LR
009900000000     C*------------------------------------------------------------------------*
010000000000     C* GESV1 - GESTIONE VIDEO UNO
010100000000     C*------------------------------------------------------------------------*
010200000000     C     GESV1         BEGSR
010300000000     C*
010400000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
010500000000     C                   EXSR      CARV1                                        *CARICA VIDEO
010600000000E   1C                   ENDIF
010700000000     C*
010800000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010900000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011000000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011100000000E   1C                   ENDIF
011200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011300000000     C*
011400000000     C                   EXFMT     TL06DV1                                      *PARAMETRI
011500000000     C*
011600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011700000000     C*
011800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011900000000SL  2C                   SELECT
012000000000     C     KEY           WHENEQ    ENTER
012100000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012200000000     C                   OTHER
012300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012400000000E   2C                   ENDSL
012500000000E   1C                   ENDIF
012600000000     C*
012700000000     C                   ENDSR
012800000000     C*------------------------------------------------------------------------*
012900000000     C* CARV1 - CARICA VIDEO UNO
013000000000     C*------------------------------------------------------------------------*
013100000000     C     CARV1         BEGSR
013200000000     C*
013300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013400000000     C*
013500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013700000000     C*
013800000000     C                   ENDSR
013900000000     C*------------------------------------------------------------------------*
014000000000     C* ENTV1 - GESTIONE TASTO ENTER
014100000000     C*------------------------------------------------------------------------*
014200000000     C     ENTV1         BEGSR
014300000000     C*
014400000000     C* CONTROLLA I CAMPI IMMESSI
014500000000     C                   EXSR      DECODI
014600000000IF  1C     $ERR          IFEQ      '0'
014700000000     C*
014800000000     C                   MOVEL     V1CGSE        D00GSE                         *GIORNO DI PARTENZA
014900000000     C*
015000000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
015100000000     C                   MOVEL     '1'           $FINE
015200000000E   1C                   ENDIF
015300000000     C*
015400000000     C                   ENDSR
015500000000     C*------------------------------------------------------------------------*
015600000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
015700000000     C*------------------------------------------------------------------------*
015800000000     C     DECODI        BEGSR
015900000000     C*
016000000000     C* REIMPOSTA INDICATORI DI ERRORE
016100000000     C                   SETOFF                                       60
016200000000     C*
016300000000DO  1C                   DO
016400000000     C*--------------------
016500000000     C* GIORNO DELLA SETTIMANA
016600000000     C*--------------------
016700000000IF  2C     V1CGSE        IFNE      *BLANKS                                      ' '=TUTTI
016800000000     C     V1CGSE        SCAN      TLTGSE                                 28
016900000000IF  3C     *IN28         IFEQ      *OFF
017000000000     C                   MOVEL     '1'           $ERR
017100000000     C                   MOVEL     MSG(10)       $MSG
017200000000     C                   EXSR      CARMSG
017300000000     C                   SETON                                        60
017400000000     C                   LEAVE
017500000000E   3C                   ENDIF
017600000000E   2C                   ENDIF
017700000000     C*
017800000000E   1C                   ENDDO
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     IMPVID        BEGSR
018500000000     C*
018600000000     C* LEGGE IL TRAINO
018700000000     C                   Z-ADD     D00TRN        KLTTRN
018800000000     C                   Z-ADD     D00DDE        KLTDDE
018900000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
019000000000     C                   MOVEA     TLTGSE        GIO                            *PARTENZE
019100000000     C                   MOVEL     GIO(1)        V1DGS1
019200000000     C                   MOVEL     GIO(2)        V1DGS2
019300000000     C                   MOVEL     GIO(3)        V1DGS3
019400000000     C                   MOVEL     GIO(4)        V1DGS4
019500000000     C                   MOVEL     GIO(5)        V1DGS5
019600000000     C                   MOVEL     GIO(6)        V1DGS6
019700000000     C                   MOVEL     GIO(7)        V1DGS7
019800000000     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* *INZSR  - ROUTINE INIZIALE
020200000000     C*------------------------------------------------------------------------*
020300000000     C     *INZSR        BEGSR
020400000000     C*--------------------
020500000000     C* RICEVIMENTO PARAMETRI
020600000000     C*--------------------
020700000000     C     *ENTRY        PLIST
020800000000     C                   PARM                    KPJBA
020900000000     C                   MOVEL     KPJBU         DS00
021000000000     C*--------------------
021100000000     C* VARIABILI RIFERITE AL DATA BASE
021200000000     C*--------------------
021300000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
021400000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
021500000000     C*--------------------
021600000000     C* CHIAVI DI LETTURA
021700000000     C*--------------------
021800000000     C* LETTURA TNTLT01L - COMPLETA
021900000000     C     KEYTLT        KLIST
022000000000     C                   KFLD                    KLTTRN                         *TRAINO
022100000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
022200000000     C*
022300000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
022400000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
022500000000IF  1C     *IN99         IFEQ      *ON
022600000000     C     *IN99         ANDEQ     *OFF
022700000000     C                   UPDATE    TNTLT000
022800000000     C                   WRITE     TNTLT000
022900000000E   1C                   ENDIF
023000000000     C*
023100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
023200000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
023300000000     C                   EXSR      INZSTD
023400000000     C*
023500000000     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
023600000000     C                   EXSR      IMPVID
023700951212     C*
023800951212     C* EMETTE IL MESSAGGIO DI AVVERTIMENTO DI PERDITA DATI (SE C'E')
023900951212IF  1C     D00MSG        IFNE      *BLANKS
024000951212     C                   MOVEL     D00MSG        $MSG
024100951212     C                   EXSR      CARMSG
024200951212E   1C                   ENDIF
024300000000     C*
024400000000     C* EMETTO PRIMO FORMATO VIDEO
024500000000     C                   EXSR      CARV1
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025100000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
025200000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
025300000000     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
025400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025600000000     C*------------------------------------------------------------------------*
025700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
025800000000     C*------------------------------------------------------------------------*
025900000000     C     INZSTD        BEGSR
026000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
026100000000     C                   CALL      'XKEYS'
026200000000     C                   PARM                    DSKEYS
026300000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
026400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
026500000000     C     XMSGPA        PLIST
026600000000     C                   PARM                    DSMSMS
026700000000     C                   PARM                    DSMSPG
026800000000     C                   PARM                    DSMSKE
026900000000     C                   PARM                    DSMSER
027000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
027100000000     C                   MOVEL     'TNTL06R'     DSMSPG                         *NOME PGM CHIAMANTE
027200000000     C                   MOVEL     'TNTL06R'     VMCQUE                         *CODA MESSAGGI DESTI
027300000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
027400000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
027500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
027600000000     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
027700000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
027800000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F03
027900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
028000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
028100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
028200000000     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
028300000000     C* LANCIO ROUTINE PER VERIFICARE SE
028400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
028500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
028600020715
028700020715     c     *dtaara       define    §azute        azuteds
028800020715     c     *dtaara       define    §datiute      ddatiute
028900020715
029000020715     c                   in(E)     *dtaara
029100020715     c                   if        %Error or rsut = *blanks
029200020715     c                   clear                   tibs34ds
029300020715     c                   call      'TIBS34R'
029400020715     c                   parm                    Tibs34Ds
029500020715     c                   in        *dtaara
029600020715     c                   endif
029700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
029800000000     C                   BITOFF    '01234567'    POSMI1            1
029900000000     C                   BITOFF    '01234567'    POSMI2            1
030000000000     C                   BITOFF    '01234567'    POSMA1            1
030100000000     C                   BITOFF    '01234567'    POSMA2            1
030200000000     C                   BITON     '34'          POSMI1            1            *18
030300000000     C                   BITON     '7'           POSMI2            1            *01
030400000000     C                   BITON     '34'          POSMA1            1            *18
030500000000     C                   BITON     '0'           POSMA2            1            *80
030600000000     C                   MOVE      POSMI2        POSMIN            2
030700000000     C                   MOVEL     POSMI1        POSMIN
030800000000     C                   MOVE      POSMA2        POSMAX            2
030900000000     C                   MOVEL     POSMA1        POSMAX
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
031300000000     C*------------------------------------------------------------------------*
031400000000     C     FUNSTD        BEGSR
031500000000     C                   SELECT
031600000000     C     KEY           WHENEQ    F3
031700000000     C                   EXSR      F03STD
031800000000     C     KEY           WHENEQ    F12
031900000000     C                   EXSR      F12STD
032000000000     C     KEY           WHENEQ    F24
032100000000     C                   EXSR      F24STD
032200000000     C                   OTHER
032300000000     C                   MOVEL     MSG(2)        $MSG
032400000000     C                   EXSR      CARMSG
032500000000     C                   ENDSL
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
032900000000     C*------------------------------------------------------------------------*
033000000000     C     F03STD        BEGSR
033100000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
033200000000     C                   MOVEL     '1'           $FINE
033300000000     C* IMPOSTA FLAG DI FINE PROCEDURA
033400000000     C                   MOVEL     '1'           $F03
033500951212     C* PULISCE IL MESSAGGIO
033600951212     C                   MOVEL     *BLANKS       D00MSG
033700000000     C                   ENDSR
033800000000     C*------------------------------------------------------------------------*
033900000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
034000000000     C*------------------------------------------------------------------------*
034100000000     C     F12STD        BEGSR
034200000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
034300000000     C                   MOVEL     '1'           $FINE
034400000000     C* IMPOSTA FLAG DI FINE PROCEDURA
034500000000     C                   MOVEL     '1'           $F12
034600951212     C* PULISCE IL MESSAGGIO
034700951212     C                   MOVEL     *BLANKS       D00MSG
034800000000     C                   ENDSR
034900000000     C*------------------------------------------------------------------------*
035000000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
035100000000     C*------------------------------------------------------------------------*
035200000000     C     F24STD        BEGSR
035300000000     C*
035400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
035500000000     C                   EXSR      OUTFUN
035600000000     C*
035700000000     C                   ENDSR
035800000000     C*------------------------------------------------------------------------*
035900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
036000000000     C*          CODA MESSAGGI DEL PGM
036100000000     C*------------------------------------------------------------------------*
036200000000     C     CARMSG        BEGSR
036300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
036400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
036500000000     C     MSGULT        IFEQ      0
036600000000     C                   MOVEL     '1'           *IN40
036700000000     C                   WRITE     TL06DVK
036800000000     C                   MOVEL     '0'           *IN40
036900000000     C                   ENDIF
037000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
037100000000     C                   MOVEL     $MSG          DSMSMS
037200000000     C                   CALL      'XMSG'        XMSGPA
037300000000     C     DSMSER        IFEQ      0
037400000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
037500000000     C                   ENDIF
037600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
037700000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
037800000000     C                   WRITE     TL06DVM                                      *SFL MESS
037900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
038000000000     C                   MOVEL     *BLANKS       $MSG
038100000000     C                   ENDSR
038200000000     C*------------------------------------------------------------------------*
038300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
038400000000     C*------------------------------------------------------------------------*
038500000000     C     OUTMSG        BEGSR
038600000000     C* VISUALIZZAZIONE MESSAGGIO
038700000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
038800000000     C                   WRITE     TL06DVK                                      *CONTR MESS
038900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
039000000000     C                   Z-ADD     *ZEROS        MSGULT
039100000000     C                   ENDSR
039200000000     C*------------------------------------------------------------------------*
039300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
039400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
039500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
039600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
039700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
039800000000     C*------------------------------------------------------------------------*
039900000000     C     CARFUN        BEGSR
040000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
040100000000     C     1             DO        31            I                 5 0          --- 1 -->
040200000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
040300000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
040400000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
040500020715     c                   If        DutLpo = 'S'
040600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
040700000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
040800000000     C                   ELSE                                                   *INTERROG.
040900000000     C                   MOVEL     FUNASI        FAT(I)
041000000000     C                   ENDIF
041100000000     C                   ELSE                                                   <-- 2 --> *FILIALE
041200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
041300000000     C                   MOVEL     FUNAFG        FAT(I)
041400000000     C                   ELSE                                                   *INTERROG.
041500000000     C                   MOVEL     FUNAFI        FAT(I)
041600000000     C                   ENDIF
041700000000     C                   ENDIF                                                  <-- 2 ---
041800000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
041900000000     C                   MOVEL     USER          WEDP              3            *PROFILO
042000000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
042100000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
042200000000     C                   ELSE
042300000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
042400000000     C                   ENDIF
042500000000     C                   ENDIF
042600000000     C                   ENDDO                                                  <-- 1 ---
042700000000     C                   ENDSR
042800000000     C*------------------------------------------------------------------------*
042900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
043000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
043100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
043200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
043300000000     C*------------------------------------------------------------------------*
043400000000     C     OUTFUN        BEGSR
043500000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
043600000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
043700000000     C                   ENDDO
043800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
043900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
044000000000     C     1             DO        31            I
044100000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
044200000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
044300000000     C                   ADD       1             FUNNAT
044400000000     C                   ENDIF
044500000000     C                   ENDDO
044600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
044700000000     C     FUNNAT        IFGT      8
044800000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
044900000000     C                   ENDIF
045000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
045100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
045200000000     C                   Z-ADD     8             FUNVIS            5 0
045300000000     C     FAT(23)       IFEQ      'S'
045400000000     C                   SUB       1             FUNVIS
045500000000     C                   ENDIF
045600000000     C     FAT(24)       IFEQ      'S'
045700000000     C                   SUB       1             FUNVIS
045800000000     C                   ENDIF
045900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
046000000000     C                   Z-ADD     *ZEROS        W                 5 0
046100000000     C                   ADD       1             FUNULT
046200000000     C     FUNULT        IFGT      31
046300000000     C                   Z-ADD     1             FUNULT
046400000000     C                   ENDIF
046500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
046600000000     C     FUNULT        DO        31            I                              --- 1 -->
046700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
046800000000     C     W             IFLT      FUNVIS                                       --- 2 -->
046900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
047000000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
047100000000     C     FDE(I)        ANDNE     *BLANKS
047200000000     C     I             IFEQ      23                                           *TOLGO F23
047300000000     C     I             OREQ      24                                           *E F24
047400000000     C                   ELSE
047500000000     C                   ADD       1             W
047600000000     C                   MOVEL     FDE(I)        FUV(W)
047700000000     C                   ENDIF
047800000000     C                   ENDIF                                                  <-- 3 ---
047900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
048000000000     C                   Z-ADD     I             FUNULT
048100000000     C                   ENDIF                                                  <-- 2 ---
048200000000     C                   ENDDO                                                  <-- 1 ---
048300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
048400000000     C                   MOVEL     FUV(1)        VFD001
048500000000     C                   MOVEL     FUV(2)        VFD002
048600000000     C                   MOVEL     FUV(3)        VFD003
048700000000     C                   MOVEL     FUV(4)        VFD004
048800000000     C                   MOVEL     FUV(5)        VFD005
048900000000     C                   MOVEL     FUV(6)        VFD006
049000000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
049100000000     C     FAT(24)       ANDEQ     'N'
049200000000     C                   MOVEL     FUV(7)        VFD007
049300000000     C                   MOVEL     FUV(8)        VFD008
049400000000     C                   ELSE                                                   <-- 1 -->
049500000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
049600000000     C     FAT(24)       ANDEQ     'S'
049700000000     C                   MOVEL     FDE(23)       VFD007
049800000000     C                   MOVEL     FDE(24)       VFD008
049900000000     C                   ELSE                                                   <-- 2 -->
050000000000     C                   MOVEL     FUV(7)        VFD007
050100000000     C     FAT(23)       IFEQ      'S'
050200000000     C                   MOVEL     FDE(23)       VFD008
050300000000     C                   ELSE
050400000000     C                   MOVEL     FDE(24)       VFD008
050500000000     C                   ENDIF
050600000000     C                   ENDIF                                                  <-- 2 ---
050700000000     C                   ENDIF                                                  <-- 1 ---
050800000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
051200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
051300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
051400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
051500000000     C*------------------------------------------------------------------------*
051600000000     C     CONFUN        BEGSR
051700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
051800000000     C     1             DO        31            I                              --- 1 -->
051900000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
052000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
052100000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
052200000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
052300000000     C                   EXSR      CARMSG
052400000000     C                   MOVEL     '1'           $ERR
052500000000     C                   ENDIF                                                  <-- 3 ---
052600000000     C                   ENDIF                                                  <-- 2 ---
052700000000     C                   ENDDO                                                  <-- 1 ---
052800000000     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000** -MSG- (MESSAGGI)
053100000000TNTL06R- Tasto funzionale non valido.                                          1
053200000000TNTL06R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
053300000000TNTL06R- Opzione non valida.                                                   3
053400000000TNTL06R- Opzione abilitata ma non gestita. Avvertire CED.                      4
053500000000TNTL06R- Inizio elenco.                                                        5
053600000000TNTL06R- Fine elenco.                                                          6
053700000000TNTL06R- 7
053800000000TNTL06R- 8
053900000000TNTL06R- 9
054000000000TNTL06R- Inserire un giorno della settimana fra quelli possibili              10
054100000000TNTL06R- 11
054200000000TNTL06R- 12
054300000000TNTL06R- 13
054400000000TNTL06R- 14
054500000000TNTL06R- 15
054600000000TNTL06R- 16
054700000000TNTL06R- 17
054800000000TNTL06R- 18
054900000000TNTL06R- 19
055000000000TNTL06R- 20
055100000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
055200000000F1    -                   -    NN     NN
055300000000F2    -                   -    NN     NN
055400000000F3    -F3=Fine            -    NN     NN
055500000000F4    -                   -    NN     NN
055600000000F5    -                   -    NN     NN
055700000000F6    -                   -    NN     NN
055800000000F7    -                   -    NN     NN
055900000000F8    -                   -    NN     NN
056000000000F9    -                   -    NN     NN
056100000000F10   -                   -    NN     NN
056200000000F11   -                   -    NN     NN
056300000000F12   -F12=Ritorno        -    SS     SS
056400000000F13   -                   -    NN     NN
056500000000F14   -                   -    NN     NN
056600000000F15   -                   -    NN     NN
056700000000F16   -                   -    NN     NN
056800000000F17   -                   -    NN     NN
056900000000F18   -                   -    NN     NN
057000000000F19   -                   -    NN     NN
057100000000F20   -                   -    NN     NN
057200000000F21   -                   -    NN     NN
057300000000F22   -                   -    NN     NN
057400000000F23   -                   -    NN     NN
057500000000F24   -F24=Altri tasti    -    NN     NN
057600000000ENTER -                   -    SS     SS
057700000000ROLLUP-                   -    NN     NN
057800000000ROLLDN-                   -    NN     NN
057900000000HELP  -                   -    NN     NN
058000000000PRINT -                   -    NN     NN
058100000000BSPACE-                   -    NN     NN
058200000000CLEAR -                   -    NN     NN
