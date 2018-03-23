000100970722     H*------------------------------------------------------------------------*
000200970723     H* SALVATAGGIO SALDI SU CASSETTA - RICHIESTA PARAMETRI                    *
000300970722     H*------------------------------------------------------------------------*
000400970722     H DECEDIT('0,') DATEDIT(*DMY.)
000500970722     F*------------------------------------------------------------------------*
000600970722     F* DATA BASE
000700970722     F*------------------------------------------------------------------------*
000800970723     FTNSS43D   CF   E             WORKSTN INFDS(DEVDS1)
000900970723     F                                     SFILE(SS43DVM:MSGULT)
001000970724     FTABEL00F  IF   E           K DISK
001100970722     D*------------------------------------------------------------------------*
001200970722     D* SCHIERE
001300970722     D*------------------------------------------------------------------------*
001400970722     D*-------------------
001500970722     D* SCHIERE A TEMPO DI COMPILAZIONE
001600970722     D*-------------------
001700970722     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001800970722     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900970722     D*-------------------
002000970722     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100970722     D*-------------------
002200970722     D FFU             S              6    DIM(31)                              *FUNZIONE
002300970722     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002400970722     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500970722     D*-------------------
002600970722     D* TABELLA DESCRIZIONI A VIDEO
002700970722     D*-------------------
002800970722     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900970722     D*-------------------
003000970722     D* TABELLA TASTI FUNZIONALI PREMUTI
003100970722     D*-------------------
003200970722     D KEYS            S              1    DIM(40)
003300970722     D*------------------------------------------------------------------------*
003400970722     D* INPUT
003500970722     D*------------------------------------------------------------------------*
003600970722     D*-------------------
003700970722     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800970722     D*-------------------
003900970722     D DSFUN         E DS
004000970722     D  FUNRIG                 1     40
004100970722     D*-------------------
004200970722     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004300970722     D*-------------------
004400970722     D DSMSG         E DS
004500970722     D*-------------------
004600970722     D* DS PER GESTIONE TASTI FUNZIONALI
004700970722     D*-------------------
004800970722     D DSKEYS        E DS
004900970722     D  KEYRIG                 1     40
005000970722     D*-------------------
005100970722     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005200970722     D*-------------------
005300970722     D                SDS
005400970722     D  USER                 254    263                                         UTENTE
005500970722     D*-------------------
005600970722     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005700970722     D*-------------------
005800970722     D DEVDS1          DS
005900970722     D  KEY                  369    369
006000970722     D  CURSOR               370    371
006100970722     D*-------------------
006200020920     D* DS REPERIMENTO DATI UTENTE
006300970722     D*-------------------
006400020920     D TIBS34DS      E DS                                                       *Profili utente
006500020920     D DDATIUTE      E DS                                                       *Dati utente
006600020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700970723     D*-------------------
006800020701     D* DS DATI STATISTICHE
006900970723     D*-------------------
007000020701     D DS5S          E DS
007100990527     D*------------------
007200990527     D* DS CONTROLLA DATA (8)
007300990527     D*------------------
007400020701     D WLBDA8          DS                  INZ
007500990527     D  G08DAT                 1      8  0
007600990527     D  G08INV                 9     16  0
007700990527     D  G08ERR                17     17
007800990527     D  G08TGI                18     22  0
007900970723     D*------------------
008000970723     D* DS COMPOSIZIONE DATA GENERICA
008100970723     D*------------------
008200970723     D                 DS
008300990527     D  ANNO                   1      4  0
008400990527     D  MESE                   5      6  0
008500990527     D  DATA                   1      6  0
008600970722     D*------------------
008700970722     D* DS PARAMETRI LANCIO
008800970722     D*------------------
008900970722     D PARAM           DS                  INZ
009000990527     D  PARAN2                 1      4  0
009100990527     D  PARME2                 5      6  0
009200970723     D*-------------------
009300970722     D* ARCHITETTURA
009400970722     D*-------------------
009500970722     D KPJBA         E DS
009600970722     C*------------------------------------------------------------------------*
009700970722     C* MAIN LINE
009800970722     C*------------------------------------------------------------------------*
009900970722     C*
010000970722     C* CICLO FINO A CHE:
010100970722DO  1C     $FINE         DOUEQ     '1'
010200970722     C*
010300970722     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010400970722     C*
010500970722     C* CONTROLLA GESTIONE VIDEO
010600970722IF  2C     $GES          IFEQ      'V1'
010700970722     C                   EXSR      GESV1                                        *PARAMETRI
010800970722E   2C                   ENDIF
010900970722     C*
011000970722E   1C                   ENDDO
011100970722     C*
011200970722     C                   SETON                                        LR
011300970722     C*------------------------------------------------------------------------*
011400970722     C* GESV1 - GESIONE PRIMO VIDEO
011500970722     C*------------------------------------------------------------------------*
011600970722     C     GESV1         BEGSR
011700970722     C*
011800970722IF  1C     $TESVD        IFEQ      '1'                                          TESTATA DA RIEMETTER
011900970722     C                   EXSR      CARV1                                        *CARICA VIDEO
012000970722E   1C                   ENDIF
012100970722     C*
012200970722IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012300970722     C                   MOVEL     *BLANKS       $MSG
012400970722     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012500970722E   1C                   ENDIF
012600970722     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012700970722     C*
012800970723     C                   EXFMT     SS43DV1                                      *PARAMETRI
012900970722     C*
013000970722     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013100970722     C*
013200970722IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013300970722SL  2C                   SELECT
013400970722     C     KEY           WHENEQ    ENTER
013500970722     C                   EXSR      ENTV1                                        *ENTER--> DECODIFICA
013600970722     C     KEY           WHENEQ    F6
013700970722     C                   EXSR      F06GES                                       *F06 --> LANCIO
013800970722     C                   OTHER
013900970722     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014000970722E   2C                   ENDSL
014100970722E   1C                   ENDIF
014200970722     C*
014300970722     C                   ENDSR
014400970722     C*------------------------------------------------------------------------*
014500970722     C* CARV1 - CARICAMENTO PRIMO VIDEO
014600970722     C*------------------------------------------------------------------------*
014700970722     C     CARV1         BEGSR
014800970722     C*
014900970722     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015000970722     C*
015100970722     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015200970722     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015300970722     C*
015400970723     C                   WRITE     SS43DVT                                      *TESTATA
015500970723     C                   WRITE     SS43DVF                                      *FUNZIONI
015600970722     C*
015700970722     C                   ENDSR
015800970722     C*------------------------------------------------------------------------*
015900970722     C* ENTV1 - GESTIONE TASTO "ENTER" PRIMO VIDEO
016000970722     C*------------------------------------------------------------------------*
016100970722     C     ENTV1         BEGSR
016200970722     C*
016300970722     C* CONTROLLA I CAMPI IMMESSI
016400970722     C                   EXSR      DECODI
016500970722     C*
016600970722     C                   ENDSR
016700970722     C*------------------------------------------------------------------------*
016800970722     C* F06GES - GESTIONE TASTO F06 ==> CONFERMA LANCIO
016900970722     C*------------------------------------------------------------------------*
017000970722     C     F06GES        BEGSR
017100970722     C*
017200970722     C* CONTROLLA I CAMPI IMMESSI
017300970722     C                   EXSR      DECODI
017400970722     C*
017500970722     C* LANCIA PROGRAMMA DI GUIDA
017600970722IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017700970724     C                   Z-ADD     V1CAN2        PARAN2                         *ANNO FINALE
017800970724     C                   Z-ADD     V1CME2        PARME2                         *MESE FINALE
017900970724     C                   MOVEL     'SS44'        KCOAZ
018000970722     C                   MOVEL     PARAM         KPJBU
018100970722     C                   CALL      'BCH10'
018200970722     C                   PARM                    KPJBA
018300970722     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
018400970722E   1C                   ENDIF
018500970722     C*
018600970722     C                   ENDSR
018700970722     C*------------------------------------------------------------------------*
018800970722     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018900970722     C*------------------------------------------------------------------------*
019000970722     C     DECODI        BEGSR
019100970722     C*
019200970723     C* REIMPOSTA INDICATORI DI ERRORE SUI CAMPI
019300970723     C                   SETOFF                                       6061
019400970723     C*--------------------
019500970724     C* ANNO/MESE
019600970723     C*--------------------
019700970724IF  1C     $ERR          IFEQ      '0'
019800970724     C                   Z-ADD     V1CAN2        ANNO
019900970724     C                   Z-ADD     V1CME2        MESE
020000990527     C                   MOVEL     DATA          G08INV
020100990527     C                   MOVE      01            G08INV
020200990527     C                   MOVEL     '3'           G08ERR
020300990527     C                   CALL      'XSRDA8'
020400990527     C                   PARM                    WLBDA8
020500990527IF  2C     G08ERR        IFEQ      '1'
020600970723     C                   MOVEL     '1'           $ERR
020700970723     C                   MOVEL     MSG(7)        $MSG
020800970723     C                   EXSR      CARMSG
020900970724     C                   SETON                                        6061
021000970723E   2C                   ENDIF
021100970723E   1C                   ENDIF
021200970724     C*
021300970724     C* ANNO/MESE SUPERIORE DATA ULTIMO TRASFERIMENTO, ATTENZIONE
021400970723IF  1C     $ERR          IFEQ      '0'
021500020701IF  2C     G08INV        IFGT      §5SDCC
021600970723     C                   MOVEL     MSG(8)        $MSG
021700970723     C                   EXSR      CARMSG
021800970723E   2C                   ENDIF
021900970723E   1C                   ENDIF
022000970723     C*
022100970722     C                   ENDSR
022200970724     C*------------------------------------------------------------------------*
022300970724     C* CARTAB - CARICA LE TABELLE OCCORRENTI
022400970724     C*------------------------------------------------------------------------*
022500970724     C     CARTAB        BEGSR
022600970724     C*--------------------
022700020701     C* DATI STATISTICHE
022800970724     C*--------------------
022900020701     C                   CLEAR                   DS5S
023000020920     C                   Z-ADD     1             KTBKUT
023100020701     C                   MOVEL     '5S'          KTBCOD
023200970724     C     KEYTAB        CHAIN     TABEL00F                           99
023300970724IF  1C     *IN99         IFEQ      *OFF
023400020701     C                   MOVEL     TBLUNI        DS5S
023500970724E   1C                   ENDIF
023600970724     C*
023700970724     C                   ENDSR
023800020920     C*--------------------------------------------------------------------------------------------*
023900020920     C* REPERISCE I DATI UTENTE
024000020920     C*--------------------------------------------------------------------------------------------*
024100020920     C     REPDATIUTE    BEGSR
024200020920     C*
024300020920     C* INIZIALIZZA VARIABILI DI WRK
024400020920     C                   CLEAR                   TIBS34DS
024500020920     C                   CLEAR                   AZUTEDS
024600020920     C                   CLEAR                   DDATIUTE
024700020920     C*
024800020920     C     *DTAARA       DEFINE    §azute        azuteds
024900020920     C     *DTAARA       DEFINE    §datiute      ddatiute
025000020920     C                   IN(E)     *DTAARA
025100020920if  1C                   IF        %Error
025200020920     c                   EVAL      I34Tla = 'L'
025300020920     C                   CALL      'TIBS34R'
025400020920     C                   PARM                    Tibs34Ds
025500020920     C                   IN        *DTAARA
025600020920e   1C                   ENDIF
025700020920     C*
025800020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
025900020920     C                   MOVEL(P)  RSUT          DSFIRS           20
026000020920     C*
026100020920     C                   ENDSR
026200970722     C*------------------------------------------------------------------------*
026300970722     C* *INZSR - ROUTINE INIZIALE
026400970722     C*------------------------------------------------------------------------*
026500970722     C     *INZSR        BEGSR
026600970722     C*--------------------
026700970722     C* RICEVIMENTO PARAMETRI
026800970722     C*--------------------
026900970722     C     *ENTRY        PLIST
027000970722     C                   PARM                    KPJBA
027100970724     C*--------------------
027200970724     C* VARIABILI RIFERITE AL DATABASE
027300970724     C*--------------------
027400970724     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
027500970724     C     *LIKE         DEFINE    TBLCOD        KTBCOD
027600970724     C*--------------------
027700970724     C* CHAIVI DI LETTURA
027800970724     C*--------------------
027900970724     C* LETTURA TABEL00F - COMPLETA
028000970724     C     KEYTAB        KLIST
028100970724     C                   KFLD                    KTBKUT                         *CODICE UTENTE
028200970724     C                   KFLD                    KTBCOD                         *CODICE TABELLA
028300970722     C*
028400970722     C* ESEGUE OPERAZIONI INIZIALI STANDARD
028500970722     C                   EXSR      INZSTD
028600970724     C*
028700970724     C* CARICA LE TABELLE OCCORRENTI
028800970724     C                   EXSR      CARTAB
028900970722     C*
029000970723     C* IMPOSTAZIONI INIZIALI VIDEO
029100970722     C                   EXSR      IMPVID
029200970722     C*
029300970722     C* EMETTE PRIMO FORMATO VIDEO
029400970722     C                   EXSR      CARV1
029500970722     C*
029600970722     C                   ENDSR
029700970723     C*------------------------------------------------------------------------*
029800970723     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
029900970723     C*------------------------------------------------------------------------*
030000970723     C     IMPVID        BEGSR
030100970723     C*---
030200970724     C* DATA FINALE  -> DATA ULTIMA DI TASFERIMENTO
030300970723     C*---
030400020701     C                   MOVEL     §5SDCC        DATA
030500970724     C                   Z-ADD     ANNO          V1CAN2                         *ANNO FINALE
030600970724     C                   Z-ADD     MESE          V1CME2                         *MESE FINALE
030700970723     C*
030800970723     C                   ENDSR
030900970722     C*------------------------------------------------------------------------*
031000970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031100970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031200970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031300970722     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
031400970722     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
031500970722     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
031600970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031700970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031800970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031900970722     C*------------------------------------------------------------------------*
032000970722     C* INZSTD - ROUTINE INIZIALE STANDARD
032100970722     C*------------------------------------------------------------------------*
032200970722     C     INZSTD        BEGSR
032300970722     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
032400970722     C                   CALL      'XKEYS'
032500970722     C                   PARM                    DSKEYS
032600970722     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
032700970722     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
032800970722     C     XMSGPA        PLIST
032900970722     C                   PARM                    DSMSMS
033000970722     C                   PARM                    DSMSPG
033100970722     C                   PARM                    DSMSKE
033200970722     C                   PARM                    DSMSER
033300970722     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
033400970723     C                   MOVEL     'TNSS43R'     DSMSPG                         *NOME PGM CHIAMANTE
033500970723     C                   MOVEL     'TNSS43R'     VMCQUE                         *CODA MESSAGGI DESTI
033600970722     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
033700970722     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
033800970722     C* INIZIALIZZO FLAG CONTROLLO PGM
033900970722     C                   MOVEL     '0'           $ERR              1            *ERRORE
034000970722     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
034100970722     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
034200970722     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
034300970722     C                   MOVEL     '0'           $FINE             1            *FINE PGM
034400970722     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
034500970722     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
034600970722     C* LANCIO ROUTINE PER VERIFICARE SE
034700970722     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
034800970722     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
034900020920     C                   EXSR      REPDATIUTE
035000970722     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
035100970722     C                   BITOFF    '01234567'    POSMI1            1
035200970722     C                   BITOFF    '01234567'    POSMI2            1
035300970722     C                   BITOFF    '01234567'    POSMA1            1
035400970722     C                   BITOFF    '01234567'    POSMA2            1
035500970722     C                   BITON     '34'          POSMI1            1            *18
035600970722     C                   BITON     '7'           POSMI2            1            *01
035700970722     C                   BITON     '34'          POSMA1            1            *18
035800970722     C                   BITON     '0'           POSMA2            1            *80
035900970722     C                   MOVE      POSMI2        POSMIN            2
036000970722     C                   MOVEL     POSMI1        POSMIN
036100970722     C                   MOVE      POSMA2        POSMAX            2
036200970722     C                   MOVEL     POSMA1        POSMAX
036300970722     C                   ENDSR
036400970722     C*------------------------------------------------------------------------*
036500970722     C* FUNSTD - LANCIA SUBROUTINE STANDARD
036600970722     C*------------------------------------------------------------------------*
036700970722     C     FUNSTD        BEGSR
036800970722     C                   SELECT
036900970722     C     KEY           WHENEQ    F3
037000970722     C                   EXSR      F03STD
037100970722     C     KEY           WHENEQ    F12
037200970722     C                   EXSR      F12STD
037300970722     C     KEY           WHENEQ    F24
037400970722     C                   EXSR      F24STD
037500970722     C                   OTHER
037600970722     C                   MOVEL     MSG(2)        $MSG
037700970722     C                   EXSR      CARMSG
037800970722     C                   ENDSL
037900970722     C                   ENDSR
038000970722     C*------------------------------------------------------------------------*
038100970722     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
038200970722     C*------------------------------------------------------------------------*
038300970722     C     F03STD        BEGSR
038400970722     C*
038500970722     C* IMPOSTO FLAG DI FINE PROGRAMMA
038600970722     C                   MOVEL     '1'           $FINE
038700970722     C*
038800970722     C                   ENDSR
038900970722     C*------------------------------------------------------------------------*
039000970722     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
039100970722     C*------------------------------------------------------------------------*
039200970722     C     F12STD        BEGSR
039300970722     C*
039400970722     C* IMPOSTO FLAG DI FINE PROGRAMMA
039500970722     C                   MOVEL     '1'           $FINE
039600970722     C*
039700970722     C                   ENDSR
039800970722     C*------------------------------------------------------------------------*
039900970722     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
040000970722     C*------------------------------------------------------------------------*
040100970722     C     F24STD        BEGSR
040200970722     C*
040300970722     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
040400970722     C                   EXSR      OUTFUN
040500970722     C*
040600970722     C                   ENDSR
040700970722     C*------------------------------------------------------------------------*
040800970722     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
040900970722     C*          CODA MESSAGGI DEL PGM
041000970722     C*------------------------------------------------------------------------*
041100970722     C     CARMSG        BEGSR
041200970722     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
041300970722     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
041400970722     C     MSGULT        IFEQ      0
041500970722     C                   MOVEL     '1'           *IN40
041600970723     C                   WRITE     SS43DVK
041700970722     C                   MOVEL     '0'           *IN40
041800970722     C                   ENDIF
041900970722     C* INVIA MESSAGGIO ALLA CODA DEL PGM
042000970722     C                   MOVEL     $MSG          DSMSMS
042100970722     C                   CALL      'XMSG'        XMSGPA
042200970722     C     DSMSER        IFEQ      0
042300970722     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
042400970722     C                   ENDIF
042500970722     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
042600970722     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
042700970723     C                   WRITE     SS43DVM                                      *SFL MESS
042800970722     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
042900970722     C                   MOVEL     *BLANKS       $MSG
043000970722     C                   ENDSR
043100970722     C*------------------------------------------------------------------------*
043200970722     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
043300970722     C*------------------------------------------------------------------------*
043400970722     C     OUTMSG        BEGSR
043500970722     C* VISUALIZZAZIONE MESSAGGIO
043600970722     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
043700970723     C                   WRITE     SS43DVK                                      *CONTR MESS
043800970722     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
043900970722     C                   Z-ADD     *ZEROS        MSGULT
044000970722     C                   ENDSR
044100970722     C*------------------------------------------------------------------------*
044200970722     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
044300970722     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
044400970722     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
044500970722     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
044600970722     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
044700970722     C*------------------------------------------------------------------------*
044800970722     C     CARFUN        BEGSR
044900970722     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
045000970722     C     1             DO        31            I                 5 0          --- 1 -->
045100970722     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
045200970722     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
045300970722     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
045400020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
045500970722     C     $TGES         IFEQ      'G'                                          *GESTIONE
045600970722     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
045700970722     C                   ELSE                                                   *INTERROG.
045800970722     C                   MOVEL     FUNASI        FAT(I)
045900970722     C                   ENDIF
046000970722     C                   ELSE                                                   <-- 2 --> *FILIALE
046100970722     C     $TGES         IFEQ      'G'                                          *GESTIONE
046200970722     C                   MOVEL     FUNAFG        FAT(I)
046300970722     C                   ELSE                                                   *INTERROG.
046400970722     C                   MOVEL     FUNAFI        FAT(I)
046500970722     C                   ENDIF
046600970722     C                   ENDIF                                                  <-- 2 ---
046700970722     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
046800970722     C                   MOVEL     USER          WEDP              3            *PROFILO
046900970722     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
047000970722     C                   MOVEL     'S'           FAT(I)                         *ABILITO
047100970722     C                   ELSE
047200970722     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
047300970722     C                   ENDIF
047400970722     C                   ENDIF
047500970722     C                   ENDDO                                                  <-- 1 ---
047600970722     C                   ENDSR
047700970722     C*------------------------------------------------------------------------*
047800970722     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
047900970722     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
048000970722     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
048100970722     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
048200970722     C*------------------------------------------------------------------------*
048300970722     C     OUTFUN        BEGSR
048400970722     C     1             DO        8             I                              *PULIZIA FUNZIONI A
048500970722     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
048600970722     C                   ENDDO
048700970722     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
048800970722     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
048900970722     C     1             DO        31            I
049000970722     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
049100970722     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
049200970722     C                   ADD       1             FUNNAT
049300970722     C                   ENDIF
049400970722     C                   ENDDO
049500970722     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
049600970722     C     FUNNAT        IFGT      8
049700970722     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
049800970722     C                   ENDIF
049900970722     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
050000970722     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
050100970722     C                   Z-ADD     8             FUNVIS            5 0
050200970722     C     FAT(23)       IFEQ      'S'
050300970722     C                   SUB       1             FUNVIS
050400970722     C                   ENDIF
050500970722     C     FAT(24)       IFEQ      'S'
050600970722     C                   SUB       1             FUNVIS
050700970722     C                   ENDIF
050800970722     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
050900970722     C                   Z-ADD     *ZEROS        W                 5 0
051000970722     C                   ADD       1             FUNULT
051100970722     C     FUNULT        IFGT      31
051200970722     C                   Z-ADD     1             FUNULT
051300970722     C                   ENDIF
051400970722     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
051500970722     C     FUNULT        DO        31            I                              --- 1 -->
051600970722     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
051700970722     C     W             IFLT      FUNVIS                                       --- 2 -->
051800970722     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
051900970722     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
052000970722     C     FDE(I)        ANDNE     *BLANKS
052100970722     C     I             IFEQ      23                                           *TOLGO F23
052200970722     C     I             OREQ      24                                           *E F24
052300970722     C                   ELSE
052400970722     C                   ADD       1             W
052500970722     C                   MOVEL     FDE(I)        FUV(W)
052600970722     C                   ENDIF
052700970722     C                   ENDIF                                                  <-- 3 ---
052800970722     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
052900970722     C                   Z-ADD     I             FUNULT
053000970722     C                   ENDIF                                                  <-- 2 ---
053100970722     C                   ENDDO                                                  <-- 1 ---
053200970722     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
053300970722     C                   MOVEL     FUV(1)        VFD001
053400970722     C                   MOVEL     FUV(2)        VFD002
053500970722     C                   MOVEL     FUV(3)        VFD003
053600970722     C                   MOVEL     FUV(4)        VFD004
053700970722     C                   MOVEL     FUV(5)        VFD005
053800970722     C                   MOVEL     FUV(6)        VFD006
053900970722     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
054000970722     C     FAT(24)       ANDEQ     'N'
054100970722     C                   MOVEL     FUV(7)        VFD007
054200970722     C                   MOVEL     FUV(8)        VFD008
054300970722     C                   ELSE                                                   <-- 1 -->
054400970722     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
054500970722     C     FAT(24)       ANDEQ     'S'
054600970722     C                   MOVEL     FDE(23)       VFD007
054700970722     C                   MOVEL     FDE(24)       VFD008
054800970722     C                   ELSE                                                   <-- 2 -->
054900970722     C                   MOVEL     FUV(7)        VFD007
055000970722     C     FAT(23)       IFEQ      'S'
055100970722     C                   MOVEL     FDE(23)       VFD008
055200970722     C                   ELSE
055300970722     C                   MOVEL     FDE(24)       VFD008
055400970722     C                   ENDIF
055500970722     C                   ENDIF                                                  <-- 2 ---
055600970722     C                   ENDIF                                                  <-- 1 ---
055700970722     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
055800970722     C                   ENDSR
055900970722     C*------------------------------------------------------------------------*
056000970722     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
056100970722     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
056200970722     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
056300970722     C*                   - ERRORE SE FUNZIONE NON ATTIVA
056400970722     C*------------------------------------------------------------------------*
056500970722     C     CONFUN        BEGSR
056600970722     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
056700970722     C     1             DO        31            I                              --- 1 -->
056800970722     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
056900970722     C* CONTROLLO SE IL TASTO E' ATTIVO
057000970722     C     FAT(I)        IFNE      'S'                                          --- 3 -->
057100970722     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
057200970722     C                   EXSR      CARMSG
057300970722     C                   MOVEL     '1'           $ERR
057400970722     C                   ENDIF                                                  <-- 3 ---
057500970722     C                   ENDIF                                                  <-- 2 ---
057600970722     C                   ENDDO                                                  <-- 1 ---
057700970722     C                   ENDSR
057800970722     C*------------------------------------------------------------------------*
057900970722**   SCHIERA: MSG  (MESSAGGI)
058000970723TNSS43R- Tasto funzionale non valido.                                          1
058100970723TNSS43R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
058200970723TNSS43R- Opzione non valida.                                                   3
058300970723TNSS43R- Opzione abilitata ma non gestita. Avvertire CED.                      4
058400970723TNSS43R- Inizio elenco.                                                        5
058500970723TNSS43R- Fine elenco.                                                          6
058600970723TNSS43R- Anno/Mese errato                                                      7
058700970724TNSS43R- ATTENZIONE. La data è superiore a quella dell'ultimo trasferimento    8
058800970723TNSS43R- 9
058900980403TNSS43R- Non si può lanciare questo programma in SDI !!!                      10
059000970722** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
059100970722F1    -                   -    NN     NN
059200970722F2    -                   -    NN     NN
059300970722F3    -F3=Fine            -    SS     SS
059400970722F4    -                   -    NN     NN
059500970722F5    -                   -    NN     NN
059600970724F6    -F6=Conferma        -    SS     SS
059700970722F7    -                   -    NN     NN
059800970722F8    -                   -    NN     NN
059900970722F9    -                   -    NN     NN
060000970722F10   -                   -    NN     NN
060100970722F11   -                   -    NN     NN
060200970722F12   -F12=Ritorno        -    SS     SS
060300970722F13   -                   -    NN     NN
060400970722F14   -                   -    NN     NN
060500970722F15   -                   -    NN     NN
060600970722F16   -                   -    NN     NN
060700970722F17   -                   -    NN     NN
060800970722F18   -                   -    NN     NN
060900970722F19   -                   -    NN     NN
061000970722F20   -                   -    NN     NN
061100970722F21   -                   -    NN     NN
061200970722F22   -                   -    NN     NN
061300970722F23   -                   -    NN     NN
061400970722F24   -F24=Altri tasti    -    NN     NN
061500970722ENTER -                   -    SS     SS
061600970722ROLLUP-                   -    NN     NN
061700970722ROLLDN-                   -    NN     NN
061800970722HELP  -                   -    NN     NN
061900970722PRINT -                   -    NN     NN
062000970722BSPACE-                   -    NN     NN
062100970722CLEAR -                   -    NN     NN
