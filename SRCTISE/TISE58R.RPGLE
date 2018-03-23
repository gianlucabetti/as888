000100950717     H*------------------------------------------------------------------------*
000200950717     H*         - STATISTICA CLIENTE - RICHIESTA TIPO DI CLIENTE               *
000300950717     H*------------------------------------------------------------------------*
000400950717     H DECEDIT('0,') DATEDIT(*DMY.)
000500950717     F*------------------------------------------------------------------------*
000600950717     F* DATA BASE
000700950717     F*------------------------------------------------------------------------*
000800010920     FTISE58D   CF   E             WORKSTN INFDS(DEVDS1)
000900010920     F                                     SFILE(SE58DVM:MSGULT)
001000950717     D*------------------------------------------------------------------------*
001100950717     D* SCHIERE
001200950717     D*------------------------------------------------------------------------*
001300950717     D*-------------------
001400950717     D* MESSAGGI E FUNZIONI
001500950717     D*-------------------
001600950717     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700950717     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800950717     D*-------------------
001900950717     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000950717     D*-------------------
002100950717     D FFU             S              6    DIM(31)                              *FUNZIONE
002200950717     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300950717     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400950717     D*-------------------
002500950717     D* TABELLA DESCRIZIONI A VIDEO
002600950717     D*-------------------
002700950717     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800950717     D*-------------------
002900950717     D* TABELLA TASTI FUNZIONALI PREMUTI
003000950717     D*-------------------
003100950717     D KEYS            S              1    DIM(40)
003200950717     D*------------------------------------------------------------------------*
003300950717     D* INPUT
003400950717     D*------------------------------------------------------------------------*
003500950717     D*-------------
003600950717     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700950717     D*-------------
003800950717     D DSFUN         E DS
003900950717     D  FUNRIG                 1     40
004000950717     D*-------------
004100950717     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200950717     D*-------------
004300950717     D DSMSG         E DS
004400950717     D*-------------
004500950717     D* DS PER GESTIONE TASTI FUNZIONALI
004600950717     D*-------------
004700950717     D DSKEYS        E DS
004800950717     D  KEYRIG                 1     40
004900950717     D*-------------
005000950717     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100950717     D*-------------
005200950717     D                SDS
005300950717     D  USER                 254    263
005400950717     D*-------------
005500950717     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600950717     D*-------------
005700950717     D DEVDS1          DS
005800950717     D  KEY                  369    369
005900950717     D  CURSOR               370    371
006000950717     D*-------------
006100950717     D* ARCHITETTURA
006200950717     D*-------------
006300950717     D KPJBA         E DS
006400950717     D*-------------
006500950717     D* DS DI PROCEDURA
006600950717     D*-------------
006700010920     D DS60          E DS                  EXTNAME(TISE60DS)
006800020916     D TIBS34DS      E DS                                                       *Profili utente
006900020916     D DDATIUTE      E DS                                                       *Dati utente
007000020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007100950717     C*------------------------------------------------------------------------*
007200950717     C* MAIN LINE
007300950717     C*------------------------------------------------------------------------*
007400950717     C*
007500950717     C* CICLO FINO A CHE:
007600950717DO  1C     $FINE         DOUEQ     '1'
007700950717     C*
007800950717     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
007900950717     C*
008000950717     C* CONTROLLA GESTIONE VIDEO
008100950717IF  2C     $GES          IFEQ      'V1'
008200950717     C                   EXSR      GESV1                                        *PARAMETRI
008300950717E   2C                   ENDIF
008400950717     C*
008500950717E   1C                   ENDDO
008600950717     C*
008700950717     C                   SETON                                        LR
008800950717     C*------------------------------------------------------------------------*
008900950717     C* GESV1 - GESTIONE VIDEO UNO
009000950717     C*------------------------------------------------------------------------*
009100950717     C     GESV1         BEGSR
009200950717     C*
009300950717IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
009400950717     C                   EXSR      CARV1                                        *CARICA VIDEO
009500950717E   1C                   ENDIF
009600950717     C*
009700950717IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
009800950717     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
009900950717     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010000950717E   1C                   ENDIF
010100950717     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010200950717     C*
010300010920     C                   EXFMT     SE58DV1                                      *PARAMETRI
010400950717     C*
010500950717     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
010600950717     C*
010700950717IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
010800950717SL  2C                   SELECT
010900950717     C     KEY           WHENEQ    ENTER
011000950717     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011100950717     C                   OTHER
011200950717     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
011300950717E   2C                   ENDSL
011400950717E   1C                   ENDIF
011500950717     C*
011600950717     C                   ENDSR
011700950717     C*------------------------------------------------------------------------*
011800950717     C* CARV1 - CARICA VIDEO UNO
011900950717     C*------------------------------------------------------------------------*
012000950717     C     CARV1         BEGSR
012100950717     C*
012200950717     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
012300950717     C*
012400950717     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
012500950717     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
012600950717     C*
012700010920     C                   WRITE     SE58DVT                                      *TESTATA
012800010920     C                   WRITE     SE58DVF                                      *FUNZIONI
012900950717     C*
013000950717     C                   ENDSR
013100950717     C*------------------------------------------------------------------------*
013200950717     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
013300950717     C*------------------------------------------------------------------------*
013400950717     C     ENTV1         BEGSR
013500950717     C*
013600950717     C* CONTROLLA I CAMPI IMMESSI
013700950717     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
013800950717     C*
013900950717     C* NON CI SONO ERRORI
014000950717IF  1C     $ERR          IFEQ      '0'
014100950717     C                   CLEAR                   DS60                           *PULISCE DS ESTERNA
014200000816     c                   MOVEL     V1CSCE        D60SCE
014300950717     C***
014400000803     C* UN CLIENTE -SINGOLO O UNIFICANTE-
014500950717     C***
014600950717IF  2C     V1CSCE        IFEQ      '1'
014700000803     C     V1CSCE        OREQ      '3'
014800950724     C                   MOVEL     DS60          KPJBU
014900010920     C                   CALL      'TISE60R'
015000950724     C                   PARM                    KPJBA
015100950717X   2C                   ELSE
015200950717     C***
015300950717     C* PIU' CLIENTI
015400950717     C***
015500950724     C                   MOVEL     DS60          KPJBU
015600020117     C                   CALL      'TISE59R'
015700950717     C                   PARM                    KPJBA
015800950717E   2C                   ENDIF
015900950721     C*
016000950724     C                   MOVEL     KPJBU         DS60
016100950724     C*
016200950717     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
016300950717IF  2C     D60F03        IFEQ      '1'
016400950717     C                   MOVEL     '1'           $F03
016500950717     C                   MOVEL     '1'           $FINE
016600950717X   2C                   ELSE
016700950717     C*
016800950717     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
016900950724     C                   MOVEL(P)  D60MSG        $MSG
017000950717     C                   EXSR      CARMSG
017100950717     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
017200950717E   2C                   ENDIF
017300950717E   1C                   ENDIF
017400950717     C*
017500950717     C                   ENDSR
017600950717     C*------------------------------------------------------------------------*
017700950717     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017800950717     C*------------------------------------------------------------------------*
017900950717     C     DECODI        BEGSR
018000950717     C*
018100950717     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
018200950717     C                   SETOFF                                       60
018300950717DO  1C                   DO
018400950717     C*--------------------
018500950717     C* SCELTA
018600950717     C*--------------------
018700950717IF  2C     V1CSCE        IFEQ      '1'
018800950717     C     V1CSCE        OREQ      '2'
018900000803     C     V1CSCE        OREQ      '3'
019000950717X   2C                   ELSE
019100950717     C                   MOVEL     '1'           $ERR
019200950717     C                   MOVEL     MSG(8)        $MSG
019300950717     C                   EXSR      CARMSG
019400950717     C                   SETON                                        60
019500950717     C                   LEAVE
019600950717E   2C                   ENDIF
019700950717     C*
019800950717E   1C                   ENDDO
019900950717     C*
020000950717     C                   ENDSR
020100950717     C*------------------------------------------------------------------------*
020200950717     C* *INZSR  - ROUTINE INIZIALE
020300950717     C*------------------------------------------------------------------------*
020400950717     C     *INZSR        BEGSR
020500950717     C*
020600950717     C* RICEVIMENTO PARAMETRI
020700950717     C     *ENTRY        PLIST
020800950717     C                   PARM                    KPJBA
020900950717     C*
021000950717     C* ESEGUE OPERAZIONI INIZIALI STANDARD
021100950717     C                   EXSR      INZSTD
021200020916     C*
021300020916     C* REPERISCE I DATI UTENTE
021400020916     C                   EXSR      REPDATIUTE
021500020916     C*
021600020916     C* ASSEGNA LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO VIDEO
021700020916     C                   MOVEL(P)  RSUT          DSFIRS
021800950717     C*
021900950717     C* IMPOSTAZIONE INIZIALE VIDEO
022000950717     C                   EXSR      IMPVID
022100950717     C*
022200950717     C* EMETTO FORMATO VIDEO
022300950717     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
022400950717     C*
022500950717     C                   ENDSR
022600020916     C*--------------------------------------------------------------------------------------------*
022700020916     C* REPERISCE DATI UTENTE
022800020916     C*--------------------------------------------------------------------------------------------*
022900020916     C     REPDATIUTE    BEGSR
023000020916     C*
023100020916     C* REimposta le variabili di lavoro
023200020916     C                   CLEAR                   TIBS34DS
023300020916     C                   CLEAR                   AZUTEDS
023400020916     C                   CLEAR                   DDATIUTE
023500020916     C*
023600020916     C* Reperisce i dati del profilo in ingresso
023700020916     C     *DTAARA       DEFINE    §azute        azuteds
023800020916     C     *DTAARA       DEFINE    §datiute      ddatiute
023900020916     C                   IN(E)     *DTAARA
024000020916if  1C                   IF        %Error                                       *NO errori
024100020916     c                   EVAL      I34Tla = 'L'
024200020916     C                   CALL      'TIBS34R'
024300020916     C                   PARM                    Tibs34Ds
024400020916     C                   IN        *DTAARA
024500020916e   1C                   ENDIF
024600020916     C*
024700020916     C                   ENDSR
024800950717     C*------------------------------------------------------------------------*
024900950717     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
025000950717     C*------------------------------------------------------------------------*
025100950717     C     IMPVID        BEGSR
025200950717     C*--------------------
025300950717     C* SCELTA
025400950717     C*--------------------
025500950720     C                   MOVEL     '1'           V1CSCE
025600950717     C*
025700950717     C                   ENDSR
025800950717     C*------------------------------------------------------------------------*
025900950717     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026000950717     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026100950717     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
026200950717     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
026300950717     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
026400950717     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026500950717     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026600950717     C*------------------------------------------------------------------------*
026700950717     C* INZSTD - ROUTINE INIZIALE STANDARD
026800950717     C*------------------------------------------------------------------------*
026900950717     C     INZSTD        BEGSR
027000950717     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
027100950717     C                   CALL      'XKEYS'
027200950717     C                   PARM                    DSKEYS
027300950717     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
027400950717     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
027500950717     C     XMSGPA        PLIST
027600950717     C                   PARM                    DSMSMS
027700950717     C                   PARM                    DSMSPG
027800950717     C                   PARM                    DSMSKE
027900950717     C                   PARM                    DSMSER
028000950717     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
028100010920     C                   MOVEL     'TISE58R'     DSMSPG                         *NOME PGM CHIAMANTE
028200010920     C                   MOVEL     'TISE58R'     VMCQUE                         *CODA MESSAGGI DESTI
028300950717     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
028400950717     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
028500950717     C* INIZIALIZZO FLAG CONTROLLO PGM
028600950717     C                   MOVEL     '0'           $ERR              1            *ERRORE
028700950717     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
028800950717     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
028900950717     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
029000950717     C                   MOVEL     '0'           $FINE             1            *FINE PGM
029100950717     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
029200950717     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
029300950717     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
029400950717     C                   BITOFF    '01234567'    POSMI1            1
029500950717     C                   BITOFF    '01234567'    POSMI2            1
029600950717     C                   BITOFF    '01234567'    POSMA1            1
029700950717     C                   BITOFF    '01234567'    POSMA2            1
029800950717     C                   BITON     '34'          POSMI1            1            *18
029900950717     C                   BITON     '7'           POSMI2            1            *01
030000950717     C                   BITON     '34'          POSMA1            1            *18
030100950717     C                   BITON     '0'           POSMA2            1            *80
030200950717     C                   MOVE      POSMI2        POSMIN            2
030300950717     C                   MOVEL     POSMI1        POSMIN
030400950717     C                   MOVE      POSMA2        POSMAX            2
030500950717     C                   MOVEL     POSMA1        POSMAX
030600950717     C                   ENDSR
030700950717     C*------------------------------------------------------------------------*
030800950717     C* FUNSTD - LANCIA SUBROUTINE STANDARD
030900950717     C*------------------------------------------------------------------------*
031000950717     C     FUNSTD        BEGSR
031100950717     C                   SELECT
031200950717     C     KEY           WHENEQ    F3
031300950717     C                   EXSR      F03STD
031400950717     C     KEY           WHENEQ    F12
031500950717     C                   EXSR      F12STD
031600950717     C     KEY           WHENEQ    F24
031700950717     C                   EXSR      F24STD
031800950717     C                   OTHER
031900950717     C                   MOVEL     MSG(2)        $MSG
032000950717     C                   EXSR      CARMSG
032100950717     C                   ENDSL
032200950717     C                   ENDSR
032300950717     C*------------------------------------------------------------------------*
032400950717     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
032500950717     C*------------------------------------------------------------------------*
032600950717     C     F03STD        BEGSR
032700950721     C*
032800950721     C* IMPOSTO FLAG F03 USCITA PGM
032900950721     C                   MOVEL     '1'           $F03
033000950717     C*
033100950717     C* IMPOSTO FLAG DI FINE PROGRAMMA
033200950717     C                   MOVEL     '1'           $FINE
033300950717     C*
033400950717     C                   ENDSR
033500950717     C*------------------------------------------------------------------------*
033600950717     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
033700950717     C*------------------------------------------------------------------------*
033800950717     C     F12STD        BEGSR
033900950717     C*
034000950717     C* IMPOSTO FLAG DI FINE PROGRAMMA
034100950717     C                   MOVEL     '1'           $FINE
034200950717     C*
034300950717     C                   ENDSR
034400950717     C*------------------------------------------------------------------------*
034500950717     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
034600950717     C*------------------------------------------------------------------------*
034700950717     C     F24STD        BEGSR
034800950717     C*
034900950717     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
035000950717     C                   EXSR      OUTFUN
035100950717     C*
035200950717     C                   ENDSR
035300950717     C*------------------------------------------------------------------------*
035400950717     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
035500950717     C*          CODA MESSAGGI DEL PGM
035600950717     C*------------------------------------------------------------------------*
035700950717     C     CARMSG        BEGSR
035800950717     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
035900950717     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
036000950717     C     MSGULT        IFEQ      0
036100950717     C                   MOVEL     '1'           *IN40
036200010920     C                   WRITE     SE58DVK
036300950717     C                   MOVEL     '0'           *IN40
036400950717     C                   ENDIF
036500950717     C* INVIA MESSAGGIO ALLA CODA DEL PGM
036600950717     C                   MOVEL     $MSG          DSMSMS
036700950717     C                   CALL      'XMSG'        XMSGPA
036800950717     C     DSMSER        IFEQ      0
036900950717     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
037000950717     C                   ENDIF
037100950717     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
037200950717     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
037300010920     C                   WRITE     SE58DVM                                      *SFL MESS
037400950717     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
037500950717     C                   MOVEL     *BLANKS       $MSG
037600950717     C                   ENDSR
037700950717     C*------------------------------------------------------------------------*
037800950717     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
037900950717     C*------------------------------------------------------------------------*
038000950717     C     OUTMSG        BEGSR
038100950717     C* VISUALIZZAZIONE MESSAGGIO
038200950717     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
038300010920     C                   WRITE     SE58DVK                                      *CONTR MESS
038400950717     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
038500950717     C                   Z-ADD     *ZEROS        MSGULT
038600950717     C                   ENDSR
038700950717     C*------------------------------------------------------------------------*
038800950717     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
038900950717     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
039000950717     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
039100950717     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
039200950717     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
039300950717     C*------------------------------------------------------------------------*
039400950717     C     CARFUN        BEGSR
039500950717     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
039600950717     C     1             DO        31            I                 5 0          --- 1 -->
039700950717     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
039800950717     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
039900950717     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
040000020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
040100950717     C     $TGES         IFEQ      'G'                                          *GESTIONE
040200950717     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
040300950717     C                   ELSE                                                   *INTERROG.
040400950717     C                   MOVEL     FUNASI        FAT(I)
040500950717     C                   ENDIF
040600950717     C                   ELSE                                                   <-- 2 --> *FILIALE
040700950717     C     $TGES         IFEQ      'G'                                          *GESTIONE
040800950717     C                   MOVEL     FUNAFG        FAT(I)
040900950717     C                   ELSE                                                   *INTERROG.
041000950717     C                   MOVEL     FUNAFI        FAT(I)
041100950717     C                   ENDIF
041200950717     C                   ENDIF                                                  <-- 2 ---
041300950717     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
041400950717     C                   MOVEL     USER          WEDP              3            *PROFILO
041500950717     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
041600950717     C                   MOVEL     'S'           FAT(I)                         *ABILITO
041700950717     C                   ELSE
041800950717     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
041900950717     C                   ENDIF
042000950717     C                   ENDIF
042100950717     C                   ENDDO                                                  <-- 1 ---
042200950717     C                   ENDSR
042300950717     C*------------------------------------------------------------------------*
042400950717     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
042500950717     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
042600950717     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
042700950717     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
042800950717     C*------------------------------------------------------------------------*
042900950717     C     OUTFUN        BEGSR
043000950717     C     1             DO        8             I                              *PULIZIA FUNZIONI A
043100950717     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
043200950717     C                   ENDDO
043300950717     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
043400950717     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
043500950717     C     1             DO        31            I
043600950717     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
043700950717     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
043800950717     C                   ADD       1             FUNNAT
043900950717     C                   ENDIF
044000950717     C                   ENDDO
044100950717     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
044200950717     C     FUNNAT        IFGT      8
044300950717     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
044400950717     C                   ENDIF
044500950717     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
044600950717     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
044700950717     C                   Z-ADD     8             FUNVIS            5 0
044800950717     C     FAT(23)       IFEQ      'S'
044900950717     C                   SUB       1             FUNVIS
045000950717     C                   ENDIF
045100950717     C     FAT(24)       IFEQ      'S'
045200950717     C                   SUB       1             FUNVIS
045300950717     C                   ENDIF
045400950717     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
045500950717     C                   Z-ADD     *ZEROS        W                 5 0
045600950717     C                   ADD       1             FUNULT
045700950717     C     FUNULT        IFGT      31
045800950717     C                   Z-ADD     1             FUNULT
045900950717     C                   ENDIF
046000950717     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
046100950717     C     FUNULT        DO        31            I                              --- 1 -->
046200950717     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
046300950717     C     W             IFLT      FUNVIS                                       --- 2 -->
046400950717     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
046500950717     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
046600950717     C     FDE(I)        ANDNE     *BLANKS
046700950717     C     I             IFEQ      23                                           *TOLGO F23
046800950717     C     I             OREQ      24                                           *E F24
046900950717     C                   ELSE
047000950717     C                   ADD       1             W
047100950717     C                   MOVEL     FDE(I)        FUV(W)
047200950717     C                   ENDIF
047300950717     C                   ENDIF                                                  <-- 3 ---
047400950717     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
047500950717     C                   Z-ADD     I             FUNULT
047600950717     C                   ENDIF                                                  <-- 2 ---
047700950717     C                   ENDDO                                                  <-- 1 ---
047800950717     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
047900950717     C                   MOVEL     FUV(1)        VFD001
048000950717     C                   MOVEL     FUV(2)        VFD002
048100950717     C                   MOVEL     FUV(3)        VFD003
048200950717     C                   MOVEL     FUV(4)        VFD004
048300950717     C                   MOVEL     FUV(5)        VFD005
048400950717     C                   MOVEL     FUV(6)        VFD006
048500950717     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
048600950717     C     FAT(24)       ANDEQ     'N'
048700950717     C                   MOVEL     FUV(7)        VFD007
048800950717     C                   MOVEL     FUV(8)        VFD008
048900950717     C                   ELSE                                                   <-- 1 -->
049000950717     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
049100950717     C     FAT(24)       ANDEQ     'S'
049200950717     C                   MOVEL     FDE(23)       VFD007
049300950717     C                   MOVEL     FDE(24)       VFD008
049400950717     C                   ELSE                                                   <-- 2 -->
049500950717     C                   MOVEL     FUV(7)        VFD007
049600950717     C     FAT(23)       IFEQ      'S'
049700950717     C                   MOVEL     FDE(23)       VFD008
049800950717     C                   ELSE
049900950717     C                   MOVEL     FDE(24)       VFD008
050000950717     C                   ENDIF
050100950717     C                   ENDIF                                                  <-- 2 ---
050200950717     C                   ENDIF                                                  <-- 1 ---
050300950717     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
050400950717     C                   ENDSR
050500950717     C*------------------------------------------------------------------------*
050600950717     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
050700950717     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
050800950717     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
050900950717     C*                   - ERRORE SE FUNZIONE NON ATTIVA
051000950717     C*------------------------------------------------------------------------*
051100950717     C     CONFUN        BEGSR
051200950717     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
051300950717     C     1             DO        31            I                              --- 1 -->
051400950717     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
051500950717     C* CONTROLLO SE IL TASTO E' ATTIVO
051600950717     C     FAT(I)        IFNE      'S'                                          --- 3 -->
051700950717     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
051800950717     C                   EXSR      CARMSG
051900950717     C                   MOVEL     '1'           $ERR
052000950717     C                   ENDIF                                                  <-- 3 ---
052100950717     C                   ENDIF                                                  <-- 2 ---
052200950717     C                   ENDDO                                                  <-- 1 ---
052300950717     C                   ENDSR
052400950717     O*------------------------------------------------------------------------*
052500950717** ======== SCHIERA: MSG  (MESSAGGI)                       ================
052600010920TISE58R- Tasto funzionale non valido.                                          1
052700010920TISE58R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
052800010920TISE58R- Opzione non valida.                                                   3
052900010920TISE58R- Opzione abilitata ma non gestita. Avvertire CED.                      4
053000010920TISE58R- Inizio elenco.                                                        5
053100010920TISE58R- Fine elenco.                                                          6
053200010920TISE58R- 7
053300010920TISE58R- Scelta errata                                                         8
053400010920TISE58R- 9
053500010920TISE58R- 10
053600010920TISE58R- 11
053700010920TISE58R- 12
053800010920TISE58R- 13
053900010920TISE58R- 14
054000010920TISE58R- 15
054100010920TISE58R- 16
054200010920TISE58R- 17
054300010920TISE58R- 18
054400010920TISE58R- 19
054500010920TISE58R- 20
054600950717** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
054700950717F1    -                   -    NN     NN
054800950717F2    -                   -    NN     NN
054900950717F3    -F3=Fine            -    SS     SS
055000950717F4    -                   -    NN     NN
055100950717F5    -                   -    NN     NN
055200950717F6    -                   -    NN     NN
055300950717F7    -                   -    NN     NN
055400950717F8    -                   -    NN     NN
055500950717F9    -                   -    NN     NN
055600950717F10   -                   -    NN     NN
055700950717F11   -                   -    NN     NN
055800950717F12   -F12-Ritorno        -    SS     SS
055900950717F13   -                   -    NN     NN
056000950717F14   -                   -    NN     NN
056100950717F15   -                   -    NN     NN
056200950717F16   -                   -    NN     NN
056300950717F17   -                   -    NN     NN
056400950717F18   -                   -    NN     NN
056500950717F19   -                   -    NN     NN
056600950717F20   -                   -    NN     NN
056700950717F21   -                   -    NN     NN
056800950717F22   -                   -    NN     NN
056900950717F23   -                   -    NN     NN
057000950717F24   -F24=Altri tasti    -    NN     NN
057100950717ENTER -                   -    SS     SS
057200950717ROLLUP-                   -    NN     NN
057300950717ROLLDN-                   -    NN     NN
057400950717HELP  -                   -    NN     NN
057500950717PRINT -                   -    NN     NN
057600950717BSPACE-                   -    NN     NN
057700950717CLEAR -                   -    NN     NN
