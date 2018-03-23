000100020718     H DECEDIT('0,') DATEDIT(*DMY.)
000200970722     H*------------------------------------------------------------------------*
000300971113     H* SALVATAGGIO BARTVAS/SDIVAS + INTERSCAMBIO SU CASSETTA - PARAMETRI      *
000400970722     H*------------------------------------------------------------------------*
000500970722     F*------------------------------------------------------------------------*
000600970722     F* DATA BASE
000700970722     F*------------------------------------------------------------------------*
000800971113     FTRTCD9D   CF   E             WORKSTN INFDS(DEVDS1)
000900971113     F                                     SFILE(TCD9DVM:MSGULT)
001000970722     D*------------------------------------------------------------------------*
001100970722     D* SCHIERE
001200970722     D*------------------------------------------------------------------------*
001300970722     D*-------------------
001400970722     D* SCHIERE A TEMPO DI COMPILAZIONE
001500970722     D*-------------------
001600970722     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700970722     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800970722     D*-------------------
001900970722     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000970722     D*-------------------
002100970722     D FFU             S              6    DIM(31)                              *FUNZIONE
002200970722     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300970722     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400970722     D*-------------------
002500970722     D* TABELLA DESCRIZIONI A VIDEO
002600970722     D*-------------------
002700970722     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800970722     D*-------------------
002900970722     D* TABELLA TASTI FUNZIONALI PREMUTI
003000970722     D*-------------------
003100970722     D KEYS            S              1    DIM(40)
003200970722     D*------------------------------------------------------------------------*
003300970722     D* INPUT
003400970722     D*------------------------------------------------------------------------*
003500970722     D*-------------------
003600970722     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700970722     D*-------------------
003800970722     D DSFUN         E DS
003900970722     D  FUNRIG                 1     40
004000970722     D*-------------------
004100970722     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200970722     D*-------------------
004300970722     D DSMSG         E DS
004400970722     D*-------------------
004500970722     D* DS PER GESTIONE TASTI FUNZIONALI
004600970722     D*-------------------
004700970722     D DSKEYS        E DS
004800970722     D  KEYRIG                 1     40
004900970722     D*-------------------
005000970722     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100970722     D*-------------------
005200970722     D                SDS
005300970722     D  USER                 254    263                                         UTENTE
005400970722     D*-------------------
005500970722     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600970722     D*-------------------
005700970722     D DEVDS1          DS
005800970722     D  KEY                  369    369
005900970722     D  CURSOR               370    371
006000020718
006100020718     d DdatiUte      e ds
006200020718     d AzuteDs       e ds                  ExtName(Azute00f)
006300020718     d Tibs34Ds      e ds                  Inz
006400020718
006500971113     D*------------------
006600971113     D* DS PARAMETRI LANCIO
006700971113     D*------------------
006800971113     D PARAM           DS                  INZ
006900980121     D  PARLIB                 1     10
007000980121     D  PARDEV                11     20
007100970723     D*-------------------
007200970722     D* ARCHITETTURA
007300970722     D*-------------------
007400970722     D KPJBA         E DS
007500970722     C*------------------------------------------------------------------------*
007600970722     C* MAIN LINE
007700970722     C*------------------------------------------------------------------------*
007800970722     C*
007900970722     C* CICLO FINO A CHE:
008000970722DO  1C     $FINE         DOUEQ     '1'
008100970722     C*
008200970722     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008300970722     C*
008400970722     C* CONTROLLA GESTIONE VIDEO
008500970722IF  2C     $GES          IFEQ      'V1'
008600970722     C                   EXSR      GESV1                                        *PARAMETRI
008700970722E   2C                   ENDIF
008800970722     C*
008900970722E   1C                   ENDDO
009000970722     C*
009100970722     C                   SETON                                        LR
009200970722     C*------------------------------------------------------------------------*
009300970722     C* GESV1 - GESIONE PRIMO VIDEO
009400970722     C*------------------------------------------------------------------------*
009500970722     C     GESV1         BEGSR
009600970722     C*
009700970722IF  1C     $TESVD        IFEQ      '1'                                          TESTATA DA RIEMETTER
009800970722     C                   EXSR      CARV1                                        *CARICA VIDEO
009900970722E   1C                   ENDIF
010000970722     C*
010100970722IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010200970722     C                   MOVEL     *BLANKS       $MSG
010300970722     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010400970722E   1C                   ENDIF
010500970722     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010600970722     C*
010700971113     C                   EXFMT     TCD9DV1                                      *PARAMETRI
010800970722     C*
010900970722     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011000020718
011100020718      * Se Profilo non OK devo uscire dal pgm
011200020718     c                   If        *In76
011300020718     c                   Eval      $err = '1'
011400020718     c                   Eval      $fine = '1'
011500020718     c                   EndIf
011600970722     C*
011700970722IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011800970722SL  2C                   SELECT
011900970722     C     KEY           WHENEQ    ENTER
012000970722     C                   EXSR      ENTV1                                        *ENTER--> DECODIFICA
012100970722     C     KEY           WHENEQ    F6
012200970722     C                   EXSR      F06GES                                       *F06 --> LANCIO
012300970722     C                   OTHER
012400970722     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012500970722E   2C                   ENDSL
012600970722E   1C                   ENDIF
012700970722     C*
012800970722     C                   ENDSR
012900970722     C*------------------------------------------------------------------------*
013000970722     C* CARV1 - CARICAMENTO PRIMO VIDEO
013100970722     C*------------------------------------------------------------------------*
013200970722     C     CARV1         BEGSR
013300970722     C*
013400970722     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013500970722     C*
013600970722     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013700970722     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013800970722     C*
013900971113     C                   WRITE     TCD9DVT                                      *TESTATA
014000971113     C                   WRITE     TCD9DVF                                      *FUNZIONI
014100970722     C*
014200970722     C                   ENDSR
014300970722     C*------------------------------------------------------------------------*
014400970722     C* ENTV1 - GESTIONE TASTO "ENTER" PRIMO VIDEO
014500970722     C*------------------------------------------------------------------------*
014600970722     C     ENTV1         BEGSR
014700970722     C*
014800970722     C* CONTROLLA I CAMPI IMMESSI
014900970722     C                   EXSR      DECODI
015000970722     C*
015100970722     C                   ENDSR
015200970722     C*------------------------------------------------------------------------*
015300970722     C* F06GES - GESTIONE TASTO F06 ==> CONFERMA LANCIO
015400970722     C*------------------------------------------------------------------------*
015500970722     C     F06GES        BEGSR
015600970722     C*
015700970722     C* CONTROLLA I CAMPI IMMESSI
015800970722     C                   EXSR      DECODI
015900970722     C*
016000970722     C* LANCIA PROGRAMMA DI GUIDA
016100970722IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
016200971113     C                   MOVEL     V1CLIB        PARLIB                         *LIBRERIA
016300980121     C                   MOVEL     V1CDEV        PARDEV                         *UNITA' NAST
016400980121     C                   MOVEL     'TCD8'        KCOAZ
016500970722     C                   MOVEL     PARAM         KPJBU
016600970722     C                   CALL      'BCH10'
016700970722     C                   PARM                    KPJBA
016800970722     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
016900970722E   1C                   ENDIF
017000970722     C*
017100970722     C                   ENDSR
017200970722     C*------------------------------------------------------------------------*
017300970722     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017400970722     C*------------------------------------------------------------------------*
017500970722     C     DECODI        BEGSR
017600970722     C*
017700970723     C* REIMPOSTA INDICATORI DI ERRORE SUI CAMPI
017800980121     C                   SETOFF                                       6061
017900970723     C*--------------------
018000971113     C* LIBRERIA
018100970723     C*--------------------
018200970724IF  1C     $ERR          IFEQ      '0'
018300971113IF  2C     V1CLIB        IFEQ      *BLANKS
018400970723     C                   MOVEL     '1'           $ERR
018500970723     C                   MOVEL     MSG(7)        $MSG
018600970723     C                   EXSR      CARMSG
018700971113     C                   SETON                                        60
018800970723E   2C                   ENDIF
018900980121IF  2C     V1CDEV        IFEQ      *BLANKS
019000980121     C                   MOVEL     '1'           $ERR
019100980121     C                   MOVEL     MSG(8)        $MSG
019200980121     C                   EXSR      CARMSG
019300980121     C                   SETON                                        61
019400980121E   2C                   ENDIF
019500970723E   1C                   ENDIF
019600970723     C*
019700970722     C                   ENDSR
019800970724     C*------------------------------------------------------------------------*
019900970724     C* CARTAB - CARICA LE TABELLE OCCORRENTI
020000970724     C*------------------------------------------------------------------------*
020100970724     C     CARTAB        BEGSR
020200970724     C*
020300970724     C                   ENDSR
020400970722     C*------------------------------------------------------------------------*
020500970722     C* *INZSR - ROUTINE INIZIALE
020600970722     C*------------------------------------------------------------------------*
020700970722     C     *INZSR        BEGSR
020800970722     C*--------------------
020900970722     C* RICEVIMENTO PARAMETRI
021000970722     C*--------------------
021100970722     C     *ENTRY        PLIST
021200970722     C                   PARM                    KPJBA
021300970724     C*--------------------
021400971113     C* VARIABILI RIFERITE AL DATABASE
021500970724     C*--------------------
021600970724     C*--------------------
021700971113     C* CHIAVI DI LETTURA
021800970724     C*--------------------
021900970722     C*
022000970722     C* ESEGUE OPERAZIONI INIZIALI STANDARD
022100970722     C                   EXSR      INZSTD
022200970724     C*
022300970724     C* CARICA LE TABELLE OCCORRENTI
022400970724     C                   EXSR      CARTAB
022500970722     C*
022600970723     C* IMPOSTAZIONI INIZIALI VIDEO
022700970722     C                   EXSR      IMPVID
022800970722     C*
022900970722     C* EMETTE PRIMO FORMATO VIDEO
023000970722     C                   EXSR      CARV1
023100970722     C*
023200970722     C                   ENDSR
023300970723     C*------------------------------------------------------------------------*
023400970723     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
023500970723     C*------------------------------------------------------------------------*
023600970723     C     IMPVID        BEGSR
023700971113     C*
023800980121     C                   MOVEL     'TAP01   '    V1CDEV
023900971113     C                   MOVEL     KNSIF         A3                3
024000971113IF  1C     A3            IFEQ      'GAI'
024100971113     C                   MOVEL     'BARTVAS'     V1CLIB
024200971113X   1C                   ELSE
024300971113     C                   MOVEL     'SDIVAS'      V1CLIB
024400971113E   1C                   ENDIF
024500970723     C*
024600970723     C                   ENDSR
024700970722     C*------------------------------------------------------------------------*
024800970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024900970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025000970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025100970722     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
025200970722     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
025300970722     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
025400970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025500970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025600970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025700970722     C*------------------------------------------------------------------------*
025800970722     C* INZSTD - ROUTINE INIZIALE STANDARD
025900970722     C*------------------------------------------------------------------------*
026000970722     C     INZSTD        BEGSR
026100970722     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
026200970722     C                   CALL      'XKEYS'
026300970722     C                   PARM                    DSKEYS
026400970722     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
026500970722     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
026600970722     C     XMSGPA        PLIST
026700970722     C                   PARM                    DSMSMS
026800970722     C                   PARM                    DSMSPG
026900970722     C                   PARM                    DSMSKE
027000970722     C                   PARM                    DSMSER
027100970722     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
027200971113     C                   MOVEL     'TRTCD9R'     DSMSPG                         *NOME PGM CHIAMANTE
027300971113     C                   MOVEL     'TRTCD9R'     VMCQUE                         *CODA MESSAGGI DESTI
027400970722     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
027500970722     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
027600970722     C* INIZIALIZZO FLAG CONTROLLO PGM
027700970722     C                   MOVEL     '0'           $ERR              1            *ERRORE
027800970722     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
027900970722     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
028000970722     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
028100970722     C                   MOVEL     '0'           $FINE             1            *FINE PGM
028200970722     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
028300970722     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
028400970722     C* LANCIO ROUTINE PER VERIFICARE SE
028500970722     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
028600970722     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
028700020718
028800020718     c     *dtaara       define    §azute        azuteds
028900020718     c     *dtaara       define    §datiute      ddatiute
029000020718
029100020718     c                   in(E)     *dtaara
029200020718     c                   if        %Error or rsut = *blanks
029300020718     c                   clear                   tibs34ds
029400020718     c                   call      'TIBS34R'
029500020718     c                   parm                    Tibs34Ds
029600020718     c                   in        *dtaara
029700020718     c                   endif
029800020718
029900020718      * Controllo il profilo utente
030000020718     c                   ExSr      CtrUte
030100020718
030200970722     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
030300970722     C                   BITOFF    '01234567'    POSMI1            1
030400970722     C                   BITOFF    '01234567'    POSMI2            1
030500970722     C                   BITOFF    '01234567'    POSMA1            1
030600970722     C                   BITOFF    '01234567'    POSMA2            1
030700970722     C                   BITON     '34'          POSMI1            1            *18
030800970722     C                   BITON     '7'           POSMI2            1            *01
030900970722     C                   BITON     '34'          POSMA1            1            *18
031000970722     C                   BITON     '0'           POSMA2            1            *80
031100970722     C                   MOVE      POSMI2        POSMIN            2
031200970722     C                   MOVEL     POSMI1        POSMIN
031300970722     C                   MOVE      POSMA2        POSMAX            2
031400970722     C                   MOVEL     POSMA1        POSMAX
031500970722     C                   ENDSR
031600020718      *------------------------------------------------------------------------*
031700020718      *  CTRUTE - ROUTINE CONTROLLO UTENTE
031800020718      *------------------------------------------------------------------------*
031900020718     c     CtrUte        BegSr
032000020718
032100020718     c                   Eval      *In76 = *Off
032200020718
032300020718      * Controllo se il profilo è ok da tibs34
032400020718     c                   If        DutErr = 'E'
032500020718     c                   Eval      $Msg = DutMsg
032600020718     c                   ExSr      CarMsg
032700020718     c                   Eval      *In76 = *On
032800020718     c                   EndIf
032900020718
033000020718     c                   EndSr
033100970722     C*------------------------------------------------------------------------*
033200970722     C* FUNSTD - LANCIA SUBROUTINE STANDARD
033300970722     C*------------------------------------------------------------------------*
033400970722     C     FUNSTD        BEGSR
033500970722     C                   SELECT
033600970722     C     KEY           WHENEQ    F3
033700970722     C                   EXSR      F03STD
033800970722     C     KEY           WHENEQ    F12
033900970722     C                   EXSR      F12STD
034000970722     C     KEY           WHENEQ    F24
034100970722     C                   EXSR      F24STD
034200970722     C                   OTHER
034300970722     C                   MOVEL     MSG(2)        $MSG
034400970722     C                   EXSR      CARMSG
034500970722     C                   ENDSL
034600970722     C                   ENDSR
034700970722     C*------------------------------------------------------------------------*
034800970722     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
034900970722     C*------------------------------------------------------------------------*
035000970722     C     F03STD        BEGSR
035100970722     C*
035200970722     C* IMPOSTO FLAG DI FINE PROGRAMMA
035300970722     C                   MOVEL     '1'           $FINE
035400970722     C*
035500970722     C                   ENDSR
035600970722     C*------------------------------------------------------------------------*
035700970722     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
035800970722     C*------------------------------------------------------------------------*
035900970722     C     F12STD        BEGSR
036000970722     C*
036100970722     C* IMPOSTO FLAG DI FINE PROGRAMMA
036200970722     C                   MOVEL     '1'           $FINE
036300970722     C*
036400970722     C                   ENDSR
036500970722     C*------------------------------------------------------------------------*
036600970722     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
036700970722     C*------------------------------------------------------------------------*
036800970722     C     F24STD        BEGSR
036900970722     C*
037000970722     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
037100970722     C                   EXSR      OUTFUN
037200970722     C*
037300970722     C                   ENDSR
037400970722     C*------------------------------------------------------------------------*
037500970722     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
037600970722     C*          CODA MESSAGGI DEL PGM
037700970722     C*------------------------------------------------------------------------*
037800970722     C     CARMSG        BEGSR
037900970722     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
038000970722     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
038100970722     C     MSGULT        IFEQ      0
038200970722     C                   MOVEL     '1'           *IN40
038300971113     C                   WRITE     TCD9DVK
038400970722     C                   MOVEL     '0'           *IN40
038500970722     C                   ENDIF
038600970722     C* INVIA MESSAGGIO ALLA CODA DEL PGM
038700970722     C                   MOVEL     $MSG          DSMSMS
038800970722     C                   CALL      'XMSG'        XMSGPA
038900970722     C     DSMSER        IFEQ      0
039000970722     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
039100970722     C                   ENDIF
039200970722     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
039300970722     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
039400971113     C                   WRITE     TCD9DVM                                      *SFL MESS
039500970722     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
039600970722     C                   MOVEL     *BLANKS       $MSG
039700970722     C                   ENDSR
039800970722     C*------------------------------------------------------------------------*
039900970722     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
040000970722     C*------------------------------------------------------------------------*
040100970722     C     OUTMSG        BEGSR
040200970722     C* VISUALIZZAZIONE MESSAGGIO
040300970722     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
040400971113     C                   WRITE     TCD9DVK                                      *CONTR MESS
040500970722     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
040600970722     C                   Z-ADD     *ZEROS        MSGULT
040700970722     C                   ENDSR
040800970722     C*------------------------------------------------------------------------*
040900970722     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
041000970722     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
041100970722     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
041200970722     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
041300970722     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
041400970722     C*------------------------------------------------------------------------*
041500970722     C     CARFUN        BEGSR
041600970722     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
041700970722     C     1             DO        31            I                 5 0          --- 1 -->
041800970722     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
041900970722     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
042000970722     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
042100020718     C     DutLpo        IFEQ      'S'                                          --- 2 --> *SEDE
042200970722     C     $TGES         IFEQ      'G'                                          *GESTIONE
042300970722     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
042400970722     C                   ELSE                                                   *INTERROG.
042500970722     C                   MOVEL     FUNASI        FAT(I)
042600970722     C                   ENDIF
042700970722     C                   ELSE                                                   <-- 2 --> *FILIALE
042800970722     C     $TGES         IFEQ      'G'                                          *GESTIONE
042900970722     C                   MOVEL     FUNAFG        FAT(I)
043000970722     C                   ELSE                                                   *INTERROG.
043100970722     C                   MOVEL     FUNAFI        FAT(I)
043200970722     C                   ENDIF
043300970722     C                   ENDIF                                                  <-- 2 ---
043400970722     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
043500970722     C                   MOVEL     USER          WEDP              3            *PROFILO
043600970722     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
043700970722     C                   MOVEL     'S'           FAT(I)                         *ABILITO
043800970722     C                   ELSE
043900970722     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
044000970722     C                   ENDIF
044100970722     C                   ENDIF
044200970722     C                   ENDDO                                                  <-- 1 ---
044300970722     C                   ENDSR
044400970722     C*------------------------------------------------------------------------*
044500970722     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
044600970722     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
044700970722     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
044800970722     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
044900970722     C*------------------------------------------------------------------------*
045000970722     C     OUTFUN        BEGSR
045100970722     C     1             DO        8             I                              *PULIZIA FUNZIONI A
045200970722     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
045300970722     C                   ENDDO
045400970722     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
045500970722     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
045600970722     C     1             DO        31            I
045700970722     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
045800970722     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
045900970722     C                   ADD       1             FUNNAT
046000970722     C                   ENDIF
046100970722     C                   ENDDO
046200970722     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
046300970722     C     FUNNAT        IFGT      8
046400970722     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
046500970722     C                   ENDIF
046600970722     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
046700970722     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
046800971113     C                   Z-ADD     9             FUNVIS            5 0
046900970722     C     FAT(23)       IFEQ      'S'
047000970722     C                   SUB       1             FUNVIS
047100970722     C                   ENDIF
047200970722     C     FAT(24)       IFEQ      'S'
047300970722     C                   SUB       1             FUNVIS
047400970722     C                   ENDIF
047500970722     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
047600970722     C                   Z-ADD     *ZEROS        W                 5 0
047700970722     C                   ADD       1             FUNULT
047800970722     C     FUNULT        IFGT      31
047900970722     C                   Z-ADD     1             FUNULT
048000970722     C                   ENDIF
048100970722     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
048200970722     C     FUNULT        DO        31            I                              --- 1 -->
048300970722     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
048400970722     C     W             IFLT      FUNVIS                                       --- 2 -->
048500970722     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
048600970722     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
048700970722     C     FDE(I)        ANDNE     *BLANKS
048800970722     C     I             IFEQ      23                                           *TOLGO F23
048900970722     C     I             OREQ      24                                           *E F24
049000970722     C                   ELSE
049100970722     C                   ADD       1             W
049200970722     C                   MOVEL     FDE(I)        FUV(W)
049300970722     C                   ENDIF
049400970722     C                   ENDIF                                                  <-- 3 ---
049500970722     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
049600970722     C                   Z-ADD     I             FUNULT
049700970722     C                   ENDIF                                                  <-- 2 ---
049800970722     C                   ENDDO                                                  <-- 1 ---
049900970722     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
050000970722     C                   MOVEL     FUV(1)        VFD001
050100970722     C                   MOVEL     FUV(2)        VFD002
050200970722     C                   MOVEL     FUV(3)        VFD003
050300970722     C                   MOVEL     FUV(4)        VFD004
050400970722     C                   MOVEL     FUV(5)        VFD005
050500970722     C                   MOVEL     FUV(6)        VFD006
050600970722     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
050700970722     C     FAT(24)       ANDEQ     'N'
050800970722     C                   MOVEL     FUV(7)        VFD007
050900970722     C                   MOVEL     FUV(8)        VFD008
051000970722     C                   ELSE                                                   <-- 1 -->
051100970722     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
051200970722     C     FAT(24)       ANDEQ     'S'
051300970722     C                   MOVEL     FDE(23)       VFD007
051400970722     C                   MOVEL     FDE(24)       VFD008
051500970722     C                   ELSE                                                   <-- 2 -->
051600970722     C                   MOVEL     FUV(7)        VFD007
051700970722     C     FAT(23)       IFEQ      'S'
051800970722     C                   MOVEL     FDE(23)       VFD008
051900970722     C                   ELSE
052000970722     C                   MOVEL     FDE(24)       VFD008
052100970722     C                   ENDIF
052200970722     C                   ENDIF                                                  <-- 2 ---
052300970722     C                   ENDIF                                                  <-- 1 ---
052400970722     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
052500970722     C                   ENDSR
052600970722     C*------------------------------------------------------------------------*
052700970722     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
052800970722     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
052900970722     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
053000970722     C*                   - ERRORE SE FUNZIONE NON ATTIVA
053100970722     C*------------------------------------------------------------------------*
053200970722     C     CONFUN        BEGSR
053300970722     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
053400970722     C     1             DO        31            I                              --- 1 -->
053500970722     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
053600970722     C* CONTROLLO SE IL TASTO E' ATTIVO
053700970722     C     FAT(I)        IFNE      'S'                                          --- 3 -->
053800970722     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
053900970722     C                   EXSR      CARMSG
054000970722     C                   MOVEL     '1'           $ERR
054100970722     C                   ENDIF                                                  <-- 3 ---
054200970722     C                   ENDIF                                                  <-- 2 ---
054300970722     C                   ENDDO                                                  <-- 1 ---
054400970722     C                   ENDSR
054500970722     C*------------------------------------------------------------------------*
054600970722**   SCHIERA: MSG  (MESSAGGI)
054700971113TRTCD9R- Tasto funzionale non valido.                                          1
054800971113TRTCD9R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
054900971113TRTCD9R- Opzione non valida.                                                   3
055000971113TRTCD9R- Opzione abilitata ma non gestita. Avvertire CED.                      4
055100971113TRTCD9R- Inizio elenco.                                                        5
055200971113TRTCD9R- Fine elenco.                                                          6
055300971113TRTCD9R- Nome libreria errato                                                  7
055400980121TRTCD9R- Nome Unità Errato
055500971113TRTCD9R- 9
055600971113TRTCD9R- 10
055700970722** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
055800970722F1    -                   -    NN     NN
055900970722F2    -                   -    NN     NN
056000970722F3    -F3=Fine            -    SS     SS
056100970722F4    -                   -    NN     NN
056200970722F5    -                   -    NN     NN
056300970724F6    -F6=Conferma        -    SS     SS
056400970722F7    -                   -    NN     NN
056500970722F8    -                   -    NN     NN
056600970722F9    -                   -    NN     NN
056700970722F10   -                   -    NN     NN
056800970722F11   -                   -    NN     NN
056900970722F12   -F12=Ritorno        -    SS     SS
057000970722F13   -                   -    NN     NN
057100970722F14   -                   -    NN     NN
057200970722F15   -                   -    NN     NN
057300970722F16   -                   -    NN     NN
057400970722F17   -                   -    NN     NN
057500970722F18   -                   -    NN     NN
057600970722F19   -                   -    NN     NN
057700970722F20   -                   -    NN     NN
057800970722F21   -                   -    NN     NN
057900970722F22   -                   -    NN     NN
058000970722F23   -                   -    NN     NN
058100970722F24   -F24=Altri tasti    -    NN     NN
058200970722ENTER -                   -    SS     SS
058300970722ROLLUP-                   -    NN     NN
058400970722ROLLDN-                   -    NN     NN
058500970722HELP  -                   -    NN     NN
058600970722PRINT -                   -    NN     NN
058700970722BSPACE-                   -    NN     NN
058800970722CLEAR -                   -    NN     NN
