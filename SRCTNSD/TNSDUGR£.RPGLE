000100020325     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020325      *------------------------------------------------------------------------*
000300020325      * CANCELLAZIONE MOVIMENTI UNITA' EDP - RICHIESTA PARAMETRI
000400020325      *------------------------------------------------------------------------*
000500020325
000600020325      *------------------------------------------------------------------------*
000700020325      * DATA BASE
000800020325      *------------------------------------------------------------------------*
000900971013     FTNSDUGD   CF   E             WORKSTN INFDS(DEVDS1)
001000971013     F                                     SFILE(SDUGDVM:MSGULT)
001100020325
001200971013     D*------------------------------------------------------------------------*
001300971013     D* SCHIERE
001400971013     D*------------------------------------------------------------------------*
001500971013     D*-------------
001600971013     D* SCHIERE A TEMPO DI COMPILAZIONE
001700971013     D*-------------
001800971013     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001900971013     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002000971013     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZIONI VIDEO
002100971013     D*-------------
002200971013     D* FUNZIONI
002300971013     D*-------------
002400971013     D FFU             S              6    DIM(31)                              *FUNZIONE
002500971013     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600971013     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700971013     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800971013     D*-------------
002900971013     D* TASTI FUNZIONALI PREMUTI
003000971013     D*-------------
003100971013     D KEYS            S              1    DIM(40)
003200971013     D*------------------------------------------------------------------------*
003300971013     D* INPUT
003400971013     D*------------------------------------------------------------------------*
003500971013     D*-------------
003600971013     D* RIDEFINIZIONE SCHIERA FUD - FUNZIONI
003700971013     D*-------------
003800971013     D DSFUN         E DS
003900971013     D  FUNRIG                 1     40
004000971013     D*-------------
004100971013     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
004200971013     D*-------------
004300971013     D DSMSG         E DS
004400971013     D*-------------
004500971013     D* DS PER GESTIONE TASTI FUNZIONALI
004600971013     D*-------------
004700971013     D DSKEYS        E DS
004800971013     D  KEYRIG                 1     40
004900971013     D*-------------
005000971013     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100971013     D*-------------
005200971013     D                SDS
005300971013     D  USER                 254    263
005400971013     D*-------------
005500971013     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600971013     D*-------------
005700971013     D DEVDS1          DS
005800971013     D  KEY                  369    369
005900971013     D  CURSOR               370    371
006000971013     D*-------------
006100971013     D* DS "XFIL" - REPERIMENTO DATI UTENTE
006200971013     D*-------------
006300020709     D**!!! DSFIL         E DS
006400971013     D*-------------
006500971013     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600971013     D*-------------
006700971013     D WLBDA8          DS
006800971013     D  G08DAT                 1      8  0
006900971013     D  G08INV                 9     16  0
007000971013     D  G08ERR                17     17
007100971013     D  G08TGI                18     22  0
007200971013     D*-------------
007300971013     D* DS PARAMETRI DI LANCIO
007400971013     D*-------------
007500971013     D PARAM           DS                  INZ
007600971013     D  PARMDI                 1      8  0
007700971013     D  PARMDF                 9     16  0
007800971013     D  PARMAT                17     17
007900971013     D*-------------
008000971013     D* ARCHITETTURA
008100971013     D*-------------
008200971013     D KPJBA         E DS
008300020325
008400020709     d ddatiute      e ds
008500020709     d azuteds       e ds                  extname(azute00f)
008600020709     d tibs34ds      e ds                  inz
008700971013     C*------------------------------------------------------------------------*
008800971013     C* MAIN LINE
008900971013     C*------------------------------------------------------------------------*
009000971013     C*
009100971013     C* CICLO FINO A CHE:
009200971013DO  1C     $FINE         DOUEQ     '1'
009300971013     C*
009400971013     C* CONTROLLA GESTIONE VIDEO
009500971013     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009600971013IF  2C     $GES          IFEQ      'V1'
009700971013     C                   EXSR      GESV1                                        *PARAMETRI
009800971013E   2C                   ENDIF
009900971013E   1C                   ENDDO
010000971013     C*
010100971013     C                   SETON                                        LR
010200971013     C*------------------------------------------------------------------------*
010300971013     C* GESV1 - GESTIONE VIDEO UNO
010400971013     C*------------------------------------------------------------------------*
010500971013     C     GESV1         BEGSR
010600971013     C*
010700971013IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010800971013     C                   EXSR      CARV1                                        *CARICA VIDEO
010900971013E   1C                   ENDIF
011000971013     C*
011100971013IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011200971013     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011300971013     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011400971013E   1C                   ENDIF
011500971013     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011600971013     C*
011700971013     C                   EXFMT     SDUGDV1                                      *PARAMETRI
011800971013     C*
011900971013     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012000971013     C*
012100971013IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012200971013SL  2C                   SELECT
012300971013     C     KEY           WHENEQ    ENTER
012400971013     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012500971013     C     KEY           WHENEQ    F6
012600971013     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012700971013     C     KEY           WHENEQ    F22
012800971013     C                   EXSR      F22GES                                       *GESTIONE STAMPE
012900971013     C                   OTHER
013000971013     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013100971013E   2C                   ENDSL
013200971013E   1C                   ENDIF
013300971013     C*
013400971013     C                   ENDSR
013500971013     C*------------------------------------------------------------------------*
013600971013     C* CARV1 - CARICA VIDEO UNO
013700971013     C*------------------------------------------------------------------------*
013800971013     C     CARV1         BEGSR
013900971013     C*
014000971013     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014100971013     C*
014200971013     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014300971013     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014400971013     C*
014500971013     C                   WRITE     SDUGDVT                                      *TESTATA
014600971013     C                   WRITE     SDUGDVF                                      *FUNZIONI
014700971013     C*
014800971013     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
014900971013     C*
015000971013     C                   ENDSR
015100971013     C*------------------------------------------------------------------------*
015200971013     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015300971013     C*------------------------------------------------------------------------*
015400971013     C     ENTV1         BEGSR
015500971013     C*
015600971013     C* CONTROLLA I CAMPI IMMESSI
015700971013     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015800971013     C*
015900971013     C                   ENDSR
016000971013     C*------------------------------------------------------------------------*
016100971013     C* F06GES - GESTIONE FUNZIONE F06 -> LANCIO
016200971013     C*------------------------------------------------------------------------*
016300971013     C     F06GES        BEGSR
016400971013     C*
016500971013     C* CONTROLLA I CAMPI IMMESSI
016600971013     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016700971013     C*
016800971013     C* LANCIO IL PROGRAMMA
016900971013IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017000971013     C                   Z-ADD     MDIV1C        PARMDI
017100971013     C                   Z-ADD     MDFV1C        PARMDF
017200971013     C                   MOVEL     V1CMAT        PARMAT
017300971013     C*
017400971013     C                   MOVEL     'SDUH'        KCOAZ
017500971013     C                   MOVEL     PARAM         KPJBU
017600971013     C                   CALL      'BCH10'
017700971013     C                   PARM                    KPJBA
017800971013     C*
017900971013     C                   MOVEL     '1'           $FINE                          *FINE PGM
018000971013E   1C                   ENDIF
018100971013     C*
018200971013     C                   ENDSR
018300971013     C*------------------------------------------------------------------------*
018400971013     C* F22GES - GESTIONE FUNZIONE F22 -> GESTIONE STAMPE
018500971013     C*------------------------------------------------------------------------*
018600971013     C     F22GES        BEGSR
018700971013     C*
018800971013     C                   Z-ADD     80            LENGH            15 5
018900971013     C                   CALL      'QCAEXEC'
019000971013     C                   PARM                    CMD(1)
019100971013     C                   PARM                    LENGH
019200971013     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
019300971013     C*
019400971013     C                   ENDSR
019500971013     C*------------------------------------------------------------------------*
019600971013     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019700971013     C*------------------------------------------------------------------------*
019800971013     C     DECODI        BEGSR
019900971013     C*
020000971013     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
020100971013     C                   SETOFF                                       606162
020200971013     C***
020300971013     C* DATA MOVIMENTO DAL
020400971013     C***
020500971013     C     *LIKE         DEFINE    V1CMDI        MDIV1C
020600971013     C                   Z-ADD     *ZEROS        MDIV1C
020700971013IF  1C     $ERR          IFEQ      '0'
020800971013     C                   Z-ADD     V1CMDI        G08DAT
020900971013     C                   Z-ADD     *ZEROS        G08INV
021000971013     C                   MOVEL     '0'           G08ERR
021100971013     C                   CALL      'XSRDA8'
021200971013     C                   PARM                    WLBDA8
021300971013     C                   Z-ADD     G08DAT        V1CMDI
021400971013     C                   Z-ADD     G08INV        MDIV1C
021500971015IF  2C     G08ERR        IFEQ      '1'                                          *ERRORE
021600971013     C                   MOVEL     '1'           $ERR
021700971013     C                   MOVEL     MSG(8)        $MSG
021800971013     C                   EXSR      CARMSG
021900971015     C                   SETON                                        60
022000971015E   2C                   ENDIF
022100971013E   1C                   ENDIF
022200971013     C***
022300971013     C* DATA MOVIMENTO AL
022400971013     C***
022500971013     C     *LIKE         DEFINE    V1CMDF        MDFV1C
022600971013     C                   Z-ADD     *ZEROS        MDFV1C
022700971013IF  1C     $ERR          IFEQ      '0'
022800971013     C                   Z-ADD     V1CMDF        G08DAT
022900971013     C                   Z-ADD     *ZEROS        G08INV
023000971013     C                   MOVEL     '0'           G08ERR
023100971013     C                   CALL      'XSRDA8'
023200971013     C                   PARM                    WLBDA8
023300971013     C                   Z-ADD     G08DAT        V1CMDF
023400971013     C                   Z-ADD     G08INV        MDFV1C
023500971015IF  2C     G08ERR        IFEQ      '1'                                          *ERRORE
023600971013     C                   MOVEL     '1'           $ERR
023700971013     C                   MOVEL     MSG(8)        $MSG
023800971013     C                   EXSR      CARMSG
023900971013     C                   SETON                                        61
024000971013E   2C                   ENDIF
024100971013E   1C                   ENDIF
024200971013     C***
024300971013     C* DATA MOVIMENTO DAL + AL
024400971013     C***
024500971013IF  1C     $ERR          IFEQ      '0'
024600971013IF  2C     MDFV1C        IFLT      MDIV1C
024700971013     C                   MOVEL     '1'           $ERR
024800971013     C                   MOVEL     MSG(9)        $MSG
024900971013     C                   EXSR      CARMSG
025000971013     C                   SETON                                        6061
025100971013E   2C                   ENDIF
025200971013E   1C                   ENDIF
025300971013     C***
025400971013     C* SOLO ANNULLATI
025500971013     C***
025600971013IF  1C     V1CMAT        IFEQ      'S'
025700971013     C     V1CMAT        OREQ      'N'
025800971013X   1C                   ELSE
025900971013     C                   MOVEL     '1'           $ERR
026000971013     C                   MOVEL     MSG(7)        $MSG
026100971013     C                   EXSR      CARMSG
026200971013     C                   SETON                                        62
026300971013E   1C                   ENDIF
026400971013     C*
026500971013     C                   ENDSR
026600971013     C*------------------------------------------------------------------------*
026700971013     C* *INZSR - OPERAZIONI INZIALI
026800971013     C*------------------------------------------------------------------------*
026900971013     C     *INZSR        BEGSR
027000971013     C*--------------------
027100971013     C* RICEVIMENTO PARAMETRI
027200971013     C*--------------------
027300971013     C     *ENTRY        PLIST
027400971013     C                   PARM                    KPJBA
027500971013     C*
027600971013     C* ESEGUE OPERAZIONI INIZIALI STANDARD
027700971013     C                   EXSR      INZSTD
027800971015     C*
027900971015     C* IMPOSTAZIONI INIZIALI VIDEO
028000971015     C                   Z-ADD     01011901      V1CMDI                         *DATA INZIALE
028100971015     C                   Z-ADD     31121999      V1CMDF                         *DATA FINALE
028200971015     C                   MOVEL     'S'           V1CMAT                         *SOLO ANNULLATI
028300971013     C*
028400971013     C* EMESSIONE FORMATO VIDEO
028500971013     C                   EXSR      CARV1
028600971013     C*
028700971013     C                   ENDSR
028800971013     C*------------------------------------------------------------------------*
028900971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029000971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029100971013     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
029200971013     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
029300971013     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
029400971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029500971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029600971013     C*------------------------------------------------------------------------*
029700971013     C* INZSTD - ROUTINE INIZIALE STANDARD
029800971013     C*------------------------------------------------------------------------*
029900971013     C     INZSTD        BEGSR
030000971013     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
030100971013     C                   CALL      'XKEYS'
030200971013     C                   PARM                    DSKEYS
030300971013     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
030400971013     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
030500971013     C     XMSGPA        PLIST
030600971013     C                   PARM                    DSMSMS
030700971013     C                   PARM                    DSMSPG
030800971013     C                   PARM                    DSMSKE
030900971013     C                   PARM                    DSMSER
031000971013     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
031100971013     C                   MOVEL     'TNSDUGR'     DSMSPG                         *NOME PGM CHIAMANTE
031200971013     C                   MOVEL     'TNSDUGR'     VMCQUE                         *CODA MESSAGGI DESTI
031300971013     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
031400971013     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
031500971013     C* INIZIALIZZO FLAG CONTROLLO PGM
031600971013     C                   MOVEL     '0'           $ERR              1            *ERRORE
031700971013     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
031800971013     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
031900971013     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
032000971013     C                   MOVEL     '0'           $FINE             1            *FINE PGM
032100971013     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
032200971013     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
032300971013     C* LANCIO ROUTINE PER VERIFICARE SE
032400971013     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
032500971013     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
032600020709     C**!!!              CALL      'XFIL'
032700020709     C**!!!              PARM                    DSFIL
032800020709
032900020709     c     *dtaara       define    §azute        azuteds
033000020709     c     *dtaara       define    §datiute      ddatiute
033100020709
033200020709     c                   in(E)     *dtaara
033300020709     c                   if        %Error or rsut = *blanks
033400020709     c                   clear                   tibs34ds
033500020709     c                   call      'TIBS34R'
033600020709     c                   parm                    Tibs34Ds
033700020709     c                   in        *dtaara
033800020709     c                   endif
033900971013     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
034000971013     C                   BITOFF    '01234567'    POSMI1            1
034100971013     C                   BITOFF    '01234567'    POSMI2            1
034200971013     C                   BITOFF    '01234567'    POSMA1            1
034300971013     C                   BITOFF    '01234567'    POSMA2            1
034400971013     C                   BITON     '34'          POSMI1            1            *18
034500971013     C                   BITON     '7'           POSMI2            1            *01
034600971013     C                   BITON     '34'          POSMA1            1            *18
034700971013     C                   BITON     '0'           POSMA2            1            *80
034800971013     C                   MOVE      POSMI2        POSMIN            2
034900971013     C                   MOVEL     POSMI1        POSMIN
035000971013     C                   MOVE      POSMA2        POSMAX            2
035100971013     C                   MOVEL     POSMA1        POSMAX
035200971013     C                   ENDSR
035300971013     C*------------------------------------------------------------------------*
035400971013     C* FUNSTD - LANCIA SUBROUTINE STANDARD
035500971013     C*------------------------------------------------------------------------*
035600971013     C     FUNSTD        BEGSR
035700971013     C                   SELECT
035800971013     C     KEY           WHENEQ    F3
035900971013     C                   EXSR      F03STD
036000971013     C     KEY           WHENEQ    F12
036100971013     C                   EXSR      F12STD
036200971013     C     KEY           WHENEQ    F24
036300971013     C                   EXSR      F24STD
036400971013     C                   OTHER
036500971013     C                   MOVEL     MSG(2)        $MSG
036600971013     C                   EXSR      CARMSG
036700971013     C                   ENDSL
036800971013     C                   ENDSR
036900971013     C*------------------------------------------------------------------------*
037000971013     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
037100971013     C*------------------------------------------------------------------------*
037200971013     C     F03STD        BEGSR
037300971013     C*
037400971013     C* IMPOSTO FLAG DI PREMUTO TASTO F03
037500971013     C                   MOVEL     '1'           $F03
037600971013     C*
037700971013     C* IMPOSTO FLAG DI FINE PROGRAMMA
037800971013     C                   MOVEL     '1'           $FINE
037900971013     C*
038000971013     C                   ENDSR
038100971013     C*------------------------------------------------------------------------*
038200971013     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
038300971013     C*------------------------------------------------------------------------*
038400971013     C     F12STD        BEGSR
038500971013     C*
038600971013     C* IMPOSTO FLAG DI FINE PROGRAMMA
038700971013     C                   MOVEL     '1'           $FINE
038800971013     C*
038900971013     C                   ENDSR
039000971013     C*------------------------------------------------------------------------*
039100971013     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
039200971013     C*------------------------------------------------------------------------*
039300971013     C     F24STD        BEGSR
039400971013     C*
039500971013     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
039600971013     C                   EXSR      OUTFUN
039700971013     C*
039800971013     C                   ENDSR
039900971013     C*------------------------------------------------------------------------*
040000971013     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
040100971013     C*          CODA MESSAGGI DEL PGM
040200971013     C*------------------------------------------------------------------------*
040300971013     C     CARMSG        BEGSR
040400971013     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
040500971013     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
040600971013     C     MSGULT        IFEQ      0
040700971013     C                   MOVEL     '1'           *IN40
040800971013     C                   WRITE     SDUGDVK
040900971013     C                   MOVEL     '0'           *IN40
041000971013     C                   ENDIF
041100971013     C* INVIA MESSAGGIO ALLA CODA DEL PGM
041200971013     C                   MOVEL     $MSG          DSMSMS
041300971013     C                   CALL      'XMSG'        XMSGPA
041400971013     C     DSMSER        IFEQ      0
041500971013     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
041600971013     C                   ENDIF
041700971013     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
041800971013     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
041900971013     C                   WRITE     SDUGDVM                                      *SFL MESS
042000971013     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
042100971013     C                   MOVEL     *BLANKS       $MSG
042200971013     C                   ENDSR
042300971013     C*------------------------------------------------------------------------*
042400971013     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
042500971013     C*------------------------------------------------------------------------*
042600971013     C     OUTMSG        BEGSR
042700971013     C* VISUALIZZAZIONE MESSAGGIO
042800971013     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
042900971013     C                   WRITE     SDUGDVK                                      *CONTR MESS
043000971013     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
043100971013     C                   Z-ADD     *ZEROS        MSGULT
043200971013     C                   ENDSR
043300971013     C*------------------------------------------------------------------------*
043400971013     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
043500971013     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
043600971013     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
043700971013     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
043800971013     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
043900971013     C*------------------------------------------------------------------------*
044000971013     C     CARFUN        BEGSR
044100971013     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
044200971013     C     1             DO        31            I                 5 0          --- 1 -->
044300971013     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
044400971013     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
044500971013     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
044600020709     C**!!!DSFITI        IFEQ      'S'                                          --- 2 --> *SEDE
044700020709     c                   If        DutLpo = 'S'
044800971013     C     $TGES         IFEQ      'G'                                          *GESTIONE
044900971013     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
045000971013     C                   ELSE                                                   *INTERROG.
045100971013     C                   MOVEL     FUNASI        FAT(I)
045200971013     C                   ENDIF
045300971013     C                   ELSE                                                   <-- 2 --> *FILIALE
045400971013     C     $TGES         IFEQ      'G'                                          *GESTIONE
045500971013     C                   MOVEL     FUNAFG        FAT(I)
045600971013     C                   ELSE                                                   *INTERROG.
045700971013     C                   MOVEL     FUNAFI        FAT(I)
045800971013     C                   ENDIF
045900971013     C                   ENDIF                                                  <-- 2 ---
046000971013     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
046100971013     C                   MOVEL     USER          WEDP              3            *PROFILO
046200971013     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
046300971013     C                   MOVEL     'S'           FAT(I)                         *ABILITO
046400971013     C                   ELSE
046500971013     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
046600971013     C                   ENDIF
046700971013     C                   ENDIF
046800971013     C                   ENDDO                                                  <-- 1 ---
046900971013     C                   ENDSR
047000971013     C*------------------------------------------------------------------------*
047100971013     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
047200971013     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
047300971013     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
047400971013     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
047500971013     C*------------------------------------------------------------------------*
047600971013     C     OUTFUN        BEGSR
047700971013     C     1             DO        8             I                              *PULIZIA FUNZIONI A
047800971013     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
047900971013     C                   ENDDO
048000971013     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
048100971013     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
048200971013     C     1             DO        31            I
048300971013     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
048400971013     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
048500971013     C                   ADD       1             FUNNAT
048600971013     C                   ENDIF
048700971013     C                   ENDDO
048800971013     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
048900971013     C     FUNNAT        IFGT      8
049000971013     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
049100971013     C                   ENDIF
049200971013     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
049300971013     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
049400971013     C                   Z-ADD     8             FUNVIS            5 0
049500971013     C     FAT(23)       IFEQ      'S'
049600971013     C                   SUB       1             FUNVIS
049700971013     C                   ENDIF
049800971013     C     FAT(24)       IFEQ      'S'
049900971013     C                   SUB       1             FUNVIS
050000971013     C                   ENDIF
050100971013     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
050200971013     C                   Z-ADD     *ZEROS        W                 5 0
050300971013     C                   ADD       1             FUNULT
050400971013     C     FUNULT        IFGT      31
050500971013     C                   Z-ADD     1             FUNULT
050600971013     C                   ENDIF
050700971013     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
050800971013     C     FUNULT        DO        31            I                              --- 1 -->
050900971013     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
051000971013     C     W             IFLT      FUNVIS                                       --- 2 -->
051100971013     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
051200971013     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
051300971013     C     FDE(I)        ANDNE     *BLANKS
051400971013     C     I             IFEQ      23                                           *TOLGO F23
051500971013     C     I             OREQ      24                                           *E F24
051600971013     C                   ELSE
051700971013     C                   ADD       1             W
051800971013     C                   MOVEL     FDE(I)        FUV(W)
051900971013     C                   ENDIF
052000971013     C                   ENDIF                                                  <-- 3 ---
052100971013     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
052200971013     C                   Z-ADD     I             FUNULT
052300971013     C                   ENDIF                                                  <-- 2 ---
052400971013     C                   ENDDO                                                  <-- 1 ---
052500971013     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
052600971013     C                   MOVEL     FUV(1)        VFD001
052700971013     C                   MOVEL     FUV(2)        VFD002
052800971013     C                   MOVEL     FUV(3)        VFD003
052900971013     C                   MOVEL     FUV(4)        VFD004
053000971013     C                   MOVEL     FUV(5)        VFD005
053100971013     C                   MOVEL     FUV(6)        VFD006
053200971013     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
053300971013     C     FAT(24)       ANDEQ     'N'
053400971013     C                   MOVEL     FUV(7)        VFD007
053500971013     C                   MOVEL     FUV(8)        VFD008
053600971013     C                   ELSE                                                   <-- 1 -->
053700971013     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
053800971013     C     FAT(24)       ANDEQ     'S'
053900971013     C                   MOVEL     FDE(23)       VFD007
054000971013     C                   MOVEL     FDE(24)       VFD008
054100971013     C                   ELSE                                                   <-- 2 -->
054200971013     C                   MOVEL     FUV(7)        VFD007
054300971013     C     FAT(23)       IFEQ      'S'
054400971013     C                   MOVEL     FDE(23)       VFD008
054500971013     C                   ELSE
054600971013     C                   MOVEL     FDE(24)       VFD008
054700971013     C                   ENDIF
054800971013     C                   ENDIF                                                  <-- 2 ---
054900971013     C                   ENDIF                                                  <-- 1 ---
055000971013     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
055100971013     C                   ENDSR
055200971013     C*------------------------------------------------------------------------*
055300971013     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
055400971013     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
055500971013     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
055600971013     C*                   - ERRORE SE FUNZIONE NON ATTIVA
055700971013     C*------------------------------------------------------------------------*
055800971013     C     CONFUN        BEGSR
055900971013     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
056000971013     C     1             DO        31            I                              --- 1 -->
056100971013     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
056200971013     C* CONTROLLO SE IL TASTO E' ATTIVO
056300971013     C     FAT(I)        IFNE      'S'                                          --- 3 -->
056400971013     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
056500971013     C                   EXSR      CARMSG
056600971013     C                   MOVEL     '1'           $ERR
056700971013     C                   ENDIF                                                  <-- 3 ---
056800971013     C                   ENDIF                                                  <-- 2 ---
056900971013     C                   ENDDO                                                  <-- 1 ---
057000971013     C                   ENDSR
057100971013     O*------------------------------------------------------------------------*
057200971013**       MSG - MESSAGGI
057300971013TNSDUGR- Tasto funzionale non valido.                                          1
057400971013TNSDUGR- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
057500971013TNSDUGR- Opzione non valida.                                                   3
057600971013TNSDUGR- Opzione abilitata ma non gestita. Avvertire CED.                      4
057700971013TNSDUGR- Inizio elenco.                                                        5
057800971013TNSDUGR- Fine elenco.                                                          6
057900971013TNSDUGR- Immettere uno dei valori possibili                                    7
058000971013TNSDUGR- Data errata                                                           8
058100971013TNSDUGR- La data AL deve essere maggiore o uguale della data DAL               9
058200971013TNSDUGR- 10
058300971013TNSDUGR- 11
058400971013TNSDUGR- 12
058500971013TNSDUGR- 13
058600971013TNSDUGR- 14
058700971013TNSDUGR- 15
058800971013TNSDUGR- 16
058900971013TNSDUGR- 17
059000971013TNSDUGR- 18
059100971013TNSDUGR- 19
059200971013TNSDUGR- 20
059300971013** ======== SCHIERA: CMD  (COMANDI)                        ================
059400971013WRKSPLF
059500971013
059600971013
059700971013
059800971013
059900971013** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
060000971013F1    -                   -    NN     NN
060100971013F2    -                   -    NN     NN
060200971013F3    -F3=Fine            -    SS     SS
060300971013F4    -                   -    NN     NN
060400971013F5    -                   -    NN     NN
060500971013F6    -F6=Lancio          -    SS     SS
060600971013F7    -                   -    NN     NN
060700971013F8    -                   -    NN     NN
060800971013F9    -                   -    NN     NN
060900971013F10   -                   -    NN     NN
061000971013F11   -                   -    NN     NN
061100971013F12   -F12-Ritorno        -    SS     SS
061200971013F13   -                   -    NN     NN
061300971013F14   -                   -    NN     NN
061400971013F15   -                   -    NN     NN
061500971013F16   -                   -    NN     NN
061600971013F17   -                   -    NN     NN
061700971013F18   -                   -    NN     NN
061800971013F19   -                   -    NN     NN
061900971013F20   -                   -    NN     NN
062000971013F21   -                   -    NN     NN
062100971013F22   -F22=Gestione stampe-    SS     SS
062200971013F23   -                   -    NN     NN
062300971013F24   -F24=Altri tasti    -    NN     NN
062400971013ENTER -                   -    SS     SS
062500971013ROLLUP-                   -    NN     NN
062600971013ROLLDN-                   -    NN     NN
062700971013HELP  -                   -    NN     NN
062800971013PRINT -                   -    NN     NN
062900971013BSPACE-                   -    NN     NN
063000971013CLEAR -                   -    NN     NN
