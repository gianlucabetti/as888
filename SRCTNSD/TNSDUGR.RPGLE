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
006100971013     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200971013     D*-------------
006300971013     D WLBDA8          DS
006400971013     D  G08DAT                 1      8  0
006500971013     D  G08INV                 9     16  0
006600971013     D  G08ERR                17     17
006700971013     D  G08TGI                18     22  0
006800971013     D*-------------
006900971013     D* DS PARAMETRI DI LANCIO
007000971013     D*-------------
007100971013     D PARAM           DS                  INZ
007200971013     D  PARMDI                 1      8  0
007300971013     D  PARMDF                 9     16  0
007400971013     D  PARMAT                17     17
007500971013     D*-------------
007600971013     D* ARCHITETTURA
007700971013     D*-------------
007800971013     D KPJBA         E DS
007900020325
008000020709     d ddatiute      e ds
008100020709     d azuteds       e ds                  extname(azute00f)
008200020709     d tibs34ds      e ds                  inz
008300971013     C*------------------------------------------------------------------------*
008400971013     C* MAIN LINE
008500971013     C*------------------------------------------------------------------------*
008600971013     C*
008700971013     C* CICLO FINO A CHE:
008800971013DO  1C     $FINE         DOUEQ     '1'
008900971013     C*
009000971013     C* CONTROLLA GESTIONE VIDEO
009100971013     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009200971013IF  2C     $GES          IFEQ      'V1'
009300971013     C                   EXSR      GESV1                                        *PARAMETRI
009400971013E   2C                   ENDIF
009500971013E   1C                   ENDDO
009600971013     C*
009700971013     C                   SETON                                        LR
009800971013     C*------------------------------------------------------------------------*
009900971013     C* GESV1 - GESTIONE VIDEO UNO
010000971013     C*------------------------------------------------------------------------*
010100971013     C     GESV1         BEGSR
010200971013     C*
010300971013IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010400971013     C                   EXSR      CARV1                                        *CARICA VIDEO
010500971013E   1C                   ENDIF
010600971013     C*
010700971013IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010800971013     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
010900971013     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011000971013E   1C                   ENDIF
011100971013     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011200971013     C*
011300971013     C                   EXFMT     SDUGDV1                                      *PARAMETRI
011400971013     C*
011500971013     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011600020717
011700020717      * Se Profilo non OK devo uscire dal pgm
011800020717     c                   If        *In76
011900020717     c                   Eval      $err = '1'
012000020717     c                   Eval      $fine = '1'
012100020717     c                   EndIf
012200971013     C*
012300971013IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012400971013SL  2C                   SELECT
012500971013     C     KEY           WHENEQ    ENTER
012600971013     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012700971013     C     KEY           WHENEQ    F6
012800971013     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012900971013     C     KEY           WHENEQ    F22
013000971013     C                   EXSR      F22GES                                       *GESTIONE STAMPE
013100971013     C                   OTHER
013200971013     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013300971013E   2C                   ENDSL
013400971013E   1C                   ENDIF
013500971013     C*
013600971013     C                   ENDSR
013700971013     C*------------------------------------------------------------------------*
013800971013     C* CARV1 - CARICA VIDEO UNO
013900971013     C*------------------------------------------------------------------------*
014000971013     C     CARV1         BEGSR
014100971013     C*
014200971013     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014300971013     C*
014400971013     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014500971013     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014600971013     C*
014700971013     C                   WRITE     SDUGDVT                                      *TESTATA
014800971013     C                   WRITE     SDUGDVF                                      *FUNZIONI
014900971013     C*
015000971013     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
015100971013     C*
015200971013     C                   ENDSR
015300971013     C*------------------------------------------------------------------------*
015400971013     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015500971013     C*------------------------------------------------------------------------*
015600971013     C     ENTV1         BEGSR
015700971013     C*
015800971013     C* CONTROLLA I CAMPI IMMESSI
015900971013     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016000971013     C*
016100971013     C                   ENDSR
016200971013     C*------------------------------------------------------------------------*
016300971013     C* F06GES - GESTIONE FUNZIONE F06 -> LANCIO
016400971013     C*------------------------------------------------------------------------*
016500971013     C     F06GES        BEGSR
016600971013     C*
016700971013     C* CONTROLLA I CAMPI IMMESSI
016800971013     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016900971013     C*
017000971013     C* LANCIO IL PROGRAMMA
017100971013IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017200971013     C                   Z-ADD     MDIV1C        PARMDI
017300971013     C                   Z-ADD     MDFV1C        PARMDF
017400971013     C                   MOVEL     V1CMAT        PARMAT
017500971013     C*
017600971013     C                   MOVEL     'SDUH'        KCOAZ
017700971013     C                   MOVEL     PARAM         KPJBU
017800971013     C                   CALL      'BCH10'
017900971013     C                   PARM                    KPJBA
018000971013     C*
018100971013     C                   MOVEL     '1'           $FINE                          *FINE PGM
018200971013E   1C                   ENDIF
018300971013     C*
018400971013     C                   ENDSR
018500971013     C*------------------------------------------------------------------------*
018600971013     C* F22GES - GESTIONE FUNZIONE F22 -> GESTIONE STAMPE
018700971013     C*------------------------------------------------------------------------*
018800971013     C     F22GES        BEGSR
018900971013     C*
019000971013     C                   Z-ADD     80            LENGH            15 5
019100971013     C                   CALL      'QCAEXEC'
019200971013     C                   PARM                    CMD(1)
019300971013     C                   PARM                    LENGH
019400971013     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
019500971013     C*
019600971013     C                   ENDSR
019700971013     C*------------------------------------------------------------------------*
019800971013     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019900971013     C*------------------------------------------------------------------------*
020000971013     C     DECODI        BEGSR
020100971013     C*
020200971013     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
020300971013     C                   SETOFF                                       606162
020400971013     C***
020500971013     C* DATA MOVIMENTO DAL
020600971013     C***
020700971013     C     *LIKE         DEFINE    V1CMDI        MDIV1C
020800971013     C                   Z-ADD     *ZEROS        MDIV1C
020900971013IF  1C     $ERR          IFEQ      '0'
021000971013     C                   Z-ADD     V1CMDI        G08DAT
021100971013     C                   Z-ADD     *ZEROS        G08INV
021200971013     C                   MOVEL     '0'           G08ERR
021300971013     C                   CALL      'XSRDA8'
021400971013     C                   PARM                    WLBDA8
021500971013     C                   Z-ADD     G08DAT        V1CMDI
021600971013     C                   Z-ADD     G08INV        MDIV1C
021700971015IF  2C     G08ERR        IFEQ      '1'                                          *ERRORE
021800971013     C                   MOVEL     '1'           $ERR
021900971013     C                   MOVEL     MSG(8)        $MSG
022000971013     C                   EXSR      CARMSG
022100971015     C                   SETON                                        60
022200971015E   2C                   ENDIF
022300971013E   1C                   ENDIF
022400971013     C***
022500971013     C* DATA MOVIMENTO AL
022600971013     C***
022700971013     C     *LIKE         DEFINE    V1CMDF        MDFV1C
022800971013     C                   Z-ADD     *ZEROS        MDFV1C
022900971013IF  1C     $ERR          IFEQ      '0'
023000971013     C                   Z-ADD     V1CMDF        G08DAT
023100971013     C                   Z-ADD     *ZEROS        G08INV
023200971013     C                   MOVEL     '0'           G08ERR
023300971013     C                   CALL      'XSRDA8'
023400971013     C                   PARM                    WLBDA8
023500971013     C                   Z-ADD     G08DAT        V1CMDF
023600971013     C                   Z-ADD     G08INV        MDFV1C
023700971015IF  2C     G08ERR        IFEQ      '1'                                          *ERRORE
023800971013     C                   MOVEL     '1'           $ERR
023900971013     C                   MOVEL     MSG(8)        $MSG
024000971013     C                   EXSR      CARMSG
024100971013     C                   SETON                                        61
024200971013E   2C                   ENDIF
024300971013E   1C                   ENDIF
024400971013     C***
024500971013     C* DATA MOVIMENTO DAL + AL
024600971013     C***
024700971013IF  1C     $ERR          IFEQ      '0'
024800971013IF  2C     MDFV1C        IFLT      MDIV1C
024900971013     C                   MOVEL     '1'           $ERR
025000971013     C                   MOVEL     MSG(9)        $MSG
025100971013     C                   EXSR      CARMSG
025200971013     C                   SETON                                        6061
025300971013E   2C                   ENDIF
025400971013E   1C                   ENDIF
025500971013     C***
025600971013     C* SOLO ANNULLATI
025700971013     C***
025800971013IF  1C     V1CMAT        IFEQ      'S'
025900971013     C     V1CMAT        OREQ      'N'
026000971013X   1C                   ELSE
026100971013     C                   MOVEL     '1'           $ERR
026200971013     C                   MOVEL     MSG(7)        $MSG
026300971013     C                   EXSR      CARMSG
026400971013     C                   SETON                                        62
026500971013E   1C                   ENDIF
026600971013     C*
026700971013     C                   ENDSR
026800971013     C*------------------------------------------------------------------------*
026900971013     C* *INZSR - OPERAZIONI INZIALI
027000971013     C*------------------------------------------------------------------------*
027100971013     C     *INZSR        BEGSR
027200971013     C*--------------------
027300971013     C* RICEVIMENTO PARAMETRI
027400971013     C*--------------------
027500971013     C     *ENTRY        PLIST
027600971013     C                   PARM                    KPJBA
027700971013     C*
027800971013     C* ESEGUE OPERAZIONI INIZIALI STANDARD
027900971013     C                   EXSR      INZSTD
028000971015     C*
028100971015     C* IMPOSTAZIONI INIZIALI VIDEO
028200971015     C                   Z-ADD     01011901      V1CMDI                         *DATA INZIALE
028300971015     C                   Z-ADD     31121999      V1CMDF                         *DATA FINALE
028400971015     C                   MOVEL     'S'           V1CMAT                         *SOLO ANNULLATI
028500971013     C*
028600971013     C* EMESSIONE FORMATO VIDEO
028700971013     C                   EXSR      CARV1
028800971013     C*
028900971013     C                   ENDSR
029000971013     C*------------------------------------------------------------------------*
029100971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029200971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029300971013     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
029400971013     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
029500971013     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
029600971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029700971013     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
029800971013     C*------------------------------------------------------------------------*
029900971013     C* INZSTD - ROUTINE INIZIALE STANDARD
030000971013     C*------------------------------------------------------------------------*
030100971013     C     INZSTD        BEGSR
030200971013     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
030300971013     C                   CALL      'XKEYS'
030400971013     C                   PARM                    DSKEYS
030500971013     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
030600971013     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
030700971013     C     XMSGPA        PLIST
030800971013     C                   PARM                    DSMSMS
030900971013     C                   PARM                    DSMSPG
031000971013     C                   PARM                    DSMSKE
031100971013     C                   PARM                    DSMSER
031200971013     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
031300971013     C                   MOVEL     'TNSDUGR'     DSMSPG                         *NOME PGM CHIAMANTE
031400971013     C                   MOVEL     'TNSDUGR'     VMCQUE                         *CODA MESSAGGI DESTI
031500971013     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
031600971013     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
031700971013     C* INIZIALIZZO FLAG CONTROLLO PGM
031800971013     C                   MOVEL     '0'           $ERR              1            *ERRORE
031900971013     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
032000971013     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
032100971013     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
032200971013     C                   MOVEL     '0'           $FINE             1            *FINE PGM
032300971013     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
032400971013     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
032500971013     C* LANCIO ROUTINE PER VERIFICARE SE
032600971013     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
032700971013     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
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
033900020717
034000020717      * Controllo il profilo utente
034100020717     c                   ExSr      CtrUte
034200020717
034300971013     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
034400971013     C                   BITOFF    '01234567'    POSMI1            1
034500971013     C                   BITOFF    '01234567'    POSMI2            1
034600971013     C                   BITOFF    '01234567'    POSMA1            1
034700971013     C                   BITOFF    '01234567'    POSMA2            1
034800971013     C                   BITON     '34'          POSMI1            1            *18
034900971013     C                   BITON     '7'           POSMI2            1            *01
035000971013     C                   BITON     '34'          POSMA1            1            *18
035100971013     C                   BITON     '0'           POSMA2            1            *80
035200971013     C                   MOVE      POSMI2        POSMIN            2
035300971013     C                   MOVEL     POSMI1        POSMIN
035400971013     C                   MOVE      POSMA2        POSMAX            2
035500971013     C                   MOVEL     POSMA1        POSMAX
035600971013     C                   ENDSR
035700020717      *------------------------------------------------------------------------*
035800020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
035900020717      *------------------------------------------------------------------------*
036000020717     c     CtrUte        BegSr
036100020717
036200020717     c                   Eval      *In76 = *Off
036300020717
036400020717      * Controllo se il profilo è ok da tibs34
036500020717     c                   If        DutErr = 'E'
036600020717     c                   eval      $Msg = DutMsg
036700020717     c                   ExSr      CarMsg
036800020717     c                   eval      *In76 = *On
036900020717     c                   endIf
037000020717
037100020717      * Controllo se il profilo di sede
037200020717     c                   If        DutLpo <> 'S'
037300020717     c                   Eval      $Msg = Msg(10)
037400020717     c                   ExSr      CarMsg
037500020717     c                   Eval      *In76 = *On
037600020717     c                   EndIf
037700020717
037800020717     c                   endSr
037900971013     C*------------------------------------------------------------------------*
038000971013     C* FUNSTD - LANCIA SUBROUTINE STANDARD
038100971013     C*------------------------------------------------------------------------*
038200971013     C     FUNSTD        BEGSR
038300971013     C                   SELECT
038400971013     C     KEY           WHENEQ    F3
038500971013     C                   EXSR      F03STD
038600971013     C     KEY           WHENEQ    F12
038700971013     C                   EXSR      F12STD
038800971013     C     KEY           WHENEQ    F24
038900971013     C                   EXSR      F24STD
039000971013     C                   OTHER
039100971013     C                   MOVEL     MSG(2)        $MSG
039200971013     C                   EXSR      CARMSG
039300971013     C                   ENDSL
039400971013     C                   ENDSR
039500971013     C*------------------------------------------------------------------------*
039600971013     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
039700971013     C*------------------------------------------------------------------------*
039800971013     C     F03STD        BEGSR
039900971013     C*
040000971013     C* IMPOSTO FLAG DI PREMUTO TASTO F03
040100971013     C                   MOVEL     '1'           $F03
040200971013     C*
040300971013     C* IMPOSTO FLAG DI FINE PROGRAMMA
040400971013     C                   MOVEL     '1'           $FINE
040500971013     C*
040600971013     C                   ENDSR
040700971013     C*------------------------------------------------------------------------*
040800971013     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
040900971013     C*------------------------------------------------------------------------*
041000971013     C     F12STD        BEGSR
041100971013     C*
041200971013     C* IMPOSTO FLAG DI FINE PROGRAMMA
041300971013     C                   MOVEL     '1'           $FINE
041400971013     C*
041500971013     C                   ENDSR
041600971013     C*------------------------------------------------------------------------*
041700971013     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
041800971013     C*------------------------------------------------------------------------*
041900971013     C     F24STD        BEGSR
042000971013     C*
042100971013     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
042200971013     C                   EXSR      OUTFUN
042300971013     C*
042400971013     C                   ENDSR
042500971013     C*------------------------------------------------------------------------*
042600971013     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
042700971013     C*          CODA MESSAGGI DEL PGM
042800971013     C*------------------------------------------------------------------------*
042900971013     C     CARMSG        BEGSR
043000971013     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
043100971013     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
043200971013     C     MSGULT        IFEQ      0
043300971013     C                   MOVEL     '1'           *IN40
043400971013     C                   WRITE     SDUGDVK
043500971013     C                   MOVEL     '0'           *IN40
043600971013     C                   ENDIF
043700971013     C* INVIA MESSAGGIO ALLA CODA DEL PGM
043800971013     C                   MOVEL     $MSG          DSMSMS
043900971013     C                   CALL      'XMSG'        XMSGPA
044000971013     C     DSMSER        IFEQ      0
044100971013     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
044200971013     C                   ENDIF
044300971013     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
044400971013     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
044500971013     C                   WRITE     SDUGDVM                                      *SFL MESS
044600971013     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
044700971013     C                   MOVEL     *BLANKS       $MSG
044800971013     C                   ENDSR
044900971013     C*------------------------------------------------------------------------*
045000971013     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
045100971013     C*------------------------------------------------------------------------*
045200971013     C     OUTMSG        BEGSR
045300971013     C* VISUALIZZAZIONE MESSAGGIO
045400971013     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
045500971013     C                   WRITE     SDUGDVK                                      *CONTR MESS
045600971013     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
045700971013     C                   Z-ADD     *ZEROS        MSGULT
045800971013     C                   ENDSR
045900971013     C*------------------------------------------------------------------------*
046000971013     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
046100971013     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
046200971013     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
046300971013     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
046400971013     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
046500971013     C*------------------------------------------------------------------------*
046600971013     C     CARFUN        BEGSR
046700971013     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
046800971013     C     1             DO        31            I                 5 0          --- 1 -->
046900971013     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
047000971013     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
047100971013     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
047200020709     c                   If        DutLpo = 'S'
047300971013     C     $TGES         IFEQ      'G'                                          *GESTIONE
047400971013     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
047500971013     C                   ELSE                                                   *INTERROG.
047600971013     C                   MOVEL     FUNASI        FAT(I)
047700971013     C                   ENDIF
047800971013     C                   ELSE                                                   <-- 2 --> *FILIALE
047900971013     C     $TGES         IFEQ      'G'                                          *GESTIONE
048000971013     C                   MOVEL     FUNAFG        FAT(I)
048100971013     C                   ELSE                                                   *INTERROG.
048200971013     C                   MOVEL     FUNAFI        FAT(I)
048300971013     C                   ENDIF
048400971013     C                   ENDIF                                                  <-- 2 ---
048500971013     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
048600971013     C                   MOVEL     USER          WEDP              3            *PROFILO
048700971013     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
048800971013     C                   MOVEL     'S'           FAT(I)                         *ABILITO
048900971013     C                   ELSE
049000971013     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
049100971013     C                   ENDIF
049200971013     C                   ENDIF
049300971013     C                   ENDDO                                                  <-- 1 ---
049400971013     C                   ENDSR
049500971013     C*------------------------------------------------------------------------*
049600971013     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
049700971013     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
049800971013     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
049900971013     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
050000971013     C*------------------------------------------------------------------------*
050100971013     C     OUTFUN        BEGSR
050200971013     C     1             DO        8             I                              *PULIZIA FUNZIONI A
050300971013     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
050400971013     C                   ENDDO
050500971013     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
050600971013     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
050700971013     C     1             DO        31            I
050800971013     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
050900971013     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
051000971013     C                   ADD       1             FUNNAT
051100971013     C                   ENDIF
051200971013     C                   ENDDO
051300971013     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
051400971013     C     FUNNAT        IFGT      8
051500971013     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
051600971013     C                   ENDIF
051700971013     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
051800971013     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
051900971013     C                   Z-ADD     8             FUNVIS            5 0
052000971013     C     FAT(23)       IFEQ      'S'
052100971013     C                   SUB       1             FUNVIS
052200971013     C                   ENDIF
052300971013     C     FAT(24)       IFEQ      'S'
052400971013     C                   SUB       1             FUNVIS
052500971013     C                   ENDIF
052600971013     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
052700971013     C                   Z-ADD     *ZEROS        W                 5 0
052800971013     C                   ADD       1             FUNULT
052900971013     C     FUNULT        IFGT      31
053000971013     C                   Z-ADD     1             FUNULT
053100971013     C                   ENDIF
053200971013     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
053300971013     C     FUNULT        DO        31            I                              --- 1 -->
053400971013     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
053500971013     C     W             IFLT      FUNVIS                                       --- 2 -->
053600971013     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
053700971013     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
053800971013     C     FDE(I)        ANDNE     *BLANKS
053900971013     C     I             IFEQ      23                                           *TOLGO F23
054000971013     C     I             OREQ      24                                           *E F24
054100971013     C                   ELSE
054200971013     C                   ADD       1             W
054300971013     C                   MOVEL     FDE(I)        FUV(W)
054400971013     C                   ENDIF
054500971013     C                   ENDIF                                                  <-- 3 ---
054600971013     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
054700971013     C                   Z-ADD     I             FUNULT
054800971013     C                   ENDIF                                                  <-- 2 ---
054900971013     C                   ENDDO                                                  <-- 1 ---
055000971013     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
055100971013     C                   MOVEL     FUV(1)        VFD001
055200971013     C                   MOVEL     FUV(2)        VFD002
055300971013     C                   MOVEL     FUV(3)        VFD003
055400971013     C                   MOVEL     FUV(4)        VFD004
055500971013     C                   MOVEL     FUV(5)        VFD005
055600971013     C                   MOVEL     FUV(6)        VFD006
055700971013     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
055800971013     C     FAT(24)       ANDEQ     'N'
055900971013     C                   MOVEL     FUV(7)        VFD007
056000971013     C                   MOVEL     FUV(8)        VFD008
056100971013     C                   ELSE                                                   <-- 1 -->
056200971013     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
056300971013     C     FAT(24)       ANDEQ     'S'
056400971013     C                   MOVEL     FDE(23)       VFD007
056500971013     C                   MOVEL     FDE(24)       VFD008
056600971013     C                   ELSE                                                   <-- 2 -->
056700971013     C                   MOVEL     FUV(7)        VFD007
056800971013     C     FAT(23)       IFEQ      'S'
056900971013     C                   MOVEL     FDE(23)       VFD008
057000971013     C                   ELSE
057100971013     C                   MOVEL     FDE(24)       VFD008
057200971013     C                   ENDIF
057300971013     C                   ENDIF                                                  <-- 2 ---
057400971013     C                   ENDIF                                                  <-- 1 ---
057500971013     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
057600971013     C                   ENDSR
057700971013     C*------------------------------------------------------------------------*
057800971013     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
057900971013     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
058000971013     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
058100971013     C*                   - ERRORE SE FUNZIONE NON ATTIVA
058200971013     C*------------------------------------------------------------------------*
058300971013     C     CONFUN        BEGSR
058400971013     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
058500971013     C     1             DO        31            I                              --- 1 -->
058600971013     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
058700971013     C* CONTROLLO SE IL TASTO E' ATTIVO
058800971013     C     FAT(I)        IFNE      'S'                                          --- 3 -->
058900971013     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
059000971013     C                   EXSR      CARMSG
059100971013     C                   MOVEL     '1'           $ERR
059200971013     C                   ENDIF                                                  <-- 3 ---
059300971013     C                   ENDIF                                                  <-- 2 ---
059400971013     C                   ENDDO                                                  <-- 1 ---
059500971013     C                   ENDSR
059600971013     O*------------------------------------------------------------------------*
059700971013**       MSG - MESSAGGI
059800971013TNSDUGR- Tasto funzionale non valido.                                          1
059900971013TNSDUGR- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
060000971013TNSDUGR- Opzione non valida.                                                   3
060100971013TNSDUGR- Opzione abilitata ma non gestita. Avvertire CED.                      4
060200971013TNSDUGR- Inizio elenco.                                                        5
060300971013TNSDUGR- Fine elenco.                                                          6
060400971013TNSDUGR- Immettere uno dei valori possibili                                    7
060500971013TNSDUGR- Data errata                                                           8
060600971013TNSDUGR- La data AL deve essere maggiore o uguale della data DAL               9
060700020717TNSDUGR- Profilo non abilitato alla funzione                                  10
060800971013TNSDUGR- 11
060900971013TNSDUGR- 12
061000971013TNSDUGR- 13
061100971013TNSDUGR- 14
061200971013TNSDUGR- 15
061300971013TNSDUGR- 16
061400971013TNSDUGR- 17
061500971013TNSDUGR- 18
061600971013TNSDUGR- 19
061700971013TNSDUGR- 20
061800971013** ======== SCHIERA: CMD  (COMANDI)                        ================
061900971013WRKSPLF
062000971013
062100971013
062200971013
062300971013
062400971013** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
062500971013F1    -                   -    NN     NN
062600971013F2    -                   -    NN     NN
062700971013F3    -F3=Fine            -    SS     SS
062800971013F4    -                   -    NN     NN
062900971013F5    -                   -    NN     NN
063000971013F6    -F6=Lancio          -    SS     SS
063100971013F7    -                   -    NN     NN
063200971013F8    -                   -    NN     NN
063300971013F9    -                   -    NN     NN
063400971013F10   -                   -    NN     NN
063500971013F11   -                   -    NN     NN
063600971013F12   -F12-Ritorno        -    SS     SS
063700971013F13   -                   -    NN     NN
063800971013F14   -                   -    NN     NN
063900971013F15   -                   -    NN     NN
064000971013F16   -                   -    NN     NN
064100971013F17   -                   -    NN     NN
064200971013F18   -                   -    NN     NN
064300971013F19   -                   -    NN     NN
064400971013F20   -                   -    NN     NN
064500971013F21   -                   -    NN     NN
064600971013F22   -F22=Gestione stampe-    SS     SS
064700971013F23   -                   -    NN     NN
064800971013F24   -F24=Altri tasti    -    NN     NN
064900971013ENTER -                   -    SS     SS
065000971013ROLLUP-                   -    NN     NN
065100971013ROLLDN-                   -    NN     NN
065200971013HELP  -                   -    NN     NN
065300971013PRINT -                   -    NN     NN
065400971013BSPACE-                   -    NN     NN
065500971013CLEAR -                   -    NN     NN
