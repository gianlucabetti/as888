000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA CLIENTI VAS - RICHIESTA PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNSS20D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(SS20DVM:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI, COMANDI E FUNZIONI
001500000000     D*-------------------
001600020717     D MSG             S             78    DIM(11) CTDATA PERRCD(1)             *MESSAGGI
001700000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001800000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900000000     D*-------------------
002000000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100000000     D*-------------------
002200000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002300000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002400000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500000000     D*-------------------
002600000000     D* TABELLA DESCRIZIONI A VIDEO
002700000000     D*-------------------
002800000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900000000     D*-------------------
003000000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003100000000     D*-------------------
003200000000     D KEYS            S              1    DIM(40)
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D*---------------------
003700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800000000     D*---------------------
003900000000     D DSFUN         E DS
004000000000     D  FUNRIG                 1     40
004100000000     D*---------------------
004200000000     D* DS PER GESTIONE TASTI FUNZIONALI
004300000000     D*---------------------
004400000000     D DSKEYS        E DS
004500000000     D  KEYRIG                 1     40
004600000000     D*---------------------
004700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004800000000     D*---------------------
004900000000     D                SDS
005000000000     D  USER                 254    263
005100000000     D*---------------------
005200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005300000000     D*---------------------
005400000000     D DEVDS1          DS
005500000000     D  KEY                  369    369
005600000000     D  CURSOR               370    371
005700000000     D*---------------------
005800000000     D* ARCHITETTURA
005900000000     D*---------------------
006000000000     D KPJBA         E DS
006100000000     D*---------------------
006200000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006300000000     D*---------------------
006400000000     D DSMSG         E DS
006500020717
006600020717     d DdatiUte      e ds
006700020717     d AzuteDs       e ds                  ExtName(Azute00f)
006800020717     d Tibs34Ds      e ds                  Inz
006900020717
007000000000     D*---------------------
007100000000     D* DS PARAMETRI DI LANCIO
007200000000     D*------------------
007300000000     D PARAM           DS                  INZ
007400000000     D  PARDTI                 1      8  0
007500000000     D  PARDTF                 9     16  0
007600000000     D  PARNBO                17     19  0
007700970919     D  PARNCO                20     22  0
007800970919     D  PARPAR                23     23
007900970919     D  PARSTE                24     24
008000000316     D  PARTRK                25     25
008200010626     D  PARCOM                27     27
008300000000     D*---------------------
008400000000     D* DS CONTROLLO DATA (8)
008500000000     D*------------------
008600000000     D WLBDA8          DS                  INZ
008700000000     D  G08DAT                 1      8  0
008800000000     D  G08INV                 9     16  0
008900000000     D  G08ERR                17     17
009000000000     D  G08TGI                18     22  0
009100000000     C*------------------------------------------------------------------------*
009200000000     C* MAIN LINE
009300000000     C*------------------------------------------------------------------------*
009400000000     C*
009500000000     C* CICLO FINO A CHE:
009600000000DO  1C     $FINE         DOUEQ     '1'
009700000000     C*
009800000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009900000000     C*
010000000000     C* CONTROLLA GESTIONE VIDEO
010100000000IF  2C     $GES          IFEQ      'V1'
010200000000     C                   EXSR      GESV1                                        *PARAMETRI
010300000000E   2C                   ENDIF
010400000000     C*
010500000000E   1C                   ENDDO
010600000000     C*
010700000000     C                   SETON                                        LR
010800000000     C*------------------------------------------------------------------------*
010900000000     C* GESV1 - GESTIONE VIDEO UNO
011000000000     C*------------------------------------------------------------------------*
011100000000     C     GESV1         BEGSR
011200000000     C*
011300000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011400000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011500000000E   1C                   ENDIF
011600000000     C*
011700000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011800000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011900000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012000000000E   1C                   ENDIF
012100000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012200000000     C*
012300000000     C                   EXFMT     SS20DV1                                      *PARAMETRI
012400000000     C*
012500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012600020717
012700020717      * Se Profilo non OK devo uscire dal pgm
012800020717     c                   If        *In76
012900020717     c                   Eval      $err = '1'
013000020717     c                   Eval      $fine = '1'
013100020717     c                   EndIf
013200000000     C*
013300000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013400000000SL  2C                   SELECT
013500000000     C     KEY           WHENEQ    ENTER
013600000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013700000000     C     KEY           WHENEQ    F6
013800000000     C                   EXSR      F06GES                                       *CONFERMA
013900010626     C                   MOVEL     '1'           $FINE
014000000000     C     KEY           WHENEQ    F22
014100000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
014200000000     C                   OTHER
014300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014400000000E   2C                   ENDSL
014500000000E   1C                   ENDIF
014600000000     C*
014700000000     C                   ENDSR
014800000000     C*------------------------------------------------------------------------*
014900000000     C* CARV1 - CARICA VIDEO UNO
015000000000     C*------------------------------------------------------------------------*
015100000000     C     CARV1         BEGSR
015200000000     C*
015300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015400000000     C*
015500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015700000000     C*
015800000000     C                   WRITE     SS20DVT                                      *TESTATA
015900000000     C                   WRITE     SS20DVF                                      *FUNZIONI
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
016400000000     C*------------------------------------------------------------------------*
016500000000     C     ENTV1         BEGSR
016600000000     C*
016700000000     C* CONTROLLA I CAMPI IMMESSI
016800000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
017300000000     C*------------------------------------------------------------------------*
017400000000     C     F06GES        BEGSR
017500000000     C*
017600000000     C* CONTROLLA I CAMPI IMMESSI
017700000000     C                   EXSR      DECODI
017800000000     C*
017900000000     C* LANCIA IL PROGRAMMA DI STAMPA
018000000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
018100000000     C                   Z-ADD     DTIV1C        PARDTI
018200000000     C                   Z-ADD     DTFV1C        PARDTF
018300000000     C                   Z-ADD     V1CNBO        PARNBO
018400970919     C                   Z-ADD     V1NCOL        PARNCO
018500970130     C                   MOVEL     V1PARZ        PARPAR
018600970130     C                   MOVEL     V1STEL        PARSTE
018700110401     C                   clear                   PARTRK
018900010626     C                   MOVEL     V1BCOM        PARCOM
019000000000     C*
019100000000     C                   MOVEL     'SS21'        KCOAZ
019200000000     C                   MOVEL     PARAM         KPJBU
019300070626if  2c                   if        KNMUS = *all'1'
019400070626     c                   call      'TNSS21R'
019500070626     c                   parm                    KPJBA
019600070626x   2c                   else
019700070626     C                   CALL      'BCH10'
019800000000     C                   PARM                    KPJBA
019900070626e   2c                   endif
020000000000     C*
020100000000     C                   MOVEL     MSG(10)       $MSG                           *STAMPA LANCIATA
020200000000E   1C                   ENDIF
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*------------------------------------------------------------------------*
020600000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
020700000000     C*------------------------------------------------------------------------*
020800000000     C     F22GES        BEGSR
020900000000     C*
021000000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
021100000000     C                   Z-ADD     80            LENGH            15 5
021200000000     C                   CALL      'QCAEXEC'
021300000000     C                   PARM                    CMD(1)
021400000000     C                   PARM                    LENGH
021500000000     C*
021600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
022100000000     C*------------------------------------------------------------------------*
022200000000     C     DECODI        BEGSR
022300000000     C*
022400000000     C* REIMPOSTA INDICATORI DI ERRORE
022500000000     C                   SETOFF                                       606162
022600000000     C*--------------------
022700000000     C* DATA INIZIO
022800000000     C*--------------------
022900000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C
023000000000     C                   Z-ADD     *ZEROS        DTIV1C
023100000000IF  1C     $ERR          IFEQ      '0'
023200000000     C                   Z-ADD     V1CDTI        WDA8
023300000000     C                   Z-ADD     *ZEROS        DA8W
023400000000     C                   EXSR      CHKDAT
023500000000     C                   Z-ADD     WDA8          V1CDTI
023600000000     C                   Z-ADD     DA8W          DTIV1C
023700000000IF  2C     $ERR          IFEQ      '1'
023800000000     C                   SETON                                        60
023900000000     C                   MOVEL     MSG(7)        $MSG
024000000000     C                   EXSR      CARMSG
024100000000E   2C                   ENDIF
024200000000E   1C                   ENDIF
024300000000     C*--------------------
024400000000     C* DATA FINE
024500000000     C*--------------------
024600000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C
024700000000     C                   Z-ADD     *ZEROS        DTFV1C
024800000000IF  1C     $ERR          IFEQ      '0'
024900000000     C                   Z-ADD     V1CDTF        WDA8
025000000000     C                   Z-ADD     *ZEROS        DA8W
025100000000     C                   EXSR      CHKDAT
025200000000     C                   Z-ADD     WDA8          V1CDTF
025300000000     C                   Z-ADD     DA8W          DTFV1C
025400000000IF  2C     $ERR          IFEQ      '1'
025500000000     C                   SETON                                        61
025600000000     C                   MOVEL     MSG(7)        $MSG
025700000000     C                   EXSR      CARMSG
025800000000E   2C                   ENDIF
025900000000E   1C                   ENDIF
026000000000     C*--------------------
026100000000     C* NUMERO BOLLE MINIMO GIORNALIERE
026200000000     C*--------------------
026300000000IF  1C     $ERR          IFEQ      '0'
026400000000IF  2C     V1CNBO        IFEQ      *ZEROS
026500970919IF  2C     V1NCOL        OREQ      *ZEROS
026600000000     C                   MOVEL     '1'           $ERR
026700000000     C                   SETON                                        62
026800000000     C                   MOVEL     MSG(8)        $MSG
026900000000     C                   EXSR      CARMSG
027000000000E   2C                   ENDIF
027100000000E   1C                   ENDIF
027200000000     C*
027300000000     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500000000     C* CHKDAT - CONTROLLA LA DATA
027600000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
027700000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
027800000000     C*------------------------------------------------------------------------*
027900000000     C     CHKDAT        BEGSR
028000000000     C*
028100000000     C     *LIKE         DEFINE    V1CDTI        WDA8
028200000000     C     *LIKE         DEFINE    V1CDTI        DA8W
028300000000     C*
028400000000     C                   Z-ADD     *ZEROS        DA8W
028500000000     C*
028600000000     C                   Z-ADD     WDA8          G08DAT
028700000000     C                   Z-ADD     *ZEROS        G08INV
028800000000     C                   MOVEL     '0'           G08ERR
028900000000     C                   CALL      'XSRDA8'
029000000000     C                   PARM                    WLBDA8
029100000000     C                   Z-ADD     G08DAT        WDA8
029200000000IF  1C     G08ERR        IFEQ      '1'
029300000000     C                   MOVEL     '1'           $ERR
029400000000     C                   ELSE
029500000000     C                   Z-ADD     G08INV        DA8W
029600000000E   1C                   ENDIF
029700000000     C*
029800000000     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* IMPVID - IMPOSTAZIONI VIDEO
030100000000     C*------------------------------------------------------------------------*
030200000000     C     IMPVID        BEGSR
030300000000     C*
030400970919     C                   Z-ADD     20            V1CNBO                         *NUMERO BOLLE MINIMO
030500970919     C                   Z-ADD     30            V1NCOL                         *NUMERO BOLLE MINIMO
030600110401     C****               MOVEL     'N'           V1STRK
030800010627     C                   MOVEL     'N'           V1BCOM
030900000000     C*
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C* *INZSR - ROUTINE INIZIALE
031300000000     C*------------------------------------------------------------------------*
031400000000     C     *INZSR        BEGSR
031500000000     C*--------------------
031600000000     C* RICEVIMENTO PARAMETRI
031700000000     C*--------------------
031800000000     C     *ENTRY        PLIST
031900000000     C                   PARM                    KPJBA
032000000000     C*--------------------
032100000000     C* VARIABILI DI CONTROLLO
032200000000     C*--------------------
032300000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
032400000000     C***
032500000000     C* IMPOSTAZIONI VIDEO
032600000000     C***
032700000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
032800000000     C*
032900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
033000000000     C                   EXSR      INZSTD
033100000000     C*
033200000000     C* IMPOSTAZIONI VIDEO
033300000000     C                   EXSR      IMPVID
033400000000     C*
033500000000     C* EMETTE VIDEATA
033600010626     C                   EVAL      *IN14 = *OFF
033700000000     C                   EXSR      CARV1
033800000000     C*
033900000000     C                   ENDSR
034000000000     C*------------------------------------------------------------------------*
034100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034300000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
034400000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
034500000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
034600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034800000000     C*------------------------------------------------------------------------*
034900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
035000000000     C*------------------------------------------------------------------------*
035100000000     C     INZSTD        BEGSR
035200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
035300000000     C                   CALL      'XKEYS'
035400000000     C                   PARM                    DSKEYS
035500000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
035600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
035700000000     C     XMSGPA        PLIST
035800000000     C                   PARM                    DSMSMS
035900000000     C                   PARM                    DSMSPG
036000000000     C                   PARM                    DSMSKE
036100000000     C                   PARM                    DSMSER
036200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
036300000000     C                   MOVEL     'TNSS20R'     DSMSPG                         *NOME PGM CHIAMANTE
036400000000     C                   MOVEL     'TNSS20R'     VMCQUE                         *CODA MESSAGGI DESTI
036500000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
036600000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
036700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
036800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
036900000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
037000000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
037100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
037200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
037300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
037400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
037500000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
037600000000     C* LANCIO ROUTINE PER VERIFICARE SE
037700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
037800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
037900020717
038000020717     c     *dtaara       define    §azute        azuteds
038100020717     c     *dtaara       define    §datiute      ddatiute
038200020717
038300020717     c                   in(E)     *dtaara
038400020717     c                   if        %Error or rsut = *blanks
038500020717     c                   clear                   tibs34ds
038600020717     c                   call      'TIBS34R'
038700020717     c                   parm                    Tibs34Ds
038800020717     c                   in        *dtaara
038900020717     c                   endif
039000020717
039100020717      * Controllo il profilo utente
039200020717     c                   ExSr      CtrUte
039300020717
039400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
039500000000     C                   BITOFF    '01234567'    POSMI1            1
039600000000     C                   BITOFF    '01234567'    POSMI2            1
039700000000     C                   BITOFF    '01234567'    POSMA1            1
039800000000     C                   BITOFF    '01234567'    POSMA2            1
039900000000     C                   BITON     '34'          POSMI1            1            *18
040000000000     C                   BITON     '7'           POSMI2            1            *01
040100000000     C                   BITON     '34'          POSMA1            1            *18
040200000000     C                   BITON     '0'           POSMA2            1            *80
040300000000     C                   MOVE      POSMI2        POSMIN            2
040400000000     C                   MOVEL     POSMI1        POSMIN
040500000000     C                   MOVE      POSMA2        POSMAX            2
040600000000     C                   MOVEL     POSMA1        POSMAX
040700000000     C                   ENDSR
040800020717      *------------------------------------------------------------------------*
040900020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
041000020717      *------------------------------------------------------------------------*
041100020717     c     CtrUte        BegSr
041200020717
041300020717     c                   Eval      *In76 = *Off
041400020717
041500020717      * Controllo se il profilo è ok da tibs34
041600020717     c                   If        DutErr = 'E'
041700020717     c                   Eval      $Msg = DutMsg
041800020717     c                   ExSr      CarMsg
041900020717     c                   Eval      *In76 = *On
042000020717     c                   EndIf
042100020717
042200020717      * Controllo se il profilo di sede
042300020717     c                   If        DutLpo <> 'S'
042400020717     c                   Eval      $Msg = Msg(11)
042500020717     c                   ExSr      CarMsg
042600020717     c                   Eval      *In76 = *On
042700020717     c                   EndIf
042800020717
042900020717     c                   EndSr
043000000000     C*------------------------------------------------------------------------*
043100000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
043200000000     C*------------------------------------------------------------------------*
043300000000     C     FUNSTD        BEGSR
043400000000     C                   SELECT
043500000000     C     KEY           WHENEQ    F3
043600000000     C                   EXSR      F03STD
043700000000     C     KEY           WHENEQ    F12
043800000000     C                   EXSR      F12STD
043900000000     C     KEY           WHENEQ    F24
044000000000     C                   EXSR      F24STD
044100000000     C                   OTHER
044200000000     C                   MOVEL     MSG(2)        $MSG
044300000000     C                   EXSR      CARMSG
044400000000     C                   ENDSL
044500000000     C                   ENDSR
044600000000     C*------------------------------------------------------------------------*
044700000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
044800000000     C*------------------------------------------------------------------------*
044900000000     C     F03STD        BEGSR
045000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
045100000000     C                   MOVEL     '1'           $F03
045200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
045300000000     C                   MOVEL     '1'           $FINE
045400000000     C                   ENDSR
045500000000     C*------------------------------------------------------------------------*
045600000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
045700000000     C*------------------------------------------------------------------------*
045800000000     C     F12STD        BEGSR
045900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
046000000000     C                   MOVEL     '1'           $F12
046100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
046200000000     C                   MOVEL     '1'           $FINE
046300000000     C                   ENDSR
046400000000     C*------------------------------------------------------------------------*
046500000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
046600000000     C*------------------------------------------------------------------------*
046700000000     C     F24STD        BEGSR
046800000000     C*
046900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
047000000000     C                   EXSR      OUTFUN
047100000000     C*
047200000000     C                   ENDSR
047300000000     C*------------------------------------------------------------------------*
047400000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
047500000000     C*          CODA MESSAGGI DEL PGM
047600000000     C*------------------------------------------------------------------------*
047700000000     C     CARMSG        BEGSR
047800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
047900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
048000000000     C     MSGULT        IFEQ      0
048100000000     C                   MOVEL     '1'           *IN40
048200000000     C                   WRITE     SS20DVK
048300000000     C                   MOVEL     '0'           *IN40
048400000000     C                   ENDIF
048500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
048600000000     C                   MOVEL     $MSG          DSMSMS
048700000000     C                   CALL      'XMSG'        XMSGPA
048800000000     C     DSMSER        IFEQ      0
048900000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
049000000000     C                   ENDIF
049100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
049200000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
049300000000     C                   WRITE     SS20DVM                                      *SFL MESS
049400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
049500000000     C                   MOVEL     *BLANKS       $MSG
049600000000     C                   ENDSR
049700000000     C*------------------------------------------------------------------------*
049800000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
049900000000     C*------------------------------------------------------------------------*
050000000000     C     OUTMSG        BEGSR
050100000000     C* VISUALIZZAZIONE MESSAGGIO
050200000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
050300000000     C                   WRITE     SS20DVK                                      *CONTR MESS
050400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
050500000000     C                   Z-ADD     *ZEROS        MSGULT
050600000000     C                   ENDSR
050700000000     C*------------------------------------------------------------------------*
050800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
050900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
051000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
051100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
051200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
051300000000     C*------------------------------------------------------------------------*
051400000000     C     CARFUN        BEGSR
051500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
051600000000     C     1             DO        31            I                 5 0          --- 1 -->
051700000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
051800000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
051900000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
052000020717     c                   If        DutLpo = 'S'
052100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
052200000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
052300000000     C                   ELSE                                                   *INTERROG.
052400000000     C                   MOVEL     FUNASI        FAT(I)
052500000000     C                   ENDIF
052600000000     C                   ELSE                                                   <-- 2 --> *FILIALE
052700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
052800000000     C                   MOVEL     FUNAFG        FAT(I)
052900000000     C                   ELSE                                                   *INTERROG.
053000000000     C                   MOVEL     FUNAFI        FAT(I)
053100000000     C                   ENDIF
053200000000     C                   ENDIF                                                  <-- 2 ---
053300000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
053400000000     C                   MOVEL     USER          WEDP              3            *PROFILO
053500000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
053600010626     C                   EVAL      *IN14 = *ON
053700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
053800000000     C                   ELSE
053900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
054000000000     C                   ENDIF
054100000000     C                   ENDIF
054200000000     C                   ENDDO                                                  <-- 1 ---
054300000000     C                   ENDSR
054400000000     C*------------------------------------------------------------------------*
054500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
054600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
054700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
054800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
054900000000     C*------------------------------------------------------------------------*
055000000000     C     OUTFUN        BEGSR
055100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
055200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
055300000000     C                   ENDDO
055400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
055500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
055600000000     C     1             DO        31            I
055700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
055800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
055900000000     C                   ADD       1             FUNNAT
056000000000     C                   ENDIF
056100000000     C                   ENDDO
056200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
056300000000     C     FUNNAT        IFGT      8
056400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
056500000000     C                   ENDIF
056600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
056700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
056800000000     C                   Z-ADD     8             FUNVIS            5 0
056900000000     C     FAT(23)       IFEQ      'S'
057000000000     C                   SUB       1             FUNVIS
057100000000     C                   ENDIF
057200000000     C     FAT(24)       IFEQ      'S'
057300000000     C                   SUB       1             FUNVIS
057400000000     C                   ENDIF
057500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
057600000000     C                   Z-ADD     *ZEROS        W                 5 0
057700000000     C                   ADD       1             FUNULT
057800000000     C     FUNULT        IFGT      31
057900000000     C                   Z-ADD     1             FUNULT
058000000000     C                   ENDIF
058100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
058200000000     C     FUNULT        DO        31            I                              --- 1 -->
058300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
058400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
058500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
058600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
058700000000     C     FDE(I)        ANDNE     *BLANKS
058800000000     C     I             IFEQ      23                                           *TOLGO F23
058900000000     C     I             OREQ      24                                           *E F24
059000000000     C                   ELSE
059100000000     C                   ADD       1             W
059200000000     C                   MOVEL     FDE(I)        FUV(W)
059300000000     C                   ENDIF
059400000000     C                   ENDIF                                                  <-- 3 ---
059500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
059600000000     C                   Z-ADD     I             FUNULT
059700000000     C                   ENDIF                                                  <-- 2 ---
059800000000     C                   ENDDO                                                  <-- 1 ---
059900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
060000000000     C                   MOVEL     FUV(1)        VFD001
060100000000     C                   MOVEL     FUV(2)        VFD002
060200000000     C                   MOVEL     FUV(3)        VFD003
060300000000     C                   MOVEL     FUV(4)        VFD004
060400000000     C                   MOVEL     FUV(5)        VFD005
060500000000     C                   MOVEL     FUV(6)        VFD006
060600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
060700000000     C     FAT(24)       ANDEQ     'N'
060800000000     C                   MOVEL     FUV(7)        VFD007
060900000000     C                   MOVEL     FUV(8)        VFD008
061000000000     C                   ELSE                                                   <-- 1 -->
061100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
061200000000     C     FAT(24)       ANDEQ     'S'
061300000000     C                   MOVEL     FDE(23)       VFD007
061400000000     C                   MOVEL     FDE(24)       VFD008
061500000000     C                   ELSE                                                   <-- 2 -->
061600000000     C                   MOVEL     FUV(7)        VFD007
061700000000     C     FAT(23)       IFEQ      'S'
061800000000     C                   MOVEL     FDE(23)       VFD008
061900000000     C                   ELSE
062000000000     C                   MOVEL     FDE(24)       VFD008
062100000000     C                   ENDIF
062200000000     C                   ENDIF                                                  <-- 2 ---
062300000000     C                   ENDIF                                                  <-- 1 ---
062400000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
062500000000     C                   ENDSR
062600000000     C*------------------------------------------------------------------------*
062700000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
062800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
062900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
063000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
063100000000     C*------------------------------------------------------------------------*
063200000000     C     CONFUN        BEGSR
063300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
063400000000     C     1             DO        31            I                              --- 1 -->
063500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
063600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
063700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
063800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
063900000000     C                   EXSR      CARMSG
064000000000     C                   MOVEL     '1'           $ERR
064100000000     C                   ENDIF                                                  <-- 3 ---
064200000000     C                   ENDIF                                                  <-- 2 ---
064300000000     C                   ENDDO                                                  <-- 1 ---
064400000000     C                   ENDSR
064500000000     O*------------------------------------------------------------------------*
064600000000** MSG - (MESSAGGI)
064700000000TNSS20R- Tasto funzionale non valido.                                          1
064800000000TNSS20R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
064900000000TNSS20R- Opzione non valida.                                                   3
065000000000TNSS20R- Opzione abilitata ma non gestita. Avvertire CED.                      4
065100000000TNSS20R- Inizio elenco.                                                        5
065200000000TNSS20R- Fine elenco.                                                          6
065300000000TNSS20R- Data errata                                                           7
065400000000TNSS20R- Numero minimo bolle giornaliere errato                                8
065500021001                                                                               9
065600000000TNSS20R- Stampa lanciata ....                                                 10
065700020717TNSS20R- Profilo non abilitato alla funzione                                  11
065800000000** CMD  (COMANDI)
065900000000WRKSPLF
066000000000
066100000000
066200000000
066300000000
066400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
066500000000F1    -                   -    NN     NN
066600000000F2    -                   -    NN     NN
066700000000F3    -F3=Fine            -    SS     SS
066800000000F4    -                   -    NN     NN
066900000000F5    -                   -    NN     NN
067000000000F6    -F6=Conferma        -    SS     SS
067100000000F7    -                   -    NN     NN
067200000000F8    -                   -    NN     NN
067300000000F9    -                   -    NN     NN
067400000000F10   -                   -    NN     NN
067500000000F11   -                   -    NN     NN
067600000000F12   -F12=Ritorno        -    SS     SS
067700000000F13   -                   -    NN     NN
067800000000F14   -                   -    NN     NN
067900000000F15   -                   -    NN     NN
068000000000F16   -                   -    NN     NN
068100000000F17   -                   -    NN     NN
068200000000F18   -                   -    NN     NN
068300000000F19   -                   -    NN     NN
068400000000F20   -                   -    NN     NN
068500000000F21   -                   -    NN     NN
068600000000F22   -F22=Gestione stampe-    EE     EE
068700000000F23   -                   -    NN     NN
068800000000F24   -F24=Altri tasti    -    NN     NN
068900000000ENTER -                   -    SS     SS
069000000000ROLLUP-                   -    NN     NN
069100000000ROLLDN-                   -    NN     NN
069200000000HELP  -                   -    NN     NN
069300000000PRINT -                   -    NN     NN
069400000000BSPACE-                   -    NN     NN
069500000000CLEAR -                   -    NN     NN
