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
008100010626     D  PARCOM                27     27
008200000000     D*---------------------
008300000000     D* DS CONTROLLO DATA (8)
008400000000     D*------------------
008500000000     D WLBDA8          DS                  INZ
008600000000     D  G08DAT                 1      8  0
008700000000     D  G08INV                 9     16  0
008800000000     D  G08ERR                17     17
008900000000     D  G08TGI                18     22  0
009000000000     C*------------------------------------------------------------------------*
009100000000     C* MAIN LINE
009200000000     C*------------------------------------------------------------------------*
009300000000     C*
009400000000     C* CICLO FINO A CHE:
009500000000DO  1C     $FINE         DOUEQ     '1'
009600000000     C*
009700000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009800000000     C*
009900000000     C* CONTROLLA GESTIONE VIDEO
010000000000IF  2C     $GES          IFEQ      'V1'
010100000000     C                   EXSR      GESV1                                        *PARAMETRI
010200000000E   2C                   ENDIF
010300000000     C*
010400000000E   1C                   ENDDO
010500000000     C*
010600000000     C                   SETON                                        LR
010700000000     C*------------------------------------------------------------------------*
010800000000     C* GESV1 - GESTIONE VIDEO UNO
010900000000     C*------------------------------------------------------------------------*
011000000000     C     GESV1         BEGSR
011100000000     C*
011200000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011300000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011400000000E   1C                   ENDIF
011500000000     C*
011600000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011700000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011800000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011900000000E   1C                   ENDIF
012000000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012100000000     C*
012200000000     C                   EXFMT     SS20DV1                                      *PARAMETRI
012300000000     C*
012400000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012500020717
012600020717      * Se Profilo non OK devo uscire dal pgm
012700020717     c                   If        *In76
012800020717     c                   Eval      $err = '1'
012900020717     c                   Eval      $fine = '1'
013000020717     c                   EndIf
013100000000     C*
013200000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013300000000SL  2C                   SELECT
013400000000     C     KEY           WHENEQ    ENTER
013500000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013600000000     C     KEY           WHENEQ    F6
013700000000     C                   EXSR      F06GES                                       *CONFERMA
013800010626     C                   MOVEL     '1'           $FINE
013900000000     C     KEY           WHENEQ    F22
014000000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
014100000000     C                   OTHER
014200000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014300000000E   2C                   ENDSL
014400000000E   1C                   ENDIF
014500000000     C*
014600000000     C                   ENDSR
014700000000     C*------------------------------------------------------------------------*
014800000000     C* CARV1 - CARICA VIDEO UNO
014900000000     C*------------------------------------------------------------------------*
015000000000     C     CARV1         BEGSR
015100000000     C*
015200000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015300000000     C*
015400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015600000000     C*
015700000000     C                   WRITE     SS20DVT                                      *TESTATA
015800000000     C                   WRITE     SS20DVF                                      *FUNZIONI
015900000000     C*
016000000000     C                   ENDSR
016100000000     C*------------------------------------------------------------------------*
016200000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
016300000000     C*------------------------------------------------------------------------*
016400000000     C     ENTV1         BEGSR
016500000000     C*
016600000000     C* CONTROLLA I CAMPI IMMESSI
016700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016800000000     C*
016900000000     C                   ENDSR
017000000000     C*------------------------------------------------------------------------*
017100000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
017200000000     C*------------------------------------------------------------------------*
017300000000     C     F06GES        BEGSR
017400000000     C*
017500000000     C* CONTROLLA I CAMPI IMMESSI
017600000000     C                   EXSR      DECODI
017700000000     C*
017800000000     C* LANCIA IL PROGRAMMA DI STAMPA
017900000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
018000000000     C                   Z-ADD     DTIV1C        PARDTI
018100000000     C                   Z-ADD     DTFV1C        PARDTF
018200000000     C                   Z-ADD     V1CNBO        PARNBO
018300970919     C                   Z-ADD     V1NCOL        PARNCO
018400970130     C                   MOVEL     V1PARZ        PARPAR
018500970130     C                   MOVEL     V1STEL        PARSTE
018600110401     C                   clear                   PARTRK
018700010626     C                   MOVEL     V1BCOM        PARCOM
018800000000     C*
018900000000     C                   MOVEL     'SS21'        KCOAZ
019000000000     C                   MOVEL     PARAM         KPJBU
019100070626if  2c                   if        KNMUS = *all'1'
019200070626     c                   call      'TNSS21R'
019300070626     c                   parm                    KPJBA
019400070626x   2c                   else
019500070626     C                   CALL      'BCH10'
019600000000     C                   PARM                    KPJBA
019700070626e   2c                   endif
019800000000     C*
019900000000     C                   MOVEL     MSG(10)       $MSG                           *STAMPA LANCIATA
020000000000E   1C                   ENDIF
020100000000     C*
020200000000     C                   ENDSR
020300000000     C*------------------------------------------------------------------------*
020400000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
020500000000     C*------------------------------------------------------------------------*
020600000000     C     F22GES        BEGSR
020700000000     C*
020800000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
020900000000     C                   Z-ADD     80            LENGH            15 5
021000000000     C                   CALL      'QCAEXEC'
021100000000     C                   PARM                    CMD(1)
021200000000     C                   PARM                    LENGH
021300000000     C*
021400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
021900000000     C*------------------------------------------------------------------------*
022000000000     C     DECODI        BEGSR
022100000000     C*
022200000000     C* REIMPOSTA INDICATORI DI ERRORE
022300000000     C                   SETOFF                                       606162
022400000000     C*--------------------
022500000000     C* DATA INIZIO
022600000000     C*--------------------
022700000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C
022800000000     C                   Z-ADD     *ZEROS        DTIV1C
022900000000IF  1C     $ERR          IFEQ      '0'
023000000000     C                   Z-ADD     V1CDTI        WDA8
023100000000     C                   Z-ADD     *ZEROS        DA8W
023200000000     C                   EXSR      CHKDAT
023300000000     C                   Z-ADD     WDA8          V1CDTI
023400000000     C                   Z-ADD     DA8W          DTIV1C
023500000000IF  2C     $ERR          IFEQ      '1'
023600000000     C                   SETON                                        60
023700000000     C                   MOVEL     MSG(7)        $MSG
023800000000     C                   EXSR      CARMSG
023900000000E   2C                   ENDIF
024000000000E   1C                   ENDIF
024100000000     C*--------------------
024200000000     C* DATA FINE
024300000000     C*--------------------
024400000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C
024500000000     C                   Z-ADD     *ZEROS        DTFV1C
024600000000IF  1C     $ERR          IFEQ      '0'
024700000000     C                   Z-ADD     V1CDTF        WDA8
024800000000     C                   Z-ADD     *ZEROS        DA8W
024900000000     C                   EXSR      CHKDAT
025000000000     C                   Z-ADD     WDA8          V1CDTF
025100000000     C                   Z-ADD     DA8W          DTFV1C
025200000000IF  2C     $ERR          IFEQ      '1'
025300000000     C                   SETON                                        61
025400140702     C                   MOVEL     MSG(7)        $MSG
025500000000     C                   EXSR      CARMSG
025600000000E   2C                   ENDIF
025700000000E   1C                   ENDIF
025800140702     c* data fine deve essere > data inizio
025900140702IF  1C     $ERR          IFEQ      '0'
026000140702     c                   if        dtfv1c<dtiv1c
026100140702     c                   eval      $err='1'
026200140702     C                   SETON                                        61
026300140702     C                   MOVEL     MSG(9)        $MSG
026400140702     C                   EXSR      CARMSG
026500140702E   2C                   ENDIF
026600140702E   1C                   ENDIF
026700000000     C*--------------------
026800000000     C* NUMERO BOLLE MINIMO GIORNALIERE
026900000000     C*--------------------
027000000000IF  1C     $ERR          IFEQ      '0'
027100000000IF  2C     V1CNBO        IFEQ      *ZEROS
027200970919IF  2C     V1NCOL        OREQ      *ZEROS
027300000000     C                   MOVEL     '1'           $ERR
027400000000     C                   SETON                                        62
027500000000     C                   MOVEL     MSG(8)        $MSG
027600000000     C                   EXSR      CARMSG
027700000000E   2C                   ENDIF
027800000000E   1C                   ENDIF
027900000000     C*
028000000000     C                   ENDSR
028100000000     C*------------------------------------------------------------------------*
028200000000     C* CHKDAT - CONTROLLA LA DATA
028300000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
028400000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
028500000000     C*------------------------------------------------------------------------*
028600000000     C     CHKDAT        BEGSR
028700000000     C*
028800000000     C     *LIKE         DEFINE    V1CDTI        WDA8
028900000000     C     *LIKE         DEFINE    V1CDTI        DA8W
029000000000     C*
029100000000     C                   Z-ADD     *ZEROS        DA8W
029200000000     C*
029300000000     C                   Z-ADD     WDA8          G08DAT
029400000000     C                   Z-ADD     *ZEROS        G08INV
029500000000     C                   MOVEL     '0'           G08ERR
029600000000     C                   CALL      'XSRDA8'
029700000000     C                   PARM                    WLBDA8
029800000000     C                   Z-ADD     G08DAT        WDA8
029900000000IF  1C     G08ERR        IFEQ      '1'
030000000000     C                   MOVEL     '1'           $ERR
030100000000     C                   ELSE
030200000000     C                   Z-ADD     G08INV        DA8W
030300000000E   1C                   ENDIF
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700000000     C* IMPVID - IMPOSTAZIONI VIDEO
030800000000     C*------------------------------------------------------------------------*
030900000000     C     IMPVID        BEGSR
031000000000     C*
031100970919     C                   Z-ADD     20            V1CNBO                         *NUMERO BOLLE MINIMO
031200970919     C                   Z-ADD     30            V1NCOL                         *NUMERO BOLLE MINIMO
031300110401     C****               MOVEL     'N'           V1STRK
031400010627     C                   MOVEL     'N'           V1BCOM
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*------------------------------------------------------------------------*
031800000000     C* *INZSR - ROUTINE INIZIALE
031900000000     C*------------------------------------------------------------------------*
032000000000     C     *INZSR        BEGSR
032100000000     C*--------------------
032200000000     C* RICEVIMENTO PARAMETRI
032300000000     C*--------------------
032400000000     C     *ENTRY        PLIST
032500000000     C                   PARM                    KPJBA
032600000000     C*--------------------
032700000000     C* VARIABILI DI CONTROLLO
032800000000     C*--------------------
032900000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
033000000000     C***
033100000000     C* IMPOSTAZIONI VIDEO
033200000000     C***
033300000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
033400000000     C*
033500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
033600000000     C                   EXSR      INZSTD
033700000000     C*
033800000000     C* IMPOSTAZIONI VIDEO
033900000000     C                   EXSR      IMPVID
034000000000     C*
034100000000     C* EMETTE VIDEATA
034200010626     C                   EVAL      *IN14 = *OFF
034300000000     C                   EXSR      CARV1
034400000000     C*
034500000000     C                   ENDSR
034600000000     C*------------------------------------------------------------------------*
034700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
035000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
035100000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
035200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035400000000     C*------------------------------------------------------------------------*
035500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
035600000000     C*------------------------------------------------------------------------*
035700000000     C     INZSTD        BEGSR
035800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
035900000000     C                   CALL      'XKEYS'
036000000000     C                   PARM                    DSKEYS
036100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
036200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
036300000000     C     XMSGPA        PLIST
036400000000     C                   PARM                    DSMSMS
036500000000     C                   PARM                    DSMSPG
036600000000     C                   PARM                    DSMSKE
036700000000     C                   PARM                    DSMSER
036800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
036900000000     C                   MOVEL     'TNSS20R'     DSMSPG                         *NOME PGM CHIAMANTE
037000000000     C                   MOVEL     'TNSS20R'     VMCQUE                         *CODA MESSAGGI DESTI
037100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
037200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
037300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
037400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
037500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
037600000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
037700000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
037800000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
037900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
038000000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
038100000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
038200000000     C* LANCIO ROUTINE PER VERIFICARE SE
038300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
038400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
038500020717
038600020717     c     *dtaara       define    §azute        azuteds
038700020717     c     *dtaara       define    §datiute      ddatiute
038800020717
038900020717     c                   in(E)     *dtaara
039000020717     c                   if        %Error or rsut = *blanks
039100020717     c                   clear                   tibs34ds
039200020717     c                   call      'TIBS34R'
039300020717     c                   parm                    Tibs34Ds
039400020717     c                   in        *dtaara
039500020717     c                   endif
039600020717
039700020717      * Controllo il profilo utente
039800020717     c                   ExSr      CtrUte
039900020717
040000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
040100000000     C                   BITOFF    '01234567'    POSMI1            1
040200000000     C                   BITOFF    '01234567'    POSMI2            1
040300000000     C                   BITOFF    '01234567'    POSMA1            1
040400000000     C                   BITOFF    '01234567'    POSMA2            1
040500000000     C                   BITON     '34'          POSMI1            1            *18
040600000000     C                   BITON     '7'           POSMI2            1            *01
040700000000     C                   BITON     '34'          POSMA1            1            *18
040800000000     C                   BITON     '0'           POSMA2            1            *80
040900000000     C                   MOVE      POSMI2        POSMIN            2
041000000000     C                   MOVEL     POSMI1        POSMIN
041100000000     C                   MOVE      POSMA2        POSMAX            2
041200000000     C                   MOVEL     POSMA1        POSMAX
041300000000     C                   ENDSR
041400020717      *------------------------------------------------------------------------*
041500020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
041600020717      *------------------------------------------------------------------------*
041700020717     c     CtrUte        BegSr
041800020717
041900020717     c                   Eval      *In76 = *Off
042000020717
042100020717      * Controllo se il profilo è ok da tibs34
042200020717     c                   If        DutErr = 'E'
042300020717     c                   Eval      $Msg = DutMsg
042400020717     c                   ExSr      CarMsg
042500020717     c                   Eval      *In76 = *On
042600020717     c                   EndIf
042700020717
042800020717      * Controllo se il profilo di sede
042900020717     c                   If        DutLpo <> 'S'
043000020717     c                   Eval      $Msg = Msg(11)
043100020717     c                   ExSr      CarMsg
043200020717     c                   Eval      *In76 = *On
043300020717     c                   EndIf
043400020717
043500020717     c                   EndSr
043600000000     C*------------------------------------------------------------------------*
043700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
043800000000     C*------------------------------------------------------------------------*
043900000000     C     FUNSTD        BEGSR
044000000000     C                   SELECT
044100000000     C     KEY           WHENEQ    F3
044200000000     C                   EXSR      F03STD
044300000000     C     KEY           WHENEQ    F12
044400000000     C                   EXSR      F12STD
044500000000     C     KEY           WHENEQ    F24
044600000000     C                   EXSR      F24STD
044700000000     C                   OTHER
044800000000     C                   MOVEL     MSG(2)        $MSG
044900000000     C                   EXSR      CARMSG
045000000000     C                   ENDSL
045100000000     C                   ENDSR
045200000000     C*------------------------------------------------------------------------*
045300000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
045400000000     C*------------------------------------------------------------------------*
045500000000     C     F03STD        BEGSR
045600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
045700000000     C                   MOVEL     '1'           $F03
045800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
045900000000     C                   MOVEL     '1'           $FINE
046000000000     C                   ENDSR
046100000000     C*------------------------------------------------------------------------*
046200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
046300000000     C*------------------------------------------------------------------------*
046400000000     C     F12STD        BEGSR
046500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
046600000000     C                   MOVEL     '1'           $F12
046700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
046800000000     C                   MOVEL     '1'           $FINE
046900000000     C                   ENDSR
047000000000     C*------------------------------------------------------------------------*
047100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
047200000000     C*------------------------------------------------------------------------*
047300000000     C     F24STD        BEGSR
047400000000     C*
047500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
047600000000     C                   EXSR      OUTFUN
047700000000     C*
047800000000     C                   ENDSR
047900000000     C*------------------------------------------------------------------------*
048000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
048100000000     C*          CODA MESSAGGI DEL PGM
048200000000     C*------------------------------------------------------------------------*
048300000000     C     CARMSG        BEGSR
048400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
048500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
048600000000     C     MSGULT        IFEQ      0
048700000000     C                   MOVEL     '1'           *IN40
048800000000     C                   WRITE     SS20DVK
048900000000     C                   MOVEL     '0'           *IN40
049000000000     C                   ENDIF
049100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
049200000000     C                   MOVEL     $MSG          DSMSMS
049300000000     C                   CALL      'XMSG'        XMSGPA
049400000000     C     DSMSER        IFEQ      0
049500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
049600000000     C                   ENDIF
049700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
049800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
049900000000     C                   WRITE     SS20DVM                                      *SFL MESS
050000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
050100000000     C                   MOVEL     *BLANKS       $MSG
050200000000     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
050500000000     C*------------------------------------------------------------------------*
050600000000     C     OUTMSG        BEGSR
050700000000     C* VISUALIZZAZIONE MESSAGGIO
050800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
050900000000     C                   WRITE     SS20DVK                                      *CONTR MESS
051000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
051100000000     C                   Z-ADD     *ZEROS        MSGULT
051200000000     C                   ENDSR
051300000000     C*------------------------------------------------------------------------*
051400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
051500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
051600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
051700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
051800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
051900000000     C*------------------------------------------------------------------------*
052000000000     C     CARFUN        BEGSR
052100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
052200000000     C     1             DO        31            I                 5 0          --- 1 -->
052300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
052400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
052500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
052600020717     c                   If        DutLpo = 'S'
052700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
052800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
052900000000     C                   ELSE                                                   *INTERROG.
053000000000     C                   MOVEL     FUNASI        FAT(I)
053100000000     C                   ENDIF
053200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
053300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
053400000000     C                   MOVEL     FUNAFG        FAT(I)
053500000000     C                   ELSE                                                   *INTERROG.
053600000000     C                   MOVEL     FUNAFI        FAT(I)
053700000000     C                   ENDIF
053800000000     C                   ENDIF                                                  <-- 2 ---
053900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
054000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
054100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
054200010626     C                   EVAL      *IN14 = *ON
054300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
054400000000     C                   ELSE
054500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
054600000000     C                   ENDIF
054700000000     C                   ENDIF
054800000000     C                   ENDDO                                                  <-- 1 ---
054900000000     C                   ENDSR
055000000000     C*------------------------------------------------------------------------*
055100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
055200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
055300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
055400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
055500000000     C*------------------------------------------------------------------------*
055600000000     C     OUTFUN        BEGSR
055700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
055800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
055900000000     C                   ENDDO
056000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
056100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
056200000000     C     1             DO        31            I
056300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
056400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
056500000000     C                   ADD       1             FUNNAT
056600000000     C                   ENDIF
056700000000     C                   ENDDO
056800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
056900000000     C     FUNNAT        IFGT      8
057000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
057100000000     C                   ENDIF
057200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
057300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
057400000000     C                   Z-ADD     8             FUNVIS            5 0
057500000000     C     FAT(23)       IFEQ      'S'
057600000000     C                   SUB       1             FUNVIS
057700000000     C                   ENDIF
057800000000     C     FAT(24)       IFEQ      'S'
057900000000     C                   SUB       1             FUNVIS
058000000000     C                   ENDIF
058100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
058200000000     C                   Z-ADD     *ZEROS        W                 5 0
058300000000     C                   ADD       1             FUNULT
058400000000     C     FUNULT        IFGT      31
058500000000     C                   Z-ADD     1             FUNULT
058600000000     C                   ENDIF
058700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
058800000000     C     FUNULT        DO        31            I                              --- 1 -->
058900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
059000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
059100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
059200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
059300000000     C     FDE(I)        ANDNE     *BLANKS
059400000000     C     I             IFEQ      23                                           *TOLGO F23
059500000000     C     I             OREQ      24                                           *E F24
059600000000     C                   ELSE
059700000000     C                   ADD       1             W
059800000000     C                   MOVEL     FDE(I)        FUV(W)
059900000000     C                   ENDIF
060000000000     C                   ENDIF                                                  <-- 3 ---
060100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
060200000000     C                   Z-ADD     I             FUNULT
060300000000     C                   ENDIF                                                  <-- 2 ---
060400000000     C                   ENDDO                                                  <-- 1 ---
060500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
060600000000     C                   MOVEL     FUV(1)        VFD001
060700000000     C                   MOVEL     FUV(2)        VFD002
060800000000     C                   MOVEL     FUV(3)        VFD003
060900000000     C                   MOVEL     FUV(4)        VFD004
061000000000     C                   MOVEL     FUV(5)        VFD005
061100000000     C                   MOVEL     FUV(6)        VFD006
061200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
061300000000     C     FAT(24)       ANDEQ     'N'
061400000000     C                   MOVEL     FUV(7)        VFD007
061500000000     C                   MOVEL     FUV(8)        VFD008
061600000000     C                   ELSE                                                   <-- 1 -->
061700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
061800000000     C     FAT(24)       ANDEQ     'S'
061900000000     C                   MOVEL     FDE(23)       VFD007
062000000000     C                   MOVEL     FDE(24)       VFD008
062100000000     C                   ELSE                                                   <-- 2 -->
062200000000     C                   MOVEL     FUV(7)        VFD007
062300000000     C     FAT(23)       IFEQ      'S'
062400000000     C                   MOVEL     FDE(23)       VFD008
062500000000     C                   ELSE
062600000000     C                   MOVEL     FDE(24)       VFD008
062700000000     C                   ENDIF
062800000000     C                   ENDIF                                                  <-- 2 ---
062900000000     C                   ENDIF                                                  <-- 1 ---
063000000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
063100000000     C                   ENDSR
063200000000     C*------------------------------------------------------------------------*
063300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
063400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
063500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
063600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
063700000000     C*------------------------------------------------------------------------*
063800000000     C     CONFUN        BEGSR
063900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
064000000000     C     1             DO        31            I                              --- 1 -->
064100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
064200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
064300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
064400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
064500000000     C                   EXSR      CARMSG
064600000000     C                   MOVEL     '1'           $ERR
064700000000     C                   ENDIF                                                  <-- 3 ---
064800000000     C                   ENDIF                                                  <-- 2 ---
064900000000     C                   ENDDO                                                  <-- 1 ---
065000000000     C                   ENDSR
065100000000     O*------------------------------------------------------------------------*
065200000000** MSG - (MESSAGGI)
065300000000TNSS20R- Tasto funzionale non valido.                                          1
065400000000TNSS20R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
065500000000TNSS20R- Opzione non valida.                                                   3
065600000000TNSS20R- Opzione abilitata ma non gestita. Avvertire CED.                      4
065700000000TNSS20R- Inizio elenco.                                                        5
065800000000TNSS20R- Fine elenco.                                                          6
065900000000TNSS20R- Data errata                                                           7
066000000000TNSS20R- Numero minimo bolle giornaliere errato                                8
066100140702Data fine deve essere maggiore o uguale alla data inizio                       9
066200000000TNSS20R- Stampa lanciata ....                                                 10
066300020717TNSS20R- Profilo non abilitato alla funzione                                  11
066400000000** CMD  (COMANDI)
066500000000WRKSPLF
066600000000
066700000000
066800000000
066900000000
067000000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
067100000000F1    -                   -    NN     NN
067200000000F2    -                   -    NN     NN
067300000000F3    -F3=Fine            -    SS     SS
067400000000F4    -                   -    NN     NN
067500000000F5    -                   -    NN     NN
067600000000F6    -F6=Conferma        -    SS     SS
067700000000F7    -                   -    NN     NN
067800000000F8    -                   -    NN     NN
067900000000F9    -                   -    NN     NN
068000000000F10   -                   -    NN     NN
068100000000F11   -                   -    NN     NN
068200000000F12   -F12=Ritorno        -    SS     SS
068300000000F13   -                   -    NN     NN
068400000000F14   -                   -    NN     NN
068500000000F15   -                   -    NN     NN
068600000000F16   -                   -    NN     NN
068700000000F17   -                   -    NN     NN
068800000000F18   -                   -    NN     NN
068900000000F19   -                   -    NN     NN
069000000000F20   -                   -    NN     NN
069100000000F21   -                   -    NN     NN
069200000000F22   -F22=Gestione stampe-    EE     EE
069300000000F23   -                   -    NN     NN
069400000000F24   -F24=Altri tasti    -    NN     NN
069500000000ENTER -                   -    SS     SS
069600000000ROLLUP-                   -    NN     NN
069700000000ROLLDN-                   -    NN     NN
069800000000HELP  -                   -    NN     NN
069900000000PRINT -                   -    NN     NN
070000000000BSPACE-                   -    NN     NN
070100000000CLEAR -                   -    NN     NN
