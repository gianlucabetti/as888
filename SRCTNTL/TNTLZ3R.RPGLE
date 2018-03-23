000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300110506     H* GESTIONE TRAINI - punti scambio  - RICHIESTA PARAMETRI                        *
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800110506     FTNTLZ3D   CF   E             WORKSTN INFDS(DEVDS1)
000900110506     F                                     SFILE(TLZ3DVM:MSGULT)
001000000000     FTNTLT01L  UF A E           K DISK
001100000000     F                                     COMMIT
001200110506     FTNTLs01L  uF A E           K DISK    COMMIT
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI E FUNZIONI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100000000     D*-------------------
002200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300000000     D*-------------------
002400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700000000     D*-------------------
002800000000     D* TABELLA DESCRIZIONI A VIDEO
002900000000     D*-------------------
003000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100000000     D*-------------------
003200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003300000000     D*-------------------
003400000000     D KEYS            S              1    DIM(40)
003500000000     D*-------------------
003600000000     D* GIORNI DELLA SETTIMANA DEL TRAINO
003700000000     D*-------------------
003800000000     D GSE             S              1    DIM(8)                               *PREVISTI
003900000000     D GSI             S              1    DIM(8)                               *GIA' IMMESSI
004000000000     D*------------------------------------------------------------------------*
004100000000     D* INPUT
004200000000     D*------------------------------------------------------------------------*
004300000000     D*-------------------
004400000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004500000000     D*-------------------
004600000000     D DSFUN         E DS
004700000000     D  FUNRIG                 1     40
004800000000     D*-------------------
004900000000     D* DS PER GESTIONE TASTI FUNZIONALI
005000000000     D*-------------------
005100000000     D DSKEYS        E DS
005200000000     D  KEYRIG                 1     40
005300000000     D*-------------------
005400000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005500000000     D*-------------------
005600000000     D                SDS
005700000000     D  USER                 254    263
005800000000     D*-------------------
005900000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006000000000     D*-------------------
006100000000     D DEVDS1          DS
006200000000     D  KEY                  369    369
006300000000     D  CURSOR               370    371
006400000000     D*-------------------
006500000000     D* ARCHITETTURA
006600000000     D*-------------------
006700000000     D KPJBA         E DS
006800000000     D*---------------------
006900000000     D* DS ESTERNA DI PROCEDURA
007000000000     D*---------------------
007100000000     D DS00          E DS                  EXTNAME(TNTL00DS)
007200000000     D*-------------------
007300000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007400000000     D*-------------------
007500000000     D DSMSG         E DS
007600020715
007700020715     d DdatiUte      e ds
007800020715     d AzuteDs       e ds                  ExtName(Azute00f)
007900020715     d Tibs34Ds      e ds                  Inz
008000020715
008100000000     D*------------------
008200000000     D* DEFINIZIONE COSTANTI
008300000000     D*------------------
008400000000     D DESGES          C                   CONST('** GESTIONE **')
008500000000     D DESINT          C                   CONST('INTERROGAZIONE')
008600000000     C*------------------------------------------------------------------------*
008700000000     C* MAIN LINE
008800000000     C*------------------------------------------------------------------------*
008900000000     C*
009000000000     C* CICLO FINO A CHE:
009100000000DO  1C     $FINE         DOUEQ     '1'
009200000000     C*
009300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009400000000     C*
009500000000     C* CONTROLLA GESTIONE VIDEO
009600000000IF  2C     $GES          IFEQ      'V1'
009700000000     C                   EXSR      GESV1                                        *PARAMETRI
009800000000E   2C                   ENDIF
009900000000     C*
010000000000E   1C                   ENDDO
010100000000     C*
010200000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTA FLAG DELLA DS DI PROCEDURA
010300000000     C     $F03          IFEQ      '1'
010400000000     C                   MOVEL     '1'           D00F03
010500000000E   1C                   ENDIF
010600000000     C     $F12          IFEQ      '1'
010700000000     C                   MOVEL     '1'           D00F12
010800000000E   1C                   ENDIF
010900000000     C* FINE PGM
011000000000     C                   MOVEL     DS00          KPJBU
011100000000     C                   SETON                                        LR
011200000000     C*------------------------------------------------------------------------*
011300000000     C* GESV1 - GESTIONE VIDEO UNO
011400000000     C*------------------------------------------------------------------------*
011500000000     C     GESV1         BEGSR
011600000000     C*
011700000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011800000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011900000000E   1C                   ENDIF
012000000000     C*
012100000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012200000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012300000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012400000000E   1C                   ENDIF
012500000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012600000000     C*
012700110506     C                   EXFMT     TLZ3DV1                                      *PARAMETRI
012800000000     C*
012900000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013000000000     C*
013100000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013200000000SL  2C                   SELECT
013300000000     C     KEY           WHENEQ    ENTER
013400000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013500000000     C                   OTHER
013600000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013700000000E   2C                   ENDSL
013800000000E   1C                   ENDIF
013900000000     C*
014000000000     C                   ENDSR
014100000000     C*------------------------------------------------------------------------*
014200000000     C* CARV1 - CARICA VIDEO UNO
014300000000     C*------------------------------------------------------------------------*
014400000000     C     CARV1         BEGSR
014500000000     C*
014600000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014700000000     C*
014800000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014900000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015000000000     C*
015100110506     C                   WRITE     TLZ3DVT                                      *TESTATA
015200000000     C*
015300000000     C                   ENDSR
015400000000     C*------------------------------------------------------------------------*
015500000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015600000000     C*------------------------------------------------------------------------*
015700000000     C     ENTV1         BEGSR
015800000000     C*
015900000000     C* CONTROLLA I CAMPI IMMESSI
016000000000     C                   EXSR      DECODI
016100000000     C*
016200000000     C* LANCIA LA GESTIONE FERMATE E ORARI
016300000000IF  1C     $ERR          IFEQ      '0'
016400000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
016500000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
016600000000     C                   MOVEL     V1CGSE        WGSE                           *GIORNO DI PARTENZA
016700000000     C*
016800000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
016900000000     C                   MOVEL     D00OP1        WOP1                           *LIVELLO PROGRAMMA
017000110506     C                   MOVEL     'TNTLZ4R'     WPGM              8            *PGM DI GESTIONE
017100000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
017200000000E   1C                   ENDIF
017300000000     C*
017400000000     C                   ENDSR
017500000000     C*------------------------------------------------------------------------*
017600000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017700000000     C*------------------------------------------------------------------------*
017800000000     C     DECODI        BEGSR
017900000000     C*
018000000000     C* REIMPOSTA INDICATORI DI ERRORE
018100000000     C                   SETOFF                                       60
018200000000     C*
018300000000DO  1C                   DO
018400000000     C*--------------------
018500000000     C* GIORNI DELLA SETTIMANA
018600000000     C*--------------------
018700000000     C* PERMETTE DI ENTRARE SE IL GIORNO E' FRA QUELLI PREVISTI (1,2,3,...)
018800000000     C* OPPURE SE NON E' PREVISTO, MA DENTRO AL FILE PERO' C'E'
018900000000     C                   Z-ADD     *ZEROS        J
019000000000     C                   SETOFF                                           28
019100000000     C     V1CGSE        LOOKUP    GSE                                    28
019200000000IF  2C     *IN28         IFEQ      *OFF                                         *NON PREVISTO
019300000000IF  3C     V1CGSE        IFEQ      *BLANKS
019400000000     C                   Z-ADD     8             J
019500000000X   3C                   ELSE
019600000000     C                   MOVE      V1CGSE        J
019700000000     C                   ENDIF
019800000000IF  3C     GSI(J)        IFEQ      *BLANKS                                      *NON PRESENTE
019900000000     C                   MOVEL     '1'           $ERR
020000000000     C                   MOVEL     MSG(10)       $MSG
020100000000     C                   EXSR      CARMSG
020200000000     C                   SETON                                        60
020300000000     C                   LEAVE
020400000000E   3C                   ENDIF
020500000000E   2C                   ENDIF
020600000000     C*
020700000000E   1C                   ENDDO
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*------------------------------------------------------------------------*
021100000000     C* LANPGM - LANCIA UN PROGRAMMA
021200000000     C*------------------------------------------------------------------------*
021300000000     C     LANPGM        BEGSR
021400000000     C***
021500000000     C* PARAMETRI INFORMATIVI
021600000000     C***
021700000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
021800000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
021900000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
022000000000     C                   MOVEL     '0'           D00F12                         *F12-FINE PGM
022100000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
022200000000     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
022300000000     C***
022400000000     C* PARAMETRI
022500000000     C***
022600000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
022700000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
022800000000     C                   MOVEL     WGSE          D00GSE                         *GIORNO SETTIMANA
022900000000     C*--------------------
023000000000     C* L A N C I O
023100000000     C*--------------------
023200000000     C                   MOVEL     DS00          KPJBU
023300000000     C                   CALL      WPGM                                 99
023400000000     C                   PARM                    KPJBA
023500000000     C                   MOVEL     KPJBU         DS00
023600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
023700000000     C*
023800000000     C* SE RITORNA PREMUTO F3 CHIUDE IL PROGRAMMA, ALTRIMENTI MANDA IL MESSAGGIO
023900000000     C* RICEVUTO DAL PROGRAMMA CHIAMATO
024000000000IF  1C     D00F03        IFEQ      '1'
024100000000     C                   MOVEL     '1'           $F03
024200000000     C                   MOVEL     '1'           $FINE
024300000000X   1C                   ELSE
024400000000IF  2C     D00F12        IFEQ      '1'
024500000000     C                   MOVEL     '1'           $F12
024600000000     C                   MOVEL     '1'           $FINE
024700000000E   2C                   ENDIF
024800000000E   1C                   ENDIF
024900000000     C*
025000000000     C                   MOVEL     D00MSG        $MSG
025100000000     C                   EXSR      CARMSG
025200000000     C*
025300000000     C                   ENDSR
025400000000     C*------------------------------------------------------------------------*
025500000000     C* IMPVID - IMPOSTAIZONI VIDEO
025600000000     C*------------------------------------------------------------------------*
025700000000     C     IMPVID        BEGSR
025800000000     C*
025900000000     C* LEGGE IL RECORD
026000000000     C                   Z-ADD     D00TRN        KLTTRN
026100000000     C                   Z-ADD     D00DDE        KLTDDE
026200000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
026300000000     C                   MOVEA     TLTGSE        GSE                            *GIORNI DI PARTENZA
026400000000     C                   MOVEL     GSE(1)        V1DGS1
026500000000     C                   MOVEL     GSE(2)        V1DGS2
026600000000     C                   MOVEL     GSE(3)        V1DGS3
026700000000     C                   MOVEL     GSE(4)        V1DGS4
026800000000     C                   MOVEL     GSE(5)        V1DGS5
026900000000     C                   MOVEL     GSE(6)        V1DGS6
027000000000     C                   MOVEL     GSE(7)        V1DGS7
027100000000     C*
027200000000     C* EVIDENZIA GLI ORARI GIA' INSERITI IN ARCHIVIO
027300110506     C                   Z-ADD     D00TRN        KLsTRN
027400110506     C                   Z-ADD     D00DDE        KLsDDE
027500000000DO  1C     1             DO        8             J                 5 0
027600000000IF  2C     J             IFEQ      8
027700110506     C                   MOVEL     *BLANKS       KLsGSE
027800000000X   2C                   ELSE
027900110506     C                   MOVE      J             KLsGSE
028000000000E   2C                   ENDIF
028100110506     C     KEYTLs        SETLL     TNTLs01L
028200110506     C     KEYTLs        READE(n)  TNTLs01L                               99
028300000000DO  2C     *IN99         DOWEQ     *OFF
028400000000     C     GSI(J)        ANDEQ     *BLANKS
028500110506IF  3C     TLsATB        IFEQ      *BLANKS
028600000000     C                   MOVEL     'X'           GSI(J)
028700000000E   3C                   ENDIF
028800110506     C     KEYTLs        READE(n)  TNTLs01L                               99
028900000000E   2C                   ENDDO
029000000000E   1C                   ENDDO
029100000000     C                   MOVEL     GSI(1)        V1CGS1
029200000000     C                   MOVEL     GSI(2)        V1CGS2
029300000000     C                   MOVEL     GSI(3)        V1CGS3
029400000000     C                   MOVEL     GSI(4)        V1CGS4
029500000000     C                   MOVEL     GSI(5)        V1CGS5
029600000000     C                   MOVEL     GSI(6)        V1CGS6
029700000000     C                   MOVEL     GSI(7)        V1CGS7
029800000000     C                   MOVEL     GSI(8)        V1CGS8
029900000000     C*
030000000000     C                   ENDSR
030100000000     C*------------------------------------------------------------------------*
030200000000     C* *INZSR  - ROUTINE INIZIALE
030300000000     C*------------------------------------------------------------------------*
030400000000     C     *INZSR        BEGSR
030500000000     C*--------------------
030600000000     C* RICEVIMENTO PARAMETRI
030700000000     C*--------------------
030800000000     C     *ENTRY        PLIST
030900000000     C                   PARM                    KPJBA
031000000000     C                   MOVEL     KPJBU         DS00
031100000000     C*--------------------
031200000000     C* VARIABILI RIFERITE AL DATA BASE
031300000000     C*--------------------
031400000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
031500000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
031600110506     C     *LIKE         DEFINE    TLsTRN        KLsTRN                         *TNTLs00F
031700110506     C     *LIKE         DEFINE    TLsDDE        KLsDDE
031800110506     C     *LIKE         DEFINE    TLsGSE        KLsGSE
031900000000     C*--------------------
032000000000     C* VARIABILI NON RIFERITE AL DATA BASE
032100000000     C*--------------------
032200000000     C                   MOVEL     *BLANKS       WPGM              8
032300000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
032400000000     C     *LIKE         DEFINE    D00OP1        WOP1
032500000000     C     *LIKE         DEFINE    D00F03        WF03
032600000000     C     *LIKE         DEFINE    D00F12        WF12
032700000000     C     *LIKE         DEFINE    D00ERR        WERR
032800000000     C     *LIKE         DEFINE    D00MSG        WMSG
032900000000     C     *LIKE         DEFINE    D00TRN        WTRN
033000000000     C     *LIKE         DEFINE    D00DDE        WDDE
033100000000     C     *LIKE         DEFINE    D00GSE        WGSE
033200000000     C*--------------------
033300000000     C* CHIAVI DI LETTURA
033400000000     C*--------------------
033500000000     C* LETTURA TNTLT01L - COMPLETA
033600000000     C     KEYTLT        KLIST
033700000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
033800000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
033900110506     C* LETTURA TNTLs01L - PARZIALE
034000110506     C     KEYTLs        KLIST
034100110506     C                   KFLD                    KLsTRN                         *NUMERO TRAINO
034200110506     C                   KFLD                    KLsdDE                         *DATA DECORRENZA
034300110506     C                   KFLD                    KLsGSE                         *GIORNO SETTIMANA
034400000000     C***
034500000000     C* IMPOSTAZIONI VIDEO
034600000000     C***
034700000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
034800000000     C                   MOVEL     D00OP0        $MENU             1
034900000000     C                   MOVE      D00OP0        INTGES            2
035000000000IF  1C     INTGES        IFEQ      '02'
035100000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
035200000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
035300000000X   1C                   ELSE
035400000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
035500000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
035600000000E   1C                   ENDIF
035700000000     C*
035800000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
035900000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
036000000000IF  1C     *IN99         IFEQ      *ON
036100000000     C     *IN99         ANDEQ     *OFF
036200000000     C                   UPDATE    TNTLT000
036300000000     C                   WRITE     TNTLT000
036400000000E   1C                   ENDIF
036500000000     C*
036600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
036700000000     C                   EXSR      INZSTD
036800000000     C*
036900000000     C* IMPOSTAZIONE INIZIALE VIDEO
037000000000     C                   EXSR      IMPVID
037100000000     C*
037200000000     C* EMETTE FORMATO VIDEO
037300000000     C                   EXSR      CARV1
037400000000     C*
037500000000     C                   ENDSR
037600000000     C*------------------------------------------------------------------------*
037700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037900000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
038000000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' DA MODIFICARE:
038100000000     C* NEL CASO FARE  A T T E N Z I O N E !! .
038200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
038300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
038400000000     C*------------------------------------------------------------------------*
038500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
038600000000     C*------------------------------------------------------------------------*
038700000000     C     INZSTD        BEGSR
038800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
038900000000     C                   CALL      'XKEYS'
039000000000     C                   PARM                    DSKEYS
039100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
039200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
039300000000     C     XMSGPA        PLIST
039400000000     C                   PARM                    DSMSMS
039500000000     C                   PARM                    DSMSPG
039600000000     C                   PARM                    DSMSKE
039700000000     C                   PARM                    DSMSER
039800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
039900110506     C                   MOVEL     'TNTLZ3R'     DSMSPG                         *NOME PGM CHIAMANTE
040000110506     C                   MOVEL     'TNTLZ3R'     VMCQUE                         *CODA MESSAGGI DESTI
040100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
040200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
040300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
040400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
040500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
040600000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
040700000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
040800000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
040900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
041000000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
041100000000     C* LANCIO ROUTINE PER VERIFICARE SE
041200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
041300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
041400020715
041500020715     c     *dtaara       define    §azute        azuteds
041600020715     c     *dtaara       define    §datiute      ddatiute
041700020715
041800020715     c                   in(E)     *dtaara
041900020715     c                   if        %Error or rsut = *blanks
042000020715     c                   clear                   tibs34ds
042100020715     c                   call      'TIBS34R'
042200020715     c                   parm                    Tibs34Ds
042300020715     c                   in        *dtaara
042400020715     c                   endif
042500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
042600000000     C                   BITOFF    '01234567'    POSMI1            1
042700000000     C                   BITOFF    '01234567'    POSMI2            1
042800000000     C                   BITOFF    '01234567'    POSMA1            1
042900000000     C                   BITOFF    '01234567'    POSMA2            1
043000000000     C                   BITON     '34'          POSMI1            1            *18
043100000000     C                   BITON     '7'           POSMI2            1            *01
043200000000     C                   BITON     '34'          POSMA1            1            *18
043300000000     C                   BITON     '0'           POSMA2            1            *80
043400000000     C                   MOVE      POSMI2        POSMIN            2
043500000000     C                   MOVEL     POSMI1        POSMIN
043600000000     C                   MOVE      POSMA2        POSMAX            2
043700000000     C                   MOVEL     POSMA1        POSMAX
043800000000     C                   ENDSR
043900000000     C*------------------------------------------------------------------------*
044000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
044100000000     C*------------------------------------------------------------------------*
044200000000     C     FUNSTD        BEGSR
044300000000     C                   SELECT
044400000000     C     KEY           WHENEQ    F3
044500000000     C                   EXSR      F03STD
044600000000     C     KEY           WHENEQ    F12
044700000000     C                   EXSR      F12STD
044800000000     C     KEY           WHENEQ    F24
044900000000     C                   EXSR      F24STD
045000000000     C                   OTHER
045100000000     C                   MOVEL     MSG(2)        $MSG
045200000000     C                   EXSR      CARMSG
045300000000     C                   ENDSL
045400000000     C                   ENDSR
045500000000     C*------------------------------------------------------------------------*
045600000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
045700000000     C*------------------------------------------------------------------------*
045800000000     C     F03STD        BEGSR
045900000000     C* IMPOSTA FLAG DI FINE PGM
046000000000     C                   MOVEL     '1'           $F03
046100000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
046200000000     C                   MOVEL     '1'           $FINE
046300951212     C* PULISCE IL MESSAGGIO
046400951212     C                   MOVEL     *BLANKS       D00MSG
046500000000     C                   ENDSR
046600000000     C*------------------------------------------------------------------------*
046700000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
046800000000     C*------------------------------------------------------------------------*
046900000000     C     F12STD        BEGSR
047000000000     C* IMPOSTA FLAG DI FINE PGM
047100000000     C                   MOVEL     '1'           $F12
047200000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
047300000000     C                   MOVEL     '1'           $FINE
047400951212     C* PULISCE IL MESSAGGIO
047500951212     C                   MOVEL     *BLANKS       D00MSG
047600000000     C                   ENDSR
047700000000     C*------------------------------------------------------------------------*
047800000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
047900000000     C*------------------------------------------------------------------------*
048000000000     C     F24STD        BEGSR
048100000000     C*
048200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
048300000000     C                   EXSR      OUTFUN
048400000000     C*
048500000000     C                   ENDSR
048600000000     C*------------------------------------------------------------------------*
048700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
048800000000     C*          CODA MESSAGGI DEL PGM
048900000000     C*------------------------------------------------------------------------*
049000000000     C     CARMSG        BEGSR
049100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
049200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
049300000000     C     MSGULT        IFEQ      0
049400000000     C                   MOVEL     '1'           *IN40
049500110506     C                   WRITE     TLZ3DVK
049600000000     C                   MOVEL     '0'           *IN40
049700000000     C                   ENDIF
049800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
049900000000     C                   MOVEL     $MSG          DSMSMS
050000000000     C                   CALL      'XMSG'        XMSGPA
050100000000     C     DSMSER        IFEQ      0
050200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
050300000000     C                   ENDIF
050400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
050500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
050600110506     C                   WRITE     TLZ3DVM                                      *SFL MESS
050700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
050800000000     C                   MOVEL     *BLANKS       $MSG
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
051200000000     C*------------------------------------------------------------------------*
051300000000     C     OUTMSG        BEGSR
051400000000     C* VISUALIZZAZIONE MESSAGGIO
051500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
051600110506     C                   WRITE     TLZ3DVK                                      *CONTR MESS
051700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
051800000000     C                   Z-ADD     *ZEROS        MSGULT
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
052200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
052300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
052400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
052500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
052600000000     C*------------------------------------------------------------------------*
052700000000     C     CARFUN        BEGSR
052800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
052900000000     C     1             DO        31            I                 5 0          --- 1 -->
053000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
053100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
053200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
053300020715     c                   If        DutLpo = 'S'
053400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
053500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
053600000000     C                   ELSE                                                   *INTERROG.
053700000000     C                   MOVEL     FUNASI        FAT(I)
053800000000     C                   ENDIF
053900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
054000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
054100000000     C                   MOVEL     FUNAFG        FAT(I)
054200000000     C                   ELSE                                                   *INTERROG.
054300000000     C                   MOVEL     FUNAFI        FAT(I)
054400000000     C                   ENDIF
054500000000     C                   ENDIF                                                  <-- 2 ---
054600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
054700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
054800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
054900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
055000000000     C                   ELSE
055100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
055200000000     C                   ENDIF
055300000000     C                   ENDIF
055400000000     C                   ENDDO                                                  <-- 1 ---
055500000000     C                   ENDSR
055600000000     C*------------------------------------------------------------------------*
055700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
055800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
055900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
056000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
056100000000     C*------------------------------------------------------------------------*
056200000000     C     OUTFUN        BEGSR
056300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
056400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
056500000000     C                   ENDDO
056600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
056700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
056800000000     C     1             DO        31            I
056900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
057000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
057100000000     C                   ADD       1             FUNNAT
057200000000     C                   ENDIF
057300000000     C                   ENDDO
057400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
057500000000     C     FUNNAT        IFGT      8
057600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
057700000000     C                   ENDIF
057800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
057900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
058000000000     C                   Z-ADD     8             FUNVIS            5 0
058100000000     C     FAT(23)       IFEQ      'S'
058200000000     C                   SUB       1             FUNVIS
058300000000     C                   ENDIF
058400000000     C     FAT(24)       IFEQ      'S'
058500000000     C                   SUB       1             FUNVIS
058600000000     C                   ENDIF
058700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
058800000000     C                   Z-ADD     *ZEROS        W                 5 0
058900000000     C                   ADD       1             FUNULT
059000000000     C     FUNULT        IFGT      31
059100000000     C                   Z-ADD     1             FUNULT
059200000000     C                   ENDIF
059300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
059400000000     C     FUNULT        DO        31            I                              --- 1 -->
059500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
059600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
059700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
059800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
059900000000     C     FDE(I)        ANDNE     *BLANKS
060000000000     C     I             IFEQ      23                                           *TOLGO F23
060100000000     C     I             OREQ      24                                           *E F24
060200000000     C                   ELSE
060300000000     C                   ADD       1             W
060400000000     C                   MOVEL     FDE(I)        FUV(W)
060500000000     C                   ENDIF
060600000000     C                   ENDIF                                                  <-- 3 ---
060700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
060800000000     C                   Z-ADD     I             FUNULT
060900000000     C                   ENDIF                                                  <-- 2 ---
061000000000     C                   ENDDO                                                  <-- 1 ---
061100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
061200000000     C                   MOVEL     FUV(1)        VFD001
061300000000     C                   MOVEL     FUV(2)        VFD002
061400000000     C                   MOVEL     FUV(3)        VFD003
061500000000     C                   MOVEL     FUV(4)        VFD004
061600000000     C                   MOVEL     FUV(5)        VFD005
061700000000     C                   MOVEL     FUV(6)        VFD006
061800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
061900000000     C     FAT(24)       ANDEQ     'N'
062000000000     C                   MOVEL     FUV(7)        VFD007
062100000000     C                   MOVEL     FUV(8)        VFD008
062200000000     C                   ELSE                                                   <-- 1 -->
062300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
062400000000     C     FAT(24)       ANDEQ     'S'
062500000000     C                   MOVEL     FDE(23)       VFD007
062600000000     C                   MOVEL     FDE(24)       VFD008
062700000000     C                   ELSE                                                   <-- 2 -->
062800000000     C                   MOVEL     FUV(7)        VFD007
062900000000     C     FAT(23)       IFEQ      'S'
063000000000     C                   MOVEL     FDE(23)       VFD008
063100000000     C                   ELSE
063200000000     C                   MOVEL     FDE(24)       VFD008
063300000000     C                   ENDIF
063400000000     C                   ENDIF                                                  <-- 2 ---
063500000000     C                   ENDIF                                                  <-- 1 ---
063600000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
063700000000     C                   ENDSR
063800000000     C*------------------------------------------------------------------------*
063900000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
064000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
064100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
064200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
064300000000     C*------------------------------------------------------------------------*
064400000000     C     CONFUN        BEGSR
064500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
064600000000     C     1             DO        31            I                              --- 1 -->
064700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
064800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
064900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
065000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
065100000000     C                   EXSR      CARMSG
065200000000     C                   MOVEL     '1'           $ERR
065300000000     C                   ENDIF                                                  <-- 3 ---
065400000000     C                   ENDIF                                                  <-- 2 ---
065500000000     C                   ENDDO                                                  <-- 1 ---
065600000000     C                   ENDSR
065700000000     O*------------------------------------------------------------------------*
065800000000** -MSG- MESSAGGI
065900110506TNTLZ3R- Tasto funzionale non valido.                                          1
066000110506TNTLZ3R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
066100110506TNTLZ3R- Opzione non valida.                                                   3
066200110506TNTLZ3R- Opzione abilitata ma non gestita. Avvertire CED.                      4
066300110506TNTLZ3R- Inizio elenco.                                                        5
066400110506TNTLZ3R- Fine elenco.                                                          6
066500110506TNTLZ3R- 7
066600110506TNTLZ3R- 8
066700110506TNTLZ3R- 9
066800110506TNTLZ3R- Inserire un giorno della settimana fra quelli possibili              10
066900110506TNTLZ3R- 11
067000110506TNTLZ3R- 12
067100110506TNTLZ3R- 15
067200110506TNTLZ3R- 14
067300110506TNTLZ3R- 15
067400110506TNTLZ3R- 16
067500110506TNTLZ3R- 17
067600110506TNTLZ3R- 18
067700110506TNTLZ3R- 19
067800110506TNTLZ3R- 20
067900000000** FUN   PARAMETRI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
068000000000F1    -                   -    NN     NN
068100000000F2    -                   -    NN     NN
068200000000F3    -F3=Fine            -    NN     NN
068300000000F4    -                   -    NN     NN
068400000000F5    -                   -    NN     NN
068500000000F6    -                   -    NN     NN
068600000000F7    -                   -    NN     NN
068700000000F8    -                   -    NN     NN
068800000000F9    -                   -    NN     NN
068900000000F10   -                   -    NN     NN
069000000000F11   -                   -    NN     NN
069100951206F12   -F12=Ritorno        -    SS     SS
069200000000F13   -                   -    NN     NN
069300000000F14   -                   -    NN     NN
069400000000F15   -                   -    NN     NN
069500000000F16   -                   -    NN     NN
069600000000F17   -                   -    NN     NN
069700000000F18   -                   -    NN     NN
069800000000F19   -                   -    NN     NN
069900000000F20   -                   -    NN     NN
070000000000F21   -                   -    NN     NN
070100000000F22   -                   -    NN     NN
070200000000F23   -                   -    NN     NN
070300000000F24   -F24=Altri tasti    -    NN     NN
070400000000ENTER -                   -    SS     SS
070500000000ROLLUP-                   -    NN     NN
070600000000ROLLDN-                   -    NN     NN
070700000000HELP  -                   -    NN     NN
070800000000PRINT -                   -    NN     NN
070900000000BSPACE-                   -    NN     NN
071000000000CLEAR -                   -    NN     NN
