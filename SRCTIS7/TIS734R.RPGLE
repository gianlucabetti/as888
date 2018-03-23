000100970526     H*------------------------------------------------------------------------*
000200000808     H* Gestione - codici clienti unificanti
000300970526     H*------------------------------------------------------------------------*
000400000830     H DECEDIT('0,') DATEDIT(*DMY.) EXPROPTS(*RESDECPOS)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800010227     FTIS734D   CF   E             WORKSTN INFDS(DEVDS1)
000900010227     F                                     SFILE(S734D05:SFLULT)
001000010227     F                                     SFILE(S734DMS:MSGULT)
001100970526     F*------------
001200010717     Ftivss02l  IF   E           K DISK
001300010227     Ftivtd01l  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* DECODIFICHE
001900970526     D*------------
002000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500970528     D*------------
002600970528     D* COMANDI
002700970528     D*------------
002800970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI OPZIONI
003100970526     D*------------
003200970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* DESCRIZIONI TASTI FUNZIONALI
003500970526     D*------------
003600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* OPZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970526     D OPV             S             19    DIM(24)
004100970526     D*------------
004200970526     D* FUNZIONI DA CARICARE A VIDEO
004300970526     D*------------
004400970527     D FUV             S             19    DIM(24)
004500970526     D*------------------------------------------------------------------------*
004600970526     D* INPUT
004700970526     D*------------------------------------------------------------------------*
004800970526     D*------------
004900970526     D* RIDEFINIZIONE OPZIONI VIDEO
005000970526     D*------------
005100970526     D OPDDS           DS
005200970526     D  OPDOPZ                 1      2
005300970526     D  OPDDE1                 3     19
005400970526     D  OPDOP1                21     22
005500970526     D  OPDFI0                23     34
005600970526     D  OPDATT                35     35
005700970526     D  OPDFI1                36     80
005800970526     D  OPDDES                 1     19
005900970526     D  OPDRIG                 1     80
006000970526     D*------------
006100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006200970526     D*------------
006300970526     D FUDDS           DS
006400970526     D  FUDDE1                 1      1
006500970526     D  FUDFUN                 2      3
006600970526     D  FUDDE2                 4     19
006700970526     D  FUDFI0                20     34
006800970526     D  FUDATT                35     35
006900970526     D  FUDFI1                36     80
007000970526     D  FUDDES                 1     19
007100970526     D  FUDRIG                 1     80
007200970526     D*------------
007300970526     D* DS GESTIONE TASTI FUNZIONALI
007400970526     D*------------
007500970526     D DSKEY         E DS
007600970526     D DEVDS1          DS
007700970526     D  KEY                  369    369
007800970526     D*------------
007900970526     D* DS "XMSG" - GESTIONE MESSAGGI
008000970526     D*------------
008100970526     D DSMSG         E DS
008200970526     D*------------
008300020916     D* DS REPERIMENTO DATI UTENTE
008400970526     D*------------
008500020916     D TIBS34DS      E DS                                                       *Profili utente
008600020916     D DDATIUTE      E DS                                                       *Dati utente
008700020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800970527     D*------------------
008900970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009000970527     D*------------------
009100970527     D WLBDA8          DS                  INZ
009200970527     D  G08DAT                 1      8  0
009300970527     D  G08INV                 9     16  0
009400970527     D  G08ERR                17     17
009500970527     D  G08TGI                18     22  0
009600970526     D*------------
009700970526     D* DS DI PROCEDURA
009800970526     D*------------
009900010227     D tis734        E DS                  extname(tis734ds)
010000970526     D*------------
010100970526     D* ARCHITETTURA
010200970526     D*------------
010300970526     D KPJBA         E DS
010400970526     C*------------------------------------------------------------------------*
010500970526     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800970526     C* SE NON CI SONO ERRORI, PROSEGUE
010900010227     C     ERRL00        IFNE      '1'
011000970526     C*
011100970526     C* CARICA IL SFILE
011200970526     C                   EXSR      POSSFL
011300970526     C*
011400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011500010227     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011800970526     C*
011900970526     C* REIMPOSTA ERRORE DI PROCERURA
012000970526     C                   MOVEL     '0'           ERRL00
012100970526     C*
012200970526     C* EMETTE IL SFILE
012300970526     C                   EXSR      OUTSFL
012400970526     C*
012500970526     C* GESTIONE TASTI FUNZIONALI
012600970526     C                   EXSR      FUNCON
012700970526     C*
012800010227     C                   ENDDO
012900970527     C*
013000970527     C* CI SONO ERRORI IN ENTRATA
013100010227     C                   ELSE
013200010227     C                   MOVEL     '1'           s34ERR
013300010227     C                   MOVEL     DSMSMS        s34MSG
013400010227     C                   ENDIF
013500010227     C* FINE PGM
013600970526     C                   SETON                                        LR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013900970526     C*------------------------------------------------------------------------*
014000970526     C     F03GES        BEGSR
014100970526     C*
014200970526     C* IMPOSTA "PREMUTO F03"
014300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014400010227     C                   MOVEL     '1'           s34F03                         *DI PROCEDURA
014500970526     C*
014600970526     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F05GES        BEGSR
015100970526     C*
015200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015300970526     C*
015400970526     C                   ENDSR
015500970526     C*------------------------------------------------------------------------*
015600970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015700970526     C*------------------------------------------------------------------------*
015800970526     C     F06GES        BEGSR
015900970526     C*
016000970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
016100020916     C     DUTLPO        IFNE      'S'
016200970526     C                   MOVEL     '1'           ERRL00
016300970526     C                   MOVEL     MSG(3)        DSMSMS
016400010227     C                   ENDIF
016500970526     C*
016600010227     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016700970602     C                   MOVEL     DEPOP0        WOP0
016800970529     C                   MOVEL     'F06'         WOP1
016900010227     C                   MOVEL     'TIS735R'     WPGM              8
017000970526     C                   EXSR      LANPGM                                       *LANCIA PGM
017100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017200010227     C                   ENDIF
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600970526     C* F12GES - GESTIONE F12 -> RITORNO
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F12GES        BEGSR
017900970526     C*
018000970526     C* IMPOSTA "PREMUTO F12"
018100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018200010227     C                   MOVEL     '1'           s34F12                         *DI PROCEDURA
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
018700970526     C*------------------------------------------------------------------------*
018800970526     C     F23GES        BEGSR
018900970526     C*
019000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F24GES        BEGSR
019700970526     C*
019800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
020300970526     C*------------------------------------------------------------------------*
020400970526     C     RUPGES        BEGSR
020500970526     C*
020600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
020700970526     C*
020800970526     C                   ENDSR
020900970526     C*------------------------------------------------------------------------*
021000010227     C* ENTGES - GESTIONE ENTER -> INVIO
021100970526     C*------------------------------------------------------------------------*
021200970526     C     ENTGES        BEGSR
021300970526     C*
021400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
021500970526     C                   EXSR      NEWPOS
021600970526     C*
021700970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
021800010227     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021900010227     C     SWPOSI        IFNE      0
022000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
022100010227     C                   ELSE
022200010227     C     SFLULT        IFNE      0
022300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
022400970526     C*
022500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
022600010227     C     SWMODI        IFNE      0
022700970526     C                   EXSR      POSSFL
022800010227     C                   ENDIF
022900010227     C                   ENDIF
023000010227     C                   ENDIF
023100010227     C                   ENDIF
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500010227     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
023600970526     C*------------------------------------------------------------------------*
023700970526     C     O02GES        BEGSR
023800970526     C*
023900970527     C* MODIFICA
024000010227     C     ERRL00        IFEQ      '0'
024100010227     C                   EXSR      MOVCAMP
024200970602     C                   MOVEL     DEPOP0        WOP0
024300970526     C                   MOVEL     'O02'         WOP1
024400010227     C                   MOVEL     'TIS735R'     WPGM
024500970526     C                   EXSR      LANPGM
024600010227     C                   Z-ADD     1             swmodi
024700010227     C                   ENDIF
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
025200970526     C*------------------------------------------------------------------------*
025300010227     C     O04GES        BEGSR
025400970526     C*
025500970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
025600010227     C     VIDatb        IFEQ      'A'
025700010227     C                   MOVEL     '1'           ERRL00
025800010227     C                   MOVEL     MSG(4)        DSMSMS
025900010227     C                   ENDIF
026000010227     C*                  ENDIF
026100970527     C* ANNULLAMENTO
026200010227     C     ERRL00        IFEQ      '0'
026300010227     C                   EXSR      MOVCAMP
026400010227     C                   MOVEL     DEPOP0        WOP0
026500010227     C                   MOVEL     'O04'         WOP1
026600010227     C                   MOVEL     'TIS735R'     WPGM
026700010227     C                   EXSR      LANPGM
026800010227     C                   Z-ADD     1             swmodi
026900010227     C                   ENDIF
027000970526     C*
027100010227     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
027400970526     C*------------------------------------------------------------------------*
027500970526     C     O05GES        BEGSR
027600970526     C*
027700010227     C                   EXSR      MOVCAMP
027800970602     C                   MOVEL     DEPOP0        WOP0
027900970526     C                   MOVEL     'O05'         WOP1
028000010227     C                   MOVEL     'TIS735R'     WPGM
028100970526     C                   EXSR      LANPGM
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
028600970526     C*------------------------------------------------------------------------*
028700970526     C     O07GES        BEGSR
028800970526     C*
028900970526     C* RIPRISITINO
029000010227     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029100010227     C                   EXSR      MOVCAMP
029200970602     C                   MOVEL     DEPOP0        WOP0
029300970526     C                   MOVEL     'O07'         WOP1
029400010227     C                   MOVEL     'TIS735R'     WPGM
029500970526     C                   EXSR      LANPGM
029600010227     C                   Z-ADD     1             SWMODI
029700010227     C                   ENDIF
029800970526     C*
029900970526     C                   ENDSR
030000001020     C*-----------------------------------------------
030100010227     C* VALORIZZAZIONE CAMPI DA "PASSARE"
030200001020     C*-----------------------------------------------
030300001020     C     MOVCAMP       BEGSR
030400001020     C*
030500010717     C                   MOVEL     VIHksu        Dksu
030600010301     C                   MOVEL     VIHsed        Dsed
030700001020     C*
030800001020     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
031100970526     C*------------------------------------------------------------------------*
031200970526     C     NEWPOS        BEGSR
031300970526     C*
031400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
031500970526     C*         1= RICHIESTO POSIZIONAMENTO
031600970526     C                   Z-ADD     0             SWPOSI            7 0
031700970526     C*
031800970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
031900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
032000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
032100970526     C                   Z-ADD     0             SWMODI            1 0
032200970530     C*
032300970530     C* REIMPOSTA INDICATORI DI ERRORE
032400010227     C                   SETOFF                                       8182
032500970526     C*
032600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032700010717     C     VIDks1        IFNE      DEPks1
032800010717     C     VIDks2        ORNE      DEPks2
032900970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
033000010717     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
033100010717     C                   MOVEL     VIDks2        DEPks2
033200010227     C                   ENDIF
033300970526     C*
033400970526     C                   ENDSR
033500970526     C*------------------------------------------------------------------------*
033600970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
033700970526     C*------------------------------------------------------------------------*
033800970528     C     SETFIL        BEGSR
033900970526     C*
034000970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
034100010717     C                   MOVEL     VIDks2        KVDksu
034200010227     C     KEYvtd        SETLL     tivtd01l                           99
034300980107     C*
034400970526     C* LETTURA RECORD
034500010227     C     *IN99         IFEQ      *OFF
034600970526     C                   EXSR      LETFIL
034700010227     C                   ENDIF
034800970526     C*
034900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
035000010227     C     *IN99         IFEQ      *ON
035100970526     C                   Z-ADD     1             SFLEOF
035200010227     C                   ENDIF
035300970526     C*
035400970526     C                   ENDSR
035500970526     C*------------------------------------------------------------------------*
035600970526     C* LETFIL - LETTURA RECORD
035700970526     C*------------------------------------------------------------------------*
035800970526     C     LETFIL        BEGSR
035900970526     C*
036000970526     C* LEGGE FINO A:
036100970526     C*     - FINE FILE
036200970526     C*     - TROVATO RECORD VALIDO
036300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
036400010227     C     *IN99         DOWEQ     *OFF
036500970526     C     WRECOK        ANDEQ     'N'
036600970526     C*
036700970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
036800010227     C                   READ      tivtd01l                               99
036900010227     C*
037000970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
037100010227     C     *IN99         IFEQ      *ON                                          *FINE FILE
037200970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
037300010227     C                   ELSE
037400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
037500010227     C                   ENDIF
037600010227     C                   ENDDO
037700970526     C*
037800970526     C                   ENDSR
037900970526     C*------------------------------------------------------------------------*
038000970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
038100970526     C*------------------------------------------------------------------------*
038200970526     C     CHKREC        BEGSR
038300970526     C*
038400970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
038500010227     C                   SETOFF                                       76
038600970530     C*
038700970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
038800010717     C     VIDks1        IFEQ      *zeros
038900010717     C     VIDks1        OREQ      *blank
039000010227     C                   ELSE
039100010717     C     VIDks1        IFNE      VTDksu
039200010227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039300010227     C                   ENDIF
039400010227     C                   ENDIF
039500000216     C*
039600010227     C* VERIFICA ESISTENZA LEGAME O COMUNQUE LEGAME VALIDO
039700010227     C     WRECOK        IFEQ      'S'                                          *ALLA DATA
039800010717     C                   MOVEL     VTDksu        KVSksu
039900010227     C                   MOVEL     'TD'          KVSisv
040000010717     C     KEYvss        SETLL     tivss02l
040100010717     C     KEYvss        READE     tivss02l                               98
040200010227     C                   DOW       NOT *in98
040300010227     C     VSSvat        IFEQ      *blanks
040400010227     C     VSSdde        IFLE      datcor
040500010227     C     VSSdsc        ANDGE     datcor
040600010227     C                   LEAVE
040700010227     C                   ELSE
040800010227     C                   SETON                                        76
040900010227     C                   ENDIF
041000010227     C                   ENDIF
041100010717     C     KEYvss        READE     tivss02l                               98
041200010227     C                   ENDDO
041300010227     C                   ENDIF
041400970526     C*
041500970526     C                   ENDSR
041600970526     C*------------------------------------------------------------------------*
041700970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
041800970526     C*------------------------------------------------------------------------*
041900970526     C     IRESFL        BEGSR
042000970526     C*
042100970526     C                   MOVEL     *BLANKS       OPZKEY
042200010227     C                   MOVEL     VTDatb        VIDatb
042300010717     C                   MOVEL     VTDksu        VIDksu
042400010717     C                   MOVEL     VTDksu        VIHksu
042500010227     C                   MOVEL     VTDute        VIDute
042600010227     C                   MOVEL     VTDpwd        VIDpwd
042700010227     C                   MOVEL     VTDsed        VIDsed
042800010227     C                   MOVEL     VTDsed        VIHsed
042900010227     C                   MOVEL     VTDsea        VIDsea
043000010228     C                   IF        *IN76 = *ON
043100010228     C                   MOVEL     '1'           Dvss
043200010228     C                   ENDIF
043300010227     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* LANPGM - LANCIA UN PROGRAMMA
043700970526     C*------------------------------------------------------------------------*
043800970526     C     LANPGM        BEGSR
043900970526     C*
044000970526     C* IMPOSTA LA DS DI PROCEDURA
044100010227     C                   MOVEL     WOP0          s34OP0                         *LIVELLO PROCEDURA
044200010227     C                   MOVEL     WOP1          s34OP1                         *LIVELLO PGM
044300010227     C                   MOVEL     '0'           s34F03                         *PREMUTO F03=FINE
044400010227     C                   MOVEL     '0'           s34F12                         *PREMUTO F12=RITORNO
044500010227     C                   MOVEL     '0'           s34ERR                         *'1'=ERRORE
044600010227     C                   MOVEL     *BLANKS       s34MSG                         *MESSAGGIO DI RITORN
044700010717     C                   MOVEL     Dksu          s34ksu                         *UTENTE STRATEGI
044800010301     C                   MOVEL     Dsed          s34sed                         *SEGNACOLLO DA
044900010301     C                   MOVEL     Dvss          s34vss                         *NO VSS OK
045000970526     C*
045100010227     C                   MOVEL     tis734        KPJBU
045200970526     C                   CALL      WPGM
045300970612     C                   PARM                    KPJBA
045400970526     C*
045500970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
045600010227     C     s34F03        IFEQ      '1'                                          *PREMUTO F03
045700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
045800970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
045900001020     C                   ELSE                                                   *NON PREMUTO F03
046000010227     C                   MOVEL     s34MSG        DSMSMS                         *MESSAGGIO DI RITORN
046100001020     C                   ENDIF
046200970526     C*
046300970526     C                   ENDSR
046400970526     C*------------------------------------------------------------------------*
046500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
046600970526     C*------------------------------------------------------------------------*
046700970526     C     CHKFOP        BEGSR
046800970626     C*
046900970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
047000010227     C     DEPOP0        IFEQ      'P01'
047100970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
047200970626     C                   MOVEL     'S'           OPDATT
047300970626     C                   MOVEL     OPDRIG        OPD(1)
047400010227     C                   ELSE
047500970626     C* OPZIONI
047600000830     C                   MOVEL     OPD(5)        OPDRIG                         *VISUALIZZAZIONE
047700970626     C                   MOVEL     'S'           OPDATT
047800970626     C                   MOVEL     OPDRIG        OPD(5)
047900010227     C                   ENDIF
048000970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
048100010227     C     DEPOP0        IFEQ      'P02'
048200970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
048300970626     C                   MOVEL     'S'           OPDATT
048400970626     C                   MOVEL     OPDRIG        OPD(2)
048500010227     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
048600010227     C                   MOVEL     'S'           OPDATT
048700010227     C                   MOVEL     OPDRIG        OPD(4)
048800970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
048900970626     C                   MOVEL     'S'           OPDATT
049000970626     C                   MOVEL     OPDRIG        OPD(7)
049100970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
049200970626     C                   MOVEL     'S'           FUDATT
049300970626     C                   MOVEL     FUDRIG        FUD(6)
049400010227     C                   ENDIF
049500970626     C*
049600970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
049700020916     C     DUTLPO        IFNE      'S'                                          *FILIALE
049800970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
049900970626     C                   MOVEL     'N'           OPDATT
050000970626     C                   MOVEL     OPDRIG        OPD(4)
050100970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
050200970626     C                   MOVEL     'N'           OPDATT
050300970626     C                   MOVEL     OPDRIG        OPD(7)
050400970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
050500970626     C                   MOVEL     'N'           FUDATT
050600970626     C                   MOVEL     FUDRIG        FUD(6)
050700010227     C                   ENDIF
050800970526     C*
050900970526     C* GESTIONE OPZIONI A VIDEO
051000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
051100970526     C                   EXSR      OPZGES
051200970526     C*
051300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
051400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
051500970526     C                   EXSR      FUNGES
051600970526     C*
051700970526     C                   ENDSR
051800020916     C*--------------------------------------------------------------------------------------------*
051900020916     C* REPERISCE I DATI UTENTE
052000020916     C*--------------------------------------------------------------------------------------------*
052100020916     C     REPDATIUTE    BEGSR
052200020916     C*
052300020916     C* INIZIALIZZA VARIABILI DI WRK
052400020916     C                   CLEAR                   TIBS34DS
052500020916     C                   CLEAR                   AZUTEDS
052600020916     C                   CLEAR                   DDATIUTE
052700020916     C*
052800020916     C     *DTAARA       DEFINE    §azute        azuteds
052900020916     C     *DTAARA       DEFINE    §datiute      ddatiute
053000020916     C                   IN(E)     *DTAARA
053100020916if  1C                   IF        %Error
053200020916     c                   EVAL      I34Tla = 'L'
053300020916     C                   CALL      'TIBS34R'
053400020916     C                   PARM                    Tibs34Ds
053500020916     C                   IN        *DTAARA
053600020916e   1C                   ENDIF
053700020916     C*
053800020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
053900020916     C                   MOVEL(P)  RSUT          DSFIRS           20
054000020916     C*
054100020916     C                   ENDSR
054200970526     C*------------------------------------------------------------------------*
054300010227     C* *INZSR - OPERAZIONI INIZIALI
054400970526     C*------------------------------------------------------------------------*
054500970526     C     *INZSR        BEGSR
054600970526     C*---
054700970526     C* RICEVIMENTO PARAMETRI
054800970526     C*---
054900970526     C     *ENTRY        PLIST
055000970612     C                   PARM                    KPJBA
055100010227     C                   MOVEL     KPJBU         tis734
055200970526     C*
055300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
055400970526     C     XMSGPA        PLIST
055500970526     C                   PARM                    DSMSMS
055600970526     C                   PARM                    DSMSPG
055700970526     C                   PARM                    DSMSKE
055800970526     C                   PARM                    DSMSER
055900970526     C*
056000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
056100970526     C                   CALL      'XKEY'
056200970526     C                   PARM                    DSKEY
056300970526     C*---
056400970526     C* VARIABILI NON RIFERITE AL DATA BASE
056500970526     C*---
056600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
056700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
056800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
056900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
057000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
057100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
057200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
057300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
057400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
057500010227     C     *LIKE         DEFINE    s34OP0        DEPOP0                         *DI DEPOSITO
057600010227     C     *LIKE         DEFINE    s34OP1        DEPOP1
057700010227     C     *LIKE         DEFINE    s34OP0        WOP0                           *DI LAVORO
057800010227     C     *LIKE         DEFINE    s34OP1        WOP1
057900010717     C     *LIKE         DEFINE    s34ksu        Dksu
058000010227     C     *LIKE         DEFINE    s34sed        Dsed
058100010228     C     *LIKE         DEFINE    s34vss        Dvss
058200010717     C     *LIKE         DEFINE    VIDks1        DEPks1
058300010717     C     *LIKE         DEFINE    VIDks2        DEPks2
058400010227     C*---
058500970526     C* VARIABILI DI CONTROLLO
058600970526     C*---
058700970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
058800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
058900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
059000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
059100970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
059200970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
059300970526     C*
059400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
059500010227     C                   MOVEL     'TIS734R'     DSMSPG
059600010227     C                   MOVEL     'TIS734R'     MSGQUE
059700970526     C                   MOVEL     *BLANKS       MSGKEY
059800970526     C                   MOVEL     *BLANKS       DSMSMS
059900970526     C                   MOVEL     *BLANKS       DSMSKE
060000970526     C                   Z-ADD     *ZEROS        DSMSER
060100970526     C*---
060200970526     C* VARIABILI RIFERITE AL DATA BASE
060300970526     C*---
060400010717     C     *LIKE         DEFINE    VTDksu        KVDksu
060500010227     C     *LIKE         DEFINE    VTDsed        KVDsed
060600010717     C     *LIKE         DEFINE    VSSksu        KVSksu
060700010227     C     *LIKE         DEFINE    VSSisv        KVSisv
060800970526     C*---
060900970526     C* CHIAVI DI LETTURA
061000970526     C*---
061100010717     C* LETTURA tivss02L -
061200991104     C     KEYvss        KLIST
061300010717     C                   KFLD                    KVSksu
061400010227     C                   KFLD                    KVSisv
061500040901     C* LETTURA tivtd01L -
061600010227     C     KEYvtd        KLIST
061700010717     C                   KFLD                    KVDksu
061800010227     C                   KFLD                    KVDsed
061900970527     C*
062000970527     C* REPERIMENTO DATI UTENTE
062100020916     C                   EXSR      REPDATIUTE
062200970526     C*
062300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
062400010227     C                   MOVEL     s34OP0        DEPOP0                         *LIVELLO PROCEDURA
062500010227     C                   MOVEL     s34OP1        DEPOP1                         *LIVELLO PROGRAMMA
062600970526     C*
062700970526     C* CALCOLA LA DATA CORRENTE
062800970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
062900970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
063000970526     C                   Z-ADD     WN8           G08DAT
063100970526     C                   Z-ADD     *ZEROS        G08INV
063200970526     C                   MOVEL     '0'           G08ERR
063300970526     C                   CALL      'XSRDA8'
063400970526     C                   PARM                    WLBDA8
063500970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
063600970526     C*
063700010227     C* IMPOSTA I PARAMETRI DI INGRESSO
063800010717     C                   IF        s34ksu <> *blank
063900010717     C                   MOVEL     s34ksu        VIDks1
064000010227     C                   ELSE
064100010717     C                   MOVEL     *blanks       VIDks1
064200010227     C                   ENDIF
064300010227     C*
064400010227     C     ERRL00        IFEQ      '1'
064500010227     C                   MOVEL     '1'           s34ERR
064600010227     C                   MOVEL     DSMSMS        s34MSG
064700010227     C                   ELSE
064800010717     C                   MOVEL     VIDks2        DEPks2
064900010717     C                   MOVEL     VIDks1        DEPks1
065000010227     C                   ENDIF
065100970526     C*
065200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
065300970526     C                   EXSR      CHKFOP
065400970526     C*
065500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
065600010227     C     DEPOP0        IFEQ      'P01'
065700970530     C                   MOVEL     DEC(3)        VIDING
065800010227     C                   ENDIF
065900010227     C     DEPOP0        IFEQ      'P02'
066000970526     C                   MOVEL     DEC(1)        VIDING
066100010227     C                   ENDIF
066200010227     C     DEPOP0        IFEQ      'P05'
066300970526     C                   MOVEL     DEC(2)        VIDING
066400010227     C                   ENDIF
066500970526     C*
066600970526     C                   ENDSR
066700970526     C*------------------------------------------------------------------------*
066800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
067100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
067200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
067300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067500970526     C*------------------------------------------------------------------------*
067600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
067700970526     C*------------------------------------------------------------------------*
067800970526     C     POSSFL        BEGSR
067900970526     C*
068000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
068100970526     C                   EXSR      CLRSFL
068200970526     C*
068300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
068400970526     C                   EXSR      SETFIL
068500970526     C*
068600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
068700970526     C     SFLEOF        IFEQ      0
068800970526     C                   EXSR      CARSFL
068900970526     C                   END
069000970526     C*
069100970526     C                   ENDSR
069200970526     C*------------------------------------------------------------------------*
069300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
069400970526     C*------------------------------------------------------------------------*
069500970526     C     ENTATT        BEGSR
069600970526     C*
069700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
069800970526     C                   Z-ADD     0             OPZOK             1 0
069900970526     C*
070000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
070100970526     C                   Z-ADD     0             SFLVAR            5 0
070200970526     C*
070300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
070400970526     C                   Z-ADD     0             SFLERR
070500970526     C*
070600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
070700970526     C                   Z-ADD     SFLULT        WFLULT
070800970526     C*
070900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
071000010227     C                   READC     S734D05                                98
071100970526     C     *IN98         DOWEQ     '0'
071200970526     C*
071300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
071400970526     C                   ADD       1             SFLVAR
071500970526     C*
071600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
071700970526     C     OPZOK         IFEQ      0
071800970526     C*
071900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
072000970526     C     OPZKEY        IFNE      *BLANKS
072100970526     C                   EXSR      OPZCON
072200970526     C                   END
072300970526     C*
072400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
072500970526     C*            2. PULIZIA SCELTA IN SFL
072600970526     C     OPZOK         IFEQ      0
072700970526     C                   MOVEL     '0'           *IN41
072800970526     C                   MOVEL     *BLANKS       OPZKEY
072900970526     C                   ELSE
073000970526     C*
073100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
073200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
073300970526     C                   MOVEL     '1'           *IN41
073400970526     C                   Z-ADD     SFLULT        SFLERR
073500970526     C                   END
073600970526     C                   ELSE
073700970526     C*
073800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
073900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
074000970526     C                   MOVEL     '1'           *IN41
074100970526     C                   END                                                    OPZOK=0
074200970526     C*
074300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
074400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
074500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
074600970526     C     OPZOK         IFNE      0
074700970526     C                   Z-ADD     SFLERR        SFLNBR
074800970526     C                   ELSE
074900970526     C                   Z-ADD     SFLULT        SFLNBR
075000970526     C                   END
075100970526     C*
075200970526     C* AGGIORNO RECORD SFL
075300010227     C                   UPDATE    S734D05
075400970526     C*
075500010227     C                   READC     S734D05                                98
075600970526     C                   END                                                    DO WHILE
075700970526     C*
075800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
075900970526     C                   Z-ADD     WFLULT        SFLULT
076000970526     C*
076100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
076200970526     C     SFLVAR        IFEQ      0
076300970526     C*---                 MOVEL'1'       FINL00
076400970526     C                   END
076500970526     C*
076600970526     C                   ENDSR
076700970526     C*------------------------------------------------------------------------*
076800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
076900970526     C*------------------------------------------------------------------------*
077000970526     C     CLRSFL        BEGSR
077100970526     C*
077200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
077300970526     C* (0=NON FINE FILE  1=FINE FILE)
077400970526     C                   Z-ADD     0             SFLEOF
077500970526     C*
077600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
077700970526     C                   Z-ADD     0             SFLULT
077800970526     C*
077900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
078000970526     C                   MOVEL     '1'           *IN40
078100010227     C                   WRITE     S734D06
078200970526     C                   MOVEL     '0'           *IN40
078300970526     C*
078400970526     C                   ENDSR
078500970526     C*------------------------------------------------------------------------*
078600970526     C* CARSFL - CARICAMENTO SFL
078700970526     C*------------------------------------------------------------------------*
078800970526     C     CARSFL        BEGSR
078900970526     C*
079000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
079100970526     C     SFLULT        IFEQ      0
079200970526     C                   EXSR      SETFIL
079300970526     C                   END
079400970526     C*
079500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
079600970526     C                   Z-ADD     SFLULT        WFLULT
079700970526     C*
079800970526     C* CICLO:
079900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
080000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
080100970526     C                   Z-ADD     1             SFLRIG
080200970526     C     SFLRIG        DOWLE     SFLPAG
080300970526     C     SFLEOF        ANDEQ     0
080400970526     C*
080500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
080600970526     C                   MOVEL     *BLANKS       OPZKEY
080700970526     C*
080800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
080900970526     C                   EXSR      IRESFL
081000970526     C*
081100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
081200970526     C                   ADD       1             SFLULT
081300970526     C*
081400970526     C* SCRITTURA RECORD SFL
081500010227     C                   WRITE     S734D05
081600970526     C                   ADD       1             SFLRIG
081700970526     C*
081800970526     C* LETTURA FILE DA CARICARE
081900970526     C                   EXSR      LETFIL
082000970526     C                   END                                                    DO WHILE
082100970526     C*
082200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
082300970526     C     WFLULT        IFNE      SFLULT
082400970526     C                   Z-ADD     WFLULT        SFLNBR
082500970526     C                   ADD       1             SFLNBR
082600970526     C                   END
082700970526     C*
082800970526     C                   ENDSR
082900970526     C*------------------------------------------------------------------------*
083000970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
083100970526     C*------------------------------------------------------------------------*
083200970526     C     OUTSFL        BEGSR
083300970526     C*
083400970526     C* WRITE RECORD TESTATA VIDEO
083500010227     C                   WRITE     S734D01
083600970526     C*
083700970526     C* VISUALIZZAZIONE MESSAGGIO
083800970526     C                   EXSR      SNDMSG
083900970526     C*
084000970526     C* WRITE RECORD OPZIONI VIDEO
084100010227     C                   WRITE     S734D02
084200970526     C*
084300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
084400010227     C                   WRITE     S734D03
084500970526     C*
084600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
084700010227     C                   WRITE     S734D04
084800970526     C*
084900970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
085000970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
085100970526     C     SFLEOF        IFEQ      0
085200970526     C                   MOVEL     '0'           *IN42
085300970526     C                   ELSE
085400970526     C*
085500970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
085600970526     C                   MOVEL     '1'           *IN42
085700970526     C                   END
085800970526     C*
085900970526     C* WRITE RECORD TASTI FUNZIONALI
086000010227     C                   WRITE     S734D09
086100970526     C*
086200970526     C* CASO DI SFL VUOTO
086300970526     C     SFLULT        IFLE      0
086400970526     C*
086500970526     C*    - WRITE RECORD VUOTO
086600010227     C                   WRITE     S734D0A
086700010227     C                   READ      S734D0A                                99
086800970526     C*
086900970526     C                   ELSE
087000970526     C*
087100970526     C* CASO DI SFL CON RECORDS
087200970526     C*
087300970526     C*    - WRITE RECORD CONTROLLO SFL
087400010227     C                   WRITE     S734D06
087500010227     C                   READ      S734D06                                99
087600970526     C                   END
087700970526     C*
087800970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
087900010227     C                   READ      S734D03                                99
088000970526     C*
088100970526     C                   ENDSR
088200970526     C*------------------------------------------------------------------------*
088300970526     C* SNDMSG - MANDA MESSAGGIO
088400970526     C*------------------------------------------------------------------------*
088500970526     C     SNDMSG        BEGSR
088600970526     C*
088700970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
088800970526     C                   MOVEL     '1'           *IN40
088900010227     C                   WRITE     S734DMC
089000970526     C                   MOVEL     '0'           *IN40
089100970526     C*
089200970526     C* VISUALIZZAZIONE MESSAGGIO
089300970526     C                   Z-ADD     0             MSGULT
089400970526     C                   CALL      'XMSG'        XMSGPA
089500970526     C     DSMSER        IFEQ      0
089600970526     C                   MOVEL     DSMSKE        MSGKEY
089700970526     C                   END
089800970526     C                   Z-ADD     1             MSGULT
089900010227     C                   WRITE     S734DMS                                      SFL MESS
090000010227     C                   WRITE     S734DMC                                      CONTR MESS
090100970526     C*
090200970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
090300970526     C                   MOVEL     *BLANKS       DSMSMS
090400970526     C*
090500970526     C                   ENDSR
090600970526     C*------------------------------------------------------------------------*
090700970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
090800970526     C*------------------------------------------------------------------------*
090900970526     C     FUNCON        BEGSR
091000970526     C*
091100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
091200970526     C                   EXSR      FUNVAR
091300970526     C*
091400970526     C* PULISCO CAMPI DI LAVORO
091500970526     C                   Z-ADD     0             I                 5 0
091600970526     C                   Z-ADD     0             W                 5 0
091700970526     C*
091800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
091900970526     C     FUNKEY        IFNE      *BLANKS
092000970526     C     1             DO        24            I
092100970526     C                   MOVEL     FUD(I)        FUDRIG
092200970526     C*
092300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
092400970526     C     FUNKEY        IFEQ      FUDFUN
092500970526     C     FUDATT        ANDEQ     'S'
092600970526     C                   Z-ADD     I             W
092700970526     C                   END
092800970526     C                   END                                                    DO WHILE
092900970526     C*
093000970526     C* FUNZIONE NON ATTIVA (ERRORE)
093100970526     C     W             IFEQ      0
093200970526     C*
093300970526     C* EMISSIONE MESSAGGIO DI ERRORE
093400970526     C                   MOVEL     MSG(2)        DSMSMS
093500970526     C                   END                                                    W=0
093600970526     C                   END                                                    FUNKEY NE *BLANKS
093700970526     C*
093800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
093900970526     C     W             IFNE      0
094000970526     C     FUNKEY        OREQ      *BLANKS
094100970526     C*
094200970526     C     FUNKEY        IFEQ      *BLANKS
094300970526     C                   EXSR      ENTGES
094400970526     C                   END
094500970526     C     FUNKEY        IFEQ      '03'
094600970526     C                   EXSR      F03GES
094700970526     C                   END
094800970526     C     FUNKEY        IFEQ      '05'
094900970526     C                   EXSR      F05GES
095000970526     C                   END
095100970526     C     FUNKEY        IFEQ      '06'
095200970526     C                   EXSR      F06GES
095300970526     C                   END
095400970526     C     FUNKEY        IFEQ      '12'
095500970526     C                   EXSR      F12GES
095600970526     C                   END
095700010227     C*    FUNKEY        IFEQ      '21'
095800010227     C*                  EXSR      F21GES
095900010227     C*                  END
096000010227     C*    FUNKEY        IFEQ      '22'
096100010227     C*                  EXSR      F22GES
096200010227     C*                  END
096300970526     C     FUNKEY        IFEQ      '23'
096400970526     C                   EXSR      F23GES
096500970526     C                   END
096600970526     C     FUNKEY        IFEQ      '24'
096700970526     C                   EXSR      F24GES
096800970526     C                   END
096900970526     C     FUNKEY        IFEQ      'RU'
097000970526     C                   EXSR      RUPGES
097100970526     C                   END
097200970526     C*
097300970526     C                   END
097400970526     C*
097500970526     C                   ENDSR
097600970526     C*------------------------------------------------------------------------*
097700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
097800970526     C*------------------------------------------------------------------------*
097900970526     C     FUNVAR        BEGSR
098000970526     C*
098100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
098200970526     C     KEY           IFEQ      ENTER
098300970526     C                   MOVEL     *BLANKS       FUNKEY
098400970526     C                   END
098500970526     C     KEY           IFEQ      F1
098600970526     C                   MOVEL     '01'          FUNKEY
098700970526     C                   END
098800970526     C     KEY           IFEQ      F2
098900970526     C                   MOVEL     '02'          FUNKEY
099000970526     C                   END
099100970526     C     KEY           IFEQ      F3
099200970526     C                   MOVEL     '03'          FUNKEY
099300970526     C                   END
099400970526     C     KEY           IFEQ      F4
099500970526     C                   MOVEL     '04'          FUNKEY
099600970526     C                   END
099700970526     C     KEY           IFEQ      F5
099800970526     C                   MOVEL     '05'          FUNKEY
099900970526     C                   END
100000970526     C     KEY           IFEQ      F6
100100970526     C                   MOVEL     '06'          FUNKEY
100200970526     C                   END
100300970526     C     KEY           IFEQ      F7
100400970526     C                   MOVEL     '07'          FUNKEY
100500970526     C                   END
100600970526     C     KEY           IFEQ      F8
100700970526     C                   MOVEL     '08'          FUNKEY
100800970526     C                   END
100900970526     C     KEY           IFEQ      F9
101000970526     C                   MOVEL     '09'          FUNKEY
101100970526     C                   END
101200970526     C     KEY           IFEQ      F10
101300970526     C                   MOVEL     '10'          FUNKEY
101400970526     C                   END
101500970526     C     KEY           IFEQ      F11
101600970526     C                   MOVEL     '11'          FUNKEY
101700970526     C                   END
101800970526     C     KEY           IFEQ      F12
101900970526     C                   MOVEL     '12'          FUNKEY
102000970526     C                   END
102100970526     C     KEY           IFEQ      F13
102200970526     C                   MOVEL     '13'          FUNKEY
102300970526     C                   END
102400970526     C     KEY           IFEQ      F14
102500970526     C                   MOVEL     '14'          FUNKEY
102600970526     C                   END
102700970526     C     KEY           IFEQ      F15
102800970526     C                   MOVEL     '15'          FUNKEY
102900970526     C                   END
103000970526     C     KEY           IFEQ      F16
103100970526     C                   MOVEL     '16'          FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F17
103400970526     C                   MOVEL     '17'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F18
103700970526     C                   MOVEL     '18'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F19
104000970526     C                   MOVEL     '19'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F20
104300970526     C                   MOVEL     '20'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F21
104600970526     C                   MOVEL     '21'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F22
104900970526     C                   MOVEL     '22'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F23
105200970526     C                   MOVEL     '23'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F24
105500970526     C                   MOVEL     '24'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      ROLLUP
105800970526     C                   MOVEL     'RU'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      HELP
106100970526     C                   MOVEL     'HP'          FUNKEY
106200970526     C                   END
106300970526     C*
106400970526     C                   ENDSR
106500970526     C*------------------------------------------------------------------------*
106600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
106700970526     C*------------------------------------------------------------------------*
106800970526     C     FUNGES        BEGSR
106900970526     C*
107000970526     C* PULISCO CAMPI DI LAVORO
107100970526     C                   Z-ADD     0             W                 5 0
107200970526     C*
107300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
107400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
107500970526     C     1             DO        24            I
107600970526     C                   MOVEL     FUD(I)        FUDRIG
107700970526     C*
107800970526     C* PULIZIA FUNZIONI PRECEDENTI
107900970526     C                   MOVEL     *BLANKS       FUV(I)
108000970526     C*
108100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
108200970526     C* (SALTA F23 E F24)
108300970526     C     FUDATT        IFEQ      'S'
108400970526     C     FUDFUN        ANDNE     '23'
108500970526     C     FUDFUN        ANDNE     '24'
108600970526     C                   ADD       1             W
108700970526     C                   MOVEL     FUDDES        FUV(W)
108800970526     C                   END
108900970526     C                   END                                                    DO WHILE
109000970526     C*
109100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
109200970526     C* ALTRIMENTI DISATTIVO F24
109300970526     C     1             DO        24            I
109400970526     C                   MOVEL     FUD(I)        FUDRIG
109500970526     C*
109600970526     C     FUDFUN        IFEQ      '24'
109700970526     C     W             IFGT      6
109800970526     C                   MOVEL     'S'           FUDATT
109900970526     C                   ELSE
110000970526     C                   MOVEL     *BLANKS       FUDATT
110100970526     C                   END                                                    W GT 8
110200970526     C                   MOVEL     FUDRIG        FUD(I)
110300970526     C                   END
110400970526     C                   END                                                    DO WHILE
110500970526     C*
110600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
110700970526     C     W             DIV       6             I
110800970526     C                   MVR                     Z
110900970526     C*
111000970526     C     Z             IFNE      0
111100970526     C     I             ADD       1             Z
111200970526     C                   ELSE
111300970526     C                   Z-ADD     I             Z
111400970526     C                   END
111500970526     C*
111600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
111700970526     C                   ADD       1             FUNGRU
111800970526     C     FUNGRU        IFGT      Z
111900970526     C     FUNGRU        ORLT      1
112000970526     C                   Z-ADD     1             FUNGRU
112100970526     C                   END
112200970526     C*
112300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
112400970526     C     FUNGRU        IFEQ      1
112500970526     C                   MOVEL     FUV(1)        FUN001
112600970526     C                   MOVEL     FUV(2)        FUN002
112700970526     C                   MOVEL     FUV(3)        FUN003
112800970526     C                   MOVEL     FUV(4)        FUN004
112900970526     C                   MOVEL     FUV(5)        FUN005
113000970526     C                   MOVEL     FUV(6)        FUN006
113100970526     C                   END
113200970526     C*
113300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
113400970526     C     FUNGRU        IFEQ      2
113500970526     C                   MOVEL     FUV(7)        FUN001
113600970526     C                   MOVEL     FUV(8)        FUN002
113700970526     C                   MOVEL     FUV(9)        FUN003
113800970526     C                   MOVEL     FUV(10)       FUN004
113900970526     C                   MOVEL     FUV(11)       FUN005
114000970526     C                   MOVEL     FUV(12)       FUN006
114100970526     C                   END
114200970526     C*
114300970526     C* TERZO GRUPPO DI 6 FUNZIONI
114400970526     C     FUNGRU        IFEQ      3
114500970526     C                   MOVEL     FUV(13)       FUN001
114600970526     C                   MOVEL     FUV(14)       FUN002
114700970526     C                   MOVEL     FUV(15)       FUN003
114800970526     C                   MOVEL     FUV(16)       FUN004
114900970526     C                   MOVEL     FUV(17)       FUN005
115000970526     C                   MOVEL     FUV(18)       FUN006
115100970526     C                   END
115200970526     C*
115300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
115400970526     C     FUNGRU        IFEQ      4
115500970526     C                   MOVEL     FUV(19)       FUN001
115600970526     C                   MOVEL     FUV(20)       FUN002
115700970526     C                   MOVEL     FUV(21)       FUN003
115800970526     C                   MOVEL     FUV(22)       FUN004
115900970526     C                   MOVEL     *BLANKS       FUN005
116000970526     C                   MOVEL     *BLANKS       FUN006
116100970526     C                   END
116200970526     C*
116300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
116400970526     C                   MOVEL     *BLANKS       FUN007
116500970526     C                   MOVEL     *BLANKS       FUN008
116600970526     C*
116700970526     C     1             DO        24            I
116800970526     C                   MOVEL     FUD(I)        FUDRIG
116900970526     C     FUDFUN        IFEQ      '23'
117000970526     C     FUDATT        ANDEQ     'S'
117100970526     C                   MOVEL     FUDDES        FUN007
117200970526     C                   END
117300970526     C     FUDFUN        IFEQ      '24'
117400970526     C     FUDATT        ANDEQ     'S'
117500970526     C                   MOVEL     FUDDES        FUN008
117600970526     C                   END
117700970526     C                   END                                                    DO
117800970526     C*
117900970526     C                   ENDSR
118000970526     C*------------------------------------------------------------------------*
118100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
118200970526     C*------------------------------------------------------------------------*
118300970526     C     OPZGES        BEGSR
118400970526     C*
118500970526     C* PULISCO CAMPI DI LAVORO
118600970526     C                   Z-ADD     0             W                 5 0
118700970526     C*
118800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
118900970526     C* E CARICO TABELLA OPZIONI ATTIVE
119000970526     C     1             DO        24            I
119100970526     C                   MOVEL     OPD(I)        OPDRIG
119200970526     C*
119300970526     C* PULIZIA OPZIONI PRECEDENTI
119400970526     C                   MOVEL     *BLANKS       OPV(I)
119500970526     C*
119600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
119700970526     C     OPDATT        IFEQ      'S'
119800970526     C                   ADD       1             W
119900970526     C                   MOVEL     OPDDES        OPV(W)
120000970526     C                   END
120100970526     C                   END                                                    DO WHILE
120200970526     C*
120300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
120400970526     C* ALTRIMENTI DISATTIVO F23
120500970526     C     1             DO        24            I
120600970526     C                   MOVEL     FUD(I)        FUDRIG
120700970526     C*
120800970526     C     FUDFUN        IFEQ      '23'
120900970526     C     W             IFGT      8
121000970526     C                   MOVEL     'S'           FUDATT
121100970526     C                   ELSE
121200970526     C                   MOVEL     *BLANKS       FUDATT
121300970526     C                   END                                                    W GT 8
121400970526     C                   MOVEL     FUDRIG        FUD(I)
121500970526     C                   END
121600970526     C                   END                                                    DO WHILE
121700970526     C*
121800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
121900970526     C     W             DIV       8             I
122000970526     C                   MVR                     Z                 5 0
122100970526     C*
122200970526     C     Z             IFNE      0
122300970526     C     I             ADD       1             Z
122400970526     C                   ELSE
122500970526     C                   Z-ADD     I             Z
122600970526     C                   END
122700970526     C*
122800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
122900970526     C                   ADD       1             OPZGRU
123000970526     C     OPZGRU        IFGT      Z
123100970526     C     OPZGRU        ORLT      1
123200970526     C                   Z-ADD     1             OPZGRU
123300970526     C                   END
123400970526     C*
123500970526     C* PRIMO GRUPPO DI 8 OPZIONI
123600970526     C     OPZGRU        IFEQ      1
123700970526     C                   MOVEL     OPV(1)        OPZ001
123800970526     C                   MOVEL     OPV(2)        OPZ002
123900970526     C                   MOVEL     OPV(3)        OPZ003
124000970526     C                   MOVEL     OPV(4)        OPZ004
124100970526     C                   MOVEL     OPV(5)        OPZ005
124200970526     C                   MOVEL     OPV(6)        OPZ006
124300970526     C                   MOVEL     OPV(7)        OPZ007
124400970526     C                   MOVEL     OPV(8)        OPZ008
124500970526     C                   END
124600970526     C*
124700970526     C* SECONDO GRUPPO DI 8 OPZIONI
124800970526     C     OPZGRU        IFEQ      2
124900970526     C                   MOVEL     OPV(9)        OPZ001
125000970526     C                   MOVEL     OPV(10)       OPZ002
125100970526     C                   MOVEL     OPV(11)       OPZ003
125200970526     C                   MOVEL     OPV(12)       OPZ004
125300970526     C                   MOVEL     OPV(13)       OPZ005
125400970526     C                   MOVEL     OPV(14)       OPZ006
125500970526     C                   MOVEL     OPV(15)       OPZ007
125600970526     C                   MOVEL     OPV(16)       OPZ008
125700970526     C                   END
125800970526     C*
125900970526     C* TERZO GRUPPO DI 8 OPZIONI
126000970526     C     OPZGRU        IFEQ      3
126100970526     C                   MOVEL     OPV(17)       OPZ001
126200970526     C                   MOVEL     OPV(18)       OPZ002
126300970526     C                   MOVEL     OPV(19)       OPZ003
126400970526     C                   MOVEL     OPV(20)       OPZ004
126500970526     C                   MOVEL     OPV(21)       OPZ005
126600970526     C                   MOVEL     OPV(22)       OPZ006
126700970526     C                   MOVEL     OPV(23)       OPZ007
126800970526     C                   MOVEL     OPV(24)       OPZ008
126900970526     C                   END
127000970526     C*
127100970526     C                   ENDSR
127200970526     C*------------------------------------------------------------------------*
127300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
127400970526     C*------------------------------------------------------------------------*
127500970526     C     OPZCON        BEGSR
127600970526     C*
127700970526     C* PULISCO CAMPI DI LAVORO
127800970526     C                   Z-ADD     0             I                 5 0
127900970526     C                   Z-ADD     0             W                 5 0
128000970526     C*
128100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
128200970526     C     1             DO        24            I
128300970526     C                   MOVEL     OPD(I)        OPDRIG
128400970526     C*
128500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
128600970526     C     OPZKEY        IFEQ      OPDOPZ
128700970526     C     OPZKEY        OREQ      OPDOP1
128800970526     C*
128900970526     C     OPDATT        IFEQ      'S'
129000970526     C                   MOVEL     OPDOPZ        OPZKEY
129100970526     C                   Z-ADD     I             W
129200970526     C                   END
129300970526     C                   END
129400970526     C*
129500970526     C                   END                                                    DO WHILE
129600970526     C*
129700970526     C* OPZIONE NON ATTIVA (ERRORE)
129800970526     C     W             IFEQ      0
129900970526     C*
130000970526     C* OPZIONE NON OK
130100970526     C                   Z-ADD     1             OPZOK
130200970526     C*
130300970526     C* EMISSIONE MESSAGGIO DI ERRORE
130400970526     C                   MOVEL     MSG(1)        DSMSMS
130500970526     C                   ELSE
130600970526     C*
130700970526     C* OPZIONE ATTIVA (PROCEDI ...)
130800970526     C*
130900010227     C*    OPZKEY        IFEQ      ' 1'
131000010227     C*                  EXSR      O01GES
131100010227     C*                  END
131200970526     C     OPZKEY        IFEQ      ' 2'
131300970526     C                   EXSR      O02GES
131400970526     C                   END
131500010227     C*    OPZKEY        IFEQ      ' 3'
131600010227     C*                  EXSR      O03GES
131700010227     C*                  END
131800010227     C     OPZKEY        IFEQ      ' 4'
131900010227     C                   EXSR      O04GES
132000010227     C                   END
132100970526     C     OPZKEY        IFEQ      ' 5'
132200970526     C                   EXSR      O05GES
132300970526     C                   END
132400010227     C     OPZKEY        IFEQ      ' 7'
132500010227     C                   EXSR      O07GES
132600010227     C                   END
132700010227     C*    OPZKEY        IFEQ      ' 8'
132800010227     C*                  EXSR      O08GES
132900010227     C*                  END
133000010227     C*    OPZKEY        IFEQ      ' 9'
133100010227     C*                  EXSR      O09GES
133200010227     C*                  END
133300010227     C*    OPZKEY        IFEQ      '10'
133400010227     C*                  EXSR      O10GES
133500010227     C*                  END
133600970526     C                   END                                                    OPZ.NON ATTIVA
133700970526     C*
133800970526     C                   ENDSR
133900970526     C*------------------------------------------------------------------------*
134000970526     C* NOTE COSTRUZIONE SCHIERE
134100970526     C*------------------------------------------------------------------------*
134200970526     C*
134300970526     C* -MSG-MESSAGGI
134400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
134500970526     C* (NON TOCCARE MSG,1 E MSG,2)
134600970526     C*
134700970526     C* -CMD-COMANDI
134800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
134900970526     C*
135000970526     C* -OPD-OPZIONI
135100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
135200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
135300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
135400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
135500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
135600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
135700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
135800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
135900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
136000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
136100970526     C*
136200970526     C* -FUD-FUNZIONI
136300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
136400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
136500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
136600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
136700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
136800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
136900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
137000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
137100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
137200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
137300970526     O*------------------------------------------------------------------------*
137400970526** DEC - DECODIFICHE
137500970526    GESTIONE                                                     1
137600970526INTERROGAZIONE                                                   2
137700970530     SCELTA                                                      3
1378009705264
1379009705265
138000970526** MSG - MESSAGGI
138100010227TIS734R- Opzione non gestita                                                   1
138200010227TIS734R- Tasto funzionale non gestito                                          2
138300010227TIS734R- 3                                                                     3
138400010227TIS734R- Record annullato. Consentito solo il ripristino                       4
138500010227TIS734R- Record non annullato. Impossibile ripristinare                        5
138600010227TIS734R- 6                                                                     6
138700010227TIS734R- 7                                                                     7
138800010227TIS734R- 8                                                                     8
138900010227TIS734R- 9                                                                     9
139000010227TIS734R- 10                                                                   10
139100010227TIS734R- 11                                                                   11
139200010227TIS734R- 12                                                                   12
139300010227TIS734R- 13                                                                   13
139400010227TIS734R- 14                                                                   14
139500010227TIS734R- 15                                                                   15
139600010227TIS734R- 16                                                                   16
139700010227TIS734R- 17                                                                   17
139800010227TIS734R- 18                                                                   18
139900010227TIS734R- 19                                                                   19
140000010227TIS734R- 20                                                                   20
140100970528** CMD - COMANDI CL
140200970528WRKSPLF
140300970528
140400991108
140500970528
140600970528
140700970526** OPD - OPZIONI
140800970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
140900970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
141000991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
141100991202                   -4 -     (S/N)=N                        opz. 04¬
141200970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
141300970526                   -6 -     (S/N)=N                        opz. 06¬
141400991202                   -7 -     (S/N)=N                        opz. 07¬
141500010227                   -8 -     (S/N)=N                        opz. 08¬
141600010227                   -9 -     (S/N)=N                        opz. 09¬
141700010227                   -10-     (S/N)=N                        opz. 10¬
141800970526                   -  -     (S/N)=N                        opz. 11¬
141900970526                   -  -     (S/N)=N                        opz. 12¬
142000970526                   -  -     (S/N)=N                        opz. 13¬
142100970526                   -  -     (S/N)=N                        opz. 14¬
142200970526                   -  -     (S/N)=N                        opz. 15¬
142300970526                   -  -     (S/N)=N                        opz. 16¬
142400970526                   -  -     (S/N)=N                        opz. 17¬
142500970526                   -  -     (S/N)=N                        opz. 18¬
142600970526                   -  -     (S/N)=N                        opz. 19¬
142700970526                   -  -     (S/N)=N                        opz. 20¬
142800970526                   -  -     (S/N)=N                        opz. 21¬
142900970526                   -  -     (S/N)=N                        opz. 22¬
143000970526                   -  -     (S/N)=N                        opz. 23¬
143100970526                   -  -     (S/N)=N                        opz. 24¬
143200970526** FUD - FUNZIONI
143300970526                   -        (S/N)=N                        funz.01¬
143400970526                   -        (S/N)=N                        funz.02¬
143500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
143600970526                   -        (S/N)=N                        funz.04¬
143700970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
143800970526F06=Inserimento    -        (S/N)=N                        funz.06¬
143900970526                   -        (S/N)=N                        funz.07¬
144000970527                   -        (S/N)=N                        funz.08¬
144100970527                   -        (S/N)=N                        funz.09¬
144200970527                   -        (S/N)=N                        funz.10¬
144300970526                   -        (S/N)=N                        funz.11¬
144400970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
144500970526                   -        (S/N)=N                        funz.13¬
144600970526                   -        (S/N)=N                        funz.14¬
144700970526                   -        (S/N)=N                        funz.15¬
144800970526                   -        (S/N)=N                        funz.16¬
144900970526                   -        (S/N)=N                        funz.17¬
145000970526                   -        (S/N)=N                        funz.18¬
145100970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
145200970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
145300010227                   -        (S/N)=N                        funz.21¬
145400010227                   -        (S/N)=N                        funz.22¬
145500970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
145600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
