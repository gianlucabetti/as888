000100980429     H*------------------------------------------------------------------------*
000200980429     H* GESTIONE DATI TIPO GENERALI - SCELTA                                   *
000300980429     H* NOTE: E' PREVISTA SOLO LA SCELTA DEI DATI TIPO, QUINDI NO GESTIONE E   *
000400980429     H*       NO OPZIONI DIVERSE DA QUELLE DI INTERROGAZIONE DI ALTRI FILES.   *
000500980429     H*       PER ORA NON RICHIAMABILE DA MENU'.                               *
000600980429     H*------------------------------------------------------------------------*
000700980429     H DECEDIT('0,') DATEDIT(*DMY.)
000800980429     F*------------------------------------------------------------------------*
000900980429     F* DATA BASE
001000980429     F*------------------------------------------------------------------------*
001100980429     FTNTE32D   CF   E             WORKSTN INFDS(DEVDS1)
001200980429     F                                     SFILE(TE32D05:SFLULT)
001300980429     F                                     SFILE(TE32DMS:MSGULT)
001400011001     FTEETC02L  IF   E           K DISK
001500011001     F                                     RENAME(TEETC000:TEETC2)
001600011001     FTEETS01L  IF   E           K DISK
001601060830     FTEETS11L  IF   E           K DISK    rename(teets000:teets100)
001700011001     FTEETT01L  IF   E           K DISK
001800980429     FAZORG01L  IF   E           K DISK
001900020313     FTABEL00F  IF   E           K DISK
002000980803     FCNACO00F  IF   E           K DISK
002100041202     FTFACO00F  IF   E           K DISK    usropn
002200980803     F                                     RENAME(CNACO000:TFACO000)
002400980429     D*------------------------------------------------------------------------*
002500980429     D* SCHIERE
002600980429     D*------------------------------------------------------------------------*
002700980429     D*------------
002800980429     D* DECODIFICHE
002900980429     D*------------
003000980429     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003100980429     D*------------
003200980429     D* MESSAGGI
003300980429     D*------------
003400980429     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003500980429     D*------------
003600980429     D* DESCRIZIONI OPZIONI
003700980429     D*------------
003800980429     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003900980429     D*------------
004000980429     D* DESCRIZIONI TASTI FUNZIONALI
004100980429     D*------------
004200980429     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004300980429     D*------------
004400980429     D* OPZIONI DA CARICARE A VIDEO
004500980429     D*------------
004600980429     D OPV             S             19    DIM(24)
004700980429     D*------------
004800980429     D* FUNZIONI DA CARICARE A VIDEO
004900980429     D*------------
005000980429     D FUV             S             19    DIM(24)
005100980429     D*------------
005200980429     D* ORGANIGRAMMA
005300980429     D*------------
005400980429     D FIL             S              3  0 DIM(400)                             *FILIALI
005500980429     D FDES            S             20    DIM(400)                             *DESCRIZIONI
005600980429     D*------------------------------------------------------------------------*
005700980429     D* INPUT
005800980429     D*------------------------------------------------------------------------*
005900980429     D*------------
006000980429     D* RIDEFINIZIONE OPZIONI VIDEO
006100980429     D*------------
006200980429     D OPDDS           DS
006300980429     D  OPDOPZ                 1      2
006400980429     D  OPDDE1                 3     19
006500980429     D  OPDOP1                21     22
006600980429     D  OPDFI0                23     34
006700980429     D  OPDATT                35     35
006800980429     D  OPDFI1                36     80
006900980429     D  OPDDES                 1     19
007000980429     D  OPDRIG                 1     80
007100980429     D*------------
007200980429     D* RIDEFINIZIONE FUNZIONI VIDEO
007300980429     D*------------
007400980429     D FUDDS           DS
007500980429     D  FUDDE1                 1      1
007600980429     D  FUDFUN                 2      3
007700980429     D  FUDDE2                 4     19
007800980429     D  FUDFI0                20     34
007900980429     D  FUDATT                35     35
008000980429     D  FUDFI1                36     80
008100980429     D  FUDDES                 1     19
008200980429     D  FUDRIG                 1     80
008300980429     D*------------
008400980429     D* DS GESTIONE TASTI FUNZIONALI
008500980429     D*------------
008600091123     D tnte04ds      E DS
008601091123     D DSKEY         E DS
008700980429     D DEVDS1          DS
008800980429     D  KEY                  369    369
008900980429     D*------------
009000980429     D* DS "XMSG" - GESTIONE MESSAGGI
009100980429     D*------------
009200980429     D DSMSG         E DS
009300980429     D*------------
009400020923     D* DS REPERIMENTO DATI UTENTE
009500980429     D*------------
009600020923     D TIBS34DS      E DS                                                       *Profili utente
009700020923     D DDATIUTE      E DS                                                       *Dati utente
009800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009900020925     D*------------
010000020925     D* DS CONTROLLO/SCELTA TABELLE
010100020925     D*------------
010200020925     D TIBS02DS      E DS                                                       *Ricerca tabella
010300020925     D*------------------
010400020925     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
010500020925     D*-------------------
010600020925     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
010700020925     D*------------------
010800020925     D* DS FUNZIONI AZIENDALI
010900020925     D*-------------------
011000020925     D DLAT          E DS                  INZ                                  *Funzioni aziendali
011100020925     D*------------------
011200020925     D* DS CARICAMENTO P.O. GESTITI
011300020925     D*-------------------
011400020925     D TRUL31DS      E DS                  INZ
011500020925     D*------------------
011600020925     D* SCHIERA X LINEEE CONSENTITE
011700020925     D*------------------
011800020925     D L1              S              3    DIM(250)
011900980429     D*------------------
012000980429     D* DS "XSRDA8" - CONTROLLA DATA (8)
012100980429     D*------------------
012200980429     D WLBDA8          DS                  INZ
012300980429     D  G08DAT                 1      8  0
012400980429     D  G08INV                 9     16  0
012500980429     D  G08ERR                17     17
012600980429     D  G08TGI                18     22  0
012700980429     D*------------
012800980429     D* DS DI PROCEDURA
012900980429     D*------------
013000980429     D DSTE30        E DS                  EXTNAME(TNTE30DS)
013100980429     D*------------
013200980429     D* ARCHITETTURA
013300980429     D*------------
013400980429     D KPJBA         E DS
013500980429     C*------------------------------------------------------------------------*
013600980429     C* MAIN LINES
013700980429     C*------------------------------------------------------------------------*
013800980429     C*
013900980429     C* SE NON CI SONO ERRORI, PROSEGUE
014000980429IF  1C     ERRL00        IFNE      '1'
014100980429     C*
014200980429     C* CARICA IL SFILE
014300980429     C                   EXSR      POSSFL
014400980429     C*
014500980429     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014600980429DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014700980429     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014800980429     C     FINL00        OREQ      '1'                                          *FINE PGM
014900980429     C*
015000980429     C* REIMPOSTA ERRORE DI PROCERURA
015100980429     C                   MOVEL     '0'           ERRL00
015200980429     C*
015300980429     C* EMETTE IL SFILE
015400980429     C                   EXSR      OUTSFL
015500980429     C*
015600980429     C* GESTIONE TASTI FUNZIONALI
015700980429     C                   EXSR      FUNCON
015800980429     C*
015900980429E   2C                   ENDDO
016000980429     C*
016100980429     C* CI SONO ERRORI IN ENTRATA
016200980429X   1C                   ELSE
016300980429     C                   MOVEL     '1'           D30ERR
016400980429     C                   MOVEL     DSMSMS        D30MSG
016500980429E   1C                   ENDIF
016600980429     C* FINE PGM
016700980429     C                   SETON                                        LR
016800980429     C*------------------------------------------------------------------------*
016900980429     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017000980429     C*------------------------------------------------------------------------*
017100980429     C     F03GES        BEGSR
017200980429     C*
017300980429     C* IMPOSTA "PREMUTO F03"
017400980429     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500980429     C                   MOVEL     '1'           D30F03                         *DI PROCEDURA
017600980429     C*
017700980429     C                   ENDSR
017800980429     C*------------------------------------------------------------------------*
017900980429     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018000980429     C*------------------------------------------------------------------------*
018100980429     C     F05GES        BEGSR
018200980429     C*
018300980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
018400980429     C*
018500980429     C                   ENDSR
018600980429     C*------------------------------------------------------------------------*
018700980429     C* F06GES - GESTIONE F06 -> INSERIMENTO
018800980429     C*------------------------------------------------------------------------*
018900980429     C     F06GES        BEGSR
019000980429     C*
019100980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019300980429     C                   MOVEL     *BLANKS       DCTO
019400980429     C                   Z-ADD     *ZEROS        DKSC
019500980429     C                   Z-ADD     *ZEROS        DCTR
019600980429     C                   Z-ADD     *ZEROS        DPRG
019700980429     C                   Z-ADD     *ZEROS        DLNP
019800011003     C                   MOVEL     *BLANKS       DDIV
019900980429     C                   MOVEL     DEPOP0        WOP0
020000980429     C                   MOVEL     'F06'         WOP1
020100980429     C                   MOVEL     'TNTE33R'     WPGM              8
020200980429     C                   EXSR      LANPGM                                       *LANCIA PGM
020300980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
020400980429E   1C                   ENDIF
020500980429     C*
020600980429     C                   ENDSR
020700980429     C*------------------------------------------------------------------------*
020800980429     C* F12GES - GESTIONE F12 -> RITORNO
020900980429     C*------------------------------------------------------------------------*
021000980429     C     F12GES        BEGSR
021100980429     C*
021200980429     C* IMPOSTA "PREMUTO F12"
021300980429     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021400980429     C                   MOVEL     '1'           D30F12                         *DI PROCEDURA
021500980429     C*
021600980429     C                   ENDSR
021700980429     C*------------------------------------------------------------------------*
021800980429     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021900980429     C*------------------------------------------------------------------------*
022000980429     C     F23GES        BEGSR
022100980429     C*
022200980429     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022300980429     C*
022400980429     C                   ENDSR
022500980429     C*------------------------------------------------------------------------*
022600980429     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022700980429     C*------------------------------------------------------------------------*
022800980429     C     F24GES        BEGSR
022900980429     C*
023000980429     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023100980429     C*
023200980429     C                   ENDSR
023300980429     C*------------------------------------------------------------------------*
023400980429     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023500980429     C*------------------------------------------------------------------------*
023600980429     C     RUPGES        BEGSR
023700980429     C*
023800980429     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023900980429     C*
024000980429     C                   ENDSR
024100980429     C*------------------------------------------------------------------------*
024200980429     C* ENTGES - GESTIONE ENTER -> INVIO
024300980429     C*------------------------------------------------------------------------*
024400980429     C     ENTGES        BEGSR
024500980429     C*
024600980429     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024700980429     C                   EXSR      NEWPOS
024800980429     C*
024900980429     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025000980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025100980429IF  2C     SWPOSI        IFNE      0
025200980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
025300980429X   2C                   ELSE
025400980429IF  3C     SFLULT        IFNE      0
025500980429     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025600980429     C*
025700980429     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025800980429IF  4C     SWMODI        IFNE      0
025900980429     C                   EXSR      POSSFL
026000980429E   4C                   ENDIF
026100980429X   3C                   ELSE
026200980804     C*---                 MOVEL'1'       F12L00           *FINE PROGRAMMA
026300980804     C*---                 MOVEL'1'       D30F12           *FINE PROCEDURA
026400980429E   3C                   ENDIF
026500980429E   2C                   ENDIF
026600980429E   1C                   ENDIF
026700980429     C*
026800980429     C                   ENDSR
026900980429     C*------------------------------------------------------------------------*
027000980429     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027100980429     C*------------------------------------------------------------------------*
027200980429     C     O01GES        BEGSR
027300980429     C*
027400980429     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027600980429     C                   MOVEL     V1SCTO        D30CTO
027700980429     C                   Z-ADD     V1SKSC        D30KSC
027800980429     C                   Z-ADD     V1SCTR        D30CTR
027900980429     C                   Z-ADD     V1SPRG        D30PRG
028000980429     C                   Z-ADD     V1SLNP        D30LNP
028100980429     C                   MOVEL     '1'           FINL00                         *FINE PGM
028200980429     C*
028300980429     C                   ENDSR
028400980429     C*------------------------------------------------------------------------*
028500980429     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
028600980429     C*------------------------------------------------------------------------*
028700980429     C     O02GES        BEGSR
028800980429     C*
028900980429     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029000980429IF  1C     V1SATB        IFEQ      'A'
029100980429     C                   MOVEL     '1'           ERRL00
029200980429     C                   MOVEL     MSG(4)        DSMSMS
029300980429E   1C                   ENDIF
029400980429     C* MODIFICA
029500980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029700980429     C                   MOVEL     V1SCTO        DCTO
029800980429     C                   Z-ADD     V1SKSC        DKSC
029900980429     C                   Z-ADD     V1SCTR        DCTR
030000980429     C                   Z-ADD     V1SPRG        DPRG
030100980429     C                   Z-ADD     V1SLNP        DLNP
030200011011     C                   MOVEL     D30DIV        DDIV
030300980429     C                   MOVEL     DEPOP0        WOP0
030400980429     C                   MOVEL     'O02'         WOP1
030500980429     C                   MOVEL     'TNTE33R'     WPGM
030600980429     C                   EXSR      LANPGM
030700980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
030800980429E   1C                   ENDIF
030900980429     C*
031000980429     C                   ENDSR
031100980429     C*------------------------------------------------------------------------*
031200980429     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
031300980429     C*------------------------------------------------------------------------*
031400980429     C     O04GES        BEGSR
031500980429     C*
031600980429     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031700980429IF  1C     V1SATB        IFEQ      'A'
031800980429     C                   MOVEL     '1'           ERRL00
031900980429     C                   MOVEL     MSG(4)        DSMSMS
032000980429E   1C                   ENDIF
032100980429     C* ANNULLAMENTO
032200980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032400980429     C                   MOVEL     V1SCTO        DCTO
032500980429     C                   Z-ADD     V1SKSC        DKSC
032600980429     C                   Z-ADD     V1SCTR        DCTR
032700980429     C                   Z-ADD     V1SPRG        DPRG
032800980429     C                   Z-ADD     V1SLNP        DLNP
032900011011     C                   MOVEL     D30DIV        DDIV
033000980429     C                   MOVEL     DEPOP0        WOP0
033100980429     C                   MOVEL     'O04'         WOP1
033200980429     C                   MOVEL     'TNTE33R'     WPGM
033300980429     C                   EXSR      LANPGM
033400980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033500980429E   1C                   ENDIF
033600980429     C*
033700980429     C                   ENDSR
033800980429     C*------------------------------------------------------------------------*
033900980429     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034000980429     C*------------------------------------------------------------------------*
034100980429     C     O05GES        BEGSR
034200980429     C*
034400980429     C                   MOVEL     V1SCTO        DCTO
034500980429     C                   Z-ADD     V1SKSC        DKSC
034600980429     C                   Z-ADD     V1SCTR        DCTR
034700980429     C                   Z-ADD     V1SPRG        DPRG
034800980429     C                   Z-ADD     V1SLNP        DLNP
034900011011     C                   MOVEL     D30DIV        DDIV
035000980429     C                   MOVEL     DEPOP0        WOP0
035100980429     C                   MOVEL     'O05'         WOP1
035200980429     C                   MOVEL     'TNTE33R'     WPGM
035300980429     C                   EXSR      LANPGM
035400980429     C*
035500980429     C                   ENDSR
035600980429     C*------------------------------------------------------------------------*
035700980429     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
035800980429     C*------------------------------------------------------------------------*
035900980429     C     O07GES        BEGSR
036000980429     C*
036100980429     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
036200980429IF  1C     V1SATB        IFNE      'A'
036300980429     C                   MOVEL     '1'           ERRL00
036400980429     C                   MOVEL     MSG(5)        DSMSMS
036500980429E   1C                   ENDIF
036600980429     C* RIPRISITINO
036700980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036900980429     C                   MOVEL     V1SCTO        DCTO
037000980429     C                   Z-ADD     V1SKSC        DKSC
037100980429     C                   Z-ADD     V1SCTR        DCTR
037200980429     C                   Z-ADD     V1SPRG        DPRG
037300980429     C                   Z-ADD     V1SLNP        DLNP
037400011011     C                   MOVEL     D30DIV        DDIV
037500980429     C                   MOVEL     DEPOP0        WOP0
037600980429     C                   MOVEL     'O07'         WOP1
037700980429     C                   MOVEL     'TNTE33R'     WPGM
037800980429     C                   EXSR      LANPGM
037900980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
038000980429E   1C                   ENDIF
038100980429     C*
038200980429     C                   ENDSR
038300980429     C*------------------------------------------------------------------------*
038400980429     C* O08GES - GESTIONE OPZIONE 8=SCAGLIONI
038500980429     C*------------------------------------------------------------------------*
038600980429     C     O08GES        BEGSR
038700980429     C*
038900980429     C                   MOVEL     V1SCTO        DCTO
039000980429     C                   Z-ADD     V1SKSC        DKSC
039100980429     C                   Z-ADD     V1SCTR        DCTR
039200980429     C                   Z-ADD     V1SPRG        DPRG
039300980429     C                   Z-ADD     V1SLNP        DLNP
039400980429     C                   Z-ADD     *ZEROS        DSGL
039500011011     C                   MOVEL     D30DIV        DDIV
039600980429     C                   MOVEL     DEPOP0        WOP0
039700980429     C                   MOVEL     'O08'         WOP1
039701060830     c                   if        d30etx = '1'
039702060830     C                   MOVEL     'TNTE34C'     WPGM
039802060830     c                   else
039803060830     C                   MOVEL     'TNTE34R'     WPGM
039804060830     c                   end
039900980429     C                   EXSR      LANPGM
040000980429     C*
040100980429     C                   ENDSR
040200980429     C*------------------------------------------------------------------------*
040300980429     C* O09GES - GESTIONE OPZIONE 9=CODICI TASSAZIONE
040400980429     C*------------------------------------------------------------------------*
040500980429     C     O09GES        BEGSR
040600980429     C*
040800980429     C                   MOVEL     V1SCTO        DCTO
040900980429     C                   Z-ADD     V1SKSC        DKSC
041000980429     C                   Z-ADD     V1SCTR        DCTR
041100980429     C                   Z-ADD     V1SPRG        DPRG
041200980429     C                   Z-ADD     V1SLNP        DLNP
041300980429     C                   MOVEL     *BLANKS       DFIE
041400980429     C                   MOVEL     *BLANKS       DCTS
041500011011     C                   MOVEL     D30DIV        DDIV
041600980429     C                   MOVEL     DEPOP0        WOP0
041700980429     C                   MOVEL     'O09'         WOP1
041800980429     C                   MOVEL     'TNTE36R'     WPGM
041900980429     C                   EXSR      LANPGM
042000980429     C*
042100980429     C                   ENDSR
042200980429     C*------------------------------------------------------------------------*
042300980429     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
042400980429     C*------------------------------------------------------------------------*
042500980429     C     NEWPOS        BEGSR
042600980429     C*
042700980429     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
042800980429     C*         1= RICHIESTO POSIZIONAMENTO
042900980429     C                   Z-ADD     *ZEROS        SWPOSI            7 0
043000980429     C*
043100980429     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
043200980429     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
043300980429     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
043400980429     C                   Z-ADD     *ZEROS        SWMODI            1 0
043500980429     C*
043600980429     C* REIMPOSTA INDICATORI DI ERRORE
043700980429     C                   SETOFF                                       808182
043800980429     C                   SETOFF                                       83
043900980429     C*
044000980429     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
044100980803IF  1C     V1CKSC        IFNE      DEPKSC
044300980429     C     V1CCTO        ORNE      DEPCTO
044400980429     C     V1CLNP        ORNE      DEPLNP
044500980429     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
044600980429IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
044700980429     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
044800980803     C                   Z-ADD     V1CKSC        DEPKSC                         *DEPOSITI
045000980429     C                   MOVEL     V1CCTO        DEPCTO
045100980429     C                   Z-ADD     V1CLNP        DEPLNP
045200980429E   2C                   ENDIF
045300980429E   1C                   ENDIF
045400980429     C*
045500980429     C                   ENDSR
045600980429     C*------------------------------------------------------------------------*
045700980429     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
045800980429     C*------------------------------------------------------------------------*
045900980429     C     CHKPAR        BEGSR
046000980429     C*
046100980429     C                   ENDSR
046200980429     C*------------------------------------------------------------------------*
046300980429     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
046400980429     C*------------------------------------------------------------------------*
046500980429     C     SETFIL        BEGSR
046600980429     C*
046700980803     C* POSIZIONAMENTO SUL FILE
046800980429     C                   Z-ADD     V1CKSC        KTCKSC
046900011001     C     KEYETC        SETLL     TEETC02L                           99
047000980429IF  1C     *IN99         IFEQ      *OFF
047100980429     C                   EXSR      LETFIL
047200980429E   1C                   ENDIF
047300980429     C*
047400980429     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047500980429IF  1C     *IN99         IFEQ      *ON
047600980429     C                   Z-ADD     1             SFLEOF
047700980429E   1C                   ENDIF
047800980429     C*
047900980429     C                   ENDSR
048000980429     C*------------------------------------------------------------------------*
048100980429     C* LETFIL - LETTURA RECORD
048200980429     C*------------------------------------------------------------------------*
048300980429     C     LETFIL        BEGSR
048400980429     C*
048500980429     C* LEGGE FINO A:
048600980429     C*     - FINE FILE
048700980429     C*     - TROVATO RECORD VALIDO
048800980429     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
048900980429DO  1C     *IN99         DOWEQ     *OFF
049000980429     C     WRECOK        ANDEQ     'N'
049100011001     C                   READ      TEETC02L                               99
049200980429IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
049300980429     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
049400980429X   2C                   ELSE
049500980429     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
049600980429E   2C                   ENDIF
049700980429E   1C                   ENDDO
049800980429     C*
049900980429     C                   ENDSR
050000980429     C*------------------------------------------------------------------------*
050100980429     C* CHKREC - CONTROLLA VALIDITA' RECORD
050200980429     C*------------------------------------------------------------------------*
050300980429     C     CHKREC        BEGSR
050400980429     C*
050500980429     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
050600980505     C*
050700980505     C* RECORD ANNULLATO, ESCLUDE
050800980505IF  1C     ETCATB        IFNE      *BLANKS
050900980505     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
051000980505E   1C                   ENDIF
051800980803     C*
051900980803     C* DATI TARIFFA/OFFERTA DIVERSI DA QUELLI RICHIESTI, ESCLUDE
052000980803IF  1C     V1CCTO        IFNE      *BLANKS
052100980803IF  2C     V1CCTO        IFNE      ETCCTO
052200980803     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
052300980803E   2C                   ENDIF
052400980803E   1C                   ENDIF
052500980429     C*
052600980429     C* LINEA DI PARTENZA <> DA QUELLA RICHIESTA, ESCLUDE
052700980429IF  1C     V1CLNP        IFGT      *ZEROS
052800980429IF  2C     V1CLNP        IFNE      ETCLNP
052900980429     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053000980429E   2C                   ENDIF
053100980429E   1C                   ENDIF
053200980708     C*
053300060830     C* CONTROLLA ESISTENZA RECORD DA CARICARE x scaglioni ricavi
053400980708IF  1C     WRECOK        IFEQ      'S'
053500980708IF  2C     D30ETX        IFEQ      'S'                                          *SCAGLIONI
053600980708     C                   MOVEL     ETCDSF        KTSDSF
053700980708     C                   MOVEL     ETCCTO        KTSCTO
053800980708     C                   MOVEL     ETCKSC        KTSKSC
053900980708     C                   MOVEL     ETCCTR        KTSCTR
054000980708     C                   MOVEL     ETCPRG        KTSPRG
054100980708     C                   MOVEL     ETCLNP        KTSLNP
054200011001     C     KEYETS        SETLL     TEETS01L                               97
054300980708IF  3C     *IN97         IFEQ      *OFF                                         *INESISTENTE
054400980708     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054500980708E   3C                   ENDIF
054600980708E   2C                   ENDIF
054700980708E   1C                   ENDIF
054701060830     C* CONTROLLA ESISTENZA RECORD DA CARICARE x scaglioni competenza
054702060830IF  1C     WRECOK        IFEQ      'S'
054703060830IF  2C     D30ETX        IFEQ      '1'                                          *SCAGLIONI
054704060830     C                   MOVEL     ETCDSF        KTSDSF
054705060830     C                   MOVEL     ETCCTO        KTSCTO
054706060830     C                   MOVEL     ETCKSC        KTSKSC
054707060830     C                   MOVEL     ETCCTR        KTSCTR
054708060830     C                   MOVEL     ETCPRG        KTSPRG
054709060830     C                   MOVEL     ETCLNP        KTSLNP
054710060830     C     KEYETS        SETLL     TEETS11L                               97
054711060830IF  3C     *IN97         IFEQ      *OFF                                         *INESISTENTE
054712060830     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054713060830E   3C                   ENDIF
054714060830E   2C                   ENDIF
054715060830E   1C                   ENDIF
054800980708     C*
054900980708     C* CONTROLLA ESISTENZA RECORD DA CARICARE
055000980708IF  1C     WRECOK        IFEQ      'S'
055100980708IF  2C     D30ETX        IFEQ      'T'                                          *CODICI TASSAZIONE
055200980708     C                   MOVEL     ETCDSF        KTTDSF
055300980708     C                   MOVEL     ETCCTO        KTTCTO
055400980708     C                   MOVEL     ETCKSC        KTTKSC
055500980708     C                   MOVEL     ETCCTR        KTTCTR
055600980708     C                   MOVEL     ETCPRG        KTTPRG
055700980708     C                   MOVEL     ETCLNP        KTTLNP
055800980708     C                   MOVEL     D30TFI        KTTFIE
055900011001     C     KEYETT        SETLL     TEETT01L                               97
056000980708IF  3C     *IN97         IFEQ      *OFF                                         *INESISTENTE
056100980708     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056200980708E   3C                   ENDIF
056300980708E   2C                   ENDIF
056400980708E   1C                   ENDIF
056500020313     C*
056600020313IF  1C     WRECOK        IFEQ      'S'
056700020313     C* OFFERTA
056800020313IF  2C     ETCCTO        IFEQ      'O'
056802091123     c                   clear                   tnte04ds
056803091123     C                   movel     D30dsf        d04dsf
056804091123     C                   Z-ADD     etcKSC        d04nrv
056806091123     c                   call      'TNTE04R'
056807091123     c                   parm                    kpjba
056808091126     c                   parm                    tnte04ds
057000091123     C                   IF        d04err = ' '
057100091123     C                   IF        d04KSC > *zeros
057200091123     C                   MOVEL     d04KSC        DEPFIL            3
057300020313     C                   ELSE
057400091123     C                   MOVEL     d04CMM        DEPFIL
057500020313     C                   ENDIF
057600020313     C     DEPFIL        LOOKUP    L1                                     55
057700020313     C                   IF        *IN55 = *OFF
057800020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057900020313     C                   ENDIF
058000020313     C                   ELSE
058100020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058200020313     C                   ENDIF
058300020313     C* TARIFFA
058400020313     C                   ELSE
058500020313     C* VERIFICO SE TRATTASI DI TARIFFA DI CARTELLO (VA SEMPRE BENE)
058600020313     C                   MOVEL     ETCKSC        DEPCART           5 0
058700020313     C                   IF        DEPCART = 88888
058800020313     C                   ELSE
058900020313     C                   MOVEL     ETCKSC        DEPFIL
059000020313     C     DEPFIL        LOOKUP    L1                                     55
059100020313     C                   IF        *IN55 = *OFF
059200020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
059300020313     C                   ENDIF
059400020313     C                   ENDIF
059500020925     C                   ENDIF
059600020313     C                   ENDIF
059700020313     C*
059800980429     C                   ENDSR
059900980429     C*------------------------------------------------------------------------*
060000980429     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
060100980429     C*------------------------------------------------------------------------*
060200980429     C     IRESFL        BEGSR
060300980429     C* CAMPI
060400980429     C                   MOVEL     *BLANKS       OPZKEY
060500980429     C                   MOVEL     ETCATB        V1SATB
060700980429     C                   MOVEL     ETCCTO        V1SCTO
060800980429     C                   Z-ADD     ETCKSC        V1SKSC
060900980429     C                   Z-ADD     ETCCTR        V1SCTR
061000980429     C                   Z-ADD     ETCPRG        V1SPRG
061100980429     C                   Z-ADD     ETCLNP        V1SLNP
061200980429     C                   MOVEL     ETCDCV        V1SDCV
061300980429     C* DECODIFICHE
061400980806     C                   CLEAR                   CNACO000
061500980806     C                   CLEAR                   TFACO000
061700980803     C                   MOVEL     *BLANKS       V1SRAG                         *CLIENTE
061800020923     C                   Z-ADD     1             KACKUT
061900020923     C                   Z-ADD     DUTKCI        KACKCC
062000980803     C                   Z-ADD     ETCKSC        KACKSC
062100980803IF  1C     ETCCTO        IFEQ      'T'                                          *TARIFFA
062200980804     C     KEYACO        CHAIN     CNACO00F                           98
062300980806     C                   MOVEL     ACORAG        V1SRAG
062400980803X   1C                   ELSE                                                   *OFFERTA
062500980804     C     KEYACO        CHAIN     TFACO00F                           98
062600980806     C                   MOVEL     ACORAG        V1SRAG
062700980806IF  2C     V1SRAG        IFEQ      *BLANKS
062701091123     c                   clear                   tnte04ds
062702091123     C                   movel     D30dsf        d04dsf
062703091123     C                   Z-ADD     etcksc        d04nrv
062705091123     c                   call      'TNTE04R'
062706091123     c                   parm                    kpjba
062707091126     c                   parm                    tnte04ds
062800091123     C                   movel     d04rag        v1srag
063100980806E   2C                   ENDIF
063200980806E   1C                   ENDIF
063300980804     C*
063400980803     C                   MOVEL     *BLANKS       V1SDLN                         *FILIALE
063500980429     C                   SETOFF                                           28
063600980429     C                   Z-ADD     1             J
063700980429     C     ETCLNP        LOOKUP    FIL(J)                                 28
063800980429IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
063900980429     C                   MOVEL     FDES(J)       V1SDLN
064000980429E   1C                   ENDIF
064100980429     C*
064200980429     C                   ENDSR
064300980429     C*------------------------------------------------------------------------*
064400980429     C* LANPGM - LANCIA UN PROGRAMMA
064500980429     C*------------------------------------------------------------------------*
064600980429     C     LANPGM        BEGSR
064700980429     C*
064800980429     C* IMPOSTA LA DS DI PROCEDURA
064900980429     C                   MOVEL     WOP0          D30OP0                         *LIVELLO PROCEDURA
065000980429     C                   MOVEL     WOP1          D30OP1                         *LIVELLO PGM
065100980429     C                   MOVEL     '0'           D30F03                         *PREMUTO F03=FINE
065200980429     C                   MOVEL     '0'           D30F12                         *PREMUTO F12=RITORNO
065300980429     C                   MOVEL     '0'           D30ERR                         *'1'=ERRORE
065400980429     C                   MOVEL     *BLANKS       D30MSG                         *MESSAGGIO DI RITORN
065500980429     C*
065700980429     C                   MOVEL     DCTO          D30CTO
065800980429     C                   Z-ADD     DKSC          D30KSC
065900980429     C                   Z-ADD     DCTR          D30CTR
066000980429     C                   Z-ADD     DPRG          D30PRG
066100980429     C                   Z-ADD     DLNP          D30LNP
066200980429     C                   Z-ADD     DSGL          D30SGL
066300980429     C                   MOVEL     DFIE          D30FIE
066400980429     C                   MOVEL     DCTS          D30CTS
066500011003     C                   MOVEL     DDIV          D30DIV
066600980429     C                   CALL      WPGM
066700980429     C                   PARM                    KPJBA
066800980429     C                   PARM                    DSTE30
066900980429     C*
067000980429     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
067100980429IF  1C     D30F03        IFEQ      '1'                                          *PREMUTO F03
067200980429     C                   MOVEL     '1'           ERRL00                         *ERRORE
067300980429     C                   MOVEL     '1'           F03L00                         *FINE PGM
067400980429X   1C                   ELSE                                                   *NON PREMUTO F03
067500980429     C                   MOVEL     D30MSG        DSMSMS                         *MESSAGGIO DI RITORN
067600980429E   1C                   ENDIF
067700980429     C*
067800980429     C                   ENDSR
067900980429     C*------------------------------------------------------------------------*
068000980429     C* CARTAB - CARICA LE TABELLE OCCORRENTI
068100980429     C*------------------------------------------------------------------------*
068200980429     C     CARTAB        BEGSR
068300020313     C*
068400980429     C* ORGANIGRAMMA
068500980429     C                   Z-ADD     *ZEROS        J                 5 0
068600980429     C     *LOVAL        SETLL     AZORG01L
068700980804     C                   READ      AZORG01L                               98
068800980804DO  1C     *IN98         DOWEQ     *OFF
068900980429IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
069000980429     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
069100980429     C                   ADD       1             J
069200980429     C                   Z-ADD     ORGFIL        FIL(J)                         *FILIALE
069300980429     C                   MOVEL     ORGDES        FDES(J)                        *DESCRIZIONE
069400980429E   2C                   ENDIF
069500980804     C                   READ      AZORG01L                               98
069600980429E   1C                   ENDDO
069700020313     C*
069800980429     C                   ENDSR
069900980429     C*------------------------------------------------------------------------*
070000980429     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
070100980429     C*------------------------------------------------------------------------*
070200980429     C     CHKFOP        BEGSR
070300980429     C*
070400980504     C* SE L'UTENTE E' IN SCELTA, ABILITA ANCHE L'INTERROGAZIONE DEGLI ALTRI FILE
070500980429IF  1C     DEPOP0        IFEQ      'C01'
070600980429     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
070700980429     C                   MOVEL     'S'           OPDATT
070800980429     C                   MOVEL     OPDRIG        OPD(1)
070900980504     C                   MOVEL     OPD(8)        OPDRIG                         *SCAGLIONI
071000980504     C                   MOVEL     'S'           OPDATT
071100980504     C                   MOVEL     OPDRIG        OPD(8)
071200980504     C                   MOVEL     OPD(9)        OPDRIG                         *CODICI TASSAZIONE
071300980504     C                   MOVEL     'S'           OPDATT
071400980504     C                   MOVEL     OPDRIG        OPD(9)
071500980429X   1C                   ELSE
071600980429     C* OPZIONI
071700980429     C*---                 MOVELOPD,5     OPDRIG           *INTERROGAZIONE
071800980429     C*---                 MOVEL'S'       OPDATT
071900980429     C*---                 MOVELOPDRIG    OPD,5
072000980429     C                   MOVEL     OPD(8)        OPDRIG                         *SCAGLIONI
072100980429     C                   MOVEL     'S'           OPDATT
072200980429     C                   MOVEL     OPDRIG        OPD(8)
072300980429     C                   MOVEL     OPD(9)        OPDRIG                         *CODICI TASSAZIONE
072400980429     C                   MOVEL     'S'           OPDATT
072500980429     C                   MOVEL     OPDRIG        OPD(9)
072600980429     C* FUNZIONI
072700980429E   1C                   ENDIF
072800980429     C*
072900980429     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
073000980429IF  1C     DEPOP0        IFEQ      'C02'
073100980429     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
073200980429     C                   MOVEL     'S'           OPDATT
073300980429     C                   MOVEL     OPDRIG        OPD(2)
073400980429     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
073500980429     C                   MOVEL     'S'           OPDATT
073600980429     C                   MOVEL     OPDRIG        OPD(4)
073700980429     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
073800980429     C                   MOVEL     'S'           OPDATT
073900980429     C                   MOVEL     OPDRIG        OPD(7)
074000980429     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
074100980429     C                   MOVEL     'S'           FUDATT
074200980429     C                   MOVEL     FUDRIG        FUD(6)
074300980429E   1C                   ENDIF
074400980429     C*
074500980429     C* GESTIONE OPZIONI A VIDEO
074600980429     C                   Z-ADD     *ZEROS        OPZGRU            5 0          *INDICE X OPZIONI
074700980429     C                   EXSR      OPZGES
074800980429     C*
074900980429     C* GESTIONE TASTI FUNZIONALI A VIDEO
075000980429     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
075100980429     C                   EXSR      FUNGES
075200980429     C*
075300980429     C                   ENDSR
075400020925     C*--------------------------------------------------------------------------------------------*
075500020925     C* REPERISCE DATI UTENTE
075600020925     C*--------------------------------------------------------------------------------------------*
075700020925     C     REPDATIUTE    BEGSR
075800020925     C*
075900020925     C* REimposta le variabili di lavoro
076000020925     C                   CLEAR                   TIBS34DS
076100020925     C                   CLEAR                   AZUTEDS
076200020925     C                   CLEAR                   DDATIUTE
076300020925     C                   CLEAR                   WABI              2
076400020925     C                   CLEAR                   DLAT
076500020925     C*
076600020925     C* Reperisce i dati del profilo in ingresso
076700020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
076800020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
076900020925     C                   IN(E)     *DTAARA
077000020925     C                   IF        %Error                                       *NO errori
077100020925     C                   EVAL      I34TLA = 'L'
077200020925     C                   CALL      'TIBS34R'
077300020925     C                   PARM                    TIBS34DS
077400020925     C                   IN        *DTAARA
077500020925     C                   ENDIF
077600020925     C*
077700020925     C* Se ci sono degli errori sull'abilitazione utente, esce
077800020925     C                   SELECT
077900020925     C                   WHEN      DUTERR = 'E'
078000020925     C                   EVAL      ERRL00 = '1'
078100020925     C                   EVAL      FINL00 = '1'
078200020925     C*
078300020925     C* NON siamo in sede (filiale remota o filiale locale)
078400020925     C* --> se 1° livello, abilitazioni al terminal
078500020925     C*     se 2° livello, abilitazioni al punto operativo
078600020925     C                   WHEN      UTEAUT = *blanks
078700020925     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
078800020925     C                   EVAL      WABI   = 'TP'
078900020925     C                   ENDIF
079000020925     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
079100020925     C                   EVAL      WABI   = 'PO'
079200020925     C                   ENDIF
079300020925     C*
079400020925     C* siamo in sede
079500020925     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
079600020925     C*     carica le abilitazioni proprie del profilo
079700020925     C                   OTHER
079800020925     C                   MOVEL     UTEFAF        DUTE01
079900060913     C                   IF        §UTECTC <> *blanks                           *esiste specifica
080000060913     C                   EVAL      WABI = §UTECTC
080100020925     C                   ELSE                                                   *NON esiste specific
080200020925     C                   EVAL      WABI = UTEAUT
080300020925     C                   ENDIF
080400020925     C                   ENDSL
080500020925     C*
080600020925     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
080700020925     C                   CLEAR                   TIBS02DS
080800020925     C                   MOVEL     'C'           T02MOD
080900020925     C                   MOVEL     KNSIF         T02SIF
081000020925     C                   MOVEL     'LAT'         T02COD
081100020925     C                   MOVEL(P)  WABI          T02KE1
081200020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
081300020925     C                   CALL      'TIBS02R'
081400020925     C                   PARM                    KPJBA
081500020925     C                   PARM                    TIBS02DS
081600020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
081700020925     C                   IF        T02ERR = *blanks
081800020925     C                   MOVEL     T02UNI        DLAT
081900020925     C                   ENDIF
082000020925     C                   IF        T02ERR <> *blanks OR
082100020925     C                             §LATABI = 'S'
082200020925     C                   EVAL      ERRL00  = '1'
082300020925     C                   EVAL      FINL00  = '1'
082400020925     C                   ENDIF
082500020925     C*
082600020925     C* Reperisce i P.O. gestibili dall'utente
082700020925     C                   IF        ERRL00 <> '1'                                  *NO errori
082800020925     C                   CLEAR                   TRUL31DS
082900020925     C                   EVAL      I31ABI   = WABI
083000020925     C                   EVAL      I31CDI   = DUTDIS
083100020925     C                   EVAL      I31CAR   = DUTARE
083200020925     C                   EVAL      I31CPO   = DUTPOU
083300020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
083400020925     C                   CALL      'TRUL31R'
083500020925     C                   PARM                    KPJBA
083600020925     C                   PARM                    TRUL31DS
083700020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
083800020925if  2C                   IF        O31POG > *zeros
083900020925     C                   MOVEA     O31POG        L1
084000020925x   2C                   ELSE
084100020925     C                   EVAL      ERRL00 = '1'
084200020925     C                   EVAL      FINL00 = '1'
084300020925e   2C                   ENDIF
084400020925e   1C                   ENDIF
084500020925     C*
084600020925     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
084700020925     C                   MOVEL(P)  RSUT          DSFIRS           20
084800020925     C*
084900020925     C                   ENDSR
085000980429     C*------------------------------------------------------------------------*
085100980429     C* *INZSR - OPERAZIONI INIZIALI
085200980429     C*------------------------------------------------------------------------*
085300980429     C     *INZSR        BEGSR
085400980429     C*---
085500980429     C* RICEVIMENTO PARAMETRI
085600980429     C*---
085700980429     C     *ENTRY        PLIST
085800980429     C                   PARM                    KPJBA
085900980429     C                   PARM                    DSTE30
086000041202     c* apertura controllata di alcuni file xchè solo di filiale
086100041202     c                   open      tfaco00f                             95
086300980429     C*
086400980429     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
086500980429     C     XMSGPA        PLIST
086600980429     C                   PARM                    DSMSMS
086700980429     C                   PARM                    DSMSPG
086800980429     C                   PARM                    DSMSKE
086900980429     C                   PARM                    DSMSER
087000980429     C*---
087100980429     C* VARIABILI RIFERITE AL DATA BASE
087200980429     C*---
087300011001     C     *LIKE         DEFINE    ETCKSC        KTCKSC                         *TEETC00F
087400011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
087500980708     C     *LIKE         DEFINE    ETSCTO        KTSCTO
087600980708     C     *LIKE         DEFINE    ETSKSC        KTSKSC
087700980708     C     *LIKE         DEFINE    ETSCTR        KTSCTR
087800980708     C     *LIKE         DEFINE    ETSPRG        KTSPRG
087900980708     C     *LIKE         DEFINE    ETSLNP        KTSLNP
088000011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
088100980708     C     *LIKE         DEFINE    ETTCTO        KTTCTO
088200980708     C     *LIKE         DEFINE    ETTKSC        KTTKSC
088300980708     C     *LIKE         DEFINE    ETTCTR        KTTCTR
088400980708     C     *LIKE         DEFINE    ETTPRG        KTTPRG
088500980708     C     *LIKE         DEFINE    ETTLNP        KTTLNP
088600980708     C     *LIKE         DEFINE    ETTFIE        KTTFIE
088700980803     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
088800980803     C     *LIKE         DEFINE    ACOKCC        KACKCC
088900980803     C     *LIKE         DEFINE    ACOKSC        KACKSC
089200980429     C     *LIKE         DEFINE    ETCCTO        DEPCTO
089300980429     C     *LIKE         DEFINE    ETCKSC        DEPKSC
089400980429     C     *LIKE         DEFINE    ETCLNP        DEPLNP
089500980429     C     *LIKE         DEFINE    D30OP0        DEPOP0                         *DI DEPOSITO
089600980429     C     *LIKE         DEFINE    D30OP1        DEPOP1
089700980429     C     *LIKE         DEFINE    D30OP0        WOP0                           *DI LAVORO
089800980429     C     *LIKE         DEFINE    D30OP1        WOP1
090000980429     C     *LIKE         DEFINE    D30CTO        DCTO
090100980429     C     *LIKE         DEFINE    D30KSC        DKSC
090200980429     C     *LIKE         DEFINE    D30CTR        DCTR
090300980429     C     *LIKE         DEFINE    D30PRG        DPRG
090400980429     C     *LIKE         DEFINE    D30LNP        DLNP
090500980429     C     *LIKE         DEFINE    D30SGL        DSGL
090600980429     C     *LIKE         DEFINE    D30FIE        DFIE
090700980429     C     *LIKE         DEFINE    D30CTS        DCTS
090800011003     C     *LIKE         DEFINE    D30DIV        DDIV
090900020925     C     *LIKE         DEFINE    KPJBU         DEPKPJBU
091000980429     C*---
091100980429     C* VARIABILI  N O N  RIFERITE AL DATA BASE
091200980429     C*---
091300980429     C     *LIKE         DEFINE    SFLNBR        SFLPAG
091400980429     C     *LIKE         DEFINE    SFLPAG        WFLPAG
091500980429     C     *LIKE         DEFINE    SFLNBR        SFLEOF
091600980429     C     *LIKE         DEFINE    SFLNBR        SFLERR
091700980429     C     *LIKE         DEFINE    SFLNBR        SFLULT
091800980429     C     *LIKE         DEFINE    SFLNBR        WFLULT
091900980429     C     *LIKE         DEFINE    SFLNBR        MSGULT
092000980429     C     *LIKE         DEFINE    SFLNBR        SFLRIG
092100980429     C     *LIKE         DEFINE    FUDFUN        FUNKEY
092200980429     C*---
092300980429     C* CHIAVI DI LETTURA
092400980429     C*---
092500011001     C     KEYETC        KLIST                                                  *TEETC02L
092600980429     C                   KFLD                    KTCKSC                          -CLIENTE/VISITA
092700011001     C     KEYETS        KLIST                                                  *TEETS01L
092800980708     C                   KFLD                    KTSDSF                          -SEDE/FILIALE
092900980708     C                   KFLD                    KTSCTO                          -TARIFFA/OFFERTA
093000980708     C                   KFLD                    KTSKSC                          -CLIENTE/VISITA
093100980708     C                   KFLD                    KTSCTR                          -TARIFFA
093200980708     C                   KFLD                    KTSPRG                          -PROGRESSIVO
093300980708     C                   KFLD                    KTSLNP                          -LINEA PARTENZA
093400011001     C     KEYETT        KLIST                                                  *TEETS01L
093500980708     C                   KFLD                    KTTDSF                          -SEDE/FILIALE
093600980708     C                   KFLD                    KTTCTO                          -TARIFFA/OFFERTA
093700980708     C                   KFLD                    KTTKSC                          -CLIENTE/VISITA
093800980708     C                   KFLD                    KTTCTR                          -TARIFFA
093900980708     C                   KFLD                    KTTPRG                          -PROGRESSIVO
094000980708     C                   KFLD                    KTTLNP                          -LINEA PARTENZA
094100980708     C                   KFLD                    KTTFIE                          -ITALIA/ESTERO
094200980803     C     KEYACO        KLIST                                                  *CNACO00F
094300980803     C                   KFLD                    KACKUT                          -CODICE UTENTE
094400980803     C                   KFLD                    KACKCC                          -CODICE CAPOCONTO
094500980803     C                   KFLD                    KACKSC                          -CODICE CLIENTE
094800980429     C*---
094900980429     C* VARIABILI DI CONTROLLO
095000980429     C*---
095100980429     C                   MOVEL     '0'           FINL00            1            *FINE PGM
095200980429     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
095300980429     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
095400980429     C                   MOVEL     '0'           ERRL00            1            *ERRORE
095500980429     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
095600980429     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
095700980429     C*
095800980429     C* IMPOSTA LA DS -INVIO MESSAGGI-
095900980429     C                   MOVEL     'TNTE32R'     DSMSPG
096000980429     C                   MOVEL     'TNTE32R'     MSGQUE
096100980429     C                   MOVEL     *BLANKS       MSGKEY
096200980429     C                   MOVEL     *BLANKS       DSMSMS
096300980429     C                   MOVEL     *BLANKS       DSMSKE
096400980429     C                   Z-ADD     *ZEROS        DSMSER
096500980429     C*
096600980429     C* IMPOSTA LA DS -TASTI PREMUTI-
096700980429     C                   CALL      'XKEY'
096800980429     C                   PARM                    DSKEY
096900980429     C*
097000980429     C* REPERIMENTO DATI UTENTE
097100020923     C                   EXSR      REPDATIUTE
097200980429     C*
097300980429     C* CARICA LE TABELLE OCCORRENTI
097400980429     C                   EXSR      CARTAB
097500980429     C*
097600980429     C* DEPOSITA LE OPZIONI DI ENTRATA
097700980429     C                   MOVEL     D30OP0        DEPOP0                         *LIVELLO PROCEDURA
097800980429     C                   MOVEL     D30OP1        DEPOP1                         *LIVELLO PROGRAMMA
097900980429     C*
098000980429     C* CALCOLA LA DATA CORRENTE
098100980429     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
098200980429     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
098300980429     C                   Z-ADD     WN8           G08DAT
098400980429     C                   Z-ADD     *ZEROS        G08INV
098500980429     C                   MOVEL     '0'           G08ERR
098600980429     C                   CALL      'XSRDA8'
098700980429     C                   PARM                    WLBDA8
098800980429     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
098900980429     C*
099000980429     C* IMPOSTA I PARAMETRI DI INGRESSO
099100980803     C                   Z-ADD     D30KSC        V1CKSC
099300980429     C                   MOVEL     D30CTO        V1CCTO
099400980429     C                   Z-ADD     D30LNP        V1CLNP
099500980429     C                   EXSR      CHKPAR
099600980429IF  1C     ERRL00        IFEQ      '1'
099700980429     C                   MOVEL     '1'           D30ERR
099800980429     C                   MOVEL     DSMSMS        D30MSG
099900980429X   1C                   ELSE
100000980803     C                   Z-ADD     V1CKSC        DEPKSC                         *DEPOSITI
100200980429     C                   MOVEL     V1CCTO        DEPCTO
100300980429     C                   Z-ADD     V1CLNP        DEPLNP
100400980429E   1C                   ENDIF
100500980429     C*
100600980429     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
100700980429     C                   EXSR      CHKFOP
100800980429     C*
100900980429     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
101000980429IF  1C     DEPOP0        IFEQ      'C01'                                        *SCELTA
101100980429     C                   MOVEL     DEC(3)        VIDING
101200980429E   1C                   ENDIF
101300980429IF  1C     DEPOP0        IFEQ      'C02'                                        *INTERROGAZIONE
101400980429     C                   MOVEL     DEC(1)        VIDING
101500980429E   1C                   ENDIF
101600980429IF  1C     DEPOP0        IFEQ      'C05'                                        *GESTIONE
101700980429     C                   MOVEL     DEC(2)        VIDING
101800980429E   1C                   ENDIF
101801091123     C*
101804091123     c                   if        d30dsf =' '
101805091123     C                   eval      d1cKsc ='Cliente/Visita'                     *TARIFFA O OFFERTA
101806091123     C                   eval      d1ccto ='Tar/Off'                            *TARIFFA O OFFERTA
101808091123X   1C                   ELSE
101809091123     C                   eval      d1cKsc ='Cliente/Tratt.'                     *TARIFFA O OFFERTA
101810091201     C                   eval      d1ccto ='Tar/Off'                            *TARIFFA O OFFERTA
101811091123E   1C                   ENDIF
101900980429     C*
102000980429     C                   ENDSR
102100980429     C*------------------------------------------------------------------------*
102200980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102300980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102400980429     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
102500980429     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
102600980429     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
102700980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102800980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102900980429     C*------------------------------------------------------------------------*
103000980429     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
103100980429     C*------------------------------------------------------------------------*
103200980429     C     POSSFL        BEGSR
103300980429     C*
103400980429     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
103500980429     C                   EXSR      CLRSFL
103600980429     C*
103700980429     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
103800980429     C                   EXSR      SETFIL
103900980429     C*
104000980429     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
104100980429     C     SFLEOF        IFEQ      0
104200980429     C                   EXSR      CARSFL
104300980429     C                   END
104400980429     C*
104500980429     C                   ENDSR
104600980429     C*------------------------------------------------------------------------*
104700980429     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
104800980429     C*------------------------------------------------------------------------*
104900980429     C     ENTATT        BEGSR
105000980429     C*
105100980429     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
105200980429     C                   Z-ADD     0             OPZOK             1 0
105300980429     C*
105400980429     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
105500980429     C                   Z-ADD     0             SFLVAR            5 0
105600980429     C*
105700980429     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
105800980429     C                   Z-ADD     0             SFLERR
105900980429     C*
106000980429     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
106100980429     C                   Z-ADD     SFLULT        WFLULT
106200980429     C*
106300980429     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
106400980429     C                   READC     TE32D05                                98
106500980429     C     *IN98         DOWEQ     '0'
106600980429     C*
106700980429     C* ADD CONTATORE DI RIGHE SFL VARIATE
106800980429     C                   ADD       1             SFLVAR
106900980429     C*
107000980429     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
107100980429     C     OPZOK         IFEQ      0
107200980429     C*
107300980429     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
107400980429     C     OPZKEY        IFNE      *BLANKS
107500980429     C                   EXSR      OPZCON
107600980429     C                   END
107700980429     C*
107800980429     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
107900980429     C*            2. PULIZIA SCELTA IN SFL
108000980429     C     OPZOK         IFEQ      0
108100980429     C                   MOVEL     '0'           *IN41
108200980429     C                   MOVEL     *BLANKS       OPZKEY
108300980429     C                   ELSE
108400980429     C*
108500980429     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
108600980429     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
108700980429     C                   MOVEL     '1'           *IN41
108800980429     C                   Z-ADD     SFLULT        SFLERR
108900980429     C                   END
109000980429     C                   ELSE
109100980429     C*
109200980429     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
109300980429     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
109400980429     C                   MOVEL     '1'           *IN41
109500980429     C                   END                                                    OPZOK=0
109600980429     C*
109700980429     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
109800980429     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
109900980429     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
110000980429     C     OPZOK         IFNE      0
110100980429     C                   Z-ADD     SFLERR        SFLNBR
110200980429     C                   ELSE
110300980429     C                   Z-ADD     SFLULT        SFLNBR
110400980429     C                   END
110500980429     C*
110600980429     C* AGGIORNO RECORD SFL
110700980429     C                   UPDATE    TE32D05
110800980429     C*
110900980429     C                   READC     TE32D05                                98
111000980429     C                   END                                                    DO WHILE
111100980429     C*
111200980429     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
111300980429     C                   Z-ADD     WFLULT        SFLULT
111400980429     C*
111500980429     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
111600980429     C     SFLVAR        IFEQ      0
111700980804     C*---                 MOVEL'1'       F12L00           *FINE PROGRAMMA
111800980804     C*---                 MOVEL'1'       D30F12           *FINE PROCEDURA
111900980429     C                   END
112000980429     C*
112100980429     C                   ENDSR
112200980429     C*------------------------------------------------------------------------*
112300980429     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
112400980429     C*------------------------------------------------------------------------*
112500980429     C     CLRSFL        BEGSR
112600980429     C*
112700980429     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
112800980429     C* (0=NON FINE FILE  1=FINE FILE)
112900980429     C                   Z-ADD     0             SFLEOF
113000980429     C*
113100980429     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
113200980429     C                   Z-ADD     0             SFLULT
113300980429     C*
113400980429     C* ACCENSIONE INDICATORE PER CLEAR SFL
113500980429     C                   MOVEL     '1'           *IN40
113600980429     C                   WRITE     TE32D06
113700980429     C                   MOVEL     '0'           *IN40
113800980429     C*
113900980429     C                   ENDSR
114000980429     C*------------------------------------------------------------------------*
114100980429     C* CARSFL - CARICAMENTO SFL
114200980429     C*------------------------------------------------------------------------*
114300980429     C     CARSFL        BEGSR
114400980429     C*
114500980429     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
114600980429     C     SFLULT        IFEQ      0
114700980429     C                   EXSR      SETFIL
114800980429     C                   END
114900980429     C*
115000980429     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
115100980429     C                   Z-ADD     SFLULT        WFLULT
115200980429     C*
115300980429     C* CICLO:
115400980429     C*    -CARICO NUMERO DI RIGHE=SFLPAG
115500980429     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
115600980429     C                   Z-ADD     1             SFLRIG
115700980429     C     SFLRIG        DOWLE     SFLPAG
115800980429     C     SFLEOF        ANDEQ     0
115900980429     C*
116000980429     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
116100980429     C                   MOVEL     *BLANKS       OPZKEY
116200980429     C*
116300980429     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
116400980429     C                   EXSR      IRESFL
116500980429     C*
116600980429     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
116700980429     C                   ADD       1             SFLULT
116800980429     C*
116900980429     C* SCRITTURA RECORD SFL
117000980429     C                   WRITE     TE32D05
117100980429     C                   ADD       1             SFLRIG
117200980429     C*
117300980429     C* LETTURA FILE DA CARICARE
117400980429     C                   EXSR      LETFIL
117500980429     C                   END                                                    DO WHILE
117600980429     C*
117700980429     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
117800980429     C     WFLULT        IFNE      SFLULT
117900980429     C                   Z-ADD     WFLULT        SFLNBR
118000980429     C                   ADD       1             SFLNBR
118100980429     C                   END
118200980429     C*
118300980429     C                   ENDSR
118400980429     C*------------------------------------------------------------------------*
118500980429     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
118600980429     C*------------------------------------------------------------------------*
118700980429     C     OUTSFL        BEGSR
118800980429     C*
118900980429     C* WRITE RECORD TESTATA VIDEO
119000980429     C                   WRITE     TE32D01
119100980429     C*
119200980429     C* VISUALIZZAZIONE MESSAGGIO
119300980429     C                   EXSR      SNDMSG
119400980429     C*
119500980429     C* WRITE RECORD OPZIONI VIDEO
119600980429     C                   WRITE     TE32D02
119700980429     C*
119800980429     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
119900980429     C                   WRITE     TE32D03
120000980429     C*
120100980429     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
120200980429     C                   WRITE     TE32D04
120300980429     C*
120400980429     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
120500980429     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
120600980429     C     SFLEOF        IFEQ      0
120700980429     C                   MOVEL     '0'           *IN42
120800980429     C                   ELSE
120900980429     C*
121000980429     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
121100980429     C                   MOVEL     '1'           *IN42
121200980429     C                   END
121300980429     C*
121400980429     C* WRITE RECORD TASTI FUNZIONALI
121500980429     C                   WRITE     TE32D09
121600980429     C*
121700980429     C* CASO DI SFL VUOTO
121800980429     C     SFLULT        IFLE      0
121900980429     C*
122000980429     C*    - WRITE RECORD VUOTO
122100980429     C                   WRITE     TE32D0A
122200980804     C                   READ      TE32D0A                                98
122300980429     C*
122400980429     C                   ELSE
122500980429     C*
122600980429     C* CASO DI SFL CON RECORDS
122700980429     C*
122800980429     C*    - WRITE RECORD CONTROLLO SFL
122900980429     C                   WRITE     TE32D06
123000980804     C                   READ      TE32D06                                98
123100980429     C                   END
123200980429     C*
123300980429     C* READ RECORD PER POSIZIONAMEMTO VIDEO
123400980804     C                   READ      TE32D03                                98
123500980429     C*
123600980429     C                   ENDSR
123700980429     C*------------------------------------------------------------------------*
123800980429     C* SNDMSG - MANDA MESSAGGIO
123900980429     C*------------------------------------------------------------------------*
124000980429     C     SNDMSG        BEGSR
124100980429     C*
124200980429     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
124300980429     C                   MOVEL     '1'           *IN40
124400980429     C                   WRITE     TE32DMC
124500980429     C                   MOVEL     '0'           *IN40
124600980429     C*
124700980429     C* VISUALIZZAZIONE MESSAGGIO
124800980429     C                   Z-ADD     0             MSGULT
124900980429     C                   CALL      'XMSG'        XMSGPA
125000980429     C     DSMSER        IFEQ      0
125100980429     C                   MOVEL     DSMSKE        MSGKEY
125200980429     C                   END
125300980429     C                   Z-ADD     1             MSGULT
125400980429     C                   WRITE     TE32DMS                                      SFL MESS
125500980429     C                   WRITE     TE32DMC                                      CONTR MESS
125600980429     C*
125700980429     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
125800980429     C                   MOVEL     *BLANKS       DSMSMS
125900980429     C*
126000980429     C                   ENDSR
126100980429     C*------------------------------------------------------------------------*
126200980429     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
126300980429     C*------------------------------------------------------------------------*
126400980429     C     FUNCON        BEGSR
126500980429     C*
126600980429     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
126700980429     C                   EXSR      FUNVAR
126800980429     C*
126900980429     C* PULISCO CAMPI DI LAVORO
127000980429     C                   Z-ADD     0             I                 5 0
127100980429     C                   Z-ADD     0             W                 5 0
127200980429     C*
127300980429     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
127400980429     C     FUNKEY        IFNE      *BLANKS
127500980429     C     1             DO        24            I
127600980429     C                   MOVEL     FUD(I)        FUDRIG
127700980429     C*
127800980429     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
127900980429     C     FUNKEY        IFEQ      FUDFUN
128000980429     C     FUDATT        ANDEQ     'S'
128100980429     C                   Z-ADD     I             W
128200980429     C                   END
128300980429     C                   END                                                    DO WHILE
128400980429     C*
128500980429     C* FUNZIONE NON ATTIVA (ERRORE)
128600980429     C     W             IFEQ      0
128700980429     C*
128800980429     C* EMISSIONE MESSAGGIO DI ERRORE
128900980429     C                   MOVEL     MSG(2)        DSMSMS
129000980429     C                   END                                                    W=0
129100980429     C                   END                                                    FUNKEY NE *BLANKS
129200980429     C*
129300980429     C* FUNZIONE ATTIVA (PROCEDI ...)
129400980429     C     W             IFNE      0
129500980429     C     FUNKEY        OREQ      *BLANKS
129600980429     C*
129700980429     C     FUNKEY        IFEQ      *BLANKS
129800980429     C                   EXSR      ENTGES
129900980429     C                   END
130000980429     C     FUNKEY        IFEQ      '03'
130100980429     C                   EXSR      F03GES
130200980429     C                   END
130300980429     C     FUNKEY        IFEQ      '05'
130400980429     C                   EXSR      F05GES
130500980429     C                   END
130600980429     C     FUNKEY        IFEQ      '06'
130700980429     C                   EXSR      F06GES
130800980429     C                   END
130900980429     C     FUNKEY        IFEQ      '12'
131000980429     C                   EXSR      F12GES
131100980429     C                   END
131200980429     C     FUNKEY        IFEQ      '23'
131300980429     C                   EXSR      F23GES
131400980429     C                   END
131500980429     C     FUNKEY        IFEQ      '24'
131600980429     C                   EXSR      F24GES
131700980429     C                   END
131800980429     C     FUNKEY        IFEQ      'RU'
131900980429     C                   EXSR      RUPGES
132000980429     C                   END
132100980429     C                   END
132200980429     C*
132300980429     C                   ENDSR
132400980429     C*------------------------------------------------------------------------*
132500980429     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
132600980429     C*------------------------------------------------------------------------*
132700980429     C     FUNVAR        BEGSR
132800980429     C*
132900980429     C* FUNKEY=*BLANKS SIGNIFICA ENTER
133000980429     C     KEY           IFEQ      ENTER
133100980429     C                   MOVEL     *BLANKS       FUNKEY
133200980429     C                   END
133300980429     C     KEY           IFEQ      F1
133400980429     C                   MOVEL     '01'          FUNKEY
133500980429     C                   END
133600980429     C     KEY           IFEQ      F2
133700980429     C                   MOVEL     '02'          FUNKEY
133800980429     C                   END
133900980429     C     KEY           IFEQ      F3
134000980429     C                   MOVEL     '03'          FUNKEY
134100980429     C                   END
134200980429     C     KEY           IFEQ      F4
134300980429     C                   MOVEL     '04'          FUNKEY
134400980429     C                   END
134500980429     C     KEY           IFEQ      F5
134600980429     C                   MOVEL     '05'          FUNKEY
134700980429     C                   END
134800980429     C     KEY           IFEQ      F6
134900980429     C                   MOVEL     '06'          FUNKEY
135000980429     C                   END
135100980429     C     KEY           IFEQ      F7
135200980429     C                   MOVEL     '07'          FUNKEY
135300980429     C                   END
135400980429     C     KEY           IFEQ      F8
135500980429     C                   MOVEL     '08'          FUNKEY
135600980429     C                   END
135700980429     C     KEY           IFEQ      F9
135800980429     C                   MOVEL     '09'          FUNKEY
135900980429     C                   END
136000980429     C     KEY           IFEQ      F10
136100980429     C                   MOVEL     '10'          FUNKEY
136200980429     C                   END
136300980429     C     KEY           IFEQ      F11
136400980429     C                   MOVEL     '11'          FUNKEY
136500980429     C                   END
136600980429     C     KEY           IFEQ      F12
136700980429     C                   MOVEL     '12'          FUNKEY
136800980429     C                   END
136900980429     C     KEY           IFEQ      F13
137000980429     C                   MOVEL     '13'          FUNKEY
137100980429     C                   END
137200980429     C     KEY           IFEQ      F14
137300980429     C                   MOVEL     '14'          FUNKEY
137400980429     C                   END
137500980429     C     KEY           IFEQ      F15
137600980429     C                   MOVEL     '15'          FUNKEY
137700980429     C                   END
137800980429     C     KEY           IFEQ      F16
137900980429     C                   MOVEL     '16'          FUNKEY
138000980429     C                   END
138100980429     C     KEY           IFEQ      F17
138200980429     C                   MOVEL     '17'          FUNKEY
138300980429     C                   END
138400980429     C     KEY           IFEQ      F18
138500980429     C                   MOVEL     '18'          FUNKEY
138600980429     C                   END
138700980429     C     KEY           IFEQ      F19
138800980429     C                   MOVEL     '19'          FUNKEY
138900980429     C                   END
139000980429     C     KEY           IFEQ      F20
139100980429     C                   MOVEL     '20'          FUNKEY
139200980429     C                   END
139300980429     C     KEY           IFEQ      F21
139400980429     C                   MOVEL     '21'          FUNKEY
139500980429     C                   END
139600980429     C     KEY           IFEQ      F22
139700980429     C                   MOVEL     '22'          FUNKEY
139800980429     C                   END
139900980429     C     KEY           IFEQ      F23
140000980429     C                   MOVEL     '23'          FUNKEY
140100980429     C                   END
140200980429     C     KEY           IFEQ      F24
140300980429     C                   MOVEL     '24'          FUNKEY
140400980429     C                   END
140500980429     C     KEY           IFEQ      ROLLUP
140600980429     C                   MOVEL     'RU'          FUNKEY
140700980429     C                   END
140800980429     C     KEY           IFEQ      HELP
140900980429     C                   MOVEL     'HP'          FUNKEY
141000980429     C                   END
141100980429     C*
141200980429     C                   ENDSR
141300980429     C*------------------------------------------------------------------------*
141400980429     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
141500980429     C*------------------------------------------------------------------------*
141600980429     C     FUNGES        BEGSR
141700980429     C*
141800980429     C* PULISCO CAMPI DI LAVORO
141900980429     C                   Z-ADD     0             W                 5 0
142000980429     C*
142100980429     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
142200980429     C* E CARICO TABELLA FUNZIONI ATTIVE
142300980429     C     1             DO        24            I
142400980429     C                   MOVEL     FUD(I)        FUDRIG
142500980429     C*
142600980429     C* PULIZIA FUNZIONI PRECEDENTI
142700980429     C                   MOVEL     *BLANKS       FUV(I)
142800980429     C*
142900980429     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
143000980429     C* (SALTA F23 E F24)
143100980429     C     FUDATT        IFEQ      'S'
143200980429     C     FUDFUN        ANDNE     '23'
143300980429     C     FUDFUN        ANDNE     '24'
143400980429     C                   ADD       1             W
143500980429     C                   MOVEL     FUDDES        FUV(W)
143600980429     C                   END
143700980429     C                   END                                                    DO WHILE
143800980429     C*
143900980429     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
144000980429     C* ALTRIMENTI DISATTIVO F24
144100980429     C     1             DO        24            I
144200980429     C                   MOVEL     FUD(I)        FUDRIG
144300980429     C*
144400980429     C     FUDFUN        IFEQ      '24'
144500980429     C     W             IFGT      6
144600980429     C                   MOVEL     'S'           FUDATT
144700980429     C                   ELSE
144800980429     C                   MOVEL     *BLANKS       FUDATT
144900980429     C                   END                                                    W GT 8
145000980429     C                   MOVEL     FUDRIG        FUD(I)
145100980429     C                   END
145200980429     C                   END                                                    DO WHILE
145300980429     C*
145400980429     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
145500980429     C     W             DIV       6             I
145600980429     C                   MVR                     Z
145700980429     C*
145800980429     C     Z             IFNE      0
145900980429     C     I             ADD       1             Z
146000980429     C                   ELSE
146100980429     C                   Z-ADD     I             Z
146200980429     C                   END
146300980429     C*
146400980429     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
146500980429     C                   ADD       1             FUNGRU
146600980429     C     FUNGRU        IFGT      Z
146700980429     C     FUNGRU        ORLT      1
146800980429     C                   Z-ADD     1             FUNGRU
146900980429     C                   END
147000980429     C*
147100980429     C* PRIMO GRUPPO DI 6 FUNZIONI
147200980429     C     FUNGRU        IFEQ      1
147300980429     C                   MOVEL     FUV(1)        FUN001
147400980429     C                   MOVEL     FUV(2)        FUN002
147500980429     C                   MOVEL     FUV(3)        FUN003
147600980429     C                   MOVEL     FUV(4)        FUN004
147700980429     C                   MOVEL     FUV(5)        FUN005
147800980429     C                   MOVEL     FUV(6)        FUN006
147900980429     C                   END
148000980429     C*
148100980429     C* SECONDO GRUPPO DI 6 FUNZIONI
148200980429     C     FUNGRU        IFEQ      2
148300980429     C                   MOVEL     FUV(7)        FUN001
148400980429     C                   MOVEL     FUV(8)        FUN002
148500980429     C                   MOVEL     FUV(9)        FUN003
148600980429     C                   MOVEL     FUV(10)       FUN004
148700980429     C                   MOVEL     FUV(11)       FUN005
148800980429     C                   MOVEL     FUV(12)       FUN006
148900980429     C                   END
149000980429     C*
149100980429     C* TERZO GRUPPO DI 6 FUNZIONI
149200980429     C     FUNGRU        IFEQ      3
149300980429     C                   MOVEL     FUV(13)       FUN001
149400980429     C                   MOVEL     FUV(14)       FUN002
149500980429     C                   MOVEL     FUV(15)       FUN003
149600980429     C                   MOVEL     FUV(16)       FUN004
149700980429     C                   MOVEL     FUV(17)       FUN005
149800980429     C                   MOVEL     FUV(18)       FUN006
149900980429     C                   END
150000980429     C*
150100980429     C* QUARTO GRUPPO DI 6 FUNZIONI
150200980429     C     FUNGRU        IFEQ      4
150300980429     C                   MOVEL     FUV(19)       FUN001
150400980429     C                   MOVEL     FUV(20)       FUN002
150500980429     C                   MOVEL     FUV(21)       FUN003
150600980429     C                   MOVEL     FUV(22)       FUN004
150700980429     C                   MOVEL     *BLANKS       FUN005
150800980429     C                   MOVEL     *BLANKS       FUN006
150900980429     C                   END
151000980429     C*
151100980429     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
151200980429     C                   MOVEL     *BLANKS       FUN007
151300980429     C                   MOVEL     *BLANKS       FUN008
151400980429     C*
151500980429     C     1             DO        24            I
151600980429     C                   MOVEL     FUD(I)        FUDRIG
151700980429     C     FUDFUN        IFEQ      '23'
151800980429     C     FUDATT        ANDEQ     'S'
151900980429     C                   MOVEL     FUDDES        FUN007
152000980429     C                   END
152100980429     C     FUDFUN        IFEQ      '24'
152200980429     C     FUDATT        ANDEQ     'S'
152300980429     C                   MOVEL     FUDDES        FUN008
152400980429     C                   END
152500980429     C                   END                                                    DO
152600980429     C*
152700980429     C                   ENDSR
152800980429     C*------------------------------------------------------------------------*
152900980429     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
153000980429     C*------------------------------------------------------------------------*
153100980429     C     OPZGES        BEGSR
153200980429     C*
153300980429     C* PULISCO CAMPI DI LAVORO
153400980429     C                   Z-ADD     0             W                 5 0
153500980429     C*
153600980429     C* RICERCO QUANTE OPZIONI SONO ATTIVE
153700980429     C* E CARICO TABELLA OPZIONI ATTIVE
153800980429     C     1             DO        24            I
153900980429     C                   MOVEL     OPD(I)        OPDRIG
154000980429     C*
154100980429     C* PULIZIA OPZIONI PRECEDENTI
154200980429     C                   MOVEL     *BLANKS       OPV(I)
154300980429     C*
154400980429     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
154500980429     C     OPDATT        IFEQ      'S'
154600980429     C                   ADD       1             W
154700980429     C                   MOVEL     OPDDES        OPV(W)
154800980429     C                   END
154900980429     C                   END                                                    DO WHILE
155000980429     C*
155100980429     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
155200980429     C* ALTRIMENTI DISATTIVO F23
155300980429     C     1             DO        24            I
155400980429     C                   MOVEL     FUD(I)        FUDRIG
155500980429     C*
155600980429     C     FUDFUN        IFEQ      '23'
155700980429     C     W             IFGT      8
155800980429     C                   MOVEL     'S'           FUDATT
155900980429     C                   ELSE
156000980429     C                   MOVEL     *BLANKS       FUDATT
156100980429     C                   END                                                    W GT 8
156200980429     C                   MOVEL     FUDRIG        FUD(I)
156300980429     C                   END
156400980429     C                   END                                                    DO WHILE
156500980429     C*
156600980429     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
156700980429     C     W             DIV       8             I
156800980429     C                   MVR                     Z                 5 0
156900980429     C*
157000980429     C     Z             IFNE      0
157100980429     C     I             ADD       1             Z
157200980429     C                   ELSE
157300980429     C                   Z-ADD     I             Z
157400980429     C                   END
157500980429     C*
157600980429     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
157700980429     C                   ADD       1             OPZGRU
157800980429     C     OPZGRU        IFGT      Z
157900980429     C     OPZGRU        ORLT      1
158000980429     C                   Z-ADD     1             OPZGRU
158100980429     C                   END
158200980429     C*
158300980429     C* PRIMO GRUPPO DI 8 OPZIONI
158400980429     C     OPZGRU        IFEQ      1
158500980429     C                   MOVEL     OPV(1)        OPZ001
158600980429     C                   MOVEL     OPV(2)        OPZ002
158700980429     C                   MOVEL     OPV(3)        OPZ003
158800980429     C                   MOVEL     OPV(4)        OPZ004
158900980429     C                   MOVEL     OPV(5)        OPZ005
159000980429     C                   MOVEL     OPV(6)        OPZ006
159100980429     C                   MOVEL     OPV(7)        OPZ007
159200980429     C                   MOVEL     OPV(8)        OPZ008
159300980429     C                   END
159400980429     C*
159500980429     C* SECONDO GRUPPO DI 8 OPZIONI
159600980429     C     OPZGRU        IFEQ      2
159700980429     C                   MOVEL     OPV(9)        OPZ001
159800980429     C                   MOVEL     OPV(10)       OPZ002
159900980429     C                   MOVEL     OPV(11)       OPZ003
160000980429     C                   MOVEL     OPV(12)       OPZ004
160100980429     C                   MOVEL     OPV(13)       OPZ005
160200980429     C                   MOVEL     OPV(14)       OPZ006
160300980429     C                   MOVEL     OPV(15)       OPZ007
160400980429     C                   MOVEL     OPV(16)       OPZ008
160500980429     C                   END
160600980429     C*
160700980429     C* TERZO GRUPPO DI 8 OPZIONI
160800980429     C     OPZGRU        IFEQ      3
160900980429     C                   MOVEL     OPV(17)       OPZ001
161000980429     C                   MOVEL     OPV(18)       OPZ002
161100980429     C                   MOVEL     OPV(19)       OPZ003
161200980429     C                   MOVEL     OPV(20)       OPZ004
161300980429     C                   MOVEL     OPV(21)       OPZ005
161400980429     C                   MOVEL     OPV(22)       OPZ006
161500980429     C                   MOVEL     OPV(23)       OPZ007
161600980429     C                   MOVEL     OPV(24)       OPZ008
161700980429     C                   END
161800980429     C*
161900980429     C                   ENDSR
162000980429     C*------------------------------------------------------------------------*
162100980429     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
162200980429     C*------------------------------------------------------------------------*
162300980429     C     OPZCON        BEGSR
162400980429     C*
162500980429     C* PULISCO CAMPI DI LAVORO
162600980429     C                   Z-ADD     0             I                 5 0
162700980429     C                   Z-ADD     0             W                 5 0
162800980429     C*
162900980429     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
163000980429     C     1             DO        24            I
163100980429     C                   MOVEL     OPD(I)        OPDRIG
163200980429     C*
163300980429     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
163400980429     C     OPZKEY        IFEQ      OPDOPZ
163500980429     C     OPZKEY        OREQ      OPDOP1
163600980429     C*
163700980429     C     OPDATT        IFEQ      'S'
163800980429     C                   MOVEL     OPDOPZ        OPZKEY
163900980429     C                   Z-ADD     I             W
164000980429     C                   END
164100980429     C                   END
164200980429     C*
164300980429     C                   END                                                    DO WHILE
164400980429     C*
164500980429     C* OPZIONE NON ATTIVA (ERRORE)
164600980429     C     W             IFEQ      0
164700980429     C*
164800980429     C* OPZIONE NON OK
164900980429     C                   Z-ADD     1             OPZOK
165000980429     C*
165100980429     C* EMISSIONE MESSAGGIO DI ERRORE
165200980429     C                   MOVEL     MSG(1)        DSMSMS
165300980429     C                   ELSE
165400980429     C*
165500980429     C* OPZIONE ATTIVA (PROCEDI ...)
165600980429     C*
165700980429     C     OPZKEY        IFEQ      ' 1'
165800980429     C                   EXSR      O01GES
165900980429     C                   END
166000980429     C     OPZKEY        IFEQ      ' 2'
166100980429     C*---                 EXSR O02GES
166200980429     C                   END
166300980429     C     OPZKEY        IFEQ      ' 4'
166400980429     C*---                 EXSR O04GES
166500980429     C                   END
166600980429     C     OPZKEY        IFEQ      ' 5'
166700980429     C*---                 EXSR O05GES
166800980429     C                   END
166900980429     C     OPZKEY        IFEQ      ' 7'
167000980429     C*---                 EXSR O07GES
167100980429     C                   END
167200980429     C     OPZKEY        IFEQ      ' 8'
167300980429     C                   EXSR      O08GES
167400980429     C                   END
167500980429     C     OPZKEY        IFEQ      ' 9'
167600980429     C                   EXSR      O09GES
167700980429     C                   END
167800980429     C                   END                                                    OPZ.NON ATTIVA
167900980429     C*
168000980429     C                   ENDSR
168100980429     C*------------------------------------------------------------------------*
168200980429     C* NOTE COSTRUZIONE SCHIERE
168300980429     C*------------------------------------------------------------------------*
168400980429     C*
168500980429     C* -MSG-MESSAGGI
168600980429     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
168700980429     C* (NON TOCCARE MSG,1 E MSG,2)
168800980429     C*
168900980429     C* -CMD-COMANDI
169000980429     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
169100980429     C*
169200980429     C* -OPD-OPZIONI
169300980429     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
169400980429     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
169500980429     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
169600980429     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
169700980429     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
169800980429     C*  . POSSONO ESSERE IN ORDINE SPARSO
169900980429     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
170000980429     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
170100980429     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
170200980429     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
170300980429     C*
170400980429     C* -FUD-FUNZIONI
170500980429     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
170600980429     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
170700980429     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
170800980429     C*  . POSSONO ESSERE IN ORDINE SPARSO
170900980429     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
171000980429     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
171100980429     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
171200980429     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
171300980429     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
171400980429     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
171500980429     O*------------------------------------------------------------------------*
171600980429** DEC - DECODIFICHE
171700980429    GESTIONE                                                     1
171800980429INTERROGAZIONE                                                   2
171900980429     SCELTA                                                      3
1720009804294
1721009804295
172200980429** MSG - MESSAGGI
172300980429TNTE32R- Opzione non gestita                                                   1
172400980429TNTE32R- Tasto funzionale non gestito                                          2
172500980429TNTE32R- 3
172600980429TNTE32R- Record annullato. Consentito solo il ripristino                       4
172700980429TNTE32R- Record non annullato. Impossibile ripristinare                        5
172800980429TNTE32R- 6                                                                     6
172900980429TNTE32R- 7                                                                     7
173000980429TNTE32R- 8                                                                     8
173100980429TNTE32R- 9                                                                     9
173200980429TNTE32R- 10                                                                   10
173300980429TNTE32R- 11                                                                   11
173400980429TNTE32R- 12                                                                   12
173500980429TNTE32R- 13                                                                   13
173600980429TNTE32R- 14                                                                   14
173700980429TNTE32R- 15                                                                   15
173800980429TNTE32R- 16                                                                   16
173900980429TNTE32R- 17                                                                   17
174000980429TNTE32R- 18                                                                   18
174100980429TNTE32R- 19                                                                   19
174200980429TNTE32R- 20                                                                   20
174300980429** OPD - OPZIONI
174400980429 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
174500980429 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
174600980429                   -  -     (S/N)=N                        opz. 03¬
174700980429 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
174800980429 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
174900980429                   -  -     (S/N)=N                        opz. 06¬
175000980429 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
175100980429 8=Scaglioni       -8 -     (S/N)=N                        opz. 08¬
175200980429 9=Cod.tassazione  -9 -     (S/N)=N                        opz. 09¬
175300980429                   -  -     (S/N)=N                        opz. 10¬
175400980429                   -  -     (S/N)=N                        opz. 11¬
175500980429                   -  -     (S/N)=N                        opz. 12¬
175600980429                   -  -     (S/N)=N                        opz. 13¬
175700980429                   -  -     (S/N)=N                        opz. 14¬
175800980429                   -  -     (S/N)=N                        opz. 15¬
175900980429                   -  -     (S/N)=N                        opz. 16¬
176000980429                   -  -     (S/N)=N                        opz. 17¬
176100980429                   -  -     (S/N)=N                        opz. 18¬
176200980429                   -  -     (S/N)=N                        opz. 19¬
176300980429                   -  -     (S/N)=N                        opz. 20¬
176400980429                   -  -     (S/N)=N                        opz. 21¬
176500980429                   -  -     (S/N)=N                        opz. 22¬
176600980429                   -  -     (S/N)=N                        opz. 23¬
176700980429                   -  -     (S/N)=N                        opz. 24¬
176800980429** FUD - FUNZIONI
176900980429                   -        (S/N)=N                        funz.01¬
177000980429                   -        (S/N)=N                        funz.02¬
177100980429F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
177200980429                   -        (S/N)=N                        funz.04¬
177300980429F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
177400980429F06=Inserimento    -        (S/N)=N                        funz.06¬
177500980429                   -        (S/N)=N                        funz.07¬
177600980429                   -        (S/N)=N                        funz.08¬
177700980429                   -        (S/N)=N                        funz.09¬
177800980429                   -        (S/N)=N                        funz.10¬
177900980429                   -        (S/N)=N                        funz.11¬
178000980429F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
178100980429                   -        (S/N)=N                        funz.13¬
178200980429                   -        (S/N)=N                        funz.14¬
178300980429                   -        (S/N)=N                        funz.15¬
178400980429                   -        (S/N)=N                        funz.16¬
178500980429                   -        (S/N)=N                        funz.17¬
178600980429                   -        (S/N)=N                        funz.18¬
178700980429 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
178800980429 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
178900980429                   -        (S/N)=N                        funz.21¬
179000980429                   -        (S/N)=N                        funz.22¬
179100980429F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
179200980429F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
