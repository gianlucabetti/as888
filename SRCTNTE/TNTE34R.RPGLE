000100980429     H*------------------------------------------------------------------------*
000200980429     H* GESTIONE DATI TIPO SCAGLIONI - SCELTA                                  *
000300020313     H*------------------------------------------------------------------------*
000400980429     H DECEDIT('0,') DATEDIT(*DMY.)
000500980429     F*------------------------------------------------------------------------*
000600980429     F* DATA BASE
000700980429     F*------------------------------------------------------------------------*
000800980429     FTNTE34D   CF   E             WORKSTN INFDS(DEVDS1)
000900980429     F                                     SFILE(TE34D05:SFLULT)
001000980429     F                                     SFILE(TE34DMS:MSGULT)
001100011001     FTEETS01L  IF   E           K DISK
001200020313     FTABEL00F  IF   E           K DISK
001400980429     D*------------------------------------------------------------------------*
001500980429     D* SCHIERE
001600980429     D*------------------------------------------------------------------------*
001700980429     D*------------
001800980429     D* DECODIFICHE
001900980429     D*------------
002000980429     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002100980429     D*------------
002200980429     D* MESSAGGI
002300980429     D*------------
002400980429     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500980429     D*------------
002600980429     D* DESCRIZIONI OPZIONI
002700980429     D*------------
002800980429     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002900980429     D*------------
003000980429     D* DESCRIZIONI TASTI FUNZIONALI
003100980429     D*------------
003200980429     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300980429     D*------------
003400980429     D* OPZIONI DA CARICARE A VIDEO
003500980429     D*------------
003600980429     D OPV             S             19    DIM(24)
003700980429     D*------------
003800980429     D* FUNZIONI DA CARICARE A VIDEO
003900980429     D*------------
004000980429     D FUV             S             19    DIM(24)
004100980429     D*------------------------------------------------------------------------*
004200980429     D* INPUT
004300980429     D*------------------------------------------------------------------------*
004400980429     D*------------
004500980429     D* RIDEFINIZIONE OPZIONI VIDEO
004600980429     D*------------
004700980429     D OPDDS           DS
004800980429     D  OPDOPZ                 1      2
004900980429     D  OPDDE1                 3     19
005000980429     D  OPDOP1                21     22
005100980429     D  OPDFI0                23     34
005200980429     D  OPDATT                35     35
005300980429     D  OPDFI1                36     80
005400980429     D  OPDDES                 1     19
005500980429     D  OPDRIG                 1     80
005600980429     D*------------
005700980429     D* RIDEFINIZIONE FUNZIONI VIDEO
005800980429     D*------------
005900980429     D FUDDS           DS
006000980429     D  FUDDE1                 1      1
006100980429     D  FUDFUN                 2      3
006200980429     D  FUDDE2                 4     19
006300980429     D  FUDFI0                20     34
006400980429     D  FUDATT                35     35
006500980429     D  FUDFI1                36     80
006600980429     D  FUDDES                 1     19
006700980429     D  FUDRIG                 1     80
006800980429     D*------------
006900980429     D* DS GESTIONE TASTI FUNZIONALI
007000980429     D*------------
007100091124     D tnte04ds      E DS
007101091124     D DSKEY         E DS
007200980429     D DEVDS1          DS
007300980429     D  KEY                  369    369
007400980429     D*------------
007500980429     D* DS "XMSG" - GESTIONE MESSAGGI
007600980429     D*------------
007700980429     D DSMSG         E DS
007800980429     D*------------
007900020923     D* DS REPERIMENTO DATI UTENTE
008000980429     D*------------
008100020923     D TIBS34DS      E DS                                                       *Profili utente
008200020923     D DDATIUTE      E DS                                                       *Dati utente
008300020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008400020925     D*------------
008500020925     D* DS CONTROLLO/SCELTA TABELLE
008600020925     D*------------
008700020925     D TIBS02DS      E DS                                                       *Ricerca tabella
008800020925     D*------------------
008900020925     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
009000020925     D*-------------------
009100020925     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
009200020925     D*------------------
009300020925     D* DS FUNZIONI AZIENDALI
009400020925     D*-------------------
009500020925     D DLAT          E DS                  INZ                                  *Funzioni aziendali
009600020925     D*------------------
009700020925     D* DS CARICAMENTO P.O. GESTITI
009800020925     D*-------------------
009900020925     D TRUL31DS      E DS                  INZ
010000020925     D*------------------
010100020925     D* SCHIERA X LINEEE CONSENTITE
010200020925     D*------------------
010300020925     D L1              S              3    DIM(250)
010400980429     D*------------------
010500980429     D* DS "XSRDA8" - CONTROLLA DATA (8)
010600980429     D*------------------
010700980429     D WLBDA8          DS                  INZ
010800980429     D  G08DAT                 1      8  0
010900980429     D  G08INV                 9     16  0
011000980429     D  G08ERR                17     17
011100980429     D  G08TGI                18     22  0
011200980429     D*------------
011300980429     D* DS DI PROCEDURA
011400980429     D*------------
011500980429     D DSTE30        E DS                  EXTNAME(TNTE30DS)
011600980429     D*------------
011700980429     D* ARCHITETTURA
011800980429     D*------------
011900980429     D KPJBA         E DS
012000980429     C*------------------------------------------------------------------------*
012100980429     C* MAIN LINES
012200980429     C*------------------------------------------------------------------------*
012300980429     C*
012400980429     C* SE NON CI SONO ERRORI, PROSEGUE
012500980429IF  1C     ERRL00        IFNE      '1'
012600980429     C*
012700980429     C* CARICA IL SFILE
012800980429     C                   EXSR      POSSFL
012900980429     C*
013000980429     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013100980429DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013200980429     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013300980429     C     FINL00        OREQ      '1'                                          *FINE PGM
013400980429     C*
013500980429     C* REIMPOSTA ERRORE DI PROCERURA
013600980429     C                   MOVEL     '0'           ERRL00
013700980429     C*
013800980429     C* EMETTE IL SFILE
013900980429     C                   EXSR      OUTSFL
014000980429     C*
014100980429     C* GESTIONE TASTI FUNZIONALI
014200980429     C                   EXSR      FUNCON
014300980429     C*
014400980429E   2C                   ENDDO
014500980429     C*
014600980429     C* CI SONO ERRORI IN ENTRATA
014700980429X   1C                   ELSE
014800980429     C                   MOVEL     '1'           D30ERR
014900980429     C                   MOVEL     DSMSMS        D30MSG
015000980429E   1C                   ENDIF
015100980429     C* FINE PGM
015200980429     C                   SETON                                        LR
015300980429     C*------------------------------------------------------------------------*
015400980429     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015500980429     C*------------------------------------------------------------------------*
015600980429     C     F03GES        BEGSR
015700980429     C*
015800980429     C* IMPOSTA "PREMUTO F03"
015900980429     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016000980429     C                   MOVEL     '1'           D30F03                         *DI PROCEDURA
016100980429     C*
016200980429     C                   ENDSR
016300980429     C*------------------------------------------------------------------------*
016400980429     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016500980429     C*------------------------------------------------------------------------*
016600980429     C     F05GES        BEGSR
016700980429     C*
016800980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
016900980429     C*
017000980429     C                   ENDSR
017100980429     C*------------------------------------------------------------------------*
017200980429     C* F06GES - GESTIONE F06 -> INSERIMENTO
017300980429     C*------------------------------------------------------------------------*
017400980429     C     F06GES        BEGSR
017500980429     C*
017600980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017700980429     C                   Z-ADD     *ZEROS        DSGL
017800980429     C                   MOVEL     DEPOP0        WOP0
017900980429     C                   MOVEL     'F06'         WOP1
018000980429     C                   MOVEL     'TNTE35R'     WPGM              8
018100980429     C                   EXSR      LANPGM                                       *LANCIA PGM
018200980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
018300980429E   1C                   ENDIF
018400980429     C*
018500980429     C                   ENDSR
018600980429     C*------------------------------------------------------------------------*
018700980429     C* F12GES - GESTIONE F12 -> RITORNO
018800980429     C*------------------------------------------------------------------------*
018900980429     C     F12GES        BEGSR
019000980429     C*
019100980429     C* IMPOSTA "PREMUTO F12"
019200980429     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019300980429     C                   MOVEL     '1'           D30F12                         *DI PROCEDURA
019400980429     C*
019500980429     C                   ENDSR
019600980429     C*------------------------------------------------------------------------*
019700980429     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
019800980429     C*------------------------------------------------------------------------*
019900980429     C     F23GES        BEGSR
020000980429     C*
020100980429     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020200980429     C*
020300980429     C                   ENDSR
020400980429     C*------------------------------------------------------------------------*
020500980429     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020600980429     C*------------------------------------------------------------------------*
020700980429     C     F24GES        BEGSR
020800980429     C*
020900980429     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021000980429     C*
021100980429     C                   ENDSR
021200980429     C*------------------------------------------------------------------------*
021300980429     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021400980429     C*------------------------------------------------------------------------*
021500980429     C     RUPGES        BEGSR
021600980429     C*
021700980429     C                   EXSR      CARSFL                                       *LEGGE DATABASE
021800980429     C*
021900980429     C                   ENDSR
022000980429     C*------------------------------------------------------------------------*
022100980429     C* ENTGES - GESTIONE ENTER -> INVIO
022200980429     C*------------------------------------------------------------------------*
022300980429     C     ENTGES        BEGSR
022400980429     C*
022500980429     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
022600980429     C                   EXSR      NEWPOS
022700980429     C*
022800980429     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
022900980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023000980429IF  2C     SWPOSI        IFNE      0
023100980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
023200980429X   2C                   ELSE
023300980429IF  3C     SFLULT        IFNE      0
023400980429     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023500980429     C*
023600980429     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
023700980429IF  4C     SWMODI        IFNE      0
023800980429     C                   EXSR      POSSFL
023900980429E   4C                   ENDIF
024000980429X   3C                   ELSE
024100980429     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
024200980429     C                   MOVEL     '1'           D30F12                         *FINE PROCEDURA
024300980429E   3C                   ENDIF
024400980429E   2C                   ENDIF
024500980429E   1C                   ENDIF
024600980429     C*
024700980429     C                   ENDSR
024800980429     C*------------------------------------------------------------------------*
024900980429     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
025000980429     C*------------------------------------------------------------------------*
025100980429     C     O02GES        BEGSR
025200980429     C*
025300980429     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
025400980429IF  1C     V1SATB        IFEQ      'A'
025500980429     C                   MOVEL     '1'           ERRL00
025600980429     C                   MOVEL     MSG(4)        DSMSMS
025700980429E   1C                   ENDIF
025800980429     C* MODIFICA
025900980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026000980429     C                   Z-ADD     V1SSGL        DSGL
026100980429     C                   MOVEL     DEPOP0        WOP0
026200980429     C                   MOVEL     'O02'         WOP1
026300980429     C                   MOVEL     'TNTE35R'     WPGM
026400980429     C                   EXSR      LANPGM
026500980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
026600980429E   1C                   ENDIF
026700980429     C*
026800980429     C                   ENDSR
026900980429     C*------------------------------------------------------------------------*
027000980429     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
027100980429     C*------------------------------------------------------------------------*
027200980429     C     O04GES        BEGSR
027300980429     C*
027400980429     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
027500980429IF  1C     V1SATB        IFEQ      'A'
027600980429     C                   MOVEL     '1'           ERRL00
027700980429     C                   MOVEL     MSG(4)        DSMSMS
027800980429E   1C                   ENDIF
027900980429     C* ANNULLAMENTO
028000980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028100980429     C                   Z-ADD     V1SSGL        DSGL
028200980429     C                   MOVEL     DEPOP0        WOP0
028300980429     C                   MOVEL     'O04'         WOP1
028400980429     C                   MOVEL     'TNTE35R'     WPGM
028500980429     C                   EXSR      LANPGM
028600980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
028700980429E   1C                   ENDIF
028800980429     C*
028900980429     C                   ENDSR
029000980429     C*------------------------------------------------------------------------*
029100980429     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
029200980429     C*------------------------------------------------------------------------*
029300980429     C     O05GES        BEGSR
029400980429     C*
029500980429     C                   Z-ADD     V1SSGL        DSGL
029600980429     C                   MOVEL     DEPOP0        WOP0
029700980429     C                   MOVEL     'O05'         WOP1
029800980429     C                   MOVEL     'TNTE35R'     WPGM
029900980429     C                   EXSR      LANPGM
030000980429     C*
030100980429     C                   ENDSR
030200980429     C*------------------------------------------------------------------------*
030300980429     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
030400980429     C*------------------------------------------------------------------------*
030500980429     C     O07GES        BEGSR
030600980429     C*
030700980429     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
030800980429IF  1C     V1SATB        IFNE      'A'
030900980429     C                   MOVEL     '1'           ERRL00
031000980429     C                   MOVEL     MSG(5)        DSMSMS
031100980429E   1C                   ENDIF
031200980429     C* RIPRISITINO
031300980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031400980429     C                   Z-ADD     V1SSGL        DSGL
031500980429     C                   MOVEL     DEPOP0        WOP0
031600980429     C                   MOVEL     'O07'         WOP1
031700980429     C                   MOVEL     'TNTE35R'     WPGM
031800980429     C                   EXSR      LANPGM
031900980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
032000980429E   1C                   ENDIF
032100980429     C*
032200980429     C                   ENDSR
032300980429     C*------------------------------------------------------------------------*
032400980429     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
032500980429     C*------------------------------------------------------------------------*
032600980429     C     NEWPOS        BEGSR
032700980429     C*
032800980429     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
032900980429     C*         1= RICHIESTO POSIZIONAMENTO
033000980429     C                   Z-ADD     *ZEROS        SWPOSI            7 0
033100980429     C*
033200980429     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
033300980429     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
033400980429     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
033500980429     C                   Z-ADD     *ZEROS        SWMODI            1 0
033600980429     C*
033700980429     C                   ENDSR
033800980429     C*------------------------------------------------------------------------*
033900980429     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
034000980429     C*------------------------------------------------------------------------*
034100980429     C     SETFIL        BEGSR
034200980429     C*
034300980429     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
034400091123     C                   MOVEL     'S'           KTSDSF
034500980429     C                   MOVEL     D30CTO        KTSCTO
034600980429     C                   Z-ADD     D30KSC        KTSKSC
034700980429     C                   Z-ADD     D30CTR        KTSCTR
034800980429     C                   Z-ADD     D30PRG        KTSPRG
034900980429     C                   Z-ADD     D30LNP        KTSLNP
035000011001     C     KEYETS        SETLL     TEETS01L                           99
035100980429     C* LETTURA
035200980429IF  1C     *IN99         IFEQ      *OFF
035300980429     C                   EXSR      LETFIL
035400980429E   1C                   ENDIF
035500980429     C*
035600980429     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
035700980429IF  1C     *IN99         IFEQ      *ON
035800980429     C                   Z-ADD     1             SFLEOF
035900980429E   1C                   ENDIF
036000980429     C*
036100980429     C                   ENDSR
036200980429     C*------------------------------------------------------------------------*
036300980429     C* LETFIL - LETTURA RECORD
036400980429     C*------------------------------------------------------------------------*
036500980429     C     LETFIL        BEGSR
036600980429     C*
036700980429     C* LEGGE FINO A:
036800980429     C*     - FINE FILE
036900980429     C*     - TROVATO RECORD VALIDO
037000980429     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
037100980429DO  1C     *IN99         DOWEQ     *OFF
037200980429     C     WRECOK        ANDEQ     'N'
037300011001     C     KEYETS        READE     TEETS01L                               99
037400980429     C*
037500980429     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
037600980429IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
037700980429     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
037800980429X   2C                   ELSE
037900980429     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
038000980429E   2C                   ENDIF
038100980429E   1C                   ENDDO
038200980429     C*
038300980429     C                   ENDSR
038400980429     C*------------------------------------------------------------------------*
038500980429     C* CHKREC - CONTROLLA VALIDITA' RECORD
038600980429     C*------------------------------------------------------------------------*
038700980429     C     CHKREC        BEGSR
038800980429     C*
038900980429     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
039000980505     C*
039100980505     C* RECORD ANNULLATO, ESCLUDE
039200980505IF  1C     ETSATB        IFNE      *BLANKS
039300980505     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039400980505E   1C                   ENDIF
039500020313     C*
039600020313IF  1C     WRECOK        IFEQ      'S'
039700020313     C* OFFERTA
039800020313IF  2C     ETSCTO        IFEQ      'O'
039901091124     c                   clear                   tnte04ds
039902091124     C                   movel     D30dsf        d04dsf
039903091124     C                   Z-ADD     etsKSC        d04nrv
039905091124     c                   call      'TNTE04R'
039906091124     c                   parm                    kpjba
039907091126     c                   parm                    tnte04ds
039909091124     C                   IF        d04err = ' '
040100091124     C                   IF        d04KSC > *zeros
040200091124     C                   MOVEL     d04KSC        DEPFIL            3
040300020313     C                   ELSE
040400091124     C                   MOVEL     d04CMM        DEPFIL
040500020313     C                   ENDIF
040600020313     C     DEPFIL        LOOKUP    L1                                     55
040700020313     C                   IF        *IN55 = *OFF
040800020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040900020313     C                   ENDIF
041000020313     C                   ELSE
041100020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
041200020313     C                   ENDIF
041300020313     C* TARIFFA
041400020313     C                   ELSE
041500020313     C* VERIFICO SE TRATTASI DI TARIFFA DI CARTELLO (VA SEMPRE BENE)
041600020313     C                   MOVEL     ETSKSC        DEPCART           5 0
041700020313     C                   IF        DEPCART = 88888
041800020313     C                   ELSE
041900020313     C                   MOVEL     ETSKSC        DEPFIL
042000020313     C     DEPFIL        LOOKUP    L1                                     55
042100020313     C                   IF        *IN55 = *OFF
042200020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042300020313     C                   ENDIF
042400020313     C                   ENDIF
042500020313E   2C                   ENDIF
042600020313E   1C                   ENDIF
042700980429     C*
042800980429     C                   ENDSR
042900980429     C*------------------------------------------------------------------------*
043000980429     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
043100980429     C*------------------------------------------------------------------------*
043200980429     C     IRESFL        BEGSR
043300980429     C* CAMPI
043400980429     C                   MOVEL     *BLANKS       OPZKEY
043500980429     C                   MOVEL     ETSATB        V1SATB
043600980429     C                   Z-ADD     ETSSGL        V1SSGL
043700980429     C                   Z-ADD     ETSPSC        V1SPSC
043800980429     C                   Z-ADD     ETSCSC        V1SCSC
043900980429     C                   Z-ADD     ETSVSC        V1SVSC
044000980429     C                   Z-ADD     ETSPPE        V1SPPE
044100980429     C                   Z-ADD     ETSRPV        V1SRPV
044200980429     C                   MOVEL     ETSSDE        V1SSDE
044300980429     C                   Z-ADD     ETSPDE        V1SPDE
044400980429     C*
044500980429     C                   ENDSR
044600980429     C*------------------------------------------------------------------------*
044700980429     C* LANPGM - LANCIA UN PROGRAMMA
044800980429     C*------------------------------------------------------------------------*
044900980429     C     LANPGM        BEGSR
045000980429     C*
045100980429     C* IMPOSTA LA DS DI PROCEDURA
045200980429     C                   MOVEL     WOP0          D30OP0                         *LIVELLO PROCEDURA
045300980429     C                   MOVEL     WOP1          D30OP1                         *LIVELLO PGM
045400980429     C                   MOVEL     '0'           D30F03                         *PREMUTO F03=FINE
045500980429     C                   MOVEL     '0'           D30F12                         *PREMUTO F12=RITORNO
045600980429     C                   MOVEL     '0'           D30ERR                         *'1'=ERRORE
045700980429     C                   MOVEL     *BLANKS       D30MSG                         *MESSAGGIO DI RITORN
045800980429     C*
045900980429     C                   Z-ADD     DSGL          D30SGL                         *CAMPI CHIAVE
046000980429     C                   CALL      WPGM
046100980429     C                   PARM                    KPJBA
046200980429     C                   PARM                    DSTE30
046300980429     C*
046400980429     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
046500980429IF  1C     D30F03        IFEQ      '1'                                          *PREMUTO F03
046600980429     C                   MOVEL     '1'           ERRL00                         *ERRORE
046700980429     C                   MOVEL     '1'           F03L00                         *FINE PGM
046800980429X   1C                   ELSE                                                   *NON PREMUTO F03
046900980429     C                   MOVEL     D30MSG        DSMSMS                         *MESSAGGIO DI RITORN
047000980429E   1C                   ENDIF
047100980429     C*
047200980429     C                   ENDSR
047300980429     C*------------------------------------------------------------------------*
047400980429     C* CARTAB - CARICA LE TABELLE OCCORRENTI
047500980429     C*------------------------------------------------------------------------*
047600980429     C     CARTAB        BEGSR
047700020313     C*
047800980429     C*
047900980429     C                   ENDSR
048000980429     C*------------------------------------------------------------------------*
048100980429     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
048200980429     C*------------------------------------------------------------------------*
048300980429     C     CHKFOP        BEGSR
048400980429     C* OPZIONI
048500980429     C*---                 MOVELOPD,5     OPDRIG           *INTERROGAZIONE
048600980429     C*---                 MOVEL'S'       OPDATT
048700980429     C*---                 MOVELOPDRIG    OPD,5
048800980429     C* FUNZIONI
048900980429     C*
049000980429     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
049100980429IF  1C     DEPOP0        IFEQ      'C02'
049200980429     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
049300980429     C                   MOVEL     'S'           OPDATT
049400980429     C                   MOVEL     OPDRIG        OPD(2)
049500980429     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
049600980429     C                   MOVEL     'S'           OPDATT
049700980429     C                   MOVEL     OPDRIG        OPD(4)
049800980429     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
049900980429     C                   MOVEL     'S'           OPDATT
050000980429     C                   MOVEL     OPDRIG        OPD(7)
050100980429     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
050200980429     C                   MOVEL     'S'           FUDATT
050300980429     C                   MOVEL     FUDRIG        FUD(6)
050400980429E   1C                   ENDIF
050500980429     C*
050600980429     C* GESTIONE OPZIONI A VIDEO
050700980429     C                   Z-ADD     *ZEROS        OPZGRU            5 0          *INDICE X OPZIONI
050800980429     C                   EXSR      OPZGES
050900980429     C*
051000980429     C* GESTIONE TASTI FUNZIONALI A VIDEO
051100980429     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
051200980429     C                   EXSR      FUNGES
051300980429     C*
051400980429     C                   ENDSR
051500020925     C*--------------------------------------------------------------------------------------------*
051600020925     C* REPERISCE DATI UTENTE
051700020925     C*--------------------------------------------------------------------------------------------*
051800020925     C     REPDATIUTE    BEGSR
051900020925     C*
052000020925     C* REimposta le variabili di lavoro
052100020925     C                   CLEAR                   TIBS34DS
052200020925     C                   CLEAR                   AZUTEDS
052300020925     C                   CLEAR                   DDATIUTE
052400020925     C                   CLEAR                   WABI              2
052500020925     C                   CLEAR                   DLAT
052600020925     C*
052700020925     C* Reperisce i dati del profilo in ingresso
052800020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
052900020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
053000020925     C                   IN(E)     *DTAARA
053100020925     C                   IF        %Error                                       *NO errori
053200020925     C                   EVAL      I34TLA = 'L'
053300020925     C                   CALL      'TIBS34R'
053400020925     C                   PARM                    TIBS34DS
053500020925     C                   IN        *DTAARA
053600020925     C                   ENDIF
053700020925     C*
053800020925     C* Se ci sono degli errori sull'abilitazione utente, esce
053900020925     C                   SELECT
054000020925     C                   WHEN      DUTERR = 'E'
054100020925     C                   EVAL      ERRL00 = '1'
054200020925     C                   EVAL      FINL00 = '1'
054300020925     C*
054400020925     C* NON siamo in sede (filiale remota o filiale locale)
054500020925     C* --> se 1° livello, abilitazioni al terminal
054600020925     C*     se 2° livello, abilitazioni al punto operativo
054700020925     C                   WHEN      UTEAUT = *blanks
054800020925     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
054900020925     C                   EVAL      WABI   = 'TP'
055000020925     C                   ENDIF
055100020925     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
055200020925     C                   EVAL      WABI   = 'PO'
055300020925     C                   ENDIF
055400020925     C*
055500020925     C* siamo in sede
055600020925     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
055700020925     C*     carica le abilitazioni proprie del profilo
055800020925     C                   OTHER
055900020925     C                   MOVEL     UTEFAF        DUTE01
056000060913     C                   IF        §UTECTC <> *blanks                           *esiste specifica
056100060913     C                   EVAL      WABI = §UTECTC
056200020925     C                   ELSE                                                   *NON esiste specific
056300020925     C                   EVAL      WABI = UTEAUT
056400020925     C                   ENDIF
056500020925     C                   ENDSL
056600020925     C*
056700020925     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
056800020925     C                   CLEAR                   TIBS02DS
056900020925     C                   MOVEL     'C'           T02MOD
057000020925     C                   MOVEL     KNSIF         T02SIF
057100020925     C                   MOVEL     'LAT'         T02COD
057200020925     C                   MOVEL(P)  WABI          T02KE1
057300020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
057400020925     C                   CALL      'TIBS02R'
057500020925     C                   PARM                    KPJBA
057600020925     C                   PARM                    TIBS02DS
057700020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
057800020925     C                   IF        T02ERR = *blanks
057900020925     C                   MOVEL     T02UNI        DLAT
058000020925     C                   ENDIF
058100020925     C                   IF        T02ERR <> *blanks OR
058200020925     C                             §LATABI = 'S'
058300020925     C                   EVAL      ERRL00  = '1'
058400020925     C                   EVAL      FINL00  = '1'
058500020925     C                   ENDIF
058600020925     C*
058700020925     C* Reperisce i P.O. gestibili dall'utente
058800020925     C                   IF        ERRL00 <> '1'                                  *NO errori
058900020925     C                   CLEAR                   TRUL31DS
059000020925     C                   EVAL      I31ABI   = WABI
059100020925     C                   EVAL      I31CDI   = DUTDIS
059200020925     C                   EVAL      I31CAR   = DUTARE
059300020925     C                   EVAL      I31CPO   = DUTPOU
059400020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
059500020925     C                   CALL      'TRUL31R'
059600020925     C                   PARM                    KPJBA
059700020925     C                   PARM                    TRUL31DS
059800020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
059900020925if  2C                   IF        O31POG > *zeros
060000020925     C                   MOVEA     O31POG        L1
060100020925x   2C                   ELSE
060200020925     C                   EVAL      ERRL00 = '1'
060300020925     C                   EVAL      FINL00 = '1'
060400020925e   2C                   ENDIF
060500020925e   1C                   ENDIF
060600020925     C*
060700020925     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
060800020925     C                   MOVEL(P)  RSUT          DSFIRS           20
060900020925     C*
061000020925     C                   ENDSR
061100980429     C*------------------------------------------------------------------------*
061200980429     C* *INZSR - OPERAZIONI INIZIALI
061300980429     C*------------------------------------------------------------------------*
061400980429     C     *INZSR        BEGSR
061500980429     C*---
061600980429     C* RICEVIMENTO PARAMETRI
061700980429     C*---
061800980429     C     *ENTRY        PLIST
061900980429     C                   PARM                    KPJBA
062000980429     C                   PARM                    DSTE30
062200980429     C*
062300980429     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
062400980429     C     XMSGPA        PLIST
062500980429     C                   PARM                    DSMSMS
062600980429     C                   PARM                    DSMSPG
062700980429     C                   PARM                    DSMSKE
062800980429     C                   PARM                    DSMSER
062900980429     C*---
063000980429     C* VARIABILI RIFERITE AL DATA BASE
063100980429     C*---
063200011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
063300980429     C     *LIKE         DEFINE    ETSCTO        KTSCTO
063400980429     C     *LIKE         DEFINE    ETSKSC        KTSKSC
063500980429     C     *LIKE         DEFINE    ETSCTR        KTSCTR
063600980429     C     *LIKE         DEFINE    ETSPRG        KTSPRG
063700980429     C     *LIKE         DEFINE    ETSLNP        KTSLNP
063800980429     C     *LIKE         DEFINE    D30OP0        DEPOP0                         *DI DEPOSITO
063900980429     C     *LIKE         DEFINE    D30OP1        DEPOP1
064000980429     C     *LIKE         DEFINE    D30OP0        WOP0                           *DI LAVORO
064100980429     C     *LIKE         DEFINE    D30OP1        WOP1
064200980429     C     *LIKE         DEFINE    D30SGL        DSGL
064300020925     C     *LIKE         DEFINE    KPJBU         DEPKPJBU
064400980429     C*---
064500980429     C* VARIABILI  N O N  RIFERITE AL DATA BASE
064600980429     C*---
064700980429     C     *LIKE         DEFINE    SFLNBR        SFLPAG
064800980429     C     *LIKE         DEFINE    SFLPAG        WFLPAG
064900980429     C     *LIKE         DEFINE    SFLNBR        SFLEOF
065000980429     C     *LIKE         DEFINE    SFLNBR        SFLERR
065100980429     C     *LIKE         DEFINE    SFLNBR        SFLULT
065200980429     C     *LIKE         DEFINE    SFLNBR        WFLULT
065300980429     C     *LIKE         DEFINE    SFLNBR        MSGULT
065400980429     C     *LIKE         DEFINE    SFLNBR        SFLRIG
065500980429     C     *LIKE         DEFINE    FUDFUN        FUNKEY
065600980429     C*---
065700980429     C* CHIAVI DI LETTURA
065800980429     C*---
065900011001     C     KEYETS        KLIST                                                  *TEETS01L
066000980429     C                   KFLD                    KTSDSF                          -SEDE/FILIALE
066100980429     C                   KFLD                    KTSCTO                          -TARIFFA/OFFERTA
066200980429     C                   KFLD                    KTSKSC                          -CLIENTE/VISITA
066300980429     C                   KFLD                    KTSCTR                          -TARIFFA
066400980429     C                   KFLD                    KTSPRG                          -PROGRESSIVO
066500980429     C                   KFLD                    KTSLNP                          -LINEA PARTENZA
066600980429     C*---
066700980429     C* VARIABILI DI CONTROLLO
066800980429     C*---
066900980429     C                   MOVEL     '0'           FINL00            1            *FINE PGM
067000980429     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
067100980429     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
067200980429     C                   MOVEL     '0'           ERRL00            1            *ERRORE
067300980429     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
067400980429     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
067500980429     C*
067600980429     C* IMPOSTA LA DS -INVIO MESSAGGI-
067700980429     C                   MOVEL     'TNTE34R'     DSMSPG
067800980429     C                   MOVEL     'TNTE34R'     MSGQUE
067900980429     C                   MOVEL     *BLANKS       MSGKEY
068000980429     C                   MOVEL     *BLANKS       DSMSMS
068100980429     C                   MOVEL     *BLANKS       DSMSKE
068200980429     C                   Z-ADD     *ZEROS        DSMSER
068300980429     C*
068400980429     C* IMPOSTA LA DS -TASTI PREMUTI-
068500980429     C                   CALL      'XKEY'
068600980429     C                   PARM                    DSKEY
068700980429     C*
068800980429     C* REPERIMENTO DATI UTENTE
068900020923     C                   EXSR      REPDATIUTE
069000980429     C*
069100980429     C* CARICA LE TABELLE OCCORRENTI
069200980429     C                   EXSR      CARTAB
069300980429     C*
069400980429     C* DEPOSITA LE OPZIONI DI ENTRATA
069500980429     C                   MOVEL     D30OP0        DEPOP0                         *LIVELLO PROCEDURA
069600980429     C                   MOVEL     D30OP1        DEPOP1                         *LIVELLO PROGRAMMA
069700980429     C*
069800980429     C* CALCOLA LA DATA CORRENTE
069900980429     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
070000980429     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
070100980429     C                   Z-ADD     WN8           G08DAT
070200980429     C                   Z-ADD     *ZEROS        G08INV
070300980429     C                   MOVEL     '0'           G08ERR
070400980429     C                   CALL      'XSRDA8'
070500980429     C                   PARM                    WLBDA8
070600980429     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
070700980429     C*
070800980429     C* IMPOSTA I PARAMETRI DI INGRESSO
071000980429     C                   MOVEL     D30CTO        V1CCTO
071100980429     C                   Z-ADD     D30KSC        V1CKSC
071200980429     C                   Z-ADD     D30CTR        V1CCTR
071300980429     C                   Z-ADD     D30PRG        V1CPRG
071400980429     C                   Z-ADD     D30LNP        V1CLNP
071500011003     C                   MOVEL     D30DIV        V1CDIV
071600980429     C*
071700980429     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
071800980429     C                   EXSR      CHKFOP
071900980429     C*
072000980429     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
072001060830IF  1C     DEPOP0        IFEQ      'C01'                                        *SCELTA
072002060830     C                   MOVEL     DEC(3)        VIDING
072003060830E   1C                   ENDIF
072100980429IF  1C     DEPOP0        IFEQ      'C02'                                        *INTERROGAZIONE
072200980429     C                   MOVEL     DEC(1)        VIDING
072300980429E   1C                   ENDIF
072400980429IF  1C     DEPOP0        IFEQ      'C05'                                        *GESTIONE
072500980429     C                   MOVEL     DEC(2)        VIDING
072600980429E   1C                   ENDIF
072601060830     c* competenze/ricavi
072602060830     c                   if        d30etx = '1'
072603060830     c                   eval      dectes= 'COMPETENZE'
072604060830     c                   else
072605060830     c                   eval      dectes= '  RICAVI  '
072606060830     c                   end
072700980429     C*
072800980429     C                   ENDSR
072900980429     C*------------------------------------------------------------------------*
073000980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073100980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073200980429     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
073300980429     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
073400980429     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
073500980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073600980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073700980429     C*------------------------------------------------------------------------*
073800980429     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
073900980429     C*------------------------------------------------------------------------*
074000980429     C     POSSFL        BEGSR
074100980429     C*
074200980429     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
074300980429     C                   EXSR      CLRSFL
074400980429     C*
074500980429     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
074600980429     C                   EXSR      SETFIL
074700980429     C*
074800980429     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
074900980429     C     SFLEOF        IFEQ      0
075000980429     C                   EXSR      CARSFL
075100980429     C                   END
075200980429     C*
075300980429     C                   ENDSR
075400980429     C*------------------------------------------------------------------------*
075500980429     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
075600980429     C*------------------------------------------------------------------------*
075700980429     C     ENTATT        BEGSR
075800980429     C*
075900980429     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
076000980429     C                   Z-ADD     0             OPZOK             1 0
076100980429     C*
076200980429     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
076300980429     C                   Z-ADD     0             SFLVAR            5 0
076400980429     C*
076500980429     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
076600980429     C                   Z-ADD     0             SFLERR
076700980429     C*
076800980429     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
076900980429     C                   Z-ADD     SFLULT        WFLULT
077000980429     C*
077100980429     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
077200980429     C                   READC     TE34D05                                98
077300980429     C     *IN98         DOWEQ     '0'
077400980429     C*
077500980429     C* ADD CONTATORE DI RIGHE SFL VARIATE
077600980429     C                   ADD       1             SFLVAR
077700980429     C*
077800980429     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
077900980429     C     OPZOK         IFEQ      0
078000980429     C*
078100980429     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
078200980429     C     OPZKEY        IFNE      *BLANKS
078300980429     C                   EXSR      OPZCON
078400980429     C                   END
078500980429     C*
078600980429     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
078700980429     C*            2. PULIZIA SCELTA IN SFL
078800980429     C     OPZOK         IFEQ      0
078900980429     C                   MOVEL     '0'           *IN41
079000980429     C                   MOVEL     *BLANKS       OPZKEY
079100980429     C                   ELSE
079200980429     C*
079300980429     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
079400980429     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
079500980429     C                   MOVEL     '1'           *IN41
079600980429     C                   Z-ADD     SFLULT        SFLERR
079700980429     C                   END
079800980429     C                   ELSE
079900980429     C*
080000980429     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
080100980429     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
080200980429     C                   MOVEL     '1'           *IN41
080300980429     C                   END                                                    OPZOK=0
080400980429     C*
080500980429     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
080600980429     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
080700980429     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
080800980429     C     OPZOK         IFNE      0
080900980429     C                   Z-ADD     SFLERR        SFLNBR
081000980429     C                   ELSE
081100980429     C                   Z-ADD     SFLULT        SFLNBR
081200980429     C                   END
081300980429     C*
081400980429     C* AGGIORNO RECORD SFL
081500980429     C                   UPDATE    TE34D05
081600980429     C*
081700980429     C                   READC     TE34D05                                98
081800980429     C                   END                                                    DO WHILE
081900980429     C*
082000980429     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
082100980429     C                   Z-ADD     WFLULT        SFLULT
082200980429     C*
082300980429     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
082400980429     C     SFLVAR        IFEQ      0
082500980429     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
082600980429     C                   MOVEL     '1'           D30F12                         *FINE PROCEDURA
082700980429     C                   END
082800980429     C*
082900980429     C                   ENDSR
083000980429     C*------------------------------------------------------------------------*
083100980429     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
083200980429     C*------------------------------------------------------------------------*
083300980429     C     CLRSFL        BEGSR
083400980429     C*
083500980429     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
083600980429     C* (0=NON FINE FILE  1=FINE FILE)
083700980429     C                   Z-ADD     0             SFLEOF
083800980429     C*
083900980429     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
084000980429     C                   Z-ADD     0             SFLULT
084100980429     C*
084200980429     C* ACCENSIONE INDICATORE PER CLEAR SFL
084300980429     C                   MOVEL     '1'           *IN40
084400980429     C                   WRITE     TE34D06
084500980429     C                   MOVEL     '0'           *IN40
084600980429     C*
084700980429     C                   ENDSR
084800980429     C*------------------------------------------------------------------------*
084900980429     C* CARSFL - CARICAMENTO SFL
085000980429     C*------------------------------------------------------------------------*
085100980429     C     CARSFL        BEGSR
085200980429     C*
085300980429     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
085400980429     C     SFLULT        IFEQ      0
085500980429     C                   EXSR      SETFIL
085600980429     C                   END
085700980429     C*
085800980429     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
085900980429     C                   Z-ADD     SFLULT        WFLULT
086000980429     C*
086100980429     C* CICLO:
086200980429     C*    -CARICO NUMERO DI RIGHE=SFLPAG
086300980429     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
086400980429     C                   Z-ADD     1             SFLRIG
086500980429     C     SFLRIG        DOWLE     SFLPAG
086600980429     C     SFLEOF        ANDEQ     0
086700980429     C*
086800980429     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
086900980429     C                   MOVEL     *BLANKS       OPZKEY
087000980429     C*
087100980429     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
087200980429     C                   EXSR      IRESFL
087300980429     C*
087400980429     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
087500980429     C                   ADD       1             SFLULT
087600980429     C*
087700980429     C* SCRITTURA RECORD SFL
087800980429     C                   WRITE     TE34D05
087900980429     C                   ADD       1             SFLRIG
088000980429     C*
088100980429     C* LETTURA FILE DA CARICARE
088200980429     C                   EXSR      LETFIL
088300980429     C                   END                                                    DO WHILE
088400980429     C*
088500980429     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
088600980429     C     WFLULT        IFNE      SFLULT
088700980429     C                   Z-ADD     WFLULT        SFLNBR
088800980429     C                   ADD       1             SFLNBR
088900980429     C                   END
089000980429     C*
089100980429     C                   ENDSR
089200980429     C*------------------------------------------------------------------------*
089300980429     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
089400980429     C*------------------------------------------------------------------------*
089500980429     C     OUTSFL        BEGSR
089600980429     C*
089700980429     C* WRITE RECORD TESTATA VIDEO
089800980429     C                   WRITE     TE34D01
089900980429     C*
090000980429     C* VISUALIZZAZIONE MESSAGGIO
090100980429     C                   EXSR      SNDMSG
090200980429     C*
090300980429     C* WRITE RECORD OPZIONI VIDEO
090400980429     C                   WRITE     TE34D02
090500980429     C*
090600980429     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
090700980429     C                   WRITE     TE34D03
090800980429     C*
090900980429     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
091000980429     C                   WRITE     TE34D04
091100980429     C*
091200980429     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
091300980429     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
091400980429     C     SFLEOF        IFEQ      0
091500980429     C                   MOVEL     '0'           *IN42
091600980429     C                   ELSE
091700980429     C*
091800980429     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
091900980429     C                   MOVEL     '1'           *IN42
092000980429     C                   END
092100980429     C*
092200980429     C* WRITE RECORD TASTI FUNZIONALI
092300980429     C                   WRITE     TE34D09
092400980429     C*
092500980429     C* CASO DI SFL VUOTO
092600980429     C     SFLULT        IFLE      0
092700980429     C*
092800980429     C*    - WRITE RECORD VUOTO
092900980429     C                   WRITE     TE34D0A
093000980429     C                   READ      TE34D0A                                99
093100980429     C*
093200980429     C                   ELSE
093300980429     C*
093400980429     C* CASO DI SFL CON RECORDS
093500980429     C*
093600980429     C*    - WRITE RECORD CONTROLLO SFL
093700980429     C                   WRITE     TE34D06
093800980429     C                   READ      TE34D06                                99
093900980429     C                   END
094000980429     C*
094100980429     C* READ RECORD PER POSIZIONAMEMTO VIDEO
094200980429     C                   READ      TE34D03                                99
094300980429     C*
094400980429     C                   ENDSR
094500980429     C*------------------------------------------------------------------------*
094600980429     C* SNDMSG - MANDA MESSAGGIO
094700980429     C*------------------------------------------------------------------------*
094800980429     C     SNDMSG        BEGSR
094900980429     C*
095000980429     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
095100980429     C                   MOVEL     '1'           *IN40
095200980429     C                   WRITE     TE34DMC
095300980429     C                   MOVEL     '0'           *IN40
095400980429     C*
095500980429     C* VISUALIZZAZIONE MESSAGGIO
095600980429     C                   Z-ADD     0             MSGULT
095700980429     C                   CALL      'XMSG'        XMSGPA
095800980429     C     DSMSER        IFEQ      0
095900980429     C                   MOVEL     DSMSKE        MSGKEY
096000980429     C                   END
096100980429     C                   Z-ADD     1             MSGULT
096200980429     C                   WRITE     TE34DMS                                      SFL MESS
096300980429     C                   WRITE     TE34DMC                                      CONTR MESS
096400980429     C*
096500980429     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
096600980429     C                   MOVEL     *BLANKS       DSMSMS
096700980429     C*
096800980429     C                   ENDSR
096900980429     C*------------------------------------------------------------------------*
097000980429     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
097100980429     C*------------------------------------------------------------------------*
097200980429     C     FUNCON        BEGSR
097300980429     C*
097400980429     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
097500980429     C                   EXSR      FUNVAR
097600980429     C*
097700980429     C* PULISCO CAMPI DI LAVORO
097800980429     C                   Z-ADD     0             I                 5 0
097900980429     C                   Z-ADD     0             W                 5 0
098000980429     C*
098100980429     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
098200980429     C     FUNKEY        IFNE      *BLANKS
098300980429     C     1             DO        24            I
098400980429     C                   MOVEL     FUD(I)        FUDRIG
098500980429     C*
098600980429     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
098700980429     C     FUNKEY        IFEQ      FUDFUN
098800980429     C     FUDATT        ANDEQ     'S'
098900980429     C                   Z-ADD     I             W
099000980429     C                   END
099100980429     C                   END                                                    DO WHILE
099200980429     C*
099300980429     C* FUNZIONE NON ATTIVA (ERRORE)
099400980429     C     W             IFEQ      0
099500980429     C*
099600980429     C* EMISSIONE MESSAGGIO DI ERRORE
099700980429     C                   MOVEL     MSG(2)        DSMSMS
099800980429     C                   END                                                    W=0
099900980429     C                   END                                                    FUNKEY NE *BLANKS
100000980429     C*
100100980429     C* FUNZIONE ATTIVA (PROCEDI ...)
100200980429     C     W             IFNE      0
100300980429     C     FUNKEY        OREQ      *BLANKS
100400980429     C*
100500980429     C     FUNKEY        IFEQ      *BLANKS
100600980429     C                   EXSR      ENTGES
100700980429     C                   END
100800980429     C     FUNKEY        IFEQ      '03'
100900980429     C                   EXSR      F03GES
101000980429     C                   END
101100980429     C     FUNKEY        IFEQ      '05'
101200980429     C                   EXSR      F05GES
101300980429     C                   END
101400980429     C     FUNKEY        IFEQ      '06'
101500980429     C                   EXSR      F06GES
101600980429     C                   END
101700980429     C     FUNKEY        IFEQ      '12'
101800980429     C                   EXSR      F12GES
101900980429     C                   END
102000980429     C     FUNKEY        IFEQ      '23'
102100980429     C                   EXSR      F23GES
102200980429     C                   END
102300980429     C     FUNKEY        IFEQ      '24'
102400980429     C                   EXSR      F24GES
102500980429     C                   END
102600980429     C     FUNKEY        IFEQ      'RU'
102700980429     C                   EXSR      RUPGES
102800980429     C                   END
102900980429     C                   END
103000980429     C*
103100980429     C                   ENDSR
103200980429     C*------------------------------------------------------------------------*
103300980429     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
103400980429     C*------------------------------------------------------------------------*
103500980429     C     FUNVAR        BEGSR
103600980429     C*
103700980429     C* FUNKEY=*BLANKS SIGNIFICA ENTER
103800980429     C     KEY           IFEQ      ENTER
103900980429     C                   MOVEL     *BLANKS       FUNKEY
104000980429     C                   END
104100980429     C     KEY           IFEQ      F1
104200980429     C                   MOVEL     '01'          FUNKEY
104300980429     C                   END
104400980429     C     KEY           IFEQ      F2
104500980429     C                   MOVEL     '02'          FUNKEY
104600980429     C                   END
104700980429     C     KEY           IFEQ      F3
104800980429     C                   MOVEL     '03'          FUNKEY
104900980429     C                   END
105000980429     C     KEY           IFEQ      F4
105100980429     C                   MOVEL     '04'          FUNKEY
105200980429     C                   END
105300980429     C     KEY           IFEQ      F5
105400980429     C                   MOVEL     '05'          FUNKEY
105500980429     C                   END
105600980429     C     KEY           IFEQ      F6
105700980429     C                   MOVEL     '06'          FUNKEY
105800980429     C                   END
105900980429     C     KEY           IFEQ      F7
106000980429     C                   MOVEL     '07'          FUNKEY
106100980429     C                   END
106200980429     C     KEY           IFEQ      F8
106300980429     C                   MOVEL     '08'          FUNKEY
106400980429     C                   END
106500980429     C     KEY           IFEQ      F9
106600980429     C                   MOVEL     '09'          FUNKEY
106700980429     C                   END
106800980429     C     KEY           IFEQ      F10
106900980429     C                   MOVEL     '10'          FUNKEY
107000980429     C                   END
107100980429     C     KEY           IFEQ      F11
107200980429     C                   MOVEL     '11'          FUNKEY
107300980429     C                   END
107400980429     C     KEY           IFEQ      F12
107500980429     C                   MOVEL     '12'          FUNKEY
107600980429     C                   END
107700980429     C     KEY           IFEQ      F13
107800980429     C                   MOVEL     '13'          FUNKEY
107900980429     C                   END
108000980429     C     KEY           IFEQ      F14
108100980429     C                   MOVEL     '14'          FUNKEY
108200980429     C                   END
108300980429     C     KEY           IFEQ      F15
108400980429     C                   MOVEL     '15'          FUNKEY
108500980429     C                   END
108600980429     C     KEY           IFEQ      F16
108700980429     C                   MOVEL     '16'          FUNKEY
108800980429     C                   END
108900980429     C     KEY           IFEQ      F17
109000980429     C                   MOVEL     '17'          FUNKEY
109100980429     C                   END
109200980429     C     KEY           IFEQ      F18
109300980429     C                   MOVEL     '18'          FUNKEY
109400980429     C                   END
109500980429     C     KEY           IFEQ      F19
109600980429     C                   MOVEL     '19'          FUNKEY
109700980429     C                   END
109800980429     C     KEY           IFEQ      F20
109900980429     C                   MOVEL     '20'          FUNKEY
110000980429     C                   END
110100980429     C     KEY           IFEQ      F21
110200980429     C                   MOVEL     '21'          FUNKEY
110300980429     C                   END
110400980429     C     KEY           IFEQ      F22
110500980429     C                   MOVEL     '22'          FUNKEY
110600980429     C                   END
110700980429     C     KEY           IFEQ      F23
110800980429     C                   MOVEL     '23'          FUNKEY
110900980429     C                   END
111000980429     C     KEY           IFEQ      F24
111100980429     C                   MOVEL     '24'          FUNKEY
111200980429     C                   END
111300980429     C     KEY           IFEQ      ROLLUP
111400980429     C                   MOVEL     'RU'          FUNKEY
111500980429     C                   END
111600980429     C     KEY           IFEQ      HELP
111700980429     C                   MOVEL     'HP'          FUNKEY
111800980429     C                   END
111900980429     C*
112000980429     C                   ENDSR
112100980429     C*------------------------------------------------------------------------*
112200980429     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
112300980429     C*------------------------------------------------------------------------*
112400980429     C     FUNGES        BEGSR
112500980429     C*
112600980429     C* PULISCO CAMPI DI LAVORO
112700980429     C                   Z-ADD     0             W                 5 0
112800980429     C*
112900980429     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
113000980429     C* E CARICO TABELLA FUNZIONI ATTIVE
113100980429     C     1             DO        24            I
113200980429     C                   MOVEL     FUD(I)        FUDRIG
113300980429     C*
113400980429     C* PULIZIA FUNZIONI PRECEDENTI
113500980429     C                   MOVEL     *BLANKS       FUV(I)
113600980429     C*
113700980429     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
113800980429     C* (SALTA F23 E F24)
113900980429     C     FUDATT        IFEQ      'S'
114000980429     C     FUDFUN        ANDNE     '23'
114100980429     C     FUDFUN        ANDNE     '24'
114200980429     C                   ADD       1             W
114300980429     C                   MOVEL     FUDDES        FUV(W)
114400980429     C                   END
114500980429     C                   END                                                    DO WHILE
114600980429     C*
114700980429     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
114800980429     C* ALTRIMENTI DISATTIVO F24
114900980429     C     1             DO        24            I
115000980429     C                   MOVEL     FUD(I)        FUDRIG
115100980429     C*
115200980429     C     FUDFUN        IFEQ      '24'
115300980429     C     W             IFGT      6
115400980429     C                   MOVEL     'S'           FUDATT
115500980429     C                   ELSE
115600980429     C                   MOVEL     *BLANKS       FUDATT
115700980429     C                   END                                                    W GT 8
115800980429     C                   MOVEL     FUDRIG        FUD(I)
115900980429     C                   END
116000980429     C                   END                                                    DO WHILE
116100980429     C*
116200980429     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
116300980429     C     W             DIV       6             I
116400980429     C                   MVR                     Z
116500980429     C*
116600980429     C     Z             IFNE      0
116700980429     C     I             ADD       1             Z
116800980429     C                   ELSE
116900980429     C                   Z-ADD     I             Z
117000980429     C                   END
117100980429     C*
117200980429     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
117300980429     C                   ADD       1             FUNGRU
117400980429     C     FUNGRU        IFGT      Z
117500980429     C     FUNGRU        ORLT      1
117600980429     C                   Z-ADD     1             FUNGRU
117700980429     C                   END
117800980429     C*
117900980429     C* PRIMO GRUPPO DI 6 FUNZIONI
118000980429     C     FUNGRU        IFEQ      1
118100980429     C                   MOVEL     FUV(1)        FUN001
118200980429     C                   MOVEL     FUV(2)        FUN002
118300980429     C                   MOVEL     FUV(3)        FUN003
118400980429     C                   MOVEL     FUV(4)        FUN004
118500980429     C                   MOVEL     FUV(5)        FUN005
118600980429     C                   MOVEL     FUV(6)        FUN006
118700980429     C                   END
118800980429     C*
118900980429     C* SECONDO GRUPPO DI 6 FUNZIONI
119000980429     C     FUNGRU        IFEQ      2
119100980429     C                   MOVEL     FUV(7)        FUN001
119200980429     C                   MOVEL     FUV(8)        FUN002
119300980429     C                   MOVEL     FUV(9)        FUN003
119400980429     C                   MOVEL     FUV(10)       FUN004
119500980429     C                   MOVEL     FUV(11)       FUN005
119600980429     C                   MOVEL     FUV(12)       FUN006
119700980429     C                   END
119800980429     C*
119900980429     C* TERZO GRUPPO DI 6 FUNZIONI
120000980429     C     FUNGRU        IFEQ      3
120100980429     C                   MOVEL     FUV(13)       FUN001
120200980429     C                   MOVEL     FUV(14)       FUN002
120300980429     C                   MOVEL     FUV(15)       FUN003
120400980429     C                   MOVEL     FUV(16)       FUN004
120500980429     C                   MOVEL     FUV(17)       FUN005
120600980429     C                   MOVEL     FUV(18)       FUN006
120700980429     C                   END
120800980429     C*
120900980429     C* QUARTO GRUPPO DI 6 FUNZIONI
121000980429     C     FUNGRU        IFEQ      4
121100980429     C                   MOVEL     FUV(19)       FUN001
121200980429     C                   MOVEL     FUV(20)       FUN002
121300980429     C                   MOVEL     FUV(21)       FUN003
121400980429     C                   MOVEL     FUV(22)       FUN004
121500980429     C                   MOVEL     *BLANKS       FUN005
121600980429     C                   MOVEL     *BLANKS       FUN006
121700980429     C                   END
121800980429     C*
121900980429     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
122000980429     C                   MOVEL     *BLANKS       FUN007
122100980429     C                   MOVEL     *BLANKS       FUN008
122200980429     C*
122300980429     C     1             DO        24            I
122400980429     C                   MOVEL     FUD(I)        FUDRIG
122500980429     C     FUDFUN        IFEQ      '23'
122600980429     C     FUDATT        ANDEQ     'S'
122700980429     C                   MOVEL     FUDDES        FUN007
122800980429     C                   END
122900980429     C     FUDFUN        IFEQ      '24'
123000980429     C     FUDATT        ANDEQ     'S'
123100980429     C                   MOVEL     FUDDES        FUN008
123200980429     C                   END
123300980429     C                   END                                                    DO
123400980429     C*
123500980429     C                   ENDSR
123600980429     C*------------------------------------------------------------------------*
123700980429     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
123800980429     C*------------------------------------------------------------------------*
123900980429     C     OPZGES        BEGSR
124000980429     C*
124100980429     C* PULISCO CAMPI DI LAVORO
124200980429     C                   Z-ADD     0             W                 5 0
124300980429     C*
124400980429     C* RICERCO QUANTE OPZIONI SONO ATTIVE
124500980429     C* E CARICO TABELLA OPZIONI ATTIVE
124600980429     C     1             DO        24            I
124700980429     C                   MOVEL     OPD(I)        OPDRIG
124800980429     C*
124900980429     C* PULIZIA OPZIONI PRECEDENTI
125000980429     C                   MOVEL     *BLANKS       OPV(I)
125100980429     C*
125200980429     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
125300980429     C     OPDATT        IFEQ      'S'
125400980429     C                   ADD       1             W
125500980429     C                   MOVEL     OPDDES        OPV(W)
125600980429     C                   END
125700980429     C                   END                                                    DO WHILE
125800980429     C*
125900980429     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
126000980429     C* ALTRIMENTI DISATTIVO F23
126100980429     C     1             DO        24            I
126200980429     C                   MOVEL     FUD(I)        FUDRIG
126300980429     C*
126400980429     C     FUDFUN        IFEQ      '23'
126500980429     C     W             IFGT      8
126600980429     C                   MOVEL     'S'           FUDATT
126700980429     C                   ELSE
126800980429     C                   MOVEL     *BLANKS       FUDATT
126900980429     C                   END                                                    W GT 8
127000980429     C                   MOVEL     FUDRIG        FUD(I)
127100980429     C                   END
127200980429     C                   END                                                    DO WHILE
127300980429     C*
127400980429     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
127500980429     C     W             DIV       8             I
127600980429     C                   MVR                     Z                 5 0
127700980429     C*
127800980429     C     Z             IFNE      0
127900980429     C     I             ADD       1             Z
128000980429     C                   ELSE
128100980429     C                   Z-ADD     I             Z
128200980429     C                   END
128300980429     C*
128400980429     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
128500980429     C                   ADD       1             OPZGRU
128600980429     C     OPZGRU        IFGT      Z
128700980429     C     OPZGRU        ORLT      1
128800980429     C                   Z-ADD     1             OPZGRU
128900980429     C                   END
129000980429     C*
129100980429     C* PRIMO GRUPPO DI 8 OPZIONI
129200980429     C     OPZGRU        IFEQ      1
129300980429     C                   MOVEL     OPV(1)        OPZ001
129400980429     C                   MOVEL     OPV(2)        OPZ002
129500980429     C                   MOVEL     OPV(3)        OPZ003
129600980429     C                   MOVEL     OPV(4)        OPZ004
129700980429     C                   MOVEL     OPV(5)        OPZ005
129800980429     C                   MOVEL     OPV(6)        OPZ006
129900980429     C                   MOVEL     OPV(7)        OPZ007
130000980429     C                   MOVEL     OPV(8)        OPZ008
130100980429     C                   END
130200980429     C*
130300980429     C* SECONDO GRUPPO DI 8 OPZIONI
130400980429     C     OPZGRU        IFEQ      2
130500980429     C                   MOVEL     OPV(9)        OPZ001
130600980429     C                   MOVEL     OPV(10)       OPZ002
130700980429     C                   MOVEL     OPV(11)       OPZ003
130800980429     C                   MOVEL     OPV(12)       OPZ004
130900980429     C                   MOVEL     OPV(13)       OPZ005
131000980429     C                   MOVEL     OPV(14)       OPZ006
131100980429     C                   MOVEL     OPV(15)       OPZ007
131200980429     C                   MOVEL     OPV(16)       OPZ008
131300980429     C                   END
131400980429     C*
131500980429     C* TERZO GRUPPO DI 8 OPZIONI
131600980429     C     OPZGRU        IFEQ      3
131700980429     C                   MOVEL     OPV(17)       OPZ001
131800980429     C                   MOVEL     OPV(18)       OPZ002
131900980429     C                   MOVEL     OPV(19)       OPZ003
132000980429     C                   MOVEL     OPV(20)       OPZ004
132100980429     C                   MOVEL     OPV(21)       OPZ005
132200980429     C                   MOVEL     OPV(22)       OPZ006
132300980429     C                   MOVEL     OPV(23)       OPZ007
132400980429     C                   MOVEL     OPV(24)       OPZ008
132500980429     C                   END
132600980429     C*
132700980429     C                   ENDSR
132800980429     C*------------------------------------------------------------------------*
132900980429     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
133000980429     C*------------------------------------------------------------------------*
133100980429     C     OPZCON        BEGSR
133200980429     C*
133300980429     C* PULISCO CAMPI DI LAVORO
133400980429     C                   Z-ADD     0             I                 5 0
133500980429     C                   Z-ADD     0             W                 5 0
133600980429     C*
133700980429     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
133800980429     C     1             DO        24            I
133900980429     C                   MOVEL     OPD(I)        OPDRIG
134000980429     C*
134100980429     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
134200980429     C     OPZKEY        IFEQ      OPDOPZ
134300980429     C     OPZKEY        OREQ      OPDOP1
134400980429     C*
134500980429     C     OPDATT        IFEQ      'S'
134600980429     C                   MOVEL     OPDOPZ        OPZKEY
134700980429     C                   Z-ADD     I             W
134800980429     C                   END
134900980429     C                   END
135000980429     C*
135100980429     C                   END                                                    DO WHILE
135200980429     C*
135300980429     C* OPZIONE NON ATTIVA (ERRORE)
135400980429     C     W             IFEQ      0
135500980429     C*
135600980429     C* OPZIONE NON OK
135700980429     C                   Z-ADD     1             OPZOK
135800980429     C*
135900980429     C* EMISSIONE MESSAGGIO DI ERRORE
136000980429     C                   MOVEL     MSG(1)        DSMSMS
136100980429     C                   ELSE
136200980429     C*
136300980429     C* OPZIONE ATTIVA (PROCEDI ...)
136400980429     C*
136500980429     C     OPZKEY        IFEQ      ' 2'
136600980429     C*---                 EXSR O02GES
136700980429     C                   END
136800980429     C     OPZKEY        IFEQ      ' 4'
136900980429     C*---                 EXSR O04GES
137000980429     C                   END
137100980429     C     OPZKEY        IFEQ      ' 5'
137200980429     C*---                 EXSR O05GES
137300980429     C                   END
137400980429     C     OPZKEY        IFEQ      ' 7'
137500980429     C*---                 EXSR O07GES
137600980429     C                   END
137700980429     C                   END                                                    OPZ.NON ATTIVA
137800980429     C*
137900980429     C                   ENDSR
138000980429     C*------------------------------------------------------------------------*
138100980429     C* NOTE COSTRUZIONE SCHIERE
138200980429     C*------------------------------------------------------------------------*
138300980429     C*
138400980429     C* -MSG-MESSAGGI
138500980429     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
138600980429     C* (NON TOCCARE MSG,1 E MSG,2)
138700980429     C*
138800980429     C* -CMD-COMANDI
138900980429     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
139000980429     C*
139100980429     C* -OPD-OPZIONI
139200980429     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
139300980429     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
139400980429     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
139500980429     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
139600980429     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
139700980429     C*  . POSSONO ESSERE IN ORDINE SPARSO
139800980429     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
139900980429     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
140000980429     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
140100980429     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
140200980429     C*
140300980429     C* -FUD-FUNZIONI
140400980429     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
140500980429     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
140600980429     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
140700980429     C*  . POSSONO ESSERE IN ORDINE SPARSO
140800980429     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
140900980429     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
141000980429     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
141100980429     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
141200980429     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
141300980429     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
141400980429     O*------------------------------------------------------------------------*
141500980429** DEC - DECODIFICHE
141600980429    GESTIONE                                                     1
141700980429INTERROGAZIONE                                                   2
141701060830     SCELTA                                                      3
1419009804294
1420009804295
142100980429** MSG - MESSAGGI
142200980429TNTE34R- Opzione non gestita                                                   1
142300980429TNTE34R- Tasto funzionale non gestito                                          2
142400980429TNTE34R- 3
142500980429TNTE34R- Record annullato. Consentito solo il ripristino                       4
142600980429TNTE34R- Record non annullato. Impossibile ripristinare                        5
142700980429TNTE34R- 6                                                                     6
142800980429TNTE34R- 7                                                                     7
142900980429TNTE34R- 8                                                                     8
143000980429TNTE34R- 9                                                                     9
143100980429TNTE34R- 10                                                                   10
143200980429TNTE34R- 11                                                                   11
143300980429TNTE34R- 12                                                                   12
143400980429TNTE34R- 13                                                                   13
143500980429TNTE34R- 14                                                                   14
143600980429TNTE34R- 15                                                                   15
143700980429TNTE34R- 16                                                                   16
143800980429TNTE34R- 17                                                                   17
143900980429TNTE34R- 18                                                                   18
144000980429TNTE34R- 19                                                                   19
144100980429TNTE34R- 20                                                                   20
144200980429** OPD - OPZIONI
144300980429                   -  -     (S/N)=N                        opz. 01¬
144400980429 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
144500980429                   -  -     (S/N)=N                        opz. 03¬
144600980429 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
144700980429 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
144800980429                   -  -     (S/N)=N                        opz. 06¬
144900980429 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
145000980429                   -8 -     (S/N)=N                        opz. 08¬
145100980429                   -9 -     (S/N)=N                        opz. 09¬
145200980429                   -  -     (S/N)=N                        opz. 10¬
145300980429                   -  -     (S/N)=N                        opz. 11¬
145400980429                   -  -     (S/N)=N                        opz. 12¬
145500980429                   -  -     (S/N)=N                        opz. 13¬
145600980429                   -  -     (S/N)=N                        opz. 14¬
145700980429                   -  -     (S/N)=N                        opz. 15¬
145800980429                   -  -     (S/N)=N                        opz. 16¬
145900980429                   -  -     (S/N)=N                        opz. 17¬
146000980429                   -  -     (S/N)=N                        opz. 18¬
146100980429                   -  -     (S/N)=N                        opz. 19¬
146200980429                   -  -     (S/N)=N                        opz. 20¬
146300980429                   -  -     (S/N)=N                        opz. 21¬
146400980429                   -  -     (S/N)=N                        opz. 22¬
146500980429                   -  -     (S/N)=N                        opz. 23¬
146600980429                   -  -     (S/N)=N                        opz. 24¬
146700980429** FUD - FUNZIONI
146800980429                   -        (S/N)=N                        funz.01¬
146900980429                   -        (S/N)=N                        funz.02¬
147000980429F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
147100980429                   -        (S/N)=N                        funz.04¬
147200980429F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
147300980429F06=Inserimento    -        (S/N)=N                        funz.06¬
147400980429                   -        (S/N)=N                        funz.07¬
147500980429                   -        (S/N)=N                        funz.08¬
147600980429                   -        (S/N)=N                        funz.09¬
147700980429                   -        (S/N)=N                        funz.10¬
147800980429                   -        (S/N)=N                        funz.11¬
147900980429F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
148000980429                   -        (S/N)=N                        funz.13¬
148100980429                   -        (S/N)=N                        funz.14¬
148200980429                   -        (S/N)=N                        funz.15¬
148300980429                   -        (S/N)=N                        funz.16¬
148400980429                   -        (S/N)=N                        funz.17¬
148500980429                   -        (S/N)=N                        funz.18¬
148600980429 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
148700980429 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
148800980429                   -        (S/N)=N                        funz.21¬
148900980429                   -        (S/N)=N                        funz.22¬
149000980429F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
149100980429F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
