000100970526     H*------------------------------------------------------------------------*
000200060804     H* Gestione associazione depot dpd a codice piano dei conti - Scelta
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800170919     FTISIE5D   CF   E             WORKSTN INFDS(DEVDS1)
000900170914     F                                     SFILE(SIE5D05:SFLULT)
001000170914     F                                     SFILE(SIE5DMS:MSGULT)
001100170914     FDPLEG11L  IF   E           K DISK
001200170915     FDPCDP11i  IF   E           K DISK
001300060831     Fazorg01l  IF   E           K DISK
001400060831     Ftabel00f  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* COMANDI
002800970526     D*------------
002900970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI OPZIONI
003200970526     D*------------
003300970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* DESCRIZIONI TASTI FUNZIONALI
003600970526     D*------------
003700970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003800970526     D*------------
003900970526     D* OPZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970526     D OPV             S             19    DIM(24)
004200970526     D*------------
004300970526     D* FUNZIONI DA CARICARE A VIDEO
004400970526     D*------------
004500970526     D FUV             S             19    DIM(24)
004600970526     D*------------------------------------------------------------------------*
004700970526     D* INPUT
004800970526     D*------------------------------------------------------------------------*
004900170918     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
005000970526     D*------------
005100970526     D* RIDEFINIZIONE OPZIONI VIDEO
005200970526     D*------------
005300970526     D OPDDS           DS
005400970526     D  OPDOPZ                 1      2
005500970526     D  OPDDE1                 3     19
005600970526     D  OPDOP1                21     22
005700970526     D  OPDFI0                23     34
005800970526     D  OPDATT                35     35
005900970526     D  OPDFI1                36     80
006000970526     D  OPDDES                 1     19
006100970526     D  OPDRIG                 1     80
006200970526     D*------------
006300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006400970526     D*------------
006500970526     D FUDDS           DS
006600970526     D  FUDDE1                 1      1
006700970526     D  FUDFUN                 2      3
006800970526     D  FUDDE2                 4     19
006900970526     D  FUDFI0                20     34
007000970526     D  FUDATT                35     35
007100970526     D  FUDFI1                36     80
007200970526     D  FUDDES                 1     19
007300970526     D  FUDRIG                 1     80
007400970526     D*------------
007500970526     D* DS GESTIONE TASTI FUNZIONALI
007600970526     D*------------
007700970526     D DSKEY         E DS
007800970526     D DEVDS1          DS
007900970526     D  KEY                  369    369
008000970526     D*------------
008100970526     D* DS "XMSG" - GESTIONE MESSAGGI
008200970526     D*------------
008300970526     D DSMSG         E DS
008400970526     D*------------
008500020917     D* DS REPERIMENTO DATI UTENTE
008600970526     D*------------
008700020917     D TIBS34DS      E DS                                                       *Profili utente
008800020917     D DDATIUTE      E DS                                                       *Dati utente
008900020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009000970526     D*------------------
009100970526     D* DS "XSRDAT" - CONTROLLA DATA (6)
009200970526     D*------------------
009300970526     D WLBDAT          DS                  INZ
009400970526     D  G06DAT                 1      6  0
009500970526     D  G06INV                 7     12  0
009600970526     D  G06ERR                13     13
009700970526     D  G06TGI                14     18  0
009800970526     D*------------------
009900970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
010000970526     D*------------------
010100970526     D WLBDA8          DS                  INZ
010200970526     D  G08DAT                 1      8  0
010300970526     D  G08INV                 9     16  0
010400970526     D  G08ERR                17     17
010500970526     D  G08TGI                18     22  0
010600970526     D*------------
010700970526     D* DS DI PROCEDURA
010800970526     D*------------
010900170919     D tisie5ds      E DS
011000060919      * - Ds per Anagrafiche Clienti
011100060919     d TIBS69DS      E DS
011200060919     d DS_cnaco      E DS                  extname(CNACO00F)
011300060919     d DS_cnind      E DS                  extname(CNIND00F)
011400060919     d DS_cnclp      E DS                  extname(CNCLP00F)
011500060919     d DS_fncls      E DS                  extname(FNCLS00F)
011600060919
011700970526     D*------------
011800970526     D* ARCHITETTURA
011900970526     D*------------
012000970526     D KPJBA         E DS
012100170918     D  savkpjbu       s                   like(kpjbu)
012200060831     D*------------
012300060831     d
012400060831     d og143         E DS
012500060901     d ds15          E ds
012600060831     d trul06ds      E DS
012700060831     D  LIN                    1     90  0 dim(30)
012800060831     D*-------------------
012900060831     D* VARIABILI DI WRK
013000060831     D*-------------------
013100060901     D ldp             S              3S 0 DIM(20)
013200060831     D skNaz           S              3    DIM(1000)
013300970526     C*------------------------------------------------------------------------*
013400970526     C* MAIN LINES
013500970526     C*------------------------------------------------------------------------*
013600970526     C*
013700970526     C* SE NON CI SONO ERRORI, PROSEGUE
013800970526IF  1C     ERRL00        IFNE      '1'
013900970526     C*
014000970526     C* CARICA IL SFILE
014100970526     C                   EXSR      POSSFL
014200970526     C*
014300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014700970526     C*
014800970526     C* REIMPOSTA ERRORE DI PROCERURA
014900970526     C                   MOVEL     '0'           ERRL00
015000970526     C*
015100970526     C* EMETTE IL SFILE
015200970526     C                   EXSR      OUTSFL
015300970526     C*
015400970526     C* GESTIONE TASTI FUNZIONALI
015500970526     C                   EXSR      FUNCON
015600970526     C*
015700970526E   2C                   ENDDO
015800970527     C*
015900970527     C* CI SONO ERRORI IN ENTRATA
016000970527X   1C                   ELSE
016100060831     C                   MOVEL     '1'           DE5ERR
016200060831     C                   MOVEL     DSMSMS        DE5MSG
016300970526E   1C                   ENDIF
016400970526     C* FINE PGM
016500060919     C                   eval      I69TLA  = 'C'
016600060919     C                   call      'TIBS69R'
016700060919     C                   parm                    TIBS69DS
016800970526     C                   SETON                                        LR
016900970526     C*------------------------------------------------------------------------*
017000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017100970526     C*------------------------------------------------------------------------*
017200970526     C     F03GES        BEGSR
017300970526     C*
017400970526     C* IMPOSTA "PREMUTO F03"
017500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017600060831     C                   MOVEL     '1'           DE5F03                         *DI PROCEDURA
017700970526     C*
017800970526     C                   ENDSR
017900970526     C*------------------------------------------------------------------------*
018000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018100970526     C*------------------------------------------------------------------------*
018200970526     C     F05GES        BEGSR
018300970526     C*
018400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018500970526     C*
018600970526     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800000119     C* F06GES - GESTIONE F06 -> INSERIMENTO
018900000119     C*------------------------------------------------------------------------*
019000970526     C     F06GES        BEGSR
019100970526     C*
019200970526     C*
019300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019400970529     C                   MOVEL     DEPOP0        WOP0
019500970529     C                   MOVEL     'F06'         WOP1
019600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019700970526E   1C                   ENDIF
019800000119     C*
019900000119     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* F12GES - GESTIONE F12 -> RITORNO
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F12GES        BEGSR
020400970526     C*
020500970526     C* IMPOSTA "PREMUTO F12"
020600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020700060831     C                   MOVEL     '1'           DE5F12                         *DI PROCEDURA
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F21GES        BEGSR
021400970612     C*
021500060919IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021600060919     C                   MOVEL     DEPOP0        WOP0
021700060920     C                   MOVEL     'F21'         WOP1
021800170919     C                   MOVEL     'TISIE5R2'    WPGM             10
021900060920     C                   EXSR      LANPGM                                       *LANCIA PGM
022000060919E   1C                   ENDIF
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F22GES        BEGSR
022700970526     C*
022800970526     C                   Z-ADD     80            LENGH            15 5
022900970526     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
023000970526     C                   PARM                    CMD(1)
023100970526     C                   PARM                    LENGH
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F23GES        BEGSR
023800970526     C*
023900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024400970526     C*------------------------------------------------------------------------*
024500970526     C     F24GES        BEGSR
024600970526     C*
024700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025200970526     C*------------------------------------------------------------------------*
025300970526     C     RUPGES        BEGSR
025400970526     C*
025500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* ENTGES - GESTIONE ENTER -> INVIO
026000970526     C*------------------------------------------------------------------------*
026100970526     C     ENTGES        BEGSR
026200970526     C*
026300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026400970526     C                   EXSR      NEWPOS
026500970526     C*
026600970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026800970526IF  2C     SWPOSI        IFNE      0
026900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027000970526X   2C                   ELSE
027100970526IF  3C     SFLULT        IFNE      0
027200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027300970526     C*
027400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027500970526IF  4C     SWMODI        IFNE      0
027600970526     C                   EXSR      POSSFL
027700970526E   4C                   ENDIF
027800970526E   3C                   ENDIF
027900970526E   2C                   ENDIF
028000970526E   1C                   ENDIF
028100970526     C*
028200970526     C                   ENDSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028500970526     C*------------------------------------------------------------------------*
028600970526     C     O01GES        BEGSR
028700970526     C*
028800970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029400970526     C*------------------------------------------------------------------------*
029500970526     C     O02GES        BEGSR
029600970526     C*
029700970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029800060901IF  1C***  VIDATB        IFEQ      'A'
029900060901     C***                MOVEL     '1'           ERRL00
030000060901     C***                MOVEL     MSG(4)        DSMSMS
030100060901E   1C***                ENDIF
030200970526     C* MODIFICA
030300970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400060901     C                   MOVEL     VIDdpc        Ddpc
030500170915     C                   MOVEL     VIDbun        Dbun
030600060907     C                   z-add     VIhdde        Ddde
030700970529     C                   MOVEL     DEPOP0        WOP0
030800970526     C                   MOVEL     'O02'         WOP1
030900170919     C                   MOVEL     'TISIE5R1'    WPGM
031000970526     C                   EXSR      LANPGM
031100060919     C                   Z-ADD     1             SWMODI                     L
031200970526E   1C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500970526     C*------------------------------------------------------------------------*
031600970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
031700970526     C*------------------------------------------------------------------------*
031800970526     C     O04GES        BEGSR
031900970526     C*
032000970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032100060901IF  1C**   VIDATB        IFEQ      'A'
032200060901     C***                MOVEL     '1'           ERRL00
032300060901     C***                MOVEL     MSG(4)        DSMSMS
032400060901E   1C***                ENDIF
032500970526     C* ANNULLAMENTO
032600060901IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032700970529     C                   MOVEL     DEPOP0        WOP0
032800970526     C                   MOVEL     'O04'         WOP1
032900970526E   1C                   ENDIF
033000970526     C*
033100970526     C                   ENDSR
033200970526     C*------------------------------------------------------------------------*
033300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
033400970526     C*------------------------------------------------------------------------*
033500970526     C     O05GES        BEGSR
033600970526     C*
033700060901     C                   MOVEL     VIDdpc        Ddpc
033800170915     C                   MOVEL     VIDbun        Dbun
033900060907     C                   z-add     vihdde        Ddde
034000970529     C                   MOVEL     DEPOP0        WOP0
034100970526     C                   MOVEL     'O05'         WOP1
034200170919     C                   MOVEL     'TISIE5R1'    WPGM
034300970526     C                   EXSR      LANPGM
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     O07GES        BEGSR
035000970526     C*
035100970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
035200060901IF  1C***  VIDATB        IFNE      'A'
035300060901     C***                MOVEL     '1'           ERRL00
035400060901     C***                MOVEL     MSG(5)        DSMSMS
035500060901E   1C***                ENDIF
035600970526     C* RIPRISITINO
035700970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035800970529     C                   MOVEL     DEPOP0        WOP0
035900970526     C                   MOVEL     'O07'         WOP1
036000970526E   1C                   ENDIF
036100970526     C*
036200970526     C                   ENDSR
036300970526     C*------------------------------------------------------------------------*
036400970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036500970526     C*------------------------------------------------------------------------*
036600970526     C     NEWPOS        BEGSR
036700970526     C*
036800970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036900970526     C*         1= RICHIESTO POSIZIONAMENTO
037000970526     C                   Z-ADD     0             SWPOSI            7 0
037100970526     C*
037200970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037300970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037400970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037500970526     C                   Z-ADD     0             SWMODI            1 0
037600970530     C*
037700970530     C* REIMPOSTA INDICATORI DI ERRORE
037800970530     C                   SETOFF                                       808182
037900970530     C                   SETOFF                                       83
038000970526     C*
038100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038200060907IF  1C     VIDdde_P      IFNE      DEPdde
038300170918     C     VIDdpt        ORNE      DEPdpt
038400060901     C     VIDsel        ORNE      DEPsel
038500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
038700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
038800060907     C                   z-add     VIDdde_P      DEPdde                         *DEPOSITI
038900170918     C                   MOVEL     VIDdpt        DEPdpt
039000060901     C                   movel     VIDsel        DEPsel
039100150925     c                   z-add     0             sflnbr
039200970526E   2C                   ENDIF
039300970526E   1C                   ENDIF
039400970526     C*
039500970526     C                   ENDSR
039600970526     C*------------------------------------------------------------------------*
039700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039800970526     C*------------------------------------------------------------------------*
039900970526     C     SETFIL        BEGSR
040000970526     C*
040100060831     C* POSIZIONAMENTO SUL FILE
040200170918     c                   move      viddpt        LEGdep7
040300170915     C     LEGdep7       SETLL     DPLEG11l                           99
040400970526     C* LETTURA RECORD
040500970526IF  1C     *IN99         IFEQ      *OFF
040600970526     C                   EXSR      LETFIL
040700970526E   1C                   ENDIF
040800970526     C*
040900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041000970526IF  1C     *IN99         IFEQ      *ON
041100970526     C                   Z-ADD     1             SFLEOF
041200970526E   1C                   ENDIF
041300970526     C*
041400970526     C                   ENDSR
041500970526     C*------------------------------------------------------------------------*
041600970526     C* LETFIL - LETTURA RECORD
041700970526     C*------------------------------------------------------------------------*
041800970526     C     LETFIL        BEGSR
041900970526     C*
042000970526     C* LEGGE FINO A:
042100970526     C*     - FINE FILE
042200970526     C*     - TROVATO RECORD VALIDO
042300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
042400970526DO  1C     *IN99         DOWEQ     *OFF
042500970526     C     WRECOK        ANDEQ     'N'
042600970526     C*
042700170914     C                   READ      DPLEG11l                               99
042800970526     C*
042900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
043100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
043200970530X   2C                   ELSE
043300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
043400970530E   2C                   ENDIF
043500970526E   1C                   ENDDO
043600970526     C*
043700970526     C                   ENDSR
043800970526     C*------------------------------------------------------------------------*
043900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
044000970526     C*------------------------------------------------------------------------*
044100970526     C     CHKREC        BEGSR
044200970526     C*
044300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
044400060901     c* record non valido se:
044500150610     c********* 1) nazione deposito  non presente in schiera
044600060901     c* 2) decorrenza richiesta non inclusa nella decorrenza/scadenza
044700170914     c* 3) selezione tipologia deposito (con o senza cod. LEG) non congruent
044800060901     c*    col deposito letto
044900060901     c* nazione
045000150610     c**********         if        %subst(knmus:1:3)<>'EDP'
045100170914     c***  LEGnar        lookup    sknaz                                  98
045200150610     c******             if        *in98='0'
045300150610     c********           eval      wrecok='N'
045400150610     c*********          endif
045500150610     c**********         endif
045600060901     c* decorrenza
045700170914     c                   if        viddde_P >0 and (ddevid < LEGdde or
045800170914     c                             ddevid > LEGdsc)
045900060901     c                   eval      wrecok='N'
046000060901     c                   endif
046100170914     c* solo depositi senza codice LEG
046200170914     c                   if        vidsel='S' and LEGksc > 0
046300060901     c                   eval      wrecok='N'
046400060901     c                   endif
046500970526     C*
046600060901     C                   ENDSR
046700970526     C*------------------------------------------------------------------------*
046800970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
046900970526     C*------------------------------------------------------------------------*
047000970526     C     IRESFL        BEGSR
047100970526     C*
047200970526     C                   MOVEL     *BLANKS       OPZKEY
047300170914     C                   MOVEL     LEGnar        vidnar
047400170914     C                   MOVEL     LEGdpc        VIDdpc
047500170914     C                   MOVEL     LEGbun        VIDbun
047600060901     c* decodifica del depot (vidnam1 e vidcty) solo se non è scaduto
047700060901     c                   clear                   vidnam1
047800060901     c                   clear                   vidcty
047900170914    1c                   if        LEGdsc >= datcor
048000060901     c* recupero l'ultima versione di cappario
048100170918     C                   clear                   ISID1DRI
048200170918     C                   clear                   OSID1VER
048300170918     C                   clear                   OSID1VERD
048400170918     C                   clear                   OSID1DDE
048500170918     C                   clear                   OSID1DSC
048600170918     C                   clear                   OSID1BCID
048700170918     c                   call      'TISID1R'
048800170918     C                   PARM                    ISID1DRI
048900170918     C                   PARM                    OSID1VER
049000170918     C                   PARM                    OSID1VERD
049100170918     C                   PARM                    OSID1DDE
049200170918     C                   PARM                    OSID1DSC
049300170918     C                   PARM                    OSID1BCID
049400060901     c* se trovata versione reperisco  descrizioni deposito
049500170918    2c                   if        osid1ver>0
049600170915     C*
049700170915     c                   move      ' '           CDPATB
049800170915     c     keycdp        chain     dpcdp11i
049900170915    3c                   if        %found(dpcdp11i)
050000170915     c                   movel     cdpname1      vidnam1
050100170915     c                   movel     cdpcity       vidcty
050200060901    3c                   endif
050300060901    2c                   endif
050400060901    1c                   endif
050500060919     c                   clear                   vidrag
050600060919     c                   clear                   vidcit
050700060919     c                   clear                   vidcos
050800060919     c                   clear                   vidksc
050900060919     c                   clear                   vidsta
051000170914     c                   if        LEGksc > 0
051100170914     C                   move      LEGksc        VIDksc
051200060919      * aggancio anagrafico
051300060919
051400060919     C                   clear                   TIBS69DS
051500170914     c                   z-add     LEGksc        I69kac
051600170914     c                   z-add     LEGksc        I69kin
051700060919     C                   call      'TIBS69R'
051800060919     C                   parm                    tibs69DS
051900060919     C                   parm                    DS_cnaco
052000060919     C                   parm                    DS_cnind
052100060919     C                   parm                    DS_cnclp
052200060919     C                   parm                    DS_fncls
052300060919    2c                   If         O69ERR =  ' '
052400170918     c                   movel     'Cli.'        vidcos
052500060919     c                   movel     ACOrag        vidrag
052600060919     c                   movel     indcit        vidcit
052700060919     c                   movel     indsta        vidsta
052800060919    2c                   Endif
052900060907     c                   endif
053000060901     c* decorrenza/scadenza
053100170914     C                   Z-ADD     LEGdde        G08INV
053200060901     C                   Z-ADD     *ZEROS        G08DAT
053300060901     C                   MOVEL     '3'           G08ERR
053400060901     C                   CALL      'XSRDA8'
053500060901     C                   PARM                    WLBDA8
053600060901     C                   Z-ADD     G08DAT        VIDdde                         *DATA RIFERIMENTO
053700170914     C                   Z-ADD     LEGdde        VIhdde                         *DATA RIFERIMENTO
053800170914     C                   Z-ADD     LEGdsc        G08INV
053900060901     C                   Z-ADD     *ZEROS        G08DAT
054000060901     C                   MOVEL     '3'           G08ERR
054100060901     C                   CALL      'XSRDA8'
054200060901     C                   PARM                    WLBDA8
054300060901     C                   Z-ADD     G08DAT        VIDdsc                         *DATA RIFERIMENTO
054400060919     c*
054500170914     c                   movel     LEGorma       vidorm
054600970526     C*
054700970526     C                   ENDSR
054800970526     C*------------------------------------------------------------------------*
054900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
055000970526     C*------------------------------------------------------------------------*
055100970526     C     CHKPAR        BEGSR
055200970526     C*
055300060907     C     *LIKE         DEFINE    VIDdde_p      ddeVID                         *DATA IN A/M/G
055400060901     C                   Z-ADD     *ZEROS        ddeVID
055500060901IF  1C     ERRL00        IFEQ      '0'
055600060907IF  2C     VIDdde_P      IFGT      *ZEROS
055700060907     C                   Z-ADD     VIDdde_P      G08DAT
055800060901     C                   Z-ADD     *ZEROS        G08INV
055900060901     C                   MOVEL     '0'           G08ERR
056000060901     C                   CALL      'XSRDA8'
056100060901     C                   PARM                    WLBDA8
056200060907     C                   Z-ADD     G08DAT        VIDdde_p
056300060901     C                   Z-ADD     G08INV        ddeVID
056400060901IF  3C     G08ERR        IFEQ      '1'
056500060901     C                   MOVEL     '1'           ERRL00
056600060901     C                   MOVEL     MSG(12)       DSMSMS
056700060901     C                   SETON                                        83
056800060901E   3C                   ENDIF
056900060901E   2C                   ENDIF
057000060901E   1C                   ENDIF
057100970526     C*
057200970526     C                   ENDSR
057300970526     C*------------------------------------------------------------------------*
057400970526     C* LANPGM - LANCIA UN PROGRAMMA
057500970526     C*------------------------------------------------------------------------*
057600970526     C     LANPGM        BEGSR
057700970526     C*
057800970526     C* IMPOSTA LA DS DI PROCEDURA
057900060831     C                   MOVEL     WOP0          DE5OP0                         *LIVELLO PROCEDURA
058000060831     C                   MOVEL     WOP1          DE5OP1                         *LIVELLO PGM
058100060831     C                   MOVEL     '0'           DE5F03                         *PREMUTO F03=FINE
058200060831     C                   MOVEL     '0'           DE5F12                         *PREMUTO F12=RITORNO
058300060831     C                   MOVEL     '0'           DE5ERR                         *'1'=ERRORE
058400060831     C                   MOVEL     *BLANKS       DE5MSG                         *MESSAGGIO DI RITORN
058500060831     C                   MOVEL     Ddpc          DE5DPC                         *NAZIONE
058600060831     C                   MOVEL     DDDE          DE5DDE                         *CAP
058700170918     c                   movel     vidbun        de5dep7
058800170918     c                   move      viddpc        de5dep7
058900170918     c                   move      viddpc        de5dpt
059000170918     c                   move      vidbun        de5bun
059100060920     c                   movel     vidsel        de5sel
059200060920     c                   z-add     ddevid        de5ddev
059300970526     C*
059400970526     C                   CALL      WPGM
059500970612     C                   PARM                    KPJBA
059600170919     C                   PARM                    tisie5ds
059700970526     C*
059800970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
059900060831IF  1C     DE5F03        IFEQ      '1'                                          *PREMUTO F03
060000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
060100970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
060200970526X   1C                   ELSE                                                   *NON PREMUTO F03
060300060831     C                   MOVEL     DE5MSG        DSMSMS                         *MESSAGGIO DI RITORN
060400970526E   1C                   ENDIF
060500970526     C*
060600970526     C                   ENDSR
060700970527     C*------------------------------------------------------------------------*
060800970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
060900970527     C*------------------------------------------------------------------------*
061000970527     C     CARTAB        BEGSR
061100060831     C* CARICO TABELLA PUNTI OPERATIVI dell'area di partenza del p.o. utente
061200060831     C***
061300060831     C                   CLEAR                   trul06ds
061400060831     C                   MOVE      '£1'          D06COD
061500060831     C                   MOVEL     duttfp        D06KEY
061600060831     C                   MOVEL     'L'           D06TLA
061700060831     C                   MOVEL     'S'           D06esc
061800170918     c                   movel     kpjbu         savkpjbu
061900060831     C                   MOVEL     trul06ds      KPJBU
062000060831     C*
062100060831     C                   CALL      'TRUL06R'
062200060831     C                   PARM                    KPJBA
062300060831     C                   MOVEL     KPJBU         trul06ds
062400170918     c                   eval      kpjbu = savkpjbu
062500060831     c* carico la schiera dei p.o. dpd della £1
062600060831     c                   z-add     1             ii                2 0
062700060831     c                   z-add     0             xx                2 0
062800060831     c                   clear                   ldp
062900060901    1c                   dow       lin(ii) > *zeros  and
063000060831     c                             ii <= %elem(lin)
063100060831     c     lin(ii)       chain     azorg01l
063200060831    2c                   if        %found(azorg01l)
063300060831     c                   movel     orgde3        og143
063400060831    3c                   if        §ogntw = 'DPD'
063500060831     c                   add       1             xx
063600060901     c                   z-add     orgfil        ldp(xx)
063700060831    3c                   endif
063800060831    2c                   endif
063900060831     c                   add       1             ii
064000060831    1c                   enddo
064100150610      **    se ci sono linee DPD per la filiale in gestione
064200150610      **    altrimenti  deve uscire dal programma.
064300150610     c                   clear                   FIL_HA_DPD        1
064400150610    3c                   if        xx > 0
064500150610     c                   move      'S'           FIL_HA_DPD
064600150610      **
064700060831     c* lettura tabella nazioni a caricamento delle nazioni servite dai p.o.
064800060831     c* dpd caricati
064900060831     c* Carico tutte le nazioni servite dai p.o. dpd dell'area  di partenza
065000060831     c                   z-add     0             yy                4 0
065100060831     C                   CLEAR                   skNaz
065200060831     C                   CLEAR                   DS15
065300060831     C                   EVAL      tblKUT = 1
065400060831     C                   EVAL      tblCOD = '15'
065500060831     c     keytabel_P    setll     tabel00f
065600060831    1c                   do        *hival
065700060831     c     keytabel_P    reade     tabel00f
065800060831    2c                   if        %eof(tabel00f)
065900060831     c                   leave
066000060831    2c                   endif
066100060831     c                   movel     tbluni        ds15
066200060831    2c                   if        §15lad > 0
066300060831     c     §15lad        lookup    ldp                                    99
066400060831    3c                   if        *in99
066500060831     c                   add       1             yy
066600060831     c                   movel     tblkey        sknaz(yy)
066700060831    3c                   endif
066800060831    2c                   endif
066900060831    1c                   enddo
067000970527     C*
067100150610     c                   end
067200150610     C*
067300970527     C                   ENDSR
067400970526     C*------------------------------------------------------------------------*
067500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
067600970526     C*------------------------------------------------------------------------*
067700970526     C     CHKFOP        BEGSR
067800970526     C*
067900970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
068000060901    1c                   if        %subst(depop0:2:2)= '01'
068100970526     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
068200970526     C                   MOVEL     'S'           OPDATT
068300970526     C                   MOVEL     OPDRIG        OPD(1)
068400970626X   1C                   ELSE
068500970626     C* OPZIONI
068600970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
068700970626     C                   MOVEL     'S'           OPDATT
068800970626     C                   MOVEL     OPDRIG        OPD(5)
068900970526E   1C                   ENDIF
069000970526     C*
069100970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
069200060901    1c                   if        %subst(depop0:2:2)= '02'
069300970526     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
069400970526     C                   MOVEL     'S'           OPDATT
069500970526     C                   MOVEL     OPDRIG        OPD(2)
069600970526     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
069700060906     C                   MOVEL     'N'           FUDATT
069800970526     C                   MOVEL     FUDRIG        FUD(6)
069900970526E   1C                   ENDIF
070000970526     C*
070100970526     C* GESTIONE OPZIONI A VIDEO
070200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
070300970526     C                   EXSR      OPZGES
070400970526     C*
070500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
070600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
070700970526     C                   EXSR      FUNGES
070800970526     C*
070900970526     C                   ENDSR
071000020917     C*--------------------------------------------------------------------------------------------*
071100020917     C* REPERISCE I DATI UTENTE
071200020917     C*--------------------------------------------------------------------------------------------*
071300020917     C     REPDATIUTE    BEGSR
071400020917     C*
071500020917     C* INIZIALIZZA VARIABILI DI WRK
071600020917     C                   CLEAR                   TIBS34DS
071700020917     C                   CLEAR                   AZUTEDS
071800020917     C                   CLEAR                   DDATIUTE
071900020917     C*
072000020917     C     *DTAARA       DEFINE    §azute        azuteds
072100020917     C     *DTAARA       DEFINE    §datiute      ddatiute
072200020917     C                   IN(E)     *DTAARA
072300020917if  1C                   IF        %Error
072400020917     c                   EVAL      I34Tla = 'L'
072500020917     C                   CALL      'TIBS34R'
072600020917     C                   PARM                    Tibs34Ds
072700020917     C                   IN        *DTAARA
072800020917e   1C                   ENDIF
072900020917     C*
073000020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
073100020917     C                   MOVEL(P)  RSUT          DSFIRS           20
073200020917     C*
073300020917     C                   ENDSR
073400970526     C*------------------------------------------------------------------------*
073500970526     C* *INZSR - OPERAZIONI INIZIALI
073600970526     C*------------------------------------------------------------------------*
073700970526     C     *INZSR        BEGSR
073800970526     C*---
073900970526     C* RICEVIMENTO PARAMETRI
074000970526     C*---
074100970526     C     *ENTRY        PLIST
074200970612     C                   PARM                    KPJBA
074300170919     C                   PARM                    tisie5ds
074400970526     C*
074500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
074600970526     C     XMSGPA        PLIST
074700970526     C                   PARM                    DSMSMS
074800970526     C                   PARM                    DSMSPG
074900970526     C                   PARM                    DSMSKE
075000970526     C                   PARM                    DSMSER
075100970526     C*
075200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
075300970526     C                   CALL      'XKEY'
075400970526     C                   PARM                    DSKEY
075500970526     C*---
075600970526     C* VARIABILI NON RIFERITE AL DATA BASE
075700970526     C*---
075800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
075900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
076000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
076100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
076200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
076300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
076400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
076500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
076600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
076700060901     C     *LIKE         DEFINE    de5op0        depop0
076800060901     C     *LIKE         DEFINE    de5op1        depop1
076900060901     C     *LIKE         DEFINE    De5OP0        WOP0                           *DI LAVORO
077000060901     C     *LIKE         DEFINE    De5OP1        WOP1
077100170914     C     *LIKE         DEFINE    LEGdde        depdde
077200170915     C     *LIKE         DEFINE    LEGdpc        depdpc
077300170915     C     *LIKE         DEFINE    vidbun        depbun
077400170918     C     *LIKE         DEFINE    viddpt        depdpt
077500170914     C     *LIKE         DEFINE    LEGdde        ddde
077600170914     C     *LIKE         DEFINE    LEGdpc        ddpc
077700170915     C     *LIKE         DEFINE    LEGbun        dbun
077800060901     c*
077900060901     C     *LIKE         DEFINE    vidsel        depsel
078000970526     C*---
078100970526     C* VARIABILI DI CONTROLLO
078200970526     C*---
078300970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
078400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
078500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
078600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
078700970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
078800060919     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
078900970526     C*
079000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
079100170919     C                   MOVEL     'TISIE5R'     DSMSPG
079200170919     C                   MOVEL     'TISIE5R'     MSGQUE
079300970526     C                   MOVEL     *BLANKS       MSGKEY
079400970526     C                   MOVEL     *BLANKS       DSMSMS
079500970526     C                   MOVEL     *BLANKS       DSMSKE
079600970526     C                   Z-ADD     *ZEROS        DSMSER
079700970526     C*---
079800970526     C* VARIABILI RIFERITE AL DATA BASE
079900970526     C*---
080000970526     C*---
080100970526     C* CHIAVI DI LETTURA
080200970526     C*---
080300060901     c* TABEL00F - Parziale
080400060831     C     KEYtabel_P    KLIST
080500060831     C                   KFLD                    tblKUT
080600060831     C                   KFLD                    tblCOD
080700170915     C* DPCDP11i - Totale
080800060901     C     KEYcdp        KLIST
080900170915     c                   kfld                    CDPATB
081000170918     C                   KFLD                    osid1ver
081100170915     C                   KFLD                    LEGdep7
081200970527     C*
081300970527     C* REPERIMENTO DATI UTENTE
081400020917     C                   EXSR      REPDATIUTE
081500970527     C*
081600970527     C* CARICA LE TABELLE OCCORRENTI
081700970527     C                   EXSR      CARTAB
081800970526     C*
081900150610     C*  se non ci sono linee DPD in gestione (Finestra) ed esce dal pgm
082000150611     c                   if        FIL_HA_DPD = *blank   and
082100150611     c                             %subst(knmus:1:3)<>'EDP'
082200150610     c                   exfmt     SIE5NODPD
082300150610     c                   seton                                        LR
082400150610     c                   return
082500150610     c                   end
082600150610     C*
082700150610     C*
082800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
082900060831     C                   MOVEL     DE5OP0        DEPOP0                         *LIVELLO PROCEDURA
083000060831     C                   MOVEL     DE5OP1        DEPOP1                         *LIVELLO PROGRAMMA
083100970526     C*
083200970526     C* CALCOLA LA DATA CORRENTE
083300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
083400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
083500970526     C                   Z-ADD     WN8           G08DAT
083600970526     C                   Z-ADD     *ZEROS        G08INV
083700970526     C                   MOVEL     '0'           G08ERR
083800970526     C                   CALL      'XSRDA8'
083900970526     C                   PARM                    WLBDA8
084000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
084100970526     C*
084200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
084300060901     c* data  decorrenza
084400060831     C                   Z-ADD     datcor        G08INV
084500970526     C                   Z-ADD     *ZEROS        G08DAT
084600970526     C                   MOVEL     '3'           G08ERR
084700970526     C                   CALL      'XSRDA8'
084800970526     C                   PARM                    WLBDA8
084900060907     C                   Z-ADD     G08DAT        VIDdde_p                       *DATA RIFERIMENTO
085000060901     c* S=Solo depot senza codice Piano dei Conti
085100061012     c                   if        de5sel= *blanks
085200151012     C                   MOVEL     'N'           VIDsel                         *NAZIONE
085300061012     c                   else
085400061012     c                   movel     de5sel        vidsel
085500061012     c                   endif
085600060901     c* inizio elenco per codice depot --> imposto a blank
085700170918     C                   clear                   VIDdpt
085800970526     C                   EXSR      CHKPAR
085900970526IF  1C     ERRL00        IFEQ      '1'
086000060831     C                   MOVEL     '1'           DE5ERR
086100060831     C                   MOVEL     DSMSMS        DE5MSG
086200970526X   1C                   ELSE
086300170918     C                   MOVEL     VIDdpt        DEPdpt                         *DEPOSITI
086400060907     C                   MOVEL     VIDdde_p      DEPdde
086500060901     C                   MOVEL     VIDsel        DEPsel
086600970526E   1C                   ENDIF
086700970526     C*
086800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
086900970526     C                   EXSR      CHKFOP
087000970530     C*
087100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
087200060901IF  1C                   if        %subst(depop0:2:2)='01'
087300970530     C                   MOVEL     DEC(3)        VIDING
087400970530E   1C                   ENDIF
087500060901IF  1C                   if        %subst(depop0:2:2)='02'
087600970530     C                   MOVEL     DEC(1)        VIDING
087700970530E   1C                   ENDIF
087800060901IF  1C                   if        %subst(depop0:2:2)='05'
087900970530     C                   MOVEL     DEC(2)        VIDING
088000970530E   1C                   ENDIF
088100970526     C*
088200970526     C                   ENDSR
088300060831     c
088400060831     C*
088500970526     C*------------------------------------------------------------------------*
088600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
088900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
089000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
089100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
089200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
089300970526     C*------------------------------------------------------------------------*
089400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
089500970526     C*------------------------------------------------------------------------*
089600970526     C     POSSFL        BEGSR
089700970526     C*
089800970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
089900970526     C                   EXSR      CLRSFL
090000970526     C*
090100970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
090200970526     C                   EXSR      SETFIL
090300970526     C*
090400970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
090500970526     C     SFLEOF        IFEQ      0
090600970526     C                   EXSR      CARSFL
090700970526     C                   END
090800970526     C*
090900970526     C                   ENDSR
091000970526     C*------------------------------------------------------------------------*
091100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
091200970526     C*------------------------------------------------------------------------*
091300970526     C     ENTATT        BEGSR
091400970526     C*
091500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
091600970526     C                   Z-ADD     0             OPZOK             1 0
091700970526     C*
091800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
091900970526     C                   Z-ADD     0             SFLVAR            5 0
092000970526     C*
092100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
092200970526     C                   Z-ADD     0             SFLERR
092300970526     C*
092400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
092500970526     C                   Z-ADD     SFLULT        WFLULT
092600970526     C*
092700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
092800060901     C                   READC     SIE5D05                                98
092900970526     C     *IN98         DOWEQ     '0'
093000970526     C*
093100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
093200970526     C                   ADD       1             SFLVAR
093300970526     C*
093400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
093500970526     C     OPZOK         IFEQ      0
093600970526     C*
093700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
093800970526     C     OPZKEY        IFNE      *BLANKS
093900970526     C                   EXSR      OPZCON
094000970526     C                   END
094100970526     C*
094200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
094300970526     C*            2. PULIZIA SCELTA IN SFL
094400970526     C     OPZOK         IFEQ      0
094500970526     C                   MOVEL     '0'           *IN41
094600970526     C                   MOVEL     *BLANKS       OPZKEY
094700970526     C                   ELSE
094800970526     C*
094900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
095000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
095100970526     C                   MOVEL     '1'           *IN41
095200970526     C                   Z-ADD     SFLULT        SFLERR
095300970526     C                   END
095400970526     C                   ELSE
095500970526     C*
095600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
095700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
095800970526     C                   MOVEL     '1'           *IN41
095900970526     C                   END                                                    OPZOK=0
096000970526     C*
096100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
096200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
096300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
096400970526     C     OPZOK         IFNE      0
096500970526     C                   Z-ADD     SFLERR        SFLNBR
096600970526     C                   ELSE
096700970526     C                   Z-ADD     SFLULT        SFLNBR
096800970526     C                   END
096900150904     c     hin44         comp      '1'                                    44
097000970526     C*
097100970526     C* AGGIORNO RECORD SFL
097200060901     C                   UPDATE    SIE5D05
097300970526     C*
097400060901     C                   READC     SIE5D05                                98
097500970526     C                   END                                                    DO WHILE
097600970526     C*
097700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
097800970526     C                   Z-ADD     WFLULT        SFLULT
097900970526     C*
098000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
098100970526     C     SFLVAR        IFEQ      0
098200970526     C*---                 MOVEL'1'       FINL00
098300970526     C                   END
098400970526     C*
098500970526     C                   ENDSR
098600970526     C*------------------------------------------------------------------------*
098700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
098800970526     C*------------------------------------------------------------------------*
098900970526     C     CLRSFL        BEGSR
099000970526     C*
099100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
099200970526     C* (0=NON FINE FILE  1=FINE FILE)
099300970526     C                   Z-ADD     0             SFLEOF
099400970526     C*
099500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
099600970526     C                   Z-ADD     0             SFLULT
099700970526     C*
099800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
099900970526     C                   MOVEL     '1'           *IN40
100000060831     C                   WRITE     SIE5D06
100100970526     C                   MOVEL     '0'           *IN40
100200970526     C*
100300970526     C                   ENDSR
100400970526     C*------------------------------------------------------------------------*
100500970526     C* CARSFL - CARICAMENTO SFL
100600970526     C*------------------------------------------------------------------------*
100700970526     C     CARSFL        BEGSR
100800970526     C*
100900150924     C* x reimpostare il cursore correttamente sulla riga toccata dopo
101000150924     C*    aver ricaricato
101100150924     c                   z-add     0             cursfl            5 0
101200150924     C*
101300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
101400970526     C     SFLULT        IFEQ      0
101500970526     C                   EXSR      SETFIL
101600150924     C*
101700150924      * per potersi riposizionare immediatamente dopo aver aggiornato
101800150924     c                   if        sflnbr > 0
101900150924     c                   z-add     sflnbr        cursfl
102000150924     c                   move      '0'           sflnbr
102100150924     c                   add       sflnbr        sflpag
102200150924     c                   end
102300970526     C                   END
102400970526     C*
102500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
102600970526     C                   Z-ADD     SFLULT        WFLULT
102700970526     C*
102800970526     C* CICLO:
102900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
103000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
103100970526     C                   Z-ADD     1             SFLRIG
103200970526     C     SFLRIG        DOWLE     SFLPAG
103300970526     C     SFLEOF        ANDEQ     0
103400970526     C*
103500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
103600970526     C                   MOVEL     *BLANKS       OPZKEY
103700970526     C*
103800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
103900970526     C                   EXSR      IRESFL
104000970526     C*
104100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
104200970526     C                   ADD       1             SFLULT
104300970526     C*
104400970526     C* SCRITTURA RECORD SFL
104500150904     C                   SETOFF                                       44
104600150904     c                   move      '0'           hin44
104700150904     C*
104800150904     c                   if        vidnar <> *blank  and
104900150904     c                             vidsta <> *blank  and
105000150904     c                             vidnar <> vidsta
105100150904     C                   SETON                                        44
105200150904     c                   move      '1'           hin44
105300150904     C                   end
105400150904     C*
105500060901     C                   WRITE     SIE5D05
105600970526     C                   ADD       1             SFLRIG
105700970526     C*
105800970526     C* LETTURA FILE DA CARICARE
105900970526     C                   EXSR      LETFIL
106000970526     C                   END                                                    DO WHILE
106100970526     C*
106200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
106300970526     C     WFLULT        IFNE      SFLULT
106400970526     C                   Z-ADD     WFLULT        SFLNBR
106500970526     C                   ADD       1             SFLNBR
106600970526     C                   END
106700150924     C*
106800150924      * per reimpostare il cursore sulla riga toccata per l'aggiornamento
106900150924     c                   if         cursfl >0
107000150924     c                   eval      sflnbr =  cursfl
107100150924     C                   end
107200970526     C*
107300970526     C                   ENDSR
107400970526     C*------------------------------------------------------------------------*
107500970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
107600970526     C*------------------------------------------------------------------------*
107700970526     C     OUTSFL        BEGSR
107800970526     C*
107900970526     C* WRITE RECORD TESTATA VIDEO
108000060901     C                   WRITE     SIE5D01
108100970526     C*
108200970526     C* VISUALIZZAZIONE MESSAGGIO
108300970526     C                   EXSR      SNDMSG
108400970526     C*
108500970526     C* WRITE RECORD OPZIONI VIDEO
108600060901     C                   WRITE     SIE5D02
108700970526     C*
108800970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
108900060901     C                   WRITE     SIE5D03
109000970526     C*
109100970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
109200060901     C                   WRITE     SIE5D04
109300970526     C*
109400970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
109500970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
109600970526     C     SFLEOF        IFEQ      0
109700970526     C                   MOVEL     '0'           *IN42
109800970526     C                   ELSE
109900970526     C*
110000970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
110100970526     C                   MOVEL     '1'           *IN42
110200970526     C                   END
110300970526     C*
110400970526     C* WRITE RECORD TASTI FUNZIONALI
110500060901     C                   WRITE     SIE5D09
110600970526     C*
110700970526     C* CASO DI SFL VUOTO
110800970526     C     SFLULT        IFLE      0
110900970526     C*
111000970526     C*    - WRITE RECORD VUOTO
111100060901     C                   WRITE     SIE5D0A
111200060901     C                   READ      SIE5D0A                                99
111300970526     C*
111400970526     C                   ELSE
111500970526     C*
111600970526     C* CASO DI SFL CON RECORDS
111700970526     C*
111800970526     C*    - WRITE RECORD CONTROLLO SFL
111900150924     C                   exfmt     SIE5D06
112000150924     C***********        READ      SIE5D06                                99
112100970526     C                   END
112200970526     C*
112300970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
112400060901     C                   READ      SIE5D03                                99
112500970526     C*
112600970526     C                   ENDSR
112700970526     C*------------------------------------------------------------------------*
112800970526     C* SNDMSG - MANDA MESSAGGIO
112900970526     C*------------------------------------------------------------------------*
113000970526     C     SNDMSG        BEGSR
113100970526     C*
113200970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
113300970526     C                   MOVEL     '1'           *IN40
113400060901     C                   WRITE     SIE5DMC
113500970526     C                   MOVEL     '0'           *IN40
113600970526     C*
113700970526     C* VISUALIZZAZIONE MESSAGGIO
113800970526     C                   Z-ADD     0             MSGULT
113900970526     C                   CALL      'XMSG'        XMSGPA
114000970526     C     DSMSER        IFEQ      0
114100970526     C                   MOVEL     DSMSKE        MSGKEY
114200970526     C                   END
114300970526     C                   Z-ADD     1             MSGULT
114400060901     C                   WRITE     SIE5DMS                                      SFL MESS
114500060901     C                   WRITE     SIE5DMC                                      CONTR MESS
114600970526     C*
114700970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
114800970526     C                   MOVEL     *BLANKS       DSMSMS
114900970526     C*
115000970526     C                   ENDSR
115100970526     C*------------------------------------------------------------------------*
115200970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
115300970526     C*------------------------------------------------------------------------*
115400970526     C     FUNCON        BEGSR
115500970526     C*
115600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
115700970526     C                   EXSR      FUNVAR
115800970526     C*
115900970526     C* PULISCO CAMPI DI LAVORO
116000970526     C                   Z-ADD     0             I                 5 0
116100970526     C                   Z-ADD     0             W                 5 0
116200970526     C*
116300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
116400970526     C     FUNKEY        IFNE      *BLANKS
116500970526     C     1             DO        24            I
116600970526     C                   MOVEL     FUD(I)        FUDRIG
116700970526     C*
116800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
116900970526     C     FUNKEY        IFEQ      FUDFUN
117000970526     C     FUDATT        ANDEQ     'S'
117100970526     C                   Z-ADD     I             W
117200970526     C                   END
117300970526     C                   END                                                    DO WHILE
117400970526     C*
117500970526     C* FUNZIONE NON ATTIVA (ERRORE)
117600970526     C     W             IFEQ      0
117700970526     C*
117800970526     C* EMISSIONE MESSAGGIO DI ERRORE
117900970526     C                   MOVEL     MSG(2)        DSMSMS
118000970526     C                   END                                                    W=0
118100970526     C                   END                                                    FUNKEY NE *BLANKS
118200970526     C*
118300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
118400970526     C     W             IFNE      0
118500970526     C     FUNKEY        OREQ      *BLANKS
118600970526     C*
118700970526     C     FUNKEY        IFEQ      *BLANKS
118800970526     C                   EXSR      ENTGES
118900970526     C                   END
119000970526     C     FUNKEY        IFEQ      '03'
119100970526     C                   EXSR      F03GES
119200970526     C                   END
119300970526     C     FUNKEY        IFEQ      '05'
119400970526     C                   EXSR      F05GES
119500970526     C                   END
119600970526     C     FUNKEY        IFEQ      '06'
119700970526     C                   EXSR      F06GES
119800970526     C                   END
119900970526     C     FUNKEY        IFEQ      '12'
120000970526     C                   EXSR      F12GES
120100970526     C                   END
120200970526     C     FUNKEY        IFEQ      '21'
120300970526     C                   EXSR      F21GES
120400970526     C                   END
120500970526     C     FUNKEY        IFEQ      '22'
120600970526     C                   EXSR      F22GES
120700970526     C                   END
120800970526     C     FUNKEY        IFEQ      '23'
120900970526     C                   EXSR      F23GES
121000970526     C                   END
121100970526     C     FUNKEY        IFEQ      '24'
121200970526     C                   EXSR      F24GES
121300970526     C                   END
121400970526     C     FUNKEY        IFEQ      'RU'
121500970526     C                   EXSR      RUPGES
121600970526     C                   END
121700970526     C*
121800970526     C                   END
121900970526     C*
122000970526     C                   ENDSR
122100970526     C*------------------------------------------------------------------------*
122200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
122300970526     C*------------------------------------------------------------------------*
122400970526     C     FUNVAR        BEGSR
122500970526     C*
122600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
122700970526     C     KEY           IFEQ      ENTER
122800970526     C                   MOVEL     *BLANKS       FUNKEY
122900970526     C                   END
123000970526     C     KEY           IFEQ      F1
123100970526     C                   MOVEL     '01'          FUNKEY
123200970526     C                   END
123300970526     C     KEY           IFEQ      F2
123400970526     C                   MOVEL     '02'          FUNKEY
123500970526     C                   END
123600970526     C     KEY           IFEQ      F3
123700970526     C                   MOVEL     '03'          FUNKEY
123800970526     C                   END
123900970526     C     KEY           IFEQ      F4
124000970526     C                   MOVEL     '04'          FUNKEY
124100970526     C                   END
124200970526     C     KEY           IFEQ      F5
124300970526     C                   MOVEL     '05'          FUNKEY
124400970526     C                   END
124500970526     C     KEY           IFEQ      F6
124600970526     C                   MOVEL     '06'          FUNKEY
124700970526     C                   END
124800970526     C     KEY           IFEQ      F7
124900970526     C                   MOVEL     '07'          FUNKEY
125000970526     C                   END
125100970526     C     KEY           IFEQ      F8
125200970526     C                   MOVEL     '08'          FUNKEY
125300970526     C                   END
125400970526     C     KEY           IFEQ      F9
125500970526     C                   MOVEL     '09'          FUNKEY
125600970526     C                   END
125700970526     C     KEY           IFEQ      F10
125800970526     C                   MOVEL     '10'          FUNKEY
125900970526     C                   END
126000970526     C     KEY           IFEQ      F11
126100970526     C                   MOVEL     '11'          FUNKEY
126200970526     C                   END
126300970526     C     KEY           IFEQ      F12
126400970526     C                   MOVEL     '12'          FUNKEY
126500970526     C                   END
126600970526     C     KEY           IFEQ      F13
126700970526     C                   MOVEL     '13'          FUNKEY
126800970526     C                   END
126900970526     C     KEY           IFEQ      F14
127000970526     C                   MOVEL     '14'          FUNKEY
127100970526     C                   END
127200970526     C     KEY           IFEQ      F15
127300970526     C                   MOVEL     '15'          FUNKEY
127400970526     C                   END
127500970526     C     KEY           IFEQ      F16
127600970526     C                   MOVEL     '16'          FUNKEY
127700970526     C                   END
127800970526     C     KEY           IFEQ      F17
127900970526     C                   MOVEL     '17'          FUNKEY
128000970526     C                   END
128100970526     C     KEY           IFEQ      F18
128200970526     C                   MOVEL     '18'          FUNKEY
128300970526     C                   END
128400970526     C     KEY           IFEQ      F19
128500970526     C                   MOVEL     '19'          FUNKEY
128600970526     C                   END
128700970526     C     KEY           IFEQ      F20
128800970526     C                   MOVEL     '20'          FUNKEY
128900970526     C                   END
129000970526     C     KEY           IFEQ      F21
129100970526     C                   MOVEL     '21'          FUNKEY
129200970526     C                   END
129300970526     C     KEY           IFEQ      F22
129400970526     C                   MOVEL     '22'          FUNKEY
129500970526     C                   END
129600970526     C     KEY           IFEQ      F23
129700970526     C                   MOVEL     '23'          FUNKEY
129800970526     C                   END
129900970526     C     KEY           IFEQ      F24
130000970526     C                   MOVEL     '24'          FUNKEY
130100970526     C                   END
130200970526     C     KEY           IFEQ      ROLLUP
130300970526     C                   MOVEL     'RU'          FUNKEY
130400970526     C                   END
130500970526     C     KEY           IFEQ      HELP
130600970526     C                   MOVEL     'HP'          FUNKEY
130700970526     C                   END
130800970526     C*
130900970526     C                   ENDSR
131000970526     C*------------------------------------------------------------------------*
131100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
131200970526     C*------------------------------------------------------------------------*
131300970526     C     FUNGES        BEGSR
131400970526     C*
131500970526     C* PULISCO CAMPI DI LAVORO
131600970526     C                   Z-ADD     0             W                 5 0
131700970526     C*
131800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
131900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
132000970526     C     1             DO        24            I
132100970526     C                   MOVEL     FUD(I)        FUDRIG
132200970526     C*
132300970526     C* PULIZIA FUNZIONI PRECEDENTI
132400970526     C                   MOVEL     *BLANKS       FUV(I)
132500970526     C*
132600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
132700970526     C* (SALTA F23 E F24)
132800970526     C     FUDATT        IFEQ      'S'
132900970526     C     FUDFUN        ANDNE     '23'
133000970526     C     FUDFUN        ANDNE     '24'
133100970526     C                   ADD       1             W
133200970526     C                   MOVEL     FUDDES        FUV(W)
133300970526     C                   END
133400970526     C                   END                                                    DO WHILE
133500970526     C*
133600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
133700970526     C* ALTRIMENTI DISATTIVO F24
133800970526     C     1             DO        24            I
133900970526     C                   MOVEL     FUD(I)        FUDRIG
134000970526     C*
134100970526     C     FUDFUN        IFEQ      '24'
134200970526     C     W             IFGT      6
134300970526     C                   MOVEL     'S'           FUDATT
134400970526     C                   ELSE
134500970526     C                   MOVEL     *BLANKS       FUDATT
134600970526     C                   END                                                    W GT 8
134700970526     C                   MOVEL     FUDRIG        FUD(I)
134800970526     C                   END
134900970526     C                   END                                                    DO WHILE
135000970526     C*
135100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
135200970526     C     W             DIV       6             I
135300970526     C                   MVR                     Z
135400970526     C*
135500970526     C     Z             IFNE      0
135600970526     C     I             ADD       1             Z
135700970526     C                   ELSE
135800970526     C                   Z-ADD     I             Z
135900970526     C                   END
136000970526     C*
136100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
136200970526     C                   ADD       1             FUNGRU
136300970526     C     FUNGRU        IFGT      Z
136400970526     C     FUNGRU        ORLT      1
136500970526     C                   Z-ADD     1             FUNGRU
136600970526     C                   END
136700970526     C*
136800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
136900970526     C     FUNGRU        IFEQ      1
137000970526     C                   MOVEL     FUV(1)        FUN001
137100970526     C                   MOVEL     FUV(2)        FUN002
137200970526     C                   MOVEL     FUV(3)        FUN003
137300970526     C                   MOVEL     FUV(4)        FUN004
137400970526     C                   MOVEL     FUV(5)        FUN005
137500970526     C                   MOVEL     FUV(6)        FUN006
137600970526     C                   END
137700970526     C*
137800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
137900970526     C     FUNGRU        IFEQ      2
138000970526     C                   MOVEL     FUV(7)        FUN001
138100970526     C                   MOVEL     FUV(8)        FUN002
138200970526     C                   MOVEL     FUV(9)        FUN003
138300970526     C                   MOVEL     FUV(10)       FUN004
138400970526     C                   MOVEL     FUV(11)       FUN005
138500970526     C                   MOVEL     FUV(12)       FUN006
138600970526     C                   END
138700970526     C*
138800970526     C* TERZO GRUPPO DI 6 FUNZIONI
138900970526     C     FUNGRU        IFEQ      3
139000970526     C                   MOVEL     FUV(13)       FUN001
139100970526     C                   MOVEL     FUV(14)       FUN002
139200970526     C                   MOVEL     FUV(15)       FUN003
139300970526     C                   MOVEL     FUV(16)       FUN004
139400970526     C                   MOVEL     FUV(17)       FUN005
139500970526     C                   MOVEL     FUV(18)       FUN006
139600970526     C                   END
139700970526     C*
139800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
139900970526     C     FUNGRU        IFEQ      4
140000970526     C                   MOVEL     FUV(19)       FUN001
140100970526     C                   MOVEL     FUV(20)       FUN002
140200970526     C                   MOVEL     FUV(21)       FUN003
140300970526     C                   MOVEL     FUV(22)       FUN004
140400970526     C                   MOVEL     *BLANKS       FUN005
140500970526     C                   MOVEL     *BLANKS       FUN006
140600970526     C                   END
140700970526     C*
140800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
140900970526     C                   MOVEL     *BLANKS       FUN007
141000970526     C                   MOVEL     *BLANKS       FUN008
141100970526     C*
141200970526     C     1             DO        24            I
141300970526     C                   MOVEL     FUD(I)        FUDRIG
141400970526     C     FUDFUN        IFEQ      '23'
141500970526     C     FUDATT        ANDEQ     'S'
141600970526     C                   MOVEL     FUDDES        FUN007
141700970526     C                   END
141800970526     C     FUDFUN        IFEQ      '24'
141900970526     C     FUDATT        ANDEQ     'S'
142000970526     C                   MOVEL     FUDDES        FUN008
142100970526     C                   END
142200970526     C                   END                                                    DO
142300970526     C*
142400970526     C                   ENDSR
142500970526     C*------------------------------------------------------------------------*
142600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
142700970526     C*------------------------------------------------------------------------*
142800970526     C     OPZGES        BEGSR
142900970526     C*
143000970526     C* PULISCO CAMPI DI LAVORO
143100970526     C                   Z-ADD     0             W                 5 0
143200970526     C*
143300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
143400970526     C* E CARICO TABELLA OPZIONI ATTIVE
143500970526     C     1             DO        24            I
143600970526     C                   MOVEL     OPD(I)        OPDRIG
143700970526     C*
143800970526     C* PULIZIA OPZIONI PRECEDENTI
143900970526     C                   MOVEL     *BLANKS       OPV(I)
144000970526     C*
144100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
144200970526     C     OPDATT        IFEQ      'S'
144300970526     C                   ADD       1             W
144400970526     C                   MOVEL     OPDDES        OPV(W)
144500970526     C                   END
144600970526     C                   END                                                    DO WHILE
144700970526     C*
144800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
144900970526     C* ALTRIMENTI DISATTIVO F23
145000970526     C     1             DO        24            I
145100970526     C                   MOVEL     FUD(I)        FUDRIG
145200970526     C*
145300970526     C     FUDFUN        IFEQ      '23'
145400970526     C     W             IFGT      8
145500970526     C                   MOVEL     'S'           FUDATT
145600970526     C                   ELSE
145700970526     C                   MOVEL     *BLANKS       FUDATT
145800970526     C                   END                                                    W GT 8
145900970526     C                   MOVEL     FUDRIG        FUD(I)
146000970526     C                   END
146100970526     C                   END                                                    DO WHILE
146200970526     C*
146300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
146400970526     C     W             DIV       8             I
146500970526     C                   MVR                     Z                 5 0
146600970526     C*
146700970526     C     Z             IFNE      0
146800970526     C     I             ADD       1             Z
146900970526     C                   ELSE
147000970526     C                   Z-ADD     I             Z
147100970526     C                   END
147200970526     C*
147300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
147400970526     C                   ADD       1             OPZGRU
147500970526     C     OPZGRU        IFGT      Z
147600970526     C     OPZGRU        ORLT      1
147700970526     C                   Z-ADD     1             OPZGRU
147800970526     C                   END
147900970526     C*
148000970526     C* PRIMO GRUPPO DI 8 OPZIONI
148100970526     C     OPZGRU        IFEQ      1
148200970526     C                   MOVEL     OPV(1)        OPZ001
148300970526     C                   MOVEL     OPV(2)        OPZ002
148400970526     C                   MOVEL     OPV(3)        OPZ003
148500970526     C                   MOVEL     OPV(4)        OPZ004
148600970526     C                   MOVEL     OPV(5)        OPZ005
148700970526     C                   MOVEL     OPV(6)        OPZ006
148800970526     C                   MOVEL     OPV(7)        OPZ007
148900970526     C                   MOVEL     OPV(8)        OPZ008
149000970526     C                   END
149100970526     C*
149200970526     C* SECONDO GRUPPO DI 8 OPZIONI
149300970526     C     OPZGRU        IFEQ      2
149400970526     C                   MOVEL     OPV(9)        OPZ001
149500970526     C                   MOVEL     OPV(10)       OPZ002
149600970526     C                   MOVEL     OPV(11)       OPZ003
149700970526     C                   MOVEL     OPV(12)       OPZ004
149800970526     C                   MOVEL     OPV(13)       OPZ005
149900970526     C                   MOVEL     OPV(14)       OPZ006
150000970526     C                   MOVEL     OPV(15)       OPZ007
150100970526     C                   MOVEL     OPV(16)       OPZ008
150200970526     C                   END
150300970526     C*
150400970526     C* TERZO GRUPPO DI 8 OPZIONI
150500970526     C     OPZGRU        IFEQ      3
150600970526     C                   MOVEL     OPV(17)       OPZ001
150700970526     C                   MOVEL     OPV(18)       OPZ002
150800970526     C                   MOVEL     OPV(19)       OPZ003
150900970526     C                   MOVEL     OPV(20)       OPZ004
151000970526     C                   MOVEL     OPV(21)       OPZ005
151100970526     C                   MOVEL     OPV(22)       OPZ006
151200970526     C                   MOVEL     OPV(23)       OPZ007
151300970526     C                   MOVEL     OPV(24)       OPZ008
151400970526     C                   END
151500970526     C*
151600970526     C                   ENDSR
151700970526     C*------------------------------------------------------------------------*
151800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
151900970526     C*------------------------------------------------------------------------*
152000970526     C     OPZCON        BEGSR
152100970526     C*
152200970526     C* PULISCO CAMPI DI LAVORO
152300970526     C                   Z-ADD     0             I                 5 0
152400970526     C                   Z-ADD     0             W                 5 0
152500970526     C*
152600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
152700970526     C     1             DO        24            I
152800970526     C                   MOVEL     OPD(I)        OPDRIG
152900970526     C*
153000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
153100970526     C     OPZKEY        IFEQ      OPDOPZ
153200970526     C     OPZKEY        OREQ      OPDOP1
153300970526     C*
153400970526     C     OPDATT        IFEQ      'S'
153500970526     C                   MOVEL     OPDOPZ        OPZKEY
153600970526     C                   Z-ADD     I             W
153700970526     C                   END
153800970526     C                   END
153900970526     C*
154000970526     C                   END                                                    DO WHILE
154100970526     C*
154200970526     C* OPZIONE NON ATTIVA (ERRORE)
154300970526     C     W             IFEQ      0
154400970526     C*
154500970526     C* OPZIONE NON OK
154600970526     C                   Z-ADD     1             OPZOK
154700970526     C*
154800970526     C* EMISSIONE MESSAGGIO DI ERRORE
154900970526     C                   MOVEL     MSG(1)        DSMSMS
155000970526     C                   ELSE
155100970526     C*
155200970526     C* OPZIONE ATTIVA (PROCEDI ...)
155300970526     C*
155400970526     C     OPZKEY        IFEQ      ' 1'
155500970526     C                   EXSR      O01GES
155600970526     C                   END
155700970526     C     OPZKEY        IFEQ      ' 2'
155800970526     C                   EXSR      O02GES
155900970526     C                   END
156000970526     C     OPZKEY        IFEQ      ' 4'
156100970526     C                   EXSR      O04GES
156200970526     C                   END
156300970526     C     OPZKEY        IFEQ      ' 5'
156400970526     C                   EXSR      O05GES
156500970526     C                   END
156600970526     C     OPZKEY        IFEQ      ' 7'
156700970526     C                   EXSR      O07GES
156800970526     C                   END
156900970526     C*
157000970526     C                   END                                                    OPZ.NON ATTIVA
157100970526     C*
157200970526     C                   ENDSR
157300970526     C*------------------------------------------------------------------------*
157400970526     C* NOTE COSTRUZIONE SCHIERE
157500970526     C*------------------------------------------------------------------------*
157600970526     C*
157700970526     C* -MSG-MESSAGGI
157800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
157900970526     C* (NON TOCCARE MSG,1 E MSG,2)
158000970526     C*
158100970526     C* -CMD-COMANDI
158200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
158300970526     C*
158400970526     C* -OPD-OPZIONI
158500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
158600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
158700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
158800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
158900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
159000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
159100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
159200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
159300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
159400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
159500970526     C*
159600970526     C* -FUD-FUNZIONI
159700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
159800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
159900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
160000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
160100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
160200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
160300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
160400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
160500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
160600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
160700970526     O*------------------------------------------------------------------------*
160800970526** DEC - DECODIFICHE
160900970526    GESTIONE                                                     1
161000970526INTERROGAZIONE                                                   2
161100970530     SCELTA                                                      3
1612009705264
1613009705265
161400970526** MSG - MESSAGGI
161500060804TISIE5R- Opzione non gestita                                                   1
161600060804TISIE5R- Tasto funzionale non gestito                                          2
161700060804TISIE5R- 3
161800060804TISIE5R- Record annullato. Consentito solo il ripristino                       4
161900060804TISIE5R- Record non annullato. Impossibile ripristinare                        5
162000060804TISIE5R- Accettato solo un parametro di inizio lista                           6
162100060804TISIE5R- Errori nei parametri di ingresso                                      7
162200060804TISIE5R- Data errata                                                           8
162300060804TISIE5R- 9                                                                     9
162400060804TISIE5R- 10                                                                   10
162500060804TISIE5R- 11                                                                   11
162600060901TISIE5R- Data errata                                                          12
162700060804TISIE5R- 13                                                                   13
162800060804TISIE5R- 14                                                                   14
162900060804TISIE5R- 15                                                                   15
163000060804TISIE5R- 16                                                                   16
163100060804TISIE5R- 17                                                                   17
163200060804TISIE5R- 18                                                                   18
163300060804TISIE5R- 19                                                                   19
163400060804TISIE5R- 20                                                                   20
163500970526** CMD - COMANDI CL
163600970526WRKSPLF
163700970526
163800970526
163900970526
164000970526
164100970526** OPD - OPZIONI
164200970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
164300970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
164400000120                   -3 -     (S/N)=N                        opz. 03¬
164500970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
164600970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
164700970526                   -6 -     (S/N)=N                        opz. 06¬
164800970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
164900000119                   -8 -     (S/N)=N                        opz. 08¬
165000000119                   -9 -     (S/N)=N                        opz. 09¬
165100000119                   -10-     (S/N)=N                        opz. 10¬
165200970526                   -  -     (S/N)=N                        opz. 11¬
165300970526                   -  -     (S/N)=N                        opz. 12¬
165400970526                   -  -     (S/N)=N                        opz. 13¬
165500970526                   -  -     (S/N)=N                        opz. 14¬
165600970526                   -  -     (S/N)=N                        opz. 15¬
165700970526                   -  -     (S/N)=N                        opz. 16¬
165800970526                   -  -     (S/N)=N                        opz. 17¬
165900970526                   -  -     (S/N)=N                        opz. 18¬
166000970526                   -  -     (S/N)=N                        opz. 19¬
166100970526                   -  -     (S/N)=N                        opz. 20¬
166200970526                   -  -     (S/N)=N                        opz. 21¬
166300970526                   -  -     (S/N)=N                        opz. 22¬
166400970526                   -  -     (S/N)=N                        opz. 23¬
166500970526                   -  -     (S/N)=N                        opz. 24¬
166600970526** FUD - FUNZIONI
166700970526                   -        (S/N)=N                        funz.01¬
166800970526                   -        (S/N)=N                        funz.02¬
166900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
167000970526                   -        (S/N)=N                        funz.04¬
167100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
167200970526F06=Inserimento    -        (S/N)=N                        funz.06¬
167300061012                   -        (S/N)=N                        funz.07¬
167400000119                   -        (S/N)=N                        funz.08¬
167500060919F09=Altri Dati     -        (S/N)=S                        funz.09¬
167600000119                   -        (S/N)=N                        funz.10¬
167700970526                   -        (S/N)=N                        funz.11¬
167800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
167900970526                   -        (S/N)=N                        funz.13¬
168000970526                   -        (S/N)=N                        funz.14¬
168100970526                   -        (S/N)=N                        funz.15¬
168200970526                   -        (S/N)=N                        funz.16¬
168300970526                   -        (S/N)=N                        funz.17¬
168400970526                   -        (S/N)=N                        funz.18¬
168500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
168600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
168700060804F21=Stampa elenco  -        (S/N)=S                        funz.21¬
168800970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
168900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
169000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
