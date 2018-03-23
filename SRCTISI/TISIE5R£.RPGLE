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
001500171113      ** ------------------------------------------------------------------ */
001600171113     D prmCPD10F     e DS                  extname(DPCDP10F)
001700171113     d                                     prefix(CDP_)
001800171113     D TisieDEPds    E DS
001900970526     D*------------------------------------------------------------------------*
002000970526     D* SCHIERE
002100970526     D*------------------------------------------------------------------------*
002200970526     D*------------
002300970526     D* DECODIFICHE
002400970526     D*------------
002500970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* MESSAGGI
002800970526     D*------------
002900970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* COMANDI
003200970526     D*------------
003300970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* DESCRIZIONI OPZIONI
003600970526     D*------------
003700970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003800970526     D*------------
003900970526     D* DESCRIZIONI TASTI FUNZIONALI
004000970526     D*------------
004100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004200970526     D*------------
004300970526     D* OPZIONI DA CARICARE A VIDEO
004400970526     D*------------
004500970526     D OPV             S             19    DIM(24)
004600970526     D*------------
004700970526     D* FUNZIONI DA CARICARE A VIDEO
004800970526     D*------------
004900970526     D FUV             S             19    DIM(24)
005000970526     D*------------------------------------------------------------------------*
005100970526     D* INPUT
005200970526     D*------------------------------------------------------------------------*
005300170918     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
005400970526     D*------------
005500970526     D* RIDEFINIZIONE OPZIONI VIDEO
005600970526     D*------------
005700970526     D OPDDS           DS
005800970526     D  OPDOPZ                 1      2
005900970526     D  OPDDE1                 3     19
006000970526     D  OPDOP1                21     22
006100970526     D  OPDFI0                23     34
006200970526     D  OPDATT                35     35
006300970526     D  OPDFI1                36     80
006400970526     D  OPDDES                 1     19
006500970526     D  OPDRIG                 1     80
006600970526     D*------------
006700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006800970526     D*------------
006900970526     D FUDDS           DS
007000970526     D  FUDDE1                 1      1
007100970526     D  FUDFUN                 2      3
007200970526     D  FUDDE2                 4     19
007300970526     D  FUDFI0                20     34
007400970526     D  FUDATT                35     35
007500970526     D  FUDFI1                36     80
007600970526     D  FUDDES                 1     19
007700970526     D  FUDRIG                 1     80
007800970526     D*------------
007900970526     D* DS GESTIONE TASTI FUNZIONALI
008000970526     D*------------
008100970526     D DSKEY         E DS
008200970526     D DEVDS1          DS
008300970526     D  KEY                  369    369
008400970526     D*------------
008500970526     D* DS "XMSG" - GESTIONE MESSAGGI
008600970526     D*------------
008700970526     D DSMSG         E DS
008800970526     D*------------
008900020917     D* DS REPERIMENTO DATI UTENTE
009000970526     D*------------
009100020917     D TIBS34DS      E DS                                                       *Profili utente
009200020917     D DDATIUTE      E DS                                                       *Dati utente
009300020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009400970526     D*------------------
009500970526     D* DS "XSRDAT" - CONTROLLA DATA (6)
009600970526     D*------------------
009700970526     D WLBDAT          DS                  INZ
009800970526     D  G06DAT                 1      6  0
009900970526     D  G06INV                 7     12  0
010000970526     D  G06ERR                13     13
010100970526     D  G06TGI                14     18  0
010200970526     D*------------------
010300970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
010400970526     D*------------------
010500970526     D WLBDA8          DS                  INZ
010600970526     D  G08DAT                 1      8  0
010700970526     D  G08INV                 9     16  0
010800970526     D  G08ERR                17     17
010900970526     D  G08TGI                18     22  0
011000970526     D*------------
011100970526     D* DS DI PROCEDURA
011200970526     D*------------
011300170919     D tisie5ds      E DS
011400060919      * - Ds per Anagrafiche Clienti
011500060919     d TIBS69DS      E DS
011600060919     d DS_cnaco      E DS                  extname(CNACO00F)
011700060919     d DS_cnind      E DS                  extname(CNIND00F)
011800060919     d DS_cnclp      E DS                  extname(CNCLP00F)
011900060919     d DS_fncls      E DS                  extname(FNCLS00F)
012000060919
012100970526     D*------------
012200970526     D* ARCHITETTURA
012300970526     D*------------
012400970526     D KPJBA         E DS
012500170918     D  savkpjbu       s                   like(kpjbu)
012600060831     D*------------
012700060831     d
012800060831     d og143         E DS
012900060901     d ds15          E ds
013000060831     d trul06ds      E DS
013100060831     D  LIN                    1     90  0 dim(30)
013200060831     D*-------------------
013300060831     D* VARIABILI DI WRK
013400060831     D*-------------------
013500060901     D ldp             S              3S 0 DIM(20)
013600060831     D skNaz           S              3    DIM(1000)
013700970526     C*------------------------------------------------------------------------*
013800970526     C* MAIN LINES
013900970526     C*------------------------------------------------------------------------*
014000970526     C*
014100970526     C* SE NON CI SONO ERRORI, PROSEGUE
014200970526IF  1C     ERRL00        IFNE      '1'
014300970526     C*
014400970526     C* CARICA IL SFILE
014500970526     C                   EXSR      POSSFL
014600970526     C*
014700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015100970526     C*
015200970526     C* REIMPOSTA ERRORE DI PROCERURA
015300970526     C                   MOVEL     '0'           ERRL00
015400970526     C*
015500970526     C* EMETTE IL SFILE
015600970526     C                   EXSR      OUTSFL
015700970526     C*
015800970526     C* GESTIONE TASTI FUNZIONALI
015900970526     C                   EXSR      FUNCON
016000970526     C*
016100970526E   2C                   ENDDO
016200970527     C*
016300970527     C* CI SONO ERRORI IN ENTRATA
016400970527X   1C                   ELSE
016500060831     C                   MOVEL     '1'           DE5ERR
016600060831     C                   MOVEL     DSMSMS        DE5MSG
016700970526E   1C                   ENDIF
016800970526     C* FINE PGM
016900060919     C                   eval      I69TLA  = 'C'
017000060919     C                   call      'TIBS69R'
017100060919     C                   parm                    TIBS69DS
017200970526     C                   SETON                                        LR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000060831     C                   MOVEL     '1'           DE5F03                         *DI PROCEDURA
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F05GES        BEGSR
018700970526     C*
018800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200000119     C* F06GES - GESTIONE F06 -> INSERIMENTO
019300000119     C*------------------------------------------------------------------------*
019400970526     C     F06GES        BEGSR
019500970526     C*
019600970526     C*
019700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019800970529     C                   MOVEL     DEPOP0        WOP0
019900970529     C                   MOVEL     'F06'         WOP1
020000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020100970526E   1C                   ENDIF
020200000119     C*
020300000119     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F12GES - GESTIONE F12 -> RITORNO
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F12GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F12"
021000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100060831     C                   MOVEL     '1'           DE5F12                         *DI PROCEDURA
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F21GES        BEGSR
021800970612     C*
021900060919IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022000060919     C                   MOVEL     DEPOP0        WOP0
022100060920     C                   MOVEL     'F21'         WOP1
022200170919     C                   MOVEL     'TISIE5R2'    WPGM             10
022300060920     C                   EXSR      LANPGM                                       *LANCIA PGM
022400060919E   1C                   ENDIF
022500970526     C*
022600970526     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800970526     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022900970526     C*------------------------------------------------------------------------*
023000970526     C     F22GES        BEGSR
023100970526     C*
023200970526     C                   Z-ADD     80            LENGH            15 5
023300970526     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
023400970526     C                   PARM                    CMD(1)
023500970526     C                   PARM                    LENGH
023600970526     C*
023700970526     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
024000970526     C*------------------------------------------------------------------------*
024100970526     C     F23GES        BEGSR
024200970526     C*
024300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024800970526     C*------------------------------------------------------------------------*
024900970526     C     F24GES        BEGSR
025000970526     C*
025100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025600970526     C*------------------------------------------------------------------------*
025700970526     C     RUPGES        BEGSR
025800970526     C*
025900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
026000970526     C*
026100970526     C                   ENDSR
026200970526     C*------------------------------------------------------------------------*
026300970526     C* ENTGES - GESTIONE ENTER -> INVIO
026400970526     C*------------------------------------------------------------------------*
026500970526     C     ENTGES        BEGSR
026600970526     C*
026700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026800970526     C                   EXSR      NEWPOS
026900970526     C*
027000970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
027100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027200970526IF  2C     SWPOSI        IFNE      0
027300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027400970526X   2C                   ELSE
027500970526IF  3C     SFLULT        IFNE      0
027600970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027700970526     C*
027800970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027900970526IF  4C     SWMODI        IFNE      0
028000970526     C                   EXSR      POSSFL
028100970526E   4C                   ENDIF
028200970526E   3C                   ENDIF
028300970526E   2C                   ENDIF
028400970526E   1C                   ENDIF
028500970526     C*
028600970526     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028900970526     C*------------------------------------------------------------------------*
029000970526     C     O01GES        BEGSR
029100970526     C*
029200970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
029300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029400970526     C*
029500970526     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029800970526     C*------------------------------------------------------------------------*
029900970526     C     O02GES        BEGSR
030000970526     C*
030100970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
030200060901IF  1C***  VIDATB        IFEQ      'A'
030300060901     C***                MOVEL     '1'           ERRL00
030400060901     C***                MOVEL     MSG(4)        DSMSMS
030500060901E   1C***                ENDIF
030600970526     C* MODIFICA
030700970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800060901     C                   MOVEL     VIDdpc        Ddpc
030900170915     C                   MOVEL     VIDbun        Dbun
031000060907     C                   z-add     VIhdde        Ddde
031100970529     C                   MOVEL     DEPOP0        WOP0
031200970526     C                   MOVEL     'O02'         WOP1
031300170919     C                   MOVEL     'TISIE5R1'    WPGM
031400970526     C                   EXSR      LANPGM
031500060919     C                   Z-ADD     1             SWMODI                     L
031600970526E   1C                   ENDIF
031700970526     C*
031800970526     C                   ENDSR
031900970526     C*------------------------------------------------------------------------*
032000970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
032100970526     C*------------------------------------------------------------------------*
032200970526     C     O04GES        BEGSR
032300970526     C*
032400970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032500060901IF  1C**   VIDATB        IFEQ      'A'
032600060901     C***                MOVEL     '1'           ERRL00
032700060901     C***                MOVEL     MSG(4)        DSMSMS
032800060901E   1C***                ENDIF
032900970526     C* ANNULLAMENTO
033000060901IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033100970529     C                   MOVEL     DEPOP0        WOP0
033200970526     C                   MOVEL     'O04'         WOP1
033300970526E   1C                   ENDIF
033400970526     C*
033500970526     C                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
033800970526     C*------------------------------------------------------------------------*
033900970526     C     O05GES        BEGSR
034000970526     C*
034100060901     C                   MOVEL     VIDdpc        Ddpc
034200170915     C                   MOVEL     VIDbun        Dbun
034300060907     C                   z-add     vihdde        Ddde
034400970529     C                   MOVEL     DEPOP0        WOP0
034500970526     C                   MOVEL     'O05'         WOP1
034600170919     C                   MOVEL     'TISIE5R1'    WPGM
034700970526     C                   EXSR      LANPGM
034800970526     C*
034900970526     C                   ENDSR
035000970526     C*------------------------------------------------------------------------*
035100970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
035200970526     C*------------------------------------------------------------------------*
035300970526     C     O07GES        BEGSR
035400970526     C*
035500970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
035600060901IF  1C***  VIDATB        IFNE      'A'
035700060901     C***                MOVEL     '1'           ERRL00
035800060901     C***                MOVEL     MSG(5)        DSMSMS
035900060901E   1C***                ENDIF
036000970526     C* RIPRISITINO
036100970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036200970529     C                   MOVEL     DEPOP0        WOP0
036300970526     C                   MOVEL     'O07'         WOP1
036400970526E   1C                   ENDIF
036500970526     C*
036600970526     C                   ENDSR
036700970526     C*------------------------------------------------------------------------*
036800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036900970526     C*------------------------------------------------------------------------*
037000970526     C     NEWPOS        BEGSR
037100970526     C*
037200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
037300970526     C*         1= RICHIESTO POSIZIONAMENTO
037400970526     C                   Z-ADD     0             SWPOSI            7 0
037500970526     C*
037600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037900970526     C                   Z-ADD     0             SWMODI            1 0
038000970530     C*
038100970530     C* REIMPOSTA INDICATORI DI ERRORE
038200970530     C                   SETOFF                                       808182
038300970530     C                   SETOFF                                       83
038400970526     C*
038500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038600060907IF  1C     VIDdde_P      IFNE      DEPdde
038700170918     C     VIDdpt        ORNE      DEPdpt
038800171113     C     VIDcbu        ORNE      DEPcbu
038900171113     C     VIDfDEP       ORNE      DEPfDEP
039000060901     C     VIDsel        ORNE      DEPsel
039100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
039200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
039300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
039400060907     C                   z-add     VIDdde_P      DEPdde                         *DEPOSITI
039500170918     C                   MOVEL     VIDdpt        DEPdpt
039600171113     C                   MOVEL     VIDcbu        DEPcbu
039700171113     C                   MOVEL     VIDfDEP       DEPfDEP
039800060901     C                   movel     VIDsel        DEPsel
039900150925     c                   z-add     0             sflnbr
040000970526E   2C                   ENDIF
040100970526E   1C                   ENDIF
040200970526     C*
040300970526     C                   ENDSR
040400970526     C*------------------------------------------------------------------------*
040500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
040600970526     C*------------------------------------------------------------------------*
040700970526     C     SETFIL        BEGSR
040800970526     C*
040900060831     C* POSIZIONAMENTO SUL FILE
041000171113     c                   movel     vidcbu        LEGdep7
041100171113     c                   move      viddpt        LEGdep7
041200170915     C     LEGdep7       SETLL     DPLEG11l                           99
041300970526     C* LETTURA RECORD
041400970526IF  1C     *IN99         IFEQ      *OFF
041500970526     C                   EXSR      LETFIL
041600970526E   1C                   ENDIF
041700970526     C*
041800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041900970526IF  1C     *IN99         IFEQ      *ON
042000970526     C                   Z-ADD     1             SFLEOF
042100970526E   1C                   ENDIF
042200970526     C*
042300970526     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* LETFIL - LETTURA RECORD
042600970526     C*------------------------------------------------------------------------*
042700970526     C     LETFIL        BEGSR
042800970526     C*
042900970526     C* LEGGE FINO A:
043000970526     C*     - FINE FILE
043100970526     C*     - TROVATO RECORD VALIDO
043200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
043300970526DO  1C     *IN99         DOWEQ     *OFF
043400970526     C     WRECOK        ANDEQ     'N'
043500970526     C*
043600170914     C                   READ      DPLEG11l                               99
043700970526     C*
043800970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043900970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
044000970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
044100970530X   2C                   ELSE
044200970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044300970530E   2C                   ENDIF
044400970526E   1C                   ENDDO
044500970526     C*
044600970526     C                   ENDSR
044700970526     C*------------------------------------------------------------------------*
044800970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
044900970526     C*------------------------------------------------------------------------*
045000970526     C     CHKREC        BEGSR
045100970526     C*
045200970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045300060901     c* record non valido se:
045400150610     c********* 1) nazione deposito  non presente in schiera
045500060901     c* 2) decorrenza richiesta non inclusa nella decorrenza/scadenza
045600170914     c* 3) selezione tipologia deposito (con o senza cod. LEG) non congruent
045700060901     c*    col deposito letto
045800060901     c* nazione
045900150610     c**********         if        %subst(knmus:1:3)<>'EDP'
046000170914     c***  LEGnar        lookup    sknaz                                  98
046100150610     c******             if        *in98='0'
046200150610     c********           eval      wrecok='N'
046300150610     c*********          endif
046400150610     c**********         endif
046500060901     c* decorrenza
046600170914     c                   if        viddde_P >0 and (ddevid < LEGdde or
046700170914     c                             ddevid > LEGdsc)
046800060901     c                   eval      wrecok='N'
046900060901     c                   endif
047000170914     c* solo depositi senza codice LEG
047100170914     c                   if        vidsel='S' and LEGksc > 0
047200060901     c                   eval      wrecok='N'
047300060901     c                   endif
047400970526     C*
047500171113     c* se c'è il filtro del DEPOT
047600171113     c                   if        vidfDEP <> '0000' and
047700171113     c                             vidfDEP <> *blank and
047800171113     c                             legDPC  <> vidfDEP
047900171113     c                   eval      wrecok='N'
048000171113     c                   endif
048100171113     C*
048200060901     C                   ENDSR
048300970526     C*------------------------------------------------------------------------*
048400970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
048500970526     C*------------------------------------------------------------------------*
048600970526     C     IRESFL        BEGSR
048700970526     C*
048800970526     C                   MOVEL     *BLANKS       OPZKEY
048900170914     C                   MOVEL     LEGnar        vidnar
049000170914     C                   MOVEL     LEGdpc        VIDdpc
049100170914     C                   MOVEL     LEGbun        VIDbun
049200060901     c* decodifica del depot (vidnam1 e vidcty) solo se non è scaduto
049300060901     c                   clear                   vidnam1
049400060901     c                   clear                   vidcty
049500170914    1c                   if        LEGdsc >= datcor
049600060901     c* recupero l'ultima versione di cappario
049700170918     C                   clear                   ISID1DRI
049800170918     C                   clear                   OSID1VER
049900170918     C                   clear                   OSID1VERD
050000170918     C                   clear                   OSID1DDE
050100170918     C                   clear                   OSID1DSC
050200170918     C                   clear                   OSID1BCID
050300170918     c                   call      'TISID1R'
050400170918     C                   PARM                    ISID1DRI
050500170918     C                   PARM                    OSID1VER
050600170918     C                   PARM                    OSID1VERD
050700170918     C                   PARM                    OSID1DDE
050800170918     C                   PARM                    OSID1DSC
050900170918     C                   PARM                    OSID1BCID
051000060901     c* se trovata versione reperisco  descrizioni deposito
051100170918    2c                   if        osid1ver>0
051200170915     C*
051300170915     c                   move      ' '           CDPATB
051400170915     c     keycdp        chain     dpcdp11i
051500170915    3c                   if        %found(dpcdp11i)
051600170915     c                   movel     cdpname1      vidnam1
051700170915     c                   movel     cdpcity       vidcty
051800060901    3c                   endif
051900060901    2c                   endif
052000060901    1c                   endif
052100060919     c                   clear                   vidrag
052200060919     c                   clear                   vidcit
052300060919     c                   clear                   vidcos
052400060919     c                   clear                   vidksc
052500060919     c                   clear                   vidsta
052600170914     c                   if        LEGksc > 0
052700170914     C                   move      LEGksc        VIDksc
052800060919      * aggancio anagrafico
052900060919
053000060919     C                   clear                   TIBS69DS
053100170914     c                   z-add     LEGksc        I69kac
053200170914     c                   z-add     LEGksc        I69kin
053300060919     C                   call      'TIBS69R'
053400060919     C                   parm                    tibs69DS
053500060919     C                   parm                    DS_cnaco
053600060919     C                   parm                    DS_cnind
053700060919     C                   parm                    DS_cnclp
053800060919     C                   parm                    DS_fncls
053900060919    2c                   If         O69ERR =  ' '
054000170918     c                   movel     'Cli.'        vidcos
054100060919     c                   movel     ACOrag        vidrag
054200060919     c                   movel     indcit        vidcit
054300060919     c                   movel     indsta        vidsta
054400060919    2c                   Endif
054500060907     c                   endif
054600060901     c* decorrenza/scadenza
054700170914     C                   Z-ADD     LEGdde        G08INV
054800060901     C                   Z-ADD     *ZEROS        G08DAT
054900060901     C                   MOVEL     '3'           G08ERR
055000060901     C                   CALL      'XSRDA8'
055100060901     C                   PARM                    WLBDA8
055200060901     C                   Z-ADD     G08DAT        VIDdde                         *DATA RIFERIMENTO
055300170914     C                   Z-ADD     LEGdde        VIhdde                         *DATA RIFERIMENTO
055400170914     C                   Z-ADD     LEGdsc        G08INV
055500060901     C                   Z-ADD     *ZEROS        G08DAT
055600060901     C                   MOVEL     '3'           G08ERR
055700060901     C                   CALL      'XSRDA8'
055800060901     C                   PARM                    WLBDA8
055900060901     C                   Z-ADD     G08DAT        VIDdsc                         *DATA RIFERIMENTO
056000060919     c*
056100170914     c                   movel     LEGorma       vidorm
056200970526     C*
056300970526     C                   ENDSR
056400970526     C*------------------------------------------------------------------------*
056500970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
056600970526     C*------------------------------------------------------------------------*
056700970526     C     CHKPAR        BEGSR
056800970526     C*
056900060907     C     *LIKE         DEFINE    VIDdde_p      ddeVID                         *DATA IN A/M/G
057000060901     C                   Z-ADD     *ZEROS        ddeVID
057100060901IF  1C     ERRL00        IFEQ      '0'
057200060907IF  2C     VIDdde_P      IFGT      *ZEROS
057300060907     C                   Z-ADD     VIDdde_P      G08DAT
057400060901     C                   Z-ADD     *ZEROS        G08INV
057500060901     C                   MOVEL     '0'           G08ERR
057600060901     C                   CALL      'XSRDA8'
057700060901     C                   PARM                    WLBDA8
057800060907     C                   Z-ADD     G08DAT        VIDdde_p
057900060901     C                   Z-ADD     G08INV        ddeVID
058000060901IF  3C     G08ERR        IFEQ      '1'
058100060901     C                   MOVEL     '1'           ERRL00
058200060901     C                   MOVEL     MSG(12)       DSMSMS
058300060901     C                   SETON                                        83
058400060901E   3C                   ENDIF
058500060901E   2C                   ENDIF
058600060901E   1C                   ENDIF
058700970526     C*
058800970526     C                   ENDSR
058900970526     C*------------------------------------------------------------------------*
059000970526     C* LANPGM - LANCIA UN PROGRAMMA
059100970526     C*------------------------------------------------------------------------*
059200970526     C     LANPGM        BEGSR
059300970526     C*
059400970526     C* IMPOSTA LA DS DI PROCEDURA
059500060831     C                   MOVEL     WOP0          DE5OP0                         *LIVELLO PROCEDURA
059600060831     C                   MOVEL     WOP1          DE5OP1                         *LIVELLO PGM
059700060831     C                   MOVEL     '0'           DE5F03                         *PREMUTO F03=FINE
059800060831     C                   MOVEL     '0'           DE5F12                         *PREMUTO F12=RITORNO
059900060831     C                   MOVEL     '0'           DE5ERR                         *'1'=ERRORE
060000060831     C                   MOVEL     *BLANKS       DE5MSG                         *MESSAGGIO DI RITORN
060100060831     C                   MOVEL     Ddpc          DE5DPC                         *NAZIONE
060200060831     C                   MOVEL     DDDE          DE5DDE                         *CAP
060300170918     c                   movel     vidbun        de5dep7
060400170918     c                   move      viddpc        de5dep7
060500170918     c                   move      viddpc        de5dpt
060600170918     c                   move      vidbun        de5bun
060700060920     c                   movel     vidsel        de5sel
060800060920     c                   z-add     ddevid        de5ddev
060900970526     C*
061000970526     C                   CALL      WPGM
061100970612     C                   PARM                    KPJBA
061200170919     C                   PARM                    tisie5ds
061300970526     C*
061400970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
061500060831IF  1C     DE5F03        IFEQ      '1'                                          *PREMUTO F03
061600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
061700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
061800970526X   1C                   ELSE                                                   *NON PREMUTO F03
061900060831     C                   MOVEL     DE5MSG        DSMSMS                         *MESSAGGIO DI RITORN
062000970526E   1C                   ENDIF
062100970526     C*
062200970526     C                   ENDSR
062300970527     C*------------------------------------------------------------------------*
062400970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
062500970527     C*------------------------------------------------------------------------*
062600970527     C     CARTAB        BEGSR
062700060831     C* CARICO TABELLA PUNTI OPERATIVI dell'area di partenza del p.o. utente
062800060831     C***
062900060831     C                   CLEAR                   trul06ds
063000060831     C                   MOVE      '£1'          D06COD
063100060831     C                   MOVEL     duttfp        D06KEY
063200060831     C                   MOVEL     'L'           D06TLA
063300060831     C                   MOVEL     'S'           D06esc
063400170918     c                   movel     kpjbu         savkpjbu
063500060831     C                   MOVEL     trul06ds      KPJBU
063600060831     C*
063700060831     C                   CALL      'TRUL06R'
063800060831     C                   PARM                    KPJBA
063900060831     C                   MOVEL     KPJBU         trul06ds
064000170918     c                   eval      kpjbu = savkpjbu
064100060831     c* carico la schiera dei p.o. dpd della £1
064200060831     c                   z-add     1             ii                2 0
064300060831     c                   z-add     0             xx                2 0
064400060831     c                   clear                   ldp
064500060901    1c                   dow       lin(ii) > *zeros  and
064600060831     c                             ii <= %elem(lin)
064700060831     c     lin(ii)       chain     azorg01l
064800060831    2c                   if        %found(azorg01l)
064900060831     c                   movel     orgde3        og143
065000060831    3c                   if        §ogntw = 'DPD'
065100060831     c                   add       1             xx
065200060901     c                   z-add     orgfil        ldp(xx)
065300060831    3c                   endif
065400060831    2c                   endif
065500060831     c                   add       1             ii
065600060831    1c                   enddo
065700150610      **    se ci sono linee DPD per la filiale in gestione
065800150610      **    altrimenti  deve uscire dal programma.
065900150610     c                   clear                   FIL_HA_DPD        1
066000150610    3c                   if        xx > 0
066100150610     c                   move      'S'           FIL_HA_DPD
066200150610      **
066300060831     c* lettura tabella nazioni a caricamento delle nazioni servite dai p.o.
066400060831     c* dpd caricati
066500060831     c* Carico tutte le nazioni servite dai p.o. dpd dell'area  di partenza
066600060831     c                   z-add     0             yy                4 0
066700060831     C                   CLEAR                   skNaz
066800060831     C                   CLEAR                   DS15
066900060831     C                   EVAL      tblKUT = 1
067000060831     C                   EVAL      tblCOD = '15'
067100060831     c     keytabel_P    setll     tabel00f
067200060831    1c                   do        *hival
067300060831     c     keytabel_P    reade     tabel00f
067400060831    2c                   if        %eof(tabel00f)
067500060831     c                   leave
067600060831    2c                   endif
067700060831     c                   movel     tbluni        ds15
067800060831    2c                   if        §15lad > 0
067900060831     c     §15lad        lookup    ldp                                    99
068000060831    3c                   if        *in99
068100060831     c                   add       1             yy
068200060831     c                   movel     tblkey        sknaz(yy)
068300060831    3c                   endif
068400060831    2c                   endif
068500060831    1c                   enddo
068600970527     C*
068700150610     c                   end
068800150610     C*
068900970527     C                   ENDSR
069000970526     C*------------------------------------------------------------------------*
069100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
069200970526     C*------------------------------------------------------------------------*
069300970526     C     CHKFOP        BEGSR
069400970526     C*
069500970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
069600060901    1c                   if        %subst(depop0:2:2)= '01'
069700970526     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
069800970526     C                   MOVEL     'S'           OPDATT
069900970526     C                   MOVEL     OPDRIG        OPD(1)
070000970626X   1C                   ELSE
070100970626     C* OPZIONI
070200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
070300970626     C                   MOVEL     'S'           OPDATT
070400970626     C                   MOVEL     OPDRIG        OPD(5)
070500970526E   1C                   ENDIF
070600970526     C*
070700970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
070800060901    1c                   if        %subst(depop0:2:2)= '02'
070900970526     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
071000970526     C                   MOVEL     'S'           OPDATT
071100970526     C                   MOVEL     OPDRIG        OPD(2)
071200970526     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
071300060906     C                   MOVEL     'N'           FUDATT
071400970526     C                   MOVEL     FUDRIG        FUD(6)
071500970526E   1C                   ENDIF
071600970526     C*
071700970526     C* GESTIONE OPZIONI A VIDEO
071800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
071900970526     C                   EXSR      OPZGES
072000970526     C*
072100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
072200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
072300970526     C                   EXSR      FUNGES
072400970526     C*
072500970526     C                   ENDSR
072600020917     C*--------------------------------------------------------------------------------------------*
072700020917     C* REPERISCE I DATI UTENTE
072800020917     C*--------------------------------------------------------------------------------------------*
072900020917     C     REPDATIUTE    BEGSR
073000020917     C*
073100020917     C* INIZIALIZZA VARIABILI DI WRK
073200020917     C                   CLEAR                   TIBS34DS
073300020917     C                   CLEAR                   AZUTEDS
073400020917     C                   CLEAR                   DDATIUTE
073500020917     C*
073600020917     C     *DTAARA       DEFINE    §azute        azuteds
073700020917     C     *DTAARA       DEFINE    §datiute      ddatiute
073800020917     C                   IN(E)     *DTAARA
073900020917if  1C                   IF        %Error
074000020917     c                   EVAL      I34Tla = 'L'
074100020917     C                   CALL      'TIBS34R'
074200020917     C                   PARM                    Tibs34Ds
074300020917     C                   IN        *DTAARA
074400020917e   1C                   ENDIF
074500020917     C*
074600020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
074700020917     C                   MOVEL(P)  RSUT          DSFIRS           20
074800020917     C*
074900020917     C                   ENDSR
075000970526     C*------------------------------------------------------------------------*
075100970526     C* *INZSR - OPERAZIONI INIZIALI
075200970526     C*------------------------------------------------------------------------*
075300970526     C     *INZSR        BEGSR
075400970526     C*---
075500970526     C* RICEVIMENTO PARAMETRI
075600970526     C*---
075700970526     C     *ENTRY        PLIST
075800970612     C                   PARM                    KPJBA
075900170919     C                   PARM                    tisie5ds
076000970526     C*
076100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
076200970526     C     XMSGPA        PLIST
076300970526     C                   PARM                    DSMSMS
076400970526     C                   PARM                    DSMSPG
076500970526     C                   PARM                    DSMSKE
076600970526     C                   PARM                    DSMSER
076700970526     C*
076800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
076900970526     C                   CALL      'XKEY'
077000970526     C                   PARM                    DSKEY
077100970526     C*---
077200970526     C* VARIABILI NON RIFERITE AL DATA BASE
077300970526     C*---
077400970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
077500970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
077600970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
077700970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
077800970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
077900970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
078000970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
078100970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
078200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
078300060901     C     *LIKE         DEFINE    de5op0        depop0
078400060901     C     *LIKE         DEFINE    de5op1        depop1
078500060901     C     *LIKE         DEFINE    De5OP0        WOP0                           *DI LAVORO
078600060901     C     *LIKE         DEFINE    De5OP1        WOP1
078700170914     C     *LIKE         DEFINE    LEGdde        depdde
078800170915     C     *LIKE         DEFINE    LEGdpc        depdpc
078900170915     C     *LIKE         DEFINE    vidbun        depbun
079000170918     C     *LIKE         DEFINE    viddpt        depdpt
079100171113     C     *LIKE         DEFINE    vidfDEP       depFdep
079200171113     C     *LIKE         DEFINE    vidcbu        depcbu
079300170914     C     *LIKE         DEFINE    LEGdde        ddde
079400170914     C     *LIKE         DEFINE    LEGdpc        ddpc
079500170915     C     *LIKE         DEFINE    LEGbun        dbun
079600060901     c*
079700060901     C     *LIKE         DEFINE    vidsel        depsel
079800970526     C*---
079900970526     C* VARIABILI DI CONTROLLO
080000970526     C*---
080100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
080200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
080300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
080400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
080500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
080600060919     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
080700970526     C*
080800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
080900170919     C                   MOVEL     'TISIE5R'     DSMSPG
081000170919     C                   MOVEL     'TISIE5R'     MSGQUE
081100970526     C                   MOVEL     *BLANKS       MSGKEY
081200970526     C                   MOVEL     *BLANKS       DSMSMS
081300970526     C                   MOVEL     *BLANKS       DSMSKE
081400970526     C                   Z-ADD     *ZEROS        DSMSER
081500970526     C*---
081600970526     C* VARIABILI RIFERITE AL DATA BASE
081700970526     C*---
081800970526     C*---
081900970526     C* CHIAVI DI LETTURA
082000970526     C*---
082100060901     c* TABEL00F - Parziale
082200060831     C     KEYtabel_P    KLIST
082300060831     C                   KFLD                    tblKUT
082400060831     C                   KFLD                    tblCOD
082500170915     C* DPCDP11i - Totale
082600060901     C     KEYcdp        KLIST
082700170915     c                   kfld                    CDPATB
082800170918     C                   KFLD                    osid1ver
082900170915     C                   KFLD                    LEGdep7
083000970527     C*
083100970527     C* REPERIMENTO DATI UTENTE
083200020917     C                   EXSR      REPDATIUTE
083300970527     C*
083400970527     C* CARICA LE TABELLE OCCORRENTI
083500970527     C                   EXSR      CARTAB
083600970526     C*
083700150610     C*  se non ci sono linee DPD in gestione (Finestra) ed esce dal pgm
083800150611     c                   if        FIL_HA_DPD = *blank   and
083900150611     c                             %subst(knmus:1:3)<>'EDP'
084000150610     c                   exfmt     SIE5NODPD
084100150610     c                   seton                                        LR
084200150610     c                   return
084300150610     c                   end
084400150610     C*
084500150610     C*
084600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
084700060831     C                   MOVEL     DE5OP0        DEPOP0                         *LIVELLO PROCEDURA
084800060831     C                   MOVEL     DE5OP1        DEPOP1                         *LIVELLO PROGRAMMA
084900970526     C*
085000970526     C* CALCOLA LA DATA CORRENTE
085100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
085200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
085300970526     C                   Z-ADD     WN8           G08DAT
085400970526     C                   Z-ADD     *ZEROS        G08INV
085500970526     C                   MOVEL     '0'           G08ERR
085600970526     C                   CALL      'XSRDA8'
085700970526     C                   PARM                    WLBDA8
085800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
085900970526     C*
086000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
086100060901     c* data  decorrenza
086200060831     C                   Z-ADD     datcor        G08INV
086300970526     C                   Z-ADD     *ZEROS        G08DAT
086400970526     C                   MOVEL     '3'           G08ERR
086500970526     C                   CALL      'XSRDA8'
086600970526     C                   PARM                    WLBDA8
086700060907     C                   Z-ADD     G08DAT        VIDdde_p                       *DATA RIFERIMENTO
086800060901     c* S=Solo depot senza codice Piano dei Conti
086900061012     c                   if        de5sel= *blanks
087000151012     C                   MOVEL     'N'           VIDsel                         *NAZIONE
087100061012     c                   else
087200061012     c                   movel     de5sel        vidsel
087300061012     c                   endif
087400060901     c* inizio elenco per codice depot --> imposto a blank
087500170918     C                   clear                   VIDdpt
087600171113     C                   clear                   VIDcbu
087700970526     C                   EXSR      CHKPAR
087800970526IF  1C     ERRL00        IFEQ      '1'
087900060831     C                   MOVEL     '1'           DE5ERR
088000060831     C                   MOVEL     DSMSMS        DE5MSG
088100970526X   1C                   ELSE
088200170918     C                   MOVEL     VIDdpt        DEPdpt                         *DEPOSITI
088300171113     C                   MOVEL     VIDcbu        DEPcbu
088400060907     C                   MOVEL     VIDdde_p      DEPdde
088500060901     C                   MOVEL     VIDsel        DEPsel
088600970526E   1C                   ENDIF
088700970526     C*
088800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
088900970526     C                   EXSR      CHKFOP
089000970530     C*
089100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
089200060901IF  1C                   if        %subst(depop0:2:2)='01'
089300970530     C                   MOVEL     DEC(3)        VIDING
089400970530E   1C                   ENDIF
089500060901IF  1C                   if        %subst(depop0:2:2)='02'
089600970530     C                   MOVEL     DEC(1)        VIDING
089700970530E   1C                   ENDIF
089800060901IF  1C                   if        %subst(depop0:2:2)='05'
089900970530     C                   MOVEL     DEC(2)        VIDING
090000970530E   1C                   ENDIF
090100970526     C*
090200970526     C                   ENDSR
090300060831     c
090400060831     C*
090500970526     C*------------------------------------------------------------------------*
090600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
090700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
090800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
090900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
091000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
091100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091300970526     C*------------------------------------------------------------------------*
091400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
091500970526     C*------------------------------------------------------------------------*
091600970526     C     POSSFL        BEGSR
091700171113     C*
091800171113     C*  se inserito un DEPOT con solo i 4 digit del depot, va in anagrafica a
091900171113     C*   completare il codice con la BU.
092000171113     c                   if        vidCBU ='000' or vidCBU = *blank
092100171113     c                   exsr      completa_DEP
092200171113     c                   end
092300970526     C*
092400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
092500970526     C                   EXSR      CLRSFL
092600970526     C*
092700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
092800970526     C                   EXSR      SETFIL
092900970526     C*
093000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
093100970526     C     SFLEOF        IFEQ      0
093200970526     C                   EXSR      CARSFL
093300970526     C                   END
093400970526     C*
093500970526     C                   ENDSR
093600171113     C*------------------------------------------------------------------------*
093700171113     C* se immessi solo i 4 digit del depot senza la B.UNIT
093800171113     C*------------------------------------------------------------------------*
093900171113     C     COMPLETA_DEP  BEGSR
094000171113     C*
094100171113     c                   clear                   tisieDEPds
094200171113     c                   clear                   prmCPD10F
094300171113      *
094400171113      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
094500171113     c                   move      vidDPT        I_DEPOT4
094600171113      ** ------
094700171113      *  aggancia con il DRIVER i legami
094800171113     c                   call      'TISIEDEPR'
094900171113     c                   parm                    tisieDEPds
095000171113     c                   parm                    prmCPD10F
095100171113      ** ------
095200171113      ** -  se ritornato tutto OK
095300171113     c                   if        O_FLGVERS ='0' and O_FLGCAPP ='0' and
095400171113     c                             O_FLGLEGA ='0'
095500171113      *
095600171113     c                   move      CDP_CDPDPTN   vidDPT
095700171113     c                   move      CDP_CDPBUCN   vidCBU
095800171113      *
095900171113     c                   endIF
096000171113      * --------
096100171113     C                   ENDSR
096200171113     C*------------------------------------------------------------------------*
096300970526     C*------------------------------------------------------------------------*
096400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
096500970526     C*------------------------------------------------------------------------*
096600970526     C     ENTATT        BEGSR
096700970526     C*
096800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
096900970526     C                   Z-ADD     0             OPZOK             1 0
097000970526     C*
097100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
097200970526     C                   Z-ADD     0             SFLVAR            5 0
097300970526     C*
097400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
097500970526     C                   Z-ADD     0             SFLERR
097600970526     C*
097700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
097800970526     C                   Z-ADD     SFLULT        WFLULT
097900970526     C*
098000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
098100060901     C                   READC     SIE5D05                                98
098200970526     C     *IN98         DOWEQ     '0'
098300970526     C*
098400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
098500970526     C                   ADD       1             SFLVAR
098600970526     C*
098700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
098800970526     C     OPZOK         IFEQ      0
098900970526     C*
099000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
099100970526     C     OPZKEY        IFNE      *BLANKS
099200970526     C                   EXSR      OPZCON
099300970526     C                   END
099400970526     C*
099500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
099600970526     C*            2. PULIZIA SCELTA IN SFL
099700970526     C     OPZOK         IFEQ      0
099800970526     C                   MOVEL     '0'           *IN41
099900970526     C                   MOVEL     *BLANKS       OPZKEY
100000970526     C                   ELSE
100100970526     C*
100200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
100300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
100400970526     C                   MOVEL     '1'           *IN41
100500970526     C                   Z-ADD     SFLULT        SFLERR
100600970526     C                   END
100700970526     C                   ELSE
100800970526     C*
100900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101100970526     C                   MOVEL     '1'           *IN41
101200970526     C                   END                                                    OPZOK=0
101300970526     C*
101400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
101500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
101600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
101700970526     C     OPZOK         IFNE      0
101800970526     C                   Z-ADD     SFLERR        SFLNBR
101900970526     C                   ELSE
102000970526     C                   Z-ADD     SFLULT        SFLNBR
102100970526     C                   END
102200150904     c     hin44         comp      '1'                                    44
102300970526     C*
102400970526     C* AGGIORNO RECORD SFL
102500060901     C                   UPDATE    SIE5D05
102600970526     C*
102700060901     C                   READC     SIE5D05                                98
102800970526     C                   END                                                    DO WHILE
102900970526     C*
103000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
103100970526     C                   Z-ADD     WFLULT        SFLULT
103200970526     C*
103300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
103400970526     C     SFLVAR        IFEQ      0
103500970526     C*---                 MOVEL'1'       FINL00
103600970526     C                   END
103700970526     C*
103800970526     C                   ENDSR
103900970526     C*------------------------------------------------------------------------*
104000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
104100970526     C*------------------------------------------------------------------------*
104200970526     C     CLRSFL        BEGSR
104300970526     C*
104400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
104500970526     C* (0=NON FINE FILE  1=FINE FILE)
104600970526     C                   Z-ADD     0             SFLEOF
104700970526     C*
104800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
104900970526     C                   Z-ADD     0             SFLULT
105000970526     C*
105100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
105200970526     C                   MOVEL     '1'           *IN40
105300060831     C                   WRITE     SIE5D06
105400970526     C                   MOVEL     '0'           *IN40
105500970526     C*
105600970526     C                   ENDSR
105700970526     C*------------------------------------------------------------------------*
105800970526     C* CARSFL - CARICAMENTO SFL
105900970526     C*------------------------------------------------------------------------*
106000970526     C     CARSFL        BEGSR
106100970526     C*
106200150924     C* x reimpostare il cursore correttamente sulla riga toccata dopo
106300150924     C*    aver ricaricato
106400150924     c                   z-add     0             cursfl            5 0
106500150924     C*
106600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
106700970526     C     SFLULT        IFEQ      0
106800970526     C                   EXSR      SETFIL
106900150924     C*
107000150924      * per potersi riposizionare immediatamente dopo aver aggiornato
107100150924     c                   if        sflnbr > 0
107200150924     c                   z-add     sflnbr        cursfl
107300150924     c                   move      '0'           sflnbr
107400150924     c                   add       sflnbr        sflpag
107500150924     c                   end
107600970526     C                   END
107700970526     C*
107800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
107900970526     C                   Z-ADD     SFLULT        WFLULT
108000970526     C*
108100970526     C* CICLO:
108200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
108300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
108400970526     C                   Z-ADD     1             SFLRIG
108500970526     C     SFLRIG        DOWLE     SFLPAG
108600970526     C     SFLEOF        ANDEQ     0
108700970526     C*
108800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
108900970526     C                   MOVEL     *BLANKS       OPZKEY
109000970526     C*
109100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
109200970526     C                   EXSR      IRESFL
109300970526     C*
109400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
109500970526     C                   ADD       1             SFLULT
109600970526     C*
109700970526     C* SCRITTURA RECORD SFL
109800150904     C                   SETOFF                                       44
109900150904     c                   move      '0'           hin44
110000150904     C*
110100150904     c                   if        vidnar <> *blank  and
110200150904     c                             vidsta <> *blank  and
110300150904     c                             vidnar <> vidsta
110400150904     C                   SETON                                        44
110500150904     c                   move      '1'           hin44
110600150904     C                   end
110700150904     C*
110800060901     C                   WRITE     SIE5D05
110900970526     C                   ADD       1             SFLRIG
111000970526     C*
111100970526     C* LETTURA FILE DA CARICARE
111200970526     C                   EXSR      LETFIL
111300970526     C                   END                                                    DO WHILE
111400970526     C*
111500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
111600970526     C     WFLULT        IFNE      SFLULT
111700970526     C                   Z-ADD     WFLULT        SFLNBR
111800970526     C                   ADD       1             SFLNBR
111900970526     C                   END
112000150924     C*
112100150924      * per reimpostare il cursore sulla riga toccata per l'aggiornamento
112200150924     c                   if         cursfl >0
112300150924     c                   eval      sflnbr =  cursfl
112400150924     C                   end
112500970526     C*
112600970526     C                   ENDSR
112700970526     C*------------------------------------------------------------------------*
112800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
112900970526     C*------------------------------------------------------------------------*
113000970526     C     OUTSFL        BEGSR
113100970526     C*
113200970526     C* WRITE RECORD TESTATA VIDEO
113300060901     C                   WRITE     SIE5D01
113400970526     C*
113500970526     C* VISUALIZZAZIONE MESSAGGIO
113600970526     C                   EXSR      SNDMSG
113700970526     C*
113800970526     C* WRITE RECORD OPZIONI VIDEO
113900060901     C                   WRITE     SIE5D02
114000970526     C*
114100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
114200060901     C                   WRITE     SIE5D03
114300970526     C*
114400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
114500060901     C                   WRITE     SIE5D04
114600970526     C*
114700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
114800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
114900970526     C     SFLEOF        IFEQ      0
115000970526     C                   MOVEL     '0'           *IN42
115100970526     C                   ELSE
115200970526     C*
115300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
115400970526     C                   MOVEL     '1'           *IN42
115500970526     C                   END
115600970526     C*
115700970526     C* WRITE RECORD TASTI FUNZIONALI
115800060901     C                   WRITE     SIE5D09
115900970526     C*
116000970526     C* CASO DI SFL VUOTO
116100970526     C     SFLULT        IFLE      0
116200970526     C*
116300970526     C*    - WRITE RECORD VUOTO
116400060901     C                   WRITE     SIE5D0A
116500060901     C                   READ      SIE5D0A                                99
116600970526     C*
116700970526     C                   ELSE
116800970526     C*
116900970526     C* CASO DI SFL CON RECORDS
117000970526     C*
117100970526     C*    - WRITE RECORD CONTROLLO SFL
117200150924     C                   exfmt     SIE5D06
117300150924     C***********        READ      SIE5D06                                99
117400970526     C                   END
117500970526     C*
117600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
117700060901     C                   READ      SIE5D03                                99
117800970526     C*
117900970526     C                   ENDSR
118000970526     C*------------------------------------------------------------------------*
118100970526     C* SNDMSG - MANDA MESSAGGIO
118200970526     C*------------------------------------------------------------------------*
118300970526     C     SNDMSG        BEGSR
118400970526     C*
118500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
118600970526     C                   MOVEL     '1'           *IN40
118700060901     C                   WRITE     SIE5DMC
118800970526     C                   MOVEL     '0'           *IN40
118900970526     C*
119000970526     C* VISUALIZZAZIONE MESSAGGIO
119100970526     C                   Z-ADD     0             MSGULT
119200970526     C                   CALL      'XMSG'        XMSGPA
119300970526     C     DSMSER        IFEQ      0
119400970526     C                   MOVEL     DSMSKE        MSGKEY
119500970526     C                   END
119600970526     C                   Z-ADD     1             MSGULT
119700060901     C                   WRITE     SIE5DMS                                      SFL MESS
119800060901     C                   WRITE     SIE5DMC                                      CONTR MESS
119900970526     C*
120000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
120100970526     C                   MOVEL     *BLANKS       DSMSMS
120200970526     C*
120300970526     C                   ENDSR
120400970526     C*------------------------------------------------------------------------*
120500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
120600970526     C*------------------------------------------------------------------------*
120700970526     C     FUNCON        BEGSR
120800970526     C*
120900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
121000970526     C                   EXSR      FUNVAR
121100970526     C*
121200970526     C* PULISCO CAMPI DI LAVORO
121300970526     C                   Z-ADD     0             I                 5 0
121400970526     C                   Z-ADD     0             W                 5 0
121500970526     C*
121600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
121700970526     C     FUNKEY        IFNE      *BLANKS
121800970526     C     1             DO        24            I
121900970526     C                   MOVEL     FUD(I)        FUDRIG
122000970526     C*
122100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
122200970526     C     FUNKEY        IFEQ      FUDFUN
122300970526     C     FUDATT        ANDEQ     'S'
122400970526     C                   Z-ADD     I             W
122500970526     C                   END
122600970526     C                   END                                                    DO WHILE
122700970526     C*
122800970526     C* FUNZIONE NON ATTIVA (ERRORE)
122900970526     C     W             IFEQ      0
123000970526     C*
123100970526     C* EMISSIONE MESSAGGIO DI ERRORE
123200970526     C                   MOVEL     MSG(2)        DSMSMS
123300970526     C                   END                                                    W=0
123400970526     C                   END                                                    FUNKEY NE *BLANKS
123500970526     C*
123600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
123700970526     C     W             IFNE      0
123800970526     C     FUNKEY        OREQ      *BLANKS
123900970526     C*
124000970526     C     FUNKEY        IFEQ      *BLANKS
124100970526     C                   EXSR      ENTGES
124200970526     C                   END
124300970526     C     FUNKEY        IFEQ      '03'
124400970526     C                   EXSR      F03GES
124500970526     C                   END
124600970526     C     FUNKEY        IFEQ      '05'
124700970526     C                   EXSR      F05GES
124800970526     C                   END
124900970526     C     FUNKEY        IFEQ      '06'
125000970526     C                   EXSR      F06GES
125100970526     C                   END
125200970526     C     FUNKEY        IFEQ      '12'
125300970526     C                   EXSR      F12GES
125400970526     C                   END
125500970526     C     FUNKEY        IFEQ      '21'
125600970526     C                   EXSR      F21GES
125700970526     C                   END
125800970526     C     FUNKEY        IFEQ      '22'
125900970526     C                   EXSR      F22GES
126000970526     C                   END
126100970526     C     FUNKEY        IFEQ      '23'
126200970526     C                   EXSR      F23GES
126300970526     C                   END
126400970526     C     FUNKEY        IFEQ      '24'
126500970526     C                   EXSR      F24GES
126600970526     C                   END
126700970526     C     FUNKEY        IFEQ      'RU'
126800970526     C                   EXSR      RUPGES
126900970526     C                   END
127000970526     C*
127100970526     C                   END
127200970526     C*
127300970526     C                   ENDSR
127400970526     C*------------------------------------------------------------------------*
127500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
127600970526     C*------------------------------------------------------------------------*
127700970526     C     FUNVAR        BEGSR
127800970526     C*
127900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
128000970526     C     KEY           IFEQ      ENTER
128100970526     C                   MOVEL     *BLANKS       FUNKEY
128200970526     C                   END
128300970526     C     KEY           IFEQ      F1
128400970526     C                   MOVEL     '01'          FUNKEY
128500970526     C                   END
128600970526     C     KEY           IFEQ      F2
128700970526     C                   MOVEL     '02'          FUNKEY
128800970526     C                   END
128900970526     C     KEY           IFEQ      F3
129000970526     C                   MOVEL     '03'          FUNKEY
129100970526     C                   END
129200970526     C     KEY           IFEQ      F4
129300970526     C                   MOVEL     '04'          FUNKEY
129400970526     C                   END
129500970526     C     KEY           IFEQ      F5
129600970526     C                   MOVEL     '05'          FUNKEY
129700970526     C                   END
129800970526     C     KEY           IFEQ      F6
129900970526     C                   MOVEL     '06'          FUNKEY
130000970526     C                   END
130100970526     C     KEY           IFEQ      F7
130200970526     C                   MOVEL     '07'          FUNKEY
130300970526     C                   END
130400970526     C     KEY           IFEQ      F8
130500970526     C                   MOVEL     '08'          FUNKEY
130600970526     C                   END
130700970526     C     KEY           IFEQ      F9
130800970526     C                   MOVEL     '09'          FUNKEY
130900970526     C                   END
131000970526     C     KEY           IFEQ      F10
131100970526     C                   MOVEL     '10'          FUNKEY
131200970526     C                   END
131300970526     C     KEY           IFEQ      F11
131400970526     C                   MOVEL     '11'          FUNKEY
131500970526     C                   END
131600970526     C     KEY           IFEQ      F12
131700970526     C                   MOVEL     '12'          FUNKEY
131800970526     C                   END
131900970526     C     KEY           IFEQ      F13
132000970526     C                   MOVEL     '13'          FUNKEY
132100970526     C                   END
132200970526     C     KEY           IFEQ      F14
132300970526     C                   MOVEL     '14'          FUNKEY
132400970526     C                   END
132500970526     C     KEY           IFEQ      F15
132600970526     C                   MOVEL     '15'          FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F16
132900970526     C                   MOVEL     '16'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F17
133200970526     C                   MOVEL     '17'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F18
133500970526     C                   MOVEL     '18'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F19
133800970526     C                   MOVEL     '19'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F20
134100970526     C                   MOVEL     '20'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F21
134400970526     C                   MOVEL     '21'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F22
134700970526     C                   MOVEL     '22'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F23
135000970526     C                   MOVEL     '23'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F24
135300970526     C                   MOVEL     '24'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      ROLLUP
135600970526     C                   MOVEL     'RU'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      HELP
135900970526     C                   MOVEL     'HP'          FUNKEY
136000970526     C                   END
136100970526     C*
136200970526     C                   ENDSR
136300970526     C*------------------------------------------------------------------------*
136400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
136500970526     C*------------------------------------------------------------------------*
136600970526     C     FUNGES        BEGSR
136700970526     C*
136800970526     C* PULISCO CAMPI DI LAVORO
136900970526     C                   Z-ADD     0             W                 5 0
137000970526     C*
137100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
137200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
137300970526     C     1             DO        24            I
137400970526     C                   MOVEL     FUD(I)        FUDRIG
137500970526     C*
137600970526     C* PULIZIA FUNZIONI PRECEDENTI
137700970526     C                   MOVEL     *BLANKS       FUV(I)
137800970526     C*
137900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
138000970526     C* (SALTA F23 E F24)
138100970526     C     FUDATT        IFEQ      'S'
138200970526     C     FUDFUN        ANDNE     '23'
138300970526     C     FUDFUN        ANDNE     '24'
138400970526     C                   ADD       1             W
138500970526     C                   MOVEL     FUDDES        FUV(W)
138600970526     C                   END
138700970526     C                   END                                                    DO WHILE
138800970526     C*
138900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
139000970526     C* ALTRIMENTI DISATTIVO F24
139100970526     C     1             DO        24            I
139200970526     C                   MOVEL     FUD(I)        FUDRIG
139300970526     C*
139400970526     C     FUDFUN        IFEQ      '24'
139500970526     C     W             IFGT      6
139600970526     C                   MOVEL     'S'           FUDATT
139700970526     C                   ELSE
139800970526     C                   MOVEL     *BLANKS       FUDATT
139900970526     C                   END                                                    W GT 8
140000970526     C                   MOVEL     FUDRIG        FUD(I)
140100970526     C                   END
140200970526     C                   END                                                    DO WHILE
140300970526     C*
140400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
140500970526     C     W             DIV       6             I
140600970526     C                   MVR                     Z
140700970526     C*
140800970526     C     Z             IFNE      0
140900970526     C     I             ADD       1             Z
141000970526     C                   ELSE
141100970526     C                   Z-ADD     I             Z
141200970526     C                   END
141300970526     C*
141400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
141500970526     C                   ADD       1             FUNGRU
141600970526     C     FUNGRU        IFGT      Z
141700970526     C     FUNGRU        ORLT      1
141800970526     C                   Z-ADD     1             FUNGRU
141900970526     C                   END
142000970526     C*
142100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
142200970526     C     FUNGRU        IFEQ      1
142300970526     C                   MOVEL     FUV(1)        FUN001
142400970526     C                   MOVEL     FUV(2)        FUN002
142500970526     C                   MOVEL     FUV(3)        FUN003
142600970526     C                   MOVEL     FUV(4)        FUN004
142700970526     C                   MOVEL     FUV(5)        FUN005
142800970526     C                   MOVEL     FUV(6)        FUN006
142900970526     C                   END
143000970526     C*
143100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
143200970526     C     FUNGRU        IFEQ      2
143300970526     C                   MOVEL     FUV(7)        FUN001
143400970526     C                   MOVEL     FUV(8)        FUN002
143500970526     C                   MOVEL     FUV(9)        FUN003
143600970526     C                   MOVEL     FUV(10)       FUN004
143700970526     C                   MOVEL     FUV(11)       FUN005
143800970526     C                   MOVEL     FUV(12)       FUN006
143900970526     C                   END
144000970526     C*
144100970526     C* TERZO GRUPPO DI 6 FUNZIONI
144200970526     C     FUNGRU        IFEQ      3
144300970526     C                   MOVEL     FUV(13)       FUN001
144400970526     C                   MOVEL     FUV(14)       FUN002
144500970526     C                   MOVEL     FUV(15)       FUN003
144600970526     C                   MOVEL     FUV(16)       FUN004
144700970526     C                   MOVEL     FUV(17)       FUN005
144800970526     C                   MOVEL     FUV(18)       FUN006
144900970526     C                   END
145000970526     C*
145100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
145200970526     C     FUNGRU        IFEQ      4
145300970526     C                   MOVEL     FUV(19)       FUN001
145400970526     C                   MOVEL     FUV(20)       FUN002
145500970526     C                   MOVEL     FUV(21)       FUN003
145600970526     C                   MOVEL     FUV(22)       FUN004
145700970526     C                   MOVEL     *BLANKS       FUN005
145800970526     C                   MOVEL     *BLANKS       FUN006
145900970526     C                   END
146000970526     C*
146100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
146200970526     C                   MOVEL     *BLANKS       FUN007
146300970526     C                   MOVEL     *BLANKS       FUN008
146400970526     C*
146500970526     C     1             DO        24            I
146600970526     C                   MOVEL     FUD(I)        FUDRIG
146700970526     C     FUDFUN        IFEQ      '23'
146800970526     C     FUDATT        ANDEQ     'S'
146900970526     C                   MOVEL     FUDDES        FUN007
147000970526     C                   END
147100970526     C     FUDFUN        IFEQ      '24'
147200970526     C     FUDATT        ANDEQ     'S'
147300970526     C                   MOVEL     FUDDES        FUN008
147400970526     C                   END
147500970526     C                   END                                                    DO
147600970526     C*
147700970526     C                   ENDSR
147800970526     C*------------------------------------------------------------------------*
147900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
148000970526     C*------------------------------------------------------------------------*
148100970526     C     OPZGES        BEGSR
148200970526     C*
148300970526     C* PULISCO CAMPI DI LAVORO
148400970526     C                   Z-ADD     0             W                 5 0
148500970526     C*
148600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
148700970526     C* E CARICO TABELLA OPZIONI ATTIVE
148800970526     C     1             DO        24            I
148900970526     C                   MOVEL     OPD(I)        OPDRIG
149000970526     C*
149100970526     C* PULIZIA OPZIONI PRECEDENTI
149200970526     C                   MOVEL     *BLANKS       OPV(I)
149300970526     C*
149400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
149500970526     C     OPDATT        IFEQ      'S'
149600970526     C                   ADD       1             W
149700970526     C                   MOVEL     OPDDES        OPV(W)
149800970526     C                   END
149900970526     C                   END                                                    DO WHILE
150000970526     C*
150100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
150200970526     C* ALTRIMENTI DISATTIVO F23
150300970526     C     1             DO        24            I
150400970526     C                   MOVEL     FUD(I)        FUDRIG
150500970526     C*
150600970526     C     FUDFUN        IFEQ      '23'
150700970526     C     W             IFGT      8
150800970526     C                   MOVEL     'S'           FUDATT
150900970526     C                   ELSE
151000970526     C                   MOVEL     *BLANKS       FUDATT
151100970526     C                   END                                                    W GT 8
151200970526     C                   MOVEL     FUDRIG        FUD(I)
151300970526     C                   END
151400970526     C                   END                                                    DO WHILE
151500970526     C*
151600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
151700970526     C     W             DIV       8             I
151800970526     C                   MVR                     Z                 5 0
151900970526     C*
152000970526     C     Z             IFNE      0
152100970526     C     I             ADD       1             Z
152200970526     C                   ELSE
152300970526     C                   Z-ADD     I             Z
152400970526     C                   END
152500970526     C*
152600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
152700970526     C                   ADD       1             OPZGRU
152800970526     C     OPZGRU        IFGT      Z
152900970526     C     OPZGRU        ORLT      1
153000970526     C                   Z-ADD     1             OPZGRU
153100970526     C                   END
153200970526     C*
153300970526     C* PRIMO GRUPPO DI 8 OPZIONI
153400970526     C     OPZGRU        IFEQ      1
153500970526     C                   MOVEL     OPV(1)        OPZ001
153600970526     C                   MOVEL     OPV(2)        OPZ002
153700970526     C                   MOVEL     OPV(3)        OPZ003
153800970526     C                   MOVEL     OPV(4)        OPZ004
153900970526     C                   MOVEL     OPV(5)        OPZ005
154000970526     C                   MOVEL     OPV(6)        OPZ006
154100970526     C                   MOVEL     OPV(7)        OPZ007
154200970526     C                   MOVEL     OPV(8)        OPZ008
154300970526     C                   END
154400970526     C*
154500970526     C* SECONDO GRUPPO DI 8 OPZIONI
154600970526     C     OPZGRU        IFEQ      2
154700970526     C                   MOVEL     OPV(9)        OPZ001
154800970526     C                   MOVEL     OPV(10)       OPZ002
154900970526     C                   MOVEL     OPV(11)       OPZ003
155000970526     C                   MOVEL     OPV(12)       OPZ004
155100970526     C                   MOVEL     OPV(13)       OPZ005
155200970526     C                   MOVEL     OPV(14)       OPZ006
155300970526     C                   MOVEL     OPV(15)       OPZ007
155400970526     C                   MOVEL     OPV(16)       OPZ008
155500970526     C                   END
155600970526     C*
155700970526     C* TERZO GRUPPO DI 8 OPZIONI
155800970526     C     OPZGRU        IFEQ      3
155900970526     C                   MOVEL     OPV(17)       OPZ001
156000970526     C                   MOVEL     OPV(18)       OPZ002
156100970526     C                   MOVEL     OPV(19)       OPZ003
156200970526     C                   MOVEL     OPV(20)       OPZ004
156300970526     C                   MOVEL     OPV(21)       OPZ005
156400970526     C                   MOVEL     OPV(22)       OPZ006
156500970526     C                   MOVEL     OPV(23)       OPZ007
156600970526     C                   MOVEL     OPV(24)       OPZ008
156700970526     C                   END
156800970526     C*
156900970526     C                   ENDSR
157000970526     C*------------------------------------------------------------------------*
157100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
157200970526     C*------------------------------------------------------------------------*
157300970526     C     OPZCON        BEGSR
157400970526     C*
157500970526     C* PULISCO CAMPI DI LAVORO
157600970526     C                   Z-ADD     0             I                 5 0
157700970526     C                   Z-ADD     0             W                 5 0
157800970526     C*
157900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
158000970526     C     1             DO        24            I
158100970526     C                   MOVEL     OPD(I)        OPDRIG
158200970526     C*
158300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
158400970526     C     OPZKEY        IFEQ      OPDOPZ
158500970526     C     OPZKEY        OREQ      OPDOP1
158600970526     C*
158700970526     C     OPDATT        IFEQ      'S'
158800970526     C                   MOVEL     OPDOPZ        OPZKEY
158900970526     C                   Z-ADD     I             W
159000970526     C                   END
159100970526     C                   END
159200970526     C*
159300970526     C                   END                                                    DO WHILE
159400970526     C*
159500970526     C* OPZIONE NON ATTIVA (ERRORE)
159600970526     C     W             IFEQ      0
159700970526     C*
159800970526     C* OPZIONE NON OK
159900970526     C                   Z-ADD     1             OPZOK
160000970526     C*
160100970526     C* EMISSIONE MESSAGGIO DI ERRORE
160200970526     C                   MOVEL     MSG(1)        DSMSMS
160300970526     C                   ELSE
160400970526     C*
160500970526     C* OPZIONE ATTIVA (PROCEDI ...)
160600970526     C*
160700970526     C     OPZKEY        IFEQ      ' 1'
160800970526     C                   EXSR      O01GES
160900970526     C                   END
161000970526     C     OPZKEY        IFEQ      ' 2'
161100970526     C                   EXSR      O02GES
161200970526     C                   END
161300970526     C     OPZKEY        IFEQ      ' 4'
161400970526     C                   EXSR      O04GES
161500970526     C                   END
161600970526     C     OPZKEY        IFEQ      ' 5'
161700970526     C                   EXSR      O05GES
161800970526     C                   END
161900970526     C     OPZKEY        IFEQ      ' 7'
162000970526     C                   EXSR      O07GES
162100970526     C                   END
162200970526     C*
162300970526     C                   END                                                    OPZ.NON ATTIVA
162400970526     C*
162500970526     C                   ENDSR
162600970526     C*------------------------------------------------------------------------*
162700970526     C* NOTE COSTRUZIONE SCHIERE
162800970526     C*------------------------------------------------------------------------*
162900970526     C*
163000970526     C* -MSG-MESSAGGI
163100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
163200970526     C* (NON TOCCARE MSG,1 E MSG,2)
163300970526     C*
163400970526     C* -CMD-COMANDI
163500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
163600970526     C*
163700970526     C* -OPD-OPZIONI
163800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
163900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
164000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
164100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
164200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
164300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
164400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
164500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
164600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
164700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
164800970526     C*
164900970526     C* -FUD-FUNZIONI
165000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
165100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
165200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
165300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
165400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
165500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
165600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
165700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
165800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
165900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
166000970526     O*------------------------------------------------------------------------*
166100970526** DEC - DECODIFICHE
166200970526    GESTIONE                                                     1
166300970526INTERROGAZIONE                                                   2
166400970530     SCELTA                                                      3
1665009705264
1666009705265
166700970526** MSG - MESSAGGI
166800060804TISIE5R- Opzione non gestita                                                   1
166900060804TISIE5R- Tasto funzionale non gestito                                          2
167000060804TISIE5R- 3
167100060804TISIE5R- Record annullato. Consentito solo il ripristino                       4
167200060804TISIE5R- Record non annullato. Impossibile ripristinare                        5
167300060804TISIE5R- Accettato solo un parametro di inizio lista                           6
167400060804TISIE5R- Errori nei parametri di ingresso                                      7
167500060804TISIE5R- Data errata                                                           8
167600060804TISIE5R- 9                                                                     9
167700060804TISIE5R- 10                                                                   10
167800060804TISIE5R- 11                                                                   11
167900060901TISIE5R- Data errata                                                          12
168000060804TISIE5R- 13                                                                   13
168100060804TISIE5R- 14                                                                   14
168200060804TISIE5R- 15                                                                   15
168300060804TISIE5R- 16                                                                   16
168400060804TISIE5R- 17                                                                   17
168500060804TISIE5R- 18                                                                   18
168600060804TISIE5R- 19                                                                   19
168700060804TISIE5R- 20                                                                   20
168800970526** CMD - COMANDI CL
168900970526WRKSPLF
169000970526
169100970526
169200970526
169300970526
169400970526** OPD - OPZIONI
169500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
169600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
169700000120                   -3 -     (S/N)=N                        opz. 03¬
169800970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
169900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
170000970526                   -6 -     (S/N)=N                        opz. 06¬
170100970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
170200000119                   -8 -     (S/N)=N                        opz. 08¬
170300000119                   -9 -     (S/N)=N                        opz. 09¬
170400000119                   -10-     (S/N)=N                        opz. 10¬
170500970526                   -  -     (S/N)=N                        opz. 11¬
170600970526                   -  -     (S/N)=N                        opz. 12¬
170700970526                   -  -     (S/N)=N                        opz. 13¬
170800970526                   -  -     (S/N)=N                        opz. 14¬
170900970526                   -  -     (S/N)=N                        opz. 15¬
171000970526                   -  -     (S/N)=N                        opz. 16¬
171100970526                   -  -     (S/N)=N                        opz. 17¬
171200970526                   -  -     (S/N)=N                        opz. 18¬
171300970526                   -  -     (S/N)=N                        opz. 19¬
171400970526                   -  -     (S/N)=N                        opz. 20¬
171500970526                   -  -     (S/N)=N                        opz. 21¬
171600970526                   -  -     (S/N)=N                        opz. 22¬
171700970526                   -  -     (S/N)=N                        opz. 23¬
171800970526                   -  -     (S/N)=N                        opz. 24¬
171900970526** FUD - FUNZIONI
172000970526                   -        (S/N)=N                        funz.01¬
172100970526                   -        (S/N)=N                        funz.02¬
172200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
172300970526                   -        (S/N)=N                        funz.04¬
172400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
172500970526F06=Inserimento    -        (S/N)=N                        funz.06¬
172600061012                   -        (S/N)=N                        funz.07¬
172700000119                   -        (S/N)=N                        funz.08¬
172800060919F09=Altri Dati     -        (S/N)=S                        funz.09¬
172900000119                   -        (S/N)=N                        funz.10¬
173000970526                   -        (S/N)=N                        funz.11¬
173100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
173200970526                   -        (S/N)=N                        funz.13¬
173300970526                   -        (S/N)=N                        funz.14¬
173400970526                   -        (S/N)=N                        funz.15¬
173500970526                   -        (S/N)=N                        funz.16¬
173600970526                   -        (S/N)=N                        funz.17¬
173700970526                   -        (S/N)=N                        funz.18¬
173800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
173900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
174000060804F21=Stampa elenco  -        (S/N)=S                        funz.21¬
174100970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
174200970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
174300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
