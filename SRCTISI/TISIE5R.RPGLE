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
039000171122     C     VIDfCBU       ORNE      DEPfCBU
039100060901     C     VIDsel        ORNE      DEPsel
039200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
039300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
039400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
039500060907     C                   z-add     VIDdde_P      DEPdde                         *DEPOSITI
039600170918     C                   MOVEL     VIDdpt        DEPdpt
039700171113     C                   MOVEL     VIDcbu        DEPcbu
039800171113     C                   MOVEL     VIDfDEP       DEPfDEP
039900171122     C                   MOVEL     VIDfCBU       DEPfCBU
040000060901     C                   movel     VIDsel        DEPsel
040100150925     c                   z-add     0             sflnbr
040200970526E   2C                   ENDIF
040300970526E   1C                   ENDIF
040400970526     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
040800970526     C*------------------------------------------------------------------------*
040900970526     C     SETFIL        BEGSR
041000970526     C*
041100060831     C* POSIZIONAMENTO SUL FILE
041200171113     c                   movel     vidcbu        LEGdep7
041300171113     c                   move      viddpt        LEGdep7
041400170915     C     LEGdep7       SETLL     DPLEG11l                           99
041500970526     C* LETTURA RECORD
041600970526IF  1C     *IN99         IFEQ      *OFF
041700970526     C                   EXSR      LETFIL
041800970526E   1C                   ENDIF
041900970526     C*
042000970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
042100970526IF  1C     *IN99         IFEQ      *ON
042200970526     C                   Z-ADD     1             SFLEOF
042300970526E   1C                   ENDIF
042400970526     C*
042500970526     C                   ENDSR
042600970526     C*------------------------------------------------------------------------*
042700970526     C* LETFIL - LETTURA RECORD
042800970526     C*------------------------------------------------------------------------*
042900970526     C     LETFIL        BEGSR
043000970526     C*
043100970526     C* LEGGE FINO A:
043200970526     C*     - FINE FILE
043300970526     C*     - TROVATO RECORD VALIDO
043400970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
043500970526DO  1C     *IN99         DOWEQ     *OFF
043600970526     C     WRECOK        ANDEQ     'N'
043700970526     C*
043800170914     C                   READ      DPLEG11l                               99
043900970526     C*
044000970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
044100970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
044200970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
044300970530X   2C                   ELSE
044400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044500970530E   2C                   ENDIF
044600970526E   1C                   ENDDO
044700970526     C*
044800970526     C                   ENDSR
044900970526     C*------------------------------------------------------------------------*
045000970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
045100970526     C*------------------------------------------------------------------------*
045200970526     C     CHKREC        BEGSR
045300970526     C*
045400970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045500060901     c* record non valido se:
045600150610     c********* 1) nazione deposito  non presente in schiera
045700060901     c* 2) decorrenza richiesta non inclusa nella decorrenza/scadenza
045800170914     c* 3) selezione tipologia deposito (con o senza cod. LEG) non congruent
045900060901     c*    col deposito letto
046000060901     c* nazione
046100150610     c**********         if        %subst(knmus:1:3)<>'EDP'
046200170914     c***  LEGnar        lookup    sknaz                                  98
046300150610     c******             if        *in98='0'
046400150610     c********           eval      wrecok='N'
046500150610     c*********          endif
046600150610     c**********         endif
046700060901     c* decorrenza
046800170914     c                   if        viddde_P >0 and (ddevid < LEGdde or
046900170914     c                             ddevid > LEGdsc)
047000060901     c                   eval      wrecok='N'
047100060901     c                   endif
047200170914     c* solo depositi senza codice LEG
047300170914     c                   if        vidsel='S' and LEGksc > 0
047400060901     c                   eval      wrecok='N'
047500060901     c                   endif
047600970526     C*
047700171113     c* se c'è il filtro del DEPOT
047800171113     c                   if        vidfDEP <> '0000' and
047900171113     c                             vidfDEP <> *blank and
048000171113     c                             legDPC  <> vidfDEP
048100171113     c                   eval      wrecok='N'
048200171113     c                   endif
048300171113     C*
048400171122     c                   move      legBUN        alfBUN            3
048500171122     c                   if        vidfCBU <> '000' and
048600171122     c                             vidfCBU <> *blank and
048700171122     c                             alfBUN  <> vidfCBU
048800171122     c                   eval      wrecok='N'
048900171122     c                   endif
049000171122     C*
049100060901     C                   ENDSR
049200970526     C*------------------------------------------------------------------------*
049300970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049400970526     C*------------------------------------------------------------------------*
049500970526     C     IRESFL        BEGSR
049600970526     C*
049700970526     C                   MOVEL     *BLANKS       OPZKEY
049800170914     C                   MOVEL     LEGnar        vidnar
049900170914     C                   MOVEL     LEGdpc        VIDdpc
050000170914     C                   MOVEL     LEGbun        VIDbun
050100060901     c* decodifica del depot (vidnam1 e vidcty) solo se non è scaduto
050200060901     c                   clear                   vidnam1
050300060901     c                   clear                   vidcty
050400170914    1c                   if        LEGdsc >= datcor
050500060901     c* recupero l'ultima versione di cappario
050600170918     C                   clear                   ISID1DRI
050700170918     C                   clear                   OSID1VER
050800170918     C                   clear                   OSID1VERD
050900170918     C                   clear                   OSID1DDE
051000170918     C                   clear                   OSID1DSC
051100170918     C                   clear                   OSID1BCID
051200170918     c                   call      'TISID1R'
051300170918     C                   PARM                    ISID1DRI
051400170918     C                   PARM                    OSID1VER
051500170918     C                   PARM                    OSID1VERD
051600170918     C                   PARM                    OSID1DDE
051700170918     C                   PARM                    OSID1DSC
051800170918     C                   PARM                    OSID1BCID
051900060901     c* se trovata versione reperisco  descrizioni deposito
052000170918    2c                   if        osid1ver>0
052100170915     C*
052200170915     c                   move      ' '           CDPATB
052300170915     c     keycdp        chain     dpcdp11i
052400170915    3c                   if        %found(dpcdp11i)
052500170915     c                   movel     cdpname1      vidnam1
052600170915     c                   movel     cdpcity       vidcty
052700060901    3c                   endif
052800060901    2c                   endif
052900060901    1c                   endif
053000060919     c                   clear                   vidrag
053100060919     c                   clear                   vidcit
053200060919     c                   clear                   vidcos
053300060919     c                   clear                   vidksc
053400060919     c                   clear                   vidsta
053500170914     c                   if        LEGksc > 0
053600170914     C                   move      LEGksc        VIDksc
053700060919      * aggancio anagrafico
053800060919
053900060919     C                   clear                   TIBS69DS
054000170914     c                   z-add     LEGksc        I69kac
054100170914     c                   z-add     LEGksc        I69kin
054200060919     C                   call      'TIBS69R'
054300060919     C                   parm                    tibs69DS
054400060919     C                   parm                    DS_cnaco
054500060919     C                   parm                    DS_cnind
054600060919     C                   parm                    DS_cnclp
054700060919     C                   parm                    DS_fncls
054800060919    2c                   If         O69ERR =  ' '
054900170918     c                   movel     'Cli.'        vidcos
055000060919     c                   movel     ACOrag        vidrag
055100060919     c                   movel     indcit        vidcit
055200060919     c                   movel     indsta        vidsta
055300060919    2c                   Endif
055400060907     c                   endif
055500060901     c* decorrenza/scadenza
055600170914     C                   Z-ADD     LEGdde        G08INV
055700060901     C                   Z-ADD     *ZEROS        G08DAT
055800060901     C                   MOVEL     '3'           G08ERR
055900060901     C                   CALL      'XSRDA8'
056000060901     C                   PARM                    WLBDA8
056100060901     C                   Z-ADD     G08DAT        VIDdde                         *DATA RIFERIMENTO
056200170914     C                   Z-ADD     LEGdde        VIhdde                         *DATA RIFERIMENTO
056300170914     C                   Z-ADD     LEGdsc        G08INV
056400060901     C                   Z-ADD     *ZEROS        G08DAT
056500060901     C                   MOVEL     '3'           G08ERR
056600060901     C                   CALL      'XSRDA8'
056700060901     C                   PARM                    WLBDA8
056800060901     C                   Z-ADD     G08DAT        VIDdsc                         *DATA RIFERIMENTO
056900060919     c*
057000170914     c                   movel     LEGorma       vidorm
057100970526     C*
057200970526     C                   ENDSR
057300970526     C*------------------------------------------------------------------------*
057400970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
057500970526     C*------------------------------------------------------------------------*
057600970526     C     CHKPAR        BEGSR
057700970526     C*
057800060907     C     *LIKE         DEFINE    VIDdde_p      ddeVID                         *DATA IN A/M/G
057900060901     C                   Z-ADD     *ZEROS        ddeVID
058000060901IF  1C     ERRL00        IFEQ      '0'
058100060907IF  2C     VIDdde_P      IFGT      *ZEROS
058200060907     C                   Z-ADD     VIDdde_P      G08DAT
058300060901     C                   Z-ADD     *ZEROS        G08INV
058400060901     C                   MOVEL     '0'           G08ERR
058500060901     C                   CALL      'XSRDA8'
058600060901     C                   PARM                    WLBDA8
058700060907     C                   Z-ADD     G08DAT        VIDdde_p
058800060901     C                   Z-ADD     G08INV        ddeVID
058900060901IF  3C     G08ERR        IFEQ      '1'
059000060901     C                   MOVEL     '1'           ERRL00
059100060901     C                   MOVEL     MSG(12)       DSMSMS
059200060901     C                   SETON                                        83
059300060901E   3C                   ENDIF
059400060901E   2C                   ENDIF
059500060901E   1C                   ENDIF
059600970526     C*
059700970526     C                   ENDSR
059800970526     C*------------------------------------------------------------------------*
059900970526     C* LANPGM - LANCIA UN PROGRAMMA
060000970526     C*------------------------------------------------------------------------*
060100970526     C     LANPGM        BEGSR
060200970526     C*
060300970526     C* IMPOSTA LA DS DI PROCEDURA
060400060831     C                   MOVEL     WOP0          DE5OP0                         *LIVELLO PROCEDURA
060500060831     C                   MOVEL     WOP1          DE5OP1                         *LIVELLO PGM
060600060831     C                   MOVEL     '0'           DE5F03                         *PREMUTO F03=FINE
060700060831     C                   MOVEL     '0'           DE5F12                         *PREMUTO F12=RITORNO
060800060831     C                   MOVEL     '0'           DE5ERR                         *'1'=ERRORE
060900060831     C                   MOVEL     *BLANKS       DE5MSG                         *MESSAGGIO DI RITORN
061000060831     C                   MOVEL     Ddpc          DE5DPC                         *NAZIONE
061100060831     C                   MOVEL     DDDE          DE5DDE                         *CAP
061200170918     c                   movel     vidbun        de5dep7
061300170918     c                   move      viddpc        de5dep7
061400170918     c                   move      viddpc        de5dpt
061500170918     c                   move      vidbun        de5bun
061600060920     c                   movel     vidsel        de5sel
061700060920     c                   z-add     ddevid        de5ddev
061800970526     C*
061900970526     C                   CALL      WPGM
062000970612     C                   PARM                    KPJBA
062100170919     C                   PARM                    tisie5ds
062200970526     C*
062300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
062400060831IF  1C     DE5F03        IFEQ      '1'                                          *PREMUTO F03
062500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
062600970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
062700970526X   1C                   ELSE                                                   *NON PREMUTO F03
062800060831     C                   MOVEL     DE5MSG        DSMSMS                         *MESSAGGIO DI RITORN
062900970526E   1C                   ENDIF
063000970526     C*
063100970526     C                   ENDSR
063200970527     C*------------------------------------------------------------------------*
063300970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
063400970527     C*------------------------------------------------------------------------*
063500970527     C     CARTAB        BEGSR
063600060831     C* CARICO TABELLA PUNTI OPERATIVI dell'area di partenza del p.o. utente
063700060831     C***
063800060831     C                   CLEAR                   trul06ds
063900060831     C                   MOVE      '£1'          D06COD
064000060831     C                   MOVEL     duttfp        D06KEY
064100060831     C                   MOVEL     'L'           D06TLA
064200060831     C                   MOVEL     'S'           D06esc
064300170918     c                   movel     kpjbu         savkpjbu
064400060831     C                   MOVEL     trul06ds      KPJBU
064500060831     C*
064600060831     C                   CALL      'TRUL06R'
064700060831     C                   PARM                    KPJBA
064800060831     C                   MOVEL     KPJBU         trul06ds
064900170918     c                   eval      kpjbu = savkpjbu
065000060831     c* carico la schiera dei p.o. dpd della £1
065100060831     c                   z-add     1             ii                2 0
065200060831     c                   z-add     0             xx                2 0
065300060831     c                   clear                   ldp
065400060901    1c                   dow       lin(ii) > *zeros  and
065500060831     c                             ii <= %elem(lin)
065600060831     c     lin(ii)       chain     azorg01l
065700060831    2c                   if        %found(azorg01l)
065800060831     c                   movel     orgde3        og143
065900060831    3c                   if        §ogntw = 'DPD'
066000060831     c                   add       1             xx
066100060901     c                   z-add     orgfil        ldp(xx)
066200060831    3c                   endif
066300060831    2c                   endif
066400060831     c                   add       1             ii
066500060831    1c                   enddo
066600150610      **    se ci sono linee DPD per la filiale in gestione
066700150610      **    altrimenti  deve uscire dal programma.
066800150610     c                   clear                   FIL_HA_DPD        1
066900150610    3c                   if        xx > 0
067000150610     c                   move      'S'           FIL_HA_DPD
067100150610      **
067200060831     c* lettura tabella nazioni a caricamento delle nazioni servite dai p.o.
067300060831     c* dpd caricati
067400060831     c* Carico tutte le nazioni servite dai p.o. dpd dell'area  di partenza
067500060831     c                   z-add     0             yy                4 0
067600060831     C                   CLEAR                   skNaz
067700060831     C                   CLEAR                   DS15
067800060831     C                   EVAL      tblKUT = 1
067900060831     C                   EVAL      tblCOD = '15'
068000060831     c     keytabel_P    setll     tabel00f
068100060831    1c                   do        *hival
068200060831     c     keytabel_P    reade     tabel00f
068300060831    2c                   if        %eof(tabel00f)
068400060831     c                   leave
068500060831    2c                   endif
068600060831     c                   movel     tbluni        ds15
068700060831    2c                   if        §15lad > 0
068800060831     c     §15lad        lookup    ldp                                    99
068900060831    3c                   if        *in99
069000060831     c                   add       1             yy
069100060831     c                   movel     tblkey        sknaz(yy)
069200060831    3c                   endif
069300060831    2c                   endif
069400060831    1c                   enddo
069500970527     C*
069600150610     c                   end
069700150610     C*
069800970527     C                   ENDSR
069900970526     C*------------------------------------------------------------------------*
070000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
070100970526     C*------------------------------------------------------------------------*
070200970526     C     CHKFOP        BEGSR
070300970526     C*
070400970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
070500060901    1c                   if        %subst(depop0:2:2)= '01'
070600970526     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
070700970526     C                   MOVEL     'S'           OPDATT
070800970526     C                   MOVEL     OPDRIG        OPD(1)
070900970626X   1C                   ELSE
071000970626     C* OPZIONI
071100970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
071200970626     C                   MOVEL     'S'           OPDATT
071300970626     C                   MOVEL     OPDRIG        OPD(5)
071400970526E   1C                   ENDIF
071500970526     C*
071600970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
071700060901    1c                   if        %subst(depop0:2:2)= '02'
071800970526     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
071900970526     C                   MOVEL     'S'           OPDATT
072000970526     C                   MOVEL     OPDRIG        OPD(2)
072100970526     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
072200060906     C                   MOVEL     'N'           FUDATT
072300970526     C                   MOVEL     FUDRIG        FUD(6)
072400970526E   1C                   ENDIF
072500970526     C*
072600970526     C* GESTIONE OPZIONI A VIDEO
072700970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
072800970526     C                   EXSR      OPZGES
072900970526     C*
073000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
073100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
073200970526     C                   EXSR      FUNGES
073300970526     C*
073400970526     C                   ENDSR
073500020917     C*--------------------------------------------------------------------------------------------*
073600020917     C* REPERISCE I DATI UTENTE
073700020917     C*--------------------------------------------------------------------------------------------*
073800020917     C     REPDATIUTE    BEGSR
073900020917     C*
074000020917     C* INIZIALIZZA VARIABILI DI WRK
074100020917     C                   CLEAR                   TIBS34DS
074200020917     C                   CLEAR                   AZUTEDS
074300020917     C                   CLEAR                   DDATIUTE
074400020917     C*
074500020917     C     *DTAARA       DEFINE    §azute        azuteds
074600020917     C     *DTAARA       DEFINE    §datiute      ddatiute
074700020917     C                   IN(E)     *DTAARA
074800020917if  1C                   IF        %Error
074900020917     c                   EVAL      I34Tla = 'L'
075000020917     C                   CALL      'TIBS34R'
075100020917     C                   PARM                    Tibs34Ds
075200020917     C                   IN        *DTAARA
075300020917e   1C                   ENDIF
075400020917     C*
075500020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
075600020917     C                   MOVEL(P)  RSUT          DSFIRS           20
075700020917     C*
075800020917     C                   ENDSR
075900970526     C*------------------------------------------------------------------------*
076000970526     C* *INZSR - OPERAZIONI INIZIALI
076100970526     C*------------------------------------------------------------------------*
076200970526     C     *INZSR        BEGSR
076300970526     C*---
076400970526     C* RICEVIMENTO PARAMETRI
076500970526     C*---
076600970526     C     *ENTRY        PLIST
076700970612     C                   PARM                    KPJBA
076800170919     C                   PARM                    tisie5ds
076900970526     C*
077000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
077100970526     C     XMSGPA        PLIST
077200970526     C                   PARM                    DSMSMS
077300970526     C                   PARM                    DSMSPG
077400970526     C                   PARM                    DSMSKE
077500970526     C                   PARM                    DSMSER
077600970526     C*
077700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
077800970526     C                   CALL      'XKEY'
077900970526     C                   PARM                    DSKEY
078000970526     C*---
078100970526     C* VARIABILI NON RIFERITE AL DATA BASE
078200970526     C*---
078300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
078400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
078500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
078600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
078700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
078800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
078900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
079000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
079100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
079200060901     C     *LIKE         DEFINE    de5op0        depop0
079300060901     C     *LIKE         DEFINE    de5op1        depop1
079400060901     C     *LIKE         DEFINE    De5OP0        WOP0                           *DI LAVORO
079500060901     C     *LIKE         DEFINE    De5OP1        WOP1
079600170914     C     *LIKE         DEFINE    LEGdde        depdde
079700170915     C     *LIKE         DEFINE    LEGdpc        depdpc
079800170915     C     *LIKE         DEFINE    vidbun        depbun
079900170918     C     *LIKE         DEFINE    viddpt        depdpt
080000171113     C     *LIKE         DEFINE    vidfDEP       depFdep
080100171122     C     *LIKE         DEFINE    vidfCBU       depFcbu
080200171113     C     *LIKE         DEFINE    vidcbu        depcbu
080300170914     C     *LIKE         DEFINE    LEGdde        ddde
080400170914     C     *LIKE         DEFINE    LEGdpc        ddpc
080500170915     C     *LIKE         DEFINE    LEGbun        dbun
080600060901     c*
080700060901     C     *LIKE         DEFINE    vidsel        depsel
080800970526     C*---
080900970526     C* VARIABILI DI CONTROLLO
081000970526     C*---
081100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
081200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
081300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
081400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
081500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
081600060919     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
081700970526     C*
081800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
081900170919     C                   MOVEL     'TISIE5R'     DSMSPG
082000170919     C                   MOVEL     'TISIE5R'     MSGQUE
082100970526     C                   MOVEL     *BLANKS       MSGKEY
082200970526     C                   MOVEL     *BLANKS       DSMSMS
082300970526     C                   MOVEL     *BLANKS       DSMSKE
082400970526     C                   Z-ADD     *ZEROS        DSMSER
082500970526     C*---
082600970526     C* VARIABILI RIFERITE AL DATA BASE
082700970526     C*---
082800970526     C*---
082900970526     C* CHIAVI DI LETTURA
083000970526     C*---
083100060901     c* TABEL00F - Parziale
083200060831     C     KEYtabel_P    KLIST
083300060831     C                   KFLD                    tblKUT
083400060831     C                   KFLD                    tblCOD
083500170915     C* DPCDP11i - Totale
083600060901     C     KEYcdp        KLIST
083700170915     c                   kfld                    CDPATB
083800170918     C                   KFLD                    osid1ver
083900170915     C                   KFLD                    LEGdep7
084000970527     C*
084100970527     C* REPERIMENTO DATI UTENTE
084200020917     C                   EXSR      REPDATIUTE
084300970527     C*
084400970527     C* CARICA LE TABELLE OCCORRENTI
084500970527     C                   EXSR      CARTAB
084600970526     C*
084700150610     C*  se non ci sono linee DPD in gestione (Finestra) ed esce dal pgm
084800150611     c                   if        FIL_HA_DPD = *blank   and
084900150611     c                             %subst(knmus:1:3)<>'EDP'
085000150610     c                   exfmt     SIE5NODPD
085100150610     c                   seton                                        LR
085200150610     c                   return
085300150610     c                   end
085400150610     C*
085500150610     C*
085600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
085700060831     C                   MOVEL     DE5OP0        DEPOP0                         *LIVELLO PROCEDURA
085800060831     C                   MOVEL     DE5OP1        DEPOP1                         *LIVELLO PROGRAMMA
085900970526     C*
086000970526     C* CALCOLA LA DATA CORRENTE
086100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
086200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
086300970526     C                   Z-ADD     WN8           G08DAT
086400970526     C                   Z-ADD     *ZEROS        G08INV
086500970526     C                   MOVEL     '0'           G08ERR
086600970526     C                   CALL      'XSRDA8'
086700970526     C                   PARM                    WLBDA8
086800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
086900970526     C*
087000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
087100060901     c* data  decorrenza
087200060831     C                   Z-ADD     datcor        G08INV
087300970526     C                   Z-ADD     *ZEROS        G08DAT
087400970526     C                   MOVEL     '3'           G08ERR
087500970526     C                   CALL      'XSRDA8'
087600970526     C                   PARM                    WLBDA8
087700060907     C                   Z-ADD     G08DAT        VIDdde_p                       *DATA RIFERIMENTO
087800060901     c* S=Solo depot senza codice Piano dei Conti
087900061012     c                   if        de5sel= *blanks
088000151012     C                   MOVEL     'N'           VIDsel                         *NAZIONE
088100061012     c                   else
088200061012     c                   movel     de5sel        vidsel
088300061012     c                   endif
088400060901     c* inizio elenco per codice depot --> imposto a blank
088500170918     C                   clear                   VIDdpt
088600171113     C                   clear                   VIDcbu
088700970526     C                   EXSR      CHKPAR
088800970526IF  1C     ERRL00        IFEQ      '1'
088900060831     C                   MOVEL     '1'           DE5ERR
089000060831     C                   MOVEL     DSMSMS        DE5MSG
089100970526X   1C                   ELSE
089200170918     C                   MOVEL     VIDdpt        DEPdpt                         *DEPOSITI
089300171113     C                   MOVEL     VIDcbu        DEPcbu
089400060907     C                   MOVEL     VIDdde_p      DEPdde
089500060901     C                   MOVEL     VIDsel        DEPsel
089600970526E   1C                   ENDIF
089700970526     C*
089800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
089900970526     C                   EXSR      CHKFOP
090000970530     C*
090100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
090200060901IF  1C                   if        %subst(depop0:2:2)='01'
090300970530     C                   MOVEL     DEC(3)        VIDING
090400970530E   1C                   ENDIF
090500060901IF  1C                   if        %subst(depop0:2:2)='02'
090600970530     C                   MOVEL     DEC(1)        VIDING
090700970530E   1C                   ENDIF
090800060901IF  1C                   if        %subst(depop0:2:2)='05'
090900970530     C                   MOVEL     DEC(2)        VIDING
091000970530E   1C                   ENDIF
091100970526     C*
091200970526     C                   ENDSR
091300060831     c
091400060831     C*
091500970526     C*------------------------------------------------------------------------*
091600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
091900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
092000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
092100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092300970526     C*------------------------------------------------------------------------*
092400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
092500970526     C*------------------------------------------------------------------------*
092600970526     C     POSSFL        BEGSR
092700171113     C*
092800171113     C*  se inserito un DEPOT con solo i 4 digit del depot, va in anagrafica a
092900171113     C*   completare il codice con la BU.
093000171113     c                   if        vidCBU ='000' or vidCBU = *blank
093100171113     c                   exsr      completa_DEP
093200171113     c                   end
093300970526     C*
093400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
093500970526     C                   EXSR      CLRSFL
093600970526     C*
093700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
093800970526     C                   EXSR      SETFIL
093900970526     C*
094000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
094100970526     C     SFLEOF        IFEQ      0
094200970526     C                   EXSR      CARSFL
094300970526     C                   END
094400970526     C*
094500970526     C                   ENDSR
094600171113     C*------------------------------------------------------------------------*
094700171113     C* se immessi solo i 4 digit del depot senza la B.UNIT
094800171113     C*------------------------------------------------------------------------*
094900171113     C     COMPLETA_DEP  BEGSR
095000171113     C*
095100171113     c                   clear                   tisieDEPds
095200171113     c                   clear                   prmCPD10F
095300171113      *
095400171113      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
095500171113     c                   move      vidDPT        I_DEPOT4
095600171113      ** ------
095700171113      *  aggancia con il DRIVER i legami
095800171113     c                   call      'TISIEDEPR'
095900171113     c                   parm                    tisieDEPds
096000171113     c                   parm                    prmCPD10F
096100171113      ** ------
096200171113      ** -  se ritornato tutto OK
096300171113     c                   if        O_FLGVERS ='0' and O_FLGCAPP ='0' and
096400171113     c                             O_FLGLEGA ='0'
096500171113      *
096600171113     c                   move      CDP_CDPDPTN   vidDPT
096700171113     c                   move      CDP_CDPBUCN   vidCBU
096800171113      *
096900171113     c                   endIF
097000171113      * --------
097100171113     C                   ENDSR
097200171113     C*------------------------------------------------------------------------*
097300970526     C*------------------------------------------------------------------------*
097400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
097500970526     C*------------------------------------------------------------------------*
097600970526     C     ENTATT        BEGSR
097700970526     C*
097800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
097900970526     C                   Z-ADD     0             OPZOK             1 0
098000970526     C*
098100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
098200970526     C                   Z-ADD     0             SFLVAR            5 0
098300970526     C*
098400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098500970526     C                   Z-ADD     0             SFLERR
098600970526     C*
098700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098800970526     C                   Z-ADD     SFLULT        WFLULT
098900970526     C*
099000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
099100060901     C                   READC     SIE5D05                                98
099200970526     C     *IN98         DOWEQ     '0'
099300970526     C*
099400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
099500970526     C                   ADD       1             SFLVAR
099600970526     C*
099700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
099800970526     C     OPZOK         IFEQ      0
099900970526     C*
100000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
100100970526     C     OPZKEY        IFNE      *BLANKS
100200970526     C                   EXSR      OPZCON
100300970526     C                   END
100400970526     C*
100500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100600970526     C*            2. PULIZIA SCELTA IN SFL
100700970526     C     OPZOK         IFEQ      0
100800970526     C                   MOVEL     '0'           *IN41
100900970526     C                   MOVEL     *BLANKS       OPZKEY
101000970526     C                   ELSE
101100970526     C*
101200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101400970526     C                   MOVEL     '1'           *IN41
101500970526     C                   Z-ADD     SFLULT        SFLERR
101600970526     C                   END
101700970526     C                   ELSE
101800970526     C*
101900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
102000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
102100970526     C                   MOVEL     '1'           *IN41
102200970526     C                   END                                                    OPZOK=0
102300970526     C*
102400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102700970526     C     OPZOK         IFNE      0
102800970526     C                   Z-ADD     SFLERR        SFLNBR
102900970526     C                   ELSE
103000970526     C                   Z-ADD     SFLULT        SFLNBR
103100970526     C                   END
103200150904     c     hin44         comp      '1'                                    44
103300970526     C*
103400970526     C* AGGIORNO RECORD SFL
103500060901     C                   UPDATE    SIE5D05
103600970526     C*
103700060901     C                   READC     SIE5D05                                98
103800970526     C                   END                                                    DO WHILE
103900970526     C*
104000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
104100970526     C                   Z-ADD     WFLULT        SFLULT
104200970526     C*
104300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
104400970526     C     SFLVAR        IFEQ      0
104500970526     C*---                 MOVEL'1'       FINL00
104600970526     C                   END
104700970526     C*
104800970526     C                   ENDSR
104900970526     C*------------------------------------------------------------------------*
105000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
105100970526     C*------------------------------------------------------------------------*
105200970526     C     CLRSFL        BEGSR
105300970526     C*
105400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
105500970526     C* (0=NON FINE FILE  1=FINE FILE)
105600970526     C                   Z-ADD     0             SFLEOF
105700970526     C*
105800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
105900970526     C                   Z-ADD     0             SFLULT
106000970526     C*
106100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
106200970526     C                   MOVEL     '1'           *IN40
106300060831     C                   WRITE     SIE5D06
106400970526     C                   MOVEL     '0'           *IN40
106500970526     C*
106600970526     C                   ENDSR
106700970526     C*------------------------------------------------------------------------*
106800970526     C* CARSFL - CARICAMENTO SFL
106900970526     C*------------------------------------------------------------------------*
107000970526     C     CARSFL        BEGSR
107100970526     C*
107200150924     C* x reimpostare il cursore correttamente sulla riga toccata dopo
107300150924     C*    aver ricaricato
107400150924     c                   z-add     0             cursfl            5 0
107500150924     C*
107600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
107700970526     C     SFLULT        IFEQ      0
107800970526     C                   EXSR      SETFIL
107900150924     C*
108000150924      * per potersi riposizionare immediatamente dopo aver aggiornato
108100150924     c                   if        sflnbr > 0
108200150924     c                   z-add     sflnbr        cursfl
108300150924     c                   move      '0'           sflnbr
108400150924     c                   add       sflnbr        sflpag
108500150924     c                   end
108600970526     C                   END
108700970526     C*
108800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
108900970526     C                   Z-ADD     SFLULT        WFLULT
109000970526     C*
109100970526     C* CICLO:
109200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
109300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
109400970526     C                   Z-ADD     1             SFLRIG
109500970526     C     SFLRIG        DOWLE     SFLPAG
109600970526     C     SFLEOF        ANDEQ     0
109700970526     C*
109800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
109900970526     C                   MOVEL     *BLANKS       OPZKEY
110000970526     C*
110100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
110200970526     C                   EXSR      IRESFL
110300970526     C*
110400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
110500970526     C                   ADD       1             SFLULT
110600970526     C*
110700970526     C* SCRITTURA RECORD SFL
110800150904     C                   SETOFF                                       44
110900150904     c                   move      '0'           hin44
111000150904     C*
111100150904     c                   if        vidnar <> *blank  and
111200150904     c                             vidsta <> *blank  and
111300150904     c                             vidnar <> vidsta
111400150904     C                   SETON                                        44
111500150904     c                   move      '1'           hin44
111600150904     C                   end
111700150904     C*
111800060901     C                   WRITE     SIE5D05
111900970526     C                   ADD       1             SFLRIG
112000970526     C*
112100970526     C* LETTURA FILE DA CARICARE
112200970526     C                   EXSR      LETFIL
112300970526     C                   END                                                    DO WHILE
112400970526     C*
112500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
112600970526     C     WFLULT        IFNE      SFLULT
112700970526     C                   Z-ADD     WFLULT        SFLNBR
112800970526     C                   ADD       1             SFLNBR
112900970526     C                   END
113000150924     C*
113100150924      * per reimpostare il cursore sulla riga toccata per l'aggiornamento
113200150924     c                   if         cursfl >0
113300150924     c                   eval      sflnbr =  cursfl
113400150924     C                   end
113500970526     C*
113600970526     C                   ENDSR
113700970526     C*------------------------------------------------------------------------*
113800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
113900970526     C*------------------------------------------------------------------------*
114000970526     C     OUTSFL        BEGSR
114100970526     C*
114200970526     C* WRITE RECORD TESTATA VIDEO
114300060901     C                   WRITE     SIE5D01
114400970526     C*
114500970526     C* VISUALIZZAZIONE MESSAGGIO
114600970526     C                   EXSR      SNDMSG
114700970526     C*
114800970526     C* WRITE RECORD OPZIONI VIDEO
114900060901     C                   WRITE     SIE5D02
115000970526     C*
115100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
115200060901     C                   WRITE     SIE5D03
115300970526     C*
115400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
115500060901     C                   WRITE     SIE5D04
115600970526     C*
115700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
115800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
115900970526     C     SFLEOF        IFEQ      0
116000970526     C                   MOVEL     '0'           *IN42
116100970526     C                   ELSE
116200970526     C*
116300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
116400970526     C                   MOVEL     '1'           *IN42
116500970526     C                   END
116600970526     C*
116700970526     C* WRITE RECORD TASTI FUNZIONALI
116800060901     C                   WRITE     SIE5D09
116900970526     C*
117000970526     C* CASO DI SFL VUOTO
117100970526     C     SFLULT        IFLE      0
117200970526     C*
117300970526     C*    - WRITE RECORD VUOTO
117400060901     C                   WRITE     SIE5D0A
117500060901     C                   READ      SIE5D0A                                99
117600970526     C*
117700970526     C                   ELSE
117800970526     C*
117900970526     C* CASO DI SFL CON RECORDS
118000970526     C*
118100970526     C*    - WRITE RECORD CONTROLLO SFL
118200150924     C                   exfmt     SIE5D06
118300150924     C***********        READ      SIE5D06                                99
118400970526     C                   END
118500970526     C*
118600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
118700060901     C                   READ      SIE5D03                                99
118800970526     C*
118900970526     C                   ENDSR
119000970526     C*------------------------------------------------------------------------*
119100970526     C* SNDMSG - MANDA MESSAGGIO
119200970526     C*------------------------------------------------------------------------*
119300970526     C     SNDMSG        BEGSR
119400970526     C*
119500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
119600970526     C                   MOVEL     '1'           *IN40
119700060901     C                   WRITE     SIE5DMC
119800970526     C                   MOVEL     '0'           *IN40
119900970526     C*
120000970526     C* VISUALIZZAZIONE MESSAGGIO
120100970526     C                   Z-ADD     0             MSGULT
120200970526     C                   CALL      'XMSG'        XMSGPA
120300970526     C     DSMSER        IFEQ      0
120400970526     C                   MOVEL     DSMSKE        MSGKEY
120500970526     C                   END
120600970526     C                   Z-ADD     1             MSGULT
120700060901     C                   WRITE     SIE5DMS                                      SFL MESS
120800060901     C                   WRITE     SIE5DMC                                      CONTR MESS
120900970526     C*
121000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
121100970526     C                   MOVEL     *BLANKS       DSMSMS
121200970526     C*
121300970526     C                   ENDSR
121400970526     C*------------------------------------------------------------------------*
121500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
121600970526     C*------------------------------------------------------------------------*
121700970526     C     FUNCON        BEGSR
121800970526     C*
121900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
122000970526     C                   EXSR      FUNVAR
122100970526     C*
122200970526     C* PULISCO CAMPI DI LAVORO
122300970526     C                   Z-ADD     0             I                 5 0
122400970526     C                   Z-ADD     0             W                 5 0
122500970526     C*
122600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
122700970526     C     FUNKEY        IFNE      *BLANKS
122800970526     C     1             DO        24            I
122900970526     C                   MOVEL     FUD(I)        FUDRIG
123000970526     C*
123100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
123200970526     C     FUNKEY        IFEQ      FUDFUN
123300970526     C     FUDATT        ANDEQ     'S'
123400970526     C                   Z-ADD     I             W
123500970526     C                   END
123600970526     C                   END                                                    DO WHILE
123700970526     C*
123800970526     C* FUNZIONE NON ATTIVA (ERRORE)
123900970526     C     W             IFEQ      0
124000970526     C*
124100970526     C* EMISSIONE MESSAGGIO DI ERRORE
124200970526     C                   MOVEL     MSG(2)        DSMSMS
124300970526     C                   END                                                    W=0
124400970526     C                   END                                                    FUNKEY NE *BLANKS
124500970526     C*
124600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
124700970526     C     W             IFNE      0
124800970526     C     FUNKEY        OREQ      *BLANKS
124900970526     C*
125000970526     C     FUNKEY        IFEQ      *BLANKS
125100970526     C                   EXSR      ENTGES
125200970526     C                   END
125300970526     C     FUNKEY        IFEQ      '03'
125400970526     C                   EXSR      F03GES
125500970526     C                   END
125600970526     C     FUNKEY        IFEQ      '05'
125700970526     C                   EXSR      F05GES
125800970526     C                   END
125900970526     C     FUNKEY        IFEQ      '06'
126000970526     C                   EXSR      F06GES
126100970526     C                   END
126200970526     C     FUNKEY        IFEQ      '12'
126300970526     C                   EXSR      F12GES
126400970526     C                   END
126500970526     C     FUNKEY        IFEQ      '21'
126600970526     C                   EXSR      F21GES
126700970526     C                   END
126800970526     C     FUNKEY        IFEQ      '22'
126900970526     C                   EXSR      F22GES
127000970526     C                   END
127100970526     C     FUNKEY        IFEQ      '23'
127200970526     C                   EXSR      F23GES
127300970526     C                   END
127400970526     C     FUNKEY        IFEQ      '24'
127500970526     C                   EXSR      F24GES
127600970526     C                   END
127700970526     C     FUNKEY        IFEQ      'RU'
127800970526     C                   EXSR      RUPGES
127900970526     C                   END
128000970526     C*
128100970526     C                   END
128200970526     C*
128300970526     C                   ENDSR
128400970526     C*------------------------------------------------------------------------*
128500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
128600970526     C*------------------------------------------------------------------------*
128700970526     C     FUNVAR        BEGSR
128800970526     C*
128900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
129000970526     C     KEY           IFEQ      ENTER
129100970526     C                   MOVEL     *BLANKS       FUNKEY
129200970526     C                   END
129300970526     C     KEY           IFEQ      F1
129400970526     C                   MOVEL     '01'          FUNKEY
129500970526     C                   END
129600970526     C     KEY           IFEQ      F2
129700970526     C                   MOVEL     '02'          FUNKEY
129800970526     C                   END
129900970526     C     KEY           IFEQ      F3
130000970526     C                   MOVEL     '03'          FUNKEY
130100970526     C                   END
130200970526     C     KEY           IFEQ      F4
130300970526     C                   MOVEL     '04'          FUNKEY
130400970526     C                   END
130500970526     C     KEY           IFEQ      F5
130600970526     C                   MOVEL     '05'          FUNKEY
130700970526     C                   END
130800970526     C     KEY           IFEQ      F6
130900970526     C                   MOVEL     '06'          FUNKEY
131000970526     C                   END
131100970526     C     KEY           IFEQ      F7
131200970526     C                   MOVEL     '07'          FUNKEY
131300970526     C                   END
131400970526     C     KEY           IFEQ      F8
131500970526     C                   MOVEL     '08'          FUNKEY
131600970526     C                   END
131700970526     C     KEY           IFEQ      F9
131800970526     C                   MOVEL     '09'          FUNKEY
131900970526     C                   END
132000970526     C     KEY           IFEQ      F10
132100970526     C                   MOVEL     '10'          FUNKEY
132200970526     C                   END
132300970526     C     KEY           IFEQ      F11
132400970526     C                   MOVEL     '11'          FUNKEY
132500970526     C                   END
132600970526     C     KEY           IFEQ      F12
132700970526     C                   MOVEL     '12'          FUNKEY
132800970526     C                   END
132900970526     C     KEY           IFEQ      F13
133000970526     C                   MOVEL     '13'          FUNKEY
133100970526     C                   END
133200970526     C     KEY           IFEQ      F14
133300970526     C                   MOVEL     '14'          FUNKEY
133400970526     C                   END
133500970526     C     KEY           IFEQ      F15
133600970526     C                   MOVEL     '15'          FUNKEY
133700970526     C                   END
133800970526     C     KEY           IFEQ      F16
133900970526     C                   MOVEL     '16'          FUNKEY
134000970526     C                   END
134100970526     C     KEY           IFEQ      F17
134200970526     C                   MOVEL     '17'          FUNKEY
134300970526     C                   END
134400970526     C     KEY           IFEQ      F18
134500970526     C                   MOVEL     '18'          FUNKEY
134600970526     C                   END
134700970526     C     KEY           IFEQ      F19
134800970526     C                   MOVEL     '19'          FUNKEY
134900970526     C                   END
135000970526     C     KEY           IFEQ      F20
135100970526     C                   MOVEL     '20'          FUNKEY
135200970526     C                   END
135300970526     C     KEY           IFEQ      F21
135400970526     C                   MOVEL     '21'          FUNKEY
135500970526     C                   END
135600970526     C     KEY           IFEQ      F22
135700970526     C                   MOVEL     '22'          FUNKEY
135800970526     C                   END
135900970526     C     KEY           IFEQ      F23
136000970526     C                   MOVEL     '23'          FUNKEY
136100970526     C                   END
136200970526     C     KEY           IFEQ      F24
136300970526     C                   MOVEL     '24'          FUNKEY
136400970526     C                   END
136500970526     C     KEY           IFEQ      ROLLUP
136600970526     C                   MOVEL     'RU'          FUNKEY
136700970526     C                   END
136800970526     C     KEY           IFEQ      HELP
136900970526     C                   MOVEL     'HP'          FUNKEY
137000970526     C                   END
137100970526     C*
137200970526     C                   ENDSR
137300970526     C*------------------------------------------------------------------------*
137400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
137500970526     C*------------------------------------------------------------------------*
137600970526     C     FUNGES        BEGSR
137700970526     C*
137800970526     C* PULISCO CAMPI DI LAVORO
137900970526     C                   Z-ADD     0             W                 5 0
138000970526     C*
138100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
138200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
138300970526     C     1             DO        24            I
138400970526     C                   MOVEL     FUD(I)        FUDRIG
138500970526     C*
138600970526     C* PULIZIA FUNZIONI PRECEDENTI
138700970526     C                   MOVEL     *BLANKS       FUV(I)
138800970526     C*
138900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
139000970526     C* (SALTA F23 E F24)
139100970526     C     FUDATT        IFEQ      'S'
139200970526     C     FUDFUN        ANDNE     '23'
139300970526     C     FUDFUN        ANDNE     '24'
139400970526     C                   ADD       1             W
139500970526     C                   MOVEL     FUDDES        FUV(W)
139600970526     C                   END
139700970526     C                   END                                                    DO WHILE
139800970526     C*
139900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
140000970526     C* ALTRIMENTI DISATTIVO F24
140100970526     C     1             DO        24            I
140200970526     C                   MOVEL     FUD(I)        FUDRIG
140300970526     C*
140400970526     C     FUDFUN        IFEQ      '24'
140500970526     C     W             IFGT      6
140600970526     C                   MOVEL     'S'           FUDATT
140700970526     C                   ELSE
140800970526     C                   MOVEL     *BLANKS       FUDATT
140900970526     C                   END                                                    W GT 8
141000970526     C                   MOVEL     FUDRIG        FUD(I)
141100970526     C                   END
141200970526     C                   END                                                    DO WHILE
141300970526     C*
141400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
141500970526     C     W             DIV       6             I
141600970526     C                   MVR                     Z
141700970526     C*
141800970526     C     Z             IFNE      0
141900970526     C     I             ADD       1             Z
142000970526     C                   ELSE
142100970526     C                   Z-ADD     I             Z
142200970526     C                   END
142300970526     C*
142400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
142500970526     C                   ADD       1             FUNGRU
142600970526     C     FUNGRU        IFGT      Z
142700970526     C     FUNGRU        ORLT      1
142800970526     C                   Z-ADD     1             FUNGRU
142900970526     C                   END
143000970526     C*
143100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
143200970526     C     FUNGRU        IFEQ      1
143300970526     C                   MOVEL     FUV(1)        FUN001
143400970526     C                   MOVEL     FUV(2)        FUN002
143500970526     C                   MOVEL     FUV(3)        FUN003
143600970526     C                   MOVEL     FUV(4)        FUN004
143700970526     C                   MOVEL     FUV(5)        FUN005
143800970526     C                   MOVEL     FUV(6)        FUN006
143900970526     C                   END
144000970526     C*
144100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
144200970526     C     FUNGRU        IFEQ      2
144300970526     C                   MOVEL     FUV(7)        FUN001
144400970526     C                   MOVEL     FUV(8)        FUN002
144500970526     C                   MOVEL     FUV(9)        FUN003
144600970526     C                   MOVEL     FUV(10)       FUN004
144700970526     C                   MOVEL     FUV(11)       FUN005
144800970526     C                   MOVEL     FUV(12)       FUN006
144900970526     C                   END
145000970526     C*
145100970526     C* TERZO GRUPPO DI 6 FUNZIONI
145200970526     C     FUNGRU        IFEQ      3
145300970526     C                   MOVEL     FUV(13)       FUN001
145400970526     C                   MOVEL     FUV(14)       FUN002
145500970526     C                   MOVEL     FUV(15)       FUN003
145600970526     C                   MOVEL     FUV(16)       FUN004
145700970526     C                   MOVEL     FUV(17)       FUN005
145800970526     C                   MOVEL     FUV(18)       FUN006
145900970526     C                   END
146000970526     C*
146100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
146200970526     C     FUNGRU        IFEQ      4
146300970526     C                   MOVEL     FUV(19)       FUN001
146400970526     C                   MOVEL     FUV(20)       FUN002
146500970526     C                   MOVEL     FUV(21)       FUN003
146600970526     C                   MOVEL     FUV(22)       FUN004
146700970526     C                   MOVEL     *BLANKS       FUN005
146800970526     C                   MOVEL     *BLANKS       FUN006
146900970526     C                   END
147000970526     C*
147100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
147200970526     C                   MOVEL     *BLANKS       FUN007
147300970526     C                   MOVEL     *BLANKS       FUN008
147400970526     C*
147500970526     C     1             DO        24            I
147600970526     C                   MOVEL     FUD(I)        FUDRIG
147700970526     C     FUDFUN        IFEQ      '23'
147800970526     C     FUDATT        ANDEQ     'S'
147900970526     C                   MOVEL     FUDDES        FUN007
148000970526     C                   END
148100970526     C     FUDFUN        IFEQ      '24'
148200970526     C     FUDATT        ANDEQ     'S'
148300970526     C                   MOVEL     FUDDES        FUN008
148400970526     C                   END
148500970526     C                   END                                                    DO
148600970526     C*
148700970526     C                   ENDSR
148800970526     C*------------------------------------------------------------------------*
148900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
149000970526     C*------------------------------------------------------------------------*
149100970526     C     OPZGES        BEGSR
149200970526     C*
149300970526     C* PULISCO CAMPI DI LAVORO
149400970526     C                   Z-ADD     0             W                 5 0
149500970526     C*
149600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
149700970526     C* E CARICO TABELLA OPZIONI ATTIVE
149800970526     C     1             DO        24            I
149900970526     C                   MOVEL     OPD(I)        OPDRIG
150000970526     C*
150100970526     C* PULIZIA OPZIONI PRECEDENTI
150200970526     C                   MOVEL     *BLANKS       OPV(I)
150300970526     C*
150400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
150500970526     C     OPDATT        IFEQ      'S'
150600970526     C                   ADD       1             W
150700970526     C                   MOVEL     OPDDES        OPV(W)
150800970526     C                   END
150900970526     C                   END                                                    DO WHILE
151000970526     C*
151100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
151200970526     C* ALTRIMENTI DISATTIVO F23
151300970526     C     1             DO        24            I
151400970526     C                   MOVEL     FUD(I)        FUDRIG
151500970526     C*
151600970526     C     FUDFUN        IFEQ      '23'
151700970526     C     W             IFGT      8
151800970526     C                   MOVEL     'S'           FUDATT
151900970526     C                   ELSE
152000970526     C                   MOVEL     *BLANKS       FUDATT
152100970526     C                   END                                                    W GT 8
152200970526     C                   MOVEL     FUDRIG        FUD(I)
152300970526     C                   END
152400970526     C                   END                                                    DO WHILE
152500970526     C*
152600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
152700970526     C     W             DIV       8             I
152800970526     C                   MVR                     Z                 5 0
152900970526     C*
153000970526     C     Z             IFNE      0
153100970526     C     I             ADD       1             Z
153200970526     C                   ELSE
153300970526     C                   Z-ADD     I             Z
153400970526     C                   END
153500970526     C*
153600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
153700970526     C                   ADD       1             OPZGRU
153800970526     C     OPZGRU        IFGT      Z
153900970526     C     OPZGRU        ORLT      1
154000970526     C                   Z-ADD     1             OPZGRU
154100970526     C                   END
154200970526     C*
154300970526     C* PRIMO GRUPPO DI 8 OPZIONI
154400970526     C     OPZGRU        IFEQ      1
154500970526     C                   MOVEL     OPV(1)        OPZ001
154600970526     C                   MOVEL     OPV(2)        OPZ002
154700970526     C                   MOVEL     OPV(3)        OPZ003
154800970526     C                   MOVEL     OPV(4)        OPZ004
154900970526     C                   MOVEL     OPV(5)        OPZ005
155000970526     C                   MOVEL     OPV(6)        OPZ006
155100970526     C                   MOVEL     OPV(7)        OPZ007
155200970526     C                   MOVEL     OPV(8)        OPZ008
155300970526     C                   END
155400970526     C*
155500970526     C* SECONDO GRUPPO DI 8 OPZIONI
155600970526     C     OPZGRU        IFEQ      2
155700970526     C                   MOVEL     OPV(9)        OPZ001
155800970526     C                   MOVEL     OPV(10)       OPZ002
155900970526     C                   MOVEL     OPV(11)       OPZ003
156000970526     C                   MOVEL     OPV(12)       OPZ004
156100970526     C                   MOVEL     OPV(13)       OPZ005
156200970526     C                   MOVEL     OPV(14)       OPZ006
156300970526     C                   MOVEL     OPV(15)       OPZ007
156400970526     C                   MOVEL     OPV(16)       OPZ008
156500970526     C                   END
156600970526     C*
156700970526     C* TERZO GRUPPO DI 8 OPZIONI
156800970526     C     OPZGRU        IFEQ      3
156900970526     C                   MOVEL     OPV(17)       OPZ001
157000970526     C                   MOVEL     OPV(18)       OPZ002
157100970526     C                   MOVEL     OPV(19)       OPZ003
157200970526     C                   MOVEL     OPV(20)       OPZ004
157300970526     C                   MOVEL     OPV(21)       OPZ005
157400970526     C                   MOVEL     OPV(22)       OPZ006
157500970526     C                   MOVEL     OPV(23)       OPZ007
157600970526     C                   MOVEL     OPV(24)       OPZ008
157700970526     C                   END
157800970526     C*
157900970526     C                   ENDSR
158000970526     C*------------------------------------------------------------------------*
158100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
158200970526     C*------------------------------------------------------------------------*
158300970526     C     OPZCON        BEGSR
158400970526     C*
158500970526     C* PULISCO CAMPI DI LAVORO
158600970526     C                   Z-ADD     0             I                 5 0
158700970526     C                   Z-ADD     0             W                 5 0
158800970526     C*
158900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
159000970526     C     1             DO        24            I
159100970526     C                   MOVEL     OPD(I)        OPDRIG
159200970526     C*
159300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
159400970526     C     OPZKEY        IFEQ      OPDOPZ
159500970526     C     OPZKEY        OREQ      OPDOP1
159600970526     C*
159700970526     C     OPDATT        IFEQ      'S'
159800970526     C                   MOVEL     OPDOPZ        OPZKEY
159900970526     C                   Z-ADD     I             W
160000970526     C                   END
160100970526     C                   END
160200970526     C*
160300970526     C                   END                                                    DO WHILE
160400970526     C*
160500970526     C* OPZIONE NON ATTIVA (ERRORE)
160600970526     C     W             IFEQ      0
160700970526     C*
160800970526     C* OPZIONE NON OK
160900970526     C                   Z-ADD     1             OPZOK
161000970526     C*
161100970526     C* EMISSIONE MESSAGGIO DI ERRORE
161200970526     C                   MOVEL     MSG(1)        DSMSMS
161300970526     C                   ELSE
161400970526     C*
161500970526     C* OPZIONE ATTIVA (PROCEDI ...)
161600970526     C*
161700970526     C     OPZKEY        IFEQ      ' 1'
161800970526     C                   EXSR      O01GES
161900970526     C                   END
162000970526     C     OPZKEY        IFEQ      ' 2'
162100970526     C                   EXSR      O02GES
162200970526     C                   END
162300970526     C     OPZKEY        IFEQ      ' 4'
162400970526     C                   EXSR      O04GES
162500970526     C                   END
162600970526     C     OPZKEY        IFEQ      ' 5'
162700970526     C                   EXSR      O05GES
162800970526     C                   END
162900970526     C     OPZKEY        IFEQ      ' 7'
163000970526     C                   EXSR      O07GES
163100970526     C                   END
163200970526     C*
163300970526     C                   END                                                    OPZ.NON ATTIVA
163400970526     C*
163500970526     C                   ENDSR
163600970526     C*------------------------------------------------------------------------*
163700970526     C* NOTE COSTRUZIONE SCHIERE
163800970526     C*------------------------------------------------------------------------*
163900970526     C*
164000970526     C* -MSG-MESSAGGI
164100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
164200970526     C* (NON TOCCARE MSG,1 E MSG,2)
164300970526     C*
164400970526     C* -CMD-COMANDI
164500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
164600970526     C*
164700970526     C* -OPD-OPZIONI
164800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
164900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
165000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
165100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
165200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
165300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
165400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
165500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
165600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
165700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
165800970526     C*
165900970526     C* -FUD-FUNZIONI
166000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
166100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
166200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
166300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
166400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
166500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
166600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
166700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
166800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
166900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
167000970526     O*------------------------------------------------------------------------*
167100970526** DEC - DECODIFICHE
167200970526    GESTIONE                                                     1
167300970526INTERROGAZIONE                                                   2
167400970530     SCELTA                                                      3
1675009705264
1676009705265
167700970526** MSG - MESSAGGI
167800060804TISIE5R- Opzione non gestita                                                   1
167900060804TISIE5R- Tasto funzionale non gestito                                          2
168000060804TISIE5R- 3
168100060804TISIE5R- Record annullato. Consentito solo il ripristino                       4
168200060804TISIE5R- Record non annullato. Impossibile ripristinare                        5
168300060804TISIE5R- Accettato solo un parametro di inizio lista                           6
168400060804TISIE5R- Errori nei parametri di ingresso                                      7
168500060804TISIE5R- Data errata                                                           8
168600060804TISIE5R- 9                                                                     9
168700060804TISIE5R- 10                                                                   10
168800060804TISIE5R- 11                                                                   11
168900060901TISIE5R- Data errata                                                          12
169000060804TISIE5R- 13                                                                   13
169100060804TISIE5R- 14                                                                   14
169200060804TISIE5R- 15                                                                   15
169300060804TISIE5R- 16                                                                   16
169400060804TISIE5R- 17                                                                   17
169500060804TISIE5R- 18                                                                   18
169600060804TISIE5R- 19                                                                   19
169700060804TISIE5R- 20                                                                   20
169800970526** CMD - COMANDI CL
169900970526WRKSPLF
170000970526
170100970526
170200970526
170300970526
170400970526** OPD - OPZIONI
170500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
170600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
170700000120                   -3 -     (S/N)=N                        opz. 03¬
170800970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
170900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
171000970526                   -6 -     (S/N)=N                        opz. 06¬
171100970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
171200000119                   -8 -     (S/N)=N                        opz. 08¬
171300000119                   -9 -     (S/N)=N                        opz. 09¬
171400000119                   -10-     (S/N)=N                        opz. 10¬
171500970526                   -  -     (S/N)=N                        opz. 11¬
171600970526                   -  -     (S/N)=N                        opz. 12¬
171700970526                   -  -     (S/N)=N                        opz. 13¬
171800970526                   -  -     (S/N)=N                        opz. 14¬
171900970526                   -  -     (S/N)=N                        opz. 15¬
172000970526                   -  -     (S/N)=N                        opz. 16¬
172100970526                   -  -     (S/N)=N                        opz. 17¬
172200970526                   -  -     (S/N)=N                        opz. 18¬
172300970526                   -  -     (S/N)=N                        opz. 19¬
172400970526                   -  -     (S/N)=N                        opz. 20¬
172500970526                   -  -     (S/N)=N                        opz. 21¬
172600970526                   -  -     (S/N)=N                        opz. 22¬
172700970526                   -  -     (S/N)=N                        opz. 23¬
172800970526                   -  -     (S/N)=N                        opz. 24¬
172900970526** FUD - FUNZIONI
173000970526                   -        (S/N)=N                        funz.01¬
173100970526                   -        (S/N)=N                        funz.02¬
173200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
173300970526                   -        (S/N)=N                        funz.04¬
173400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
173500970526F06=Inserimento    -        (S/N)=N                        funz.06¬
173600061012                   -        (S/N)=N                        funz.07¬
173700000119                   -        (S/N)=N                        funz.08¬
173800060919F09=Altri Dati     -        (S/N)=S                        funz.09¬
173900000119                   -        (S/N)=N                        funz.10¬
174000970526                   -        (S/N)=N                        funz.11¬
174100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
174200970526                   -        (S/N)=N                        funz.13¬
174300970526                   -        (S/N)=N                        funz.14¬
174400970526                   -        (S/N)=N                        funz.15¬
174500970526                   -        (S/N)=N                        funz.16¬
174600970526                   -        (S/N)=N                        funz.17¬
174700970526                   -        (S/N)=N                        funz.18¬
174800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
174900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
175000060804F21=Stampa elenco  -        (S/N)=S                        funz.21¬
175100970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
175200970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
175300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
