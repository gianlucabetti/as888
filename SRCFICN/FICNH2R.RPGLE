000100970526     H*------------------------------------------------------------------------*
000200150203     H* Gestione file anagrafico e Bancali
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800160111     Fficnh2d   CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG1D05:SFLULT)
001000021211     F                                     SFILE(SIG1DMS:MSGULT)
001100970526     F*------------
001200160111     Ftnsre02l  if   E           K DISK
001201160111     Fanfrn01l  IF   E           K DISK
001202160111     Fansog01l  IF   E           K DISK
001301150924     Ftntbe01l  if   E           K DISK
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
004500991108     D*------------
004600970526     D*------------------------------------------------------------------------*
004700970526     D* INPUT
004800970526     D*------------------------------------------------------------------------*
004900970526     D*------------
005000970526     D* RIDEFINIZIONE OPZIONI VIDEO
005100970526     D*------------
005200970526     D OPDDS           DS
005300970526     D  OPDOPZ                 1      2
005400970526     D  OPDDE1                 3     19
005500970526     D  OPDOP1                21     22
005600970526     D  OPDFI0                23     34
005700970526     D  OPDATT                35     35
005800970526     D  OPDFI1                36     80
005900970526     D  OPDDES                 1     19
006000970526     D  OPDRIG                 1     80
006100970526     D*------------
006200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006300970526     D*------------
006400970526     D FUDDS           DS
006500970526     D  FUDDE1                 1      1
006600970526     D  FUDFUN                 2      3
006700970526     D  FUDDE2                 4     19
006800970526     D  FUDFI0                20     34
006900970526     D  FUDATT                35     35
007000970526     D  FUDFI1                36     80
007100970526     D  FUDDES                 1     19
007200970526     D  FUDRIG                 1     80
007300970526     D*------------
007400970526     D* DS GESTIONE TASTI FUNZIONALI
007500970526     D*------------
007600970526     D DSKEY         E DS
007700970526     D DEVDS1          DS
007800970526     D  KEY                  369    369
007801150930     D  NRG                  370    370
007802150930     D  NCL                  371    371
007803150930     D  SFLNRR               378    379B 0
007900970526     D*------------
008000970526     D* DS "XMSG" - GESTIONE MESSAGGI
008100970526     D*------------
008200970526     D DSMSG         E DS
008300970526     D*------------
008400020917     D* DS REPERIMENTO DATI UTENTE
008500970526     D*------------
008600020917     D TIBS34DS      E DS                                                       *Profili utente
008700020917     D DDATIUTE      E DS                                                       *Dati utente
008800020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008900150212     D dute01        E DS                                                       *Utenti
009200150212     D*-------------------
009300150212     D TRUL31DS      E DS
009400150212     D TRUL31DS2     E DS
009401160111     d dsre          e ds
009500150909     d wabi            s                   like(UteAut)
009700150909     d dataiso         s               d
009800150909     d dataeur         s               d   datfmt(*eur)
009801151006     d dtacor          s              8s 0
009900150212     d* p.o. abilitati
009901150928     D warea           s              3  0
010000150909     D $pogn           s              3  0 dim(250)
010100150212     D $pog            s              3    dim(250)
010200150212     D $DIG            s              1    dim(20)
010300150212     D $ARG            s              3    dim(50)
010301150928     D $pogX           s              3  0 dim(250) inz
010302150928     D $pogA           s              3  0 dim(250) inz
010303150928     D $pogD           s              3  0 dim(250) inz
010400970527     D*------------------
010500991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
010600970527     D*------------------
010700970527     D WLBDA8          DS                  INZ
010800970527     D  G08DAT                 1      8  0
010900970527     D  G08INV                 9     16  0
011000970527     D  G08ERR                17     17
011100970527     D  G08TGI                18     22  0
011101151106     C*
011102151106     d Wdata8          DS
011103151106     d  dadata                 1      8  0
011104151106     d  adata                  9     16  0
011105151106     d  GioLav                17     21  0
011200970526     D*------------
011300991110     D* DS DI PROCEDURA
011400970526     D*------------
011500970526     D* ARCHITETTURA
011600970526     D*------------
011700970526     D KPJBA         E DS
011701150917     d  param          ds
011702160111     d   parsoc                       3
011703160111     d   parfor                       8
011704160111     d   parcau                       3
011705160111     d   pardat                       8s 0
011706160111     d   paropz                       1
011800160111     d ficnh1i       E DS                  INZ
011900150909     D*------------
012000150909     D* TABELLE "XCLIR"- RICERCA CLIENTI
012100150909     D*------------
013200150211     D savKPJBu        s                   like(kpjbu)
013901150917     d pmsg            S             80            inz('0')
014700150324      *---------------------------------------------------------------------
014800150909     d w_err           S              1            inz('0')
014900150324      * specifiche per ordinamento forzato sfl
015000150324     d NotUsed         S             16A
015100150324     d ReturnSize      S              9B 0
015200150324     d SizeList        S              9B 0
015300150324     d righe           S              4  0
015400150324     d w_ord           S              1  0         inz(1)
015401150925     d costf81         C                   const('F08=Ord.x causale  ')
015402150925     d costf82         C                   const('F08=Ord.x data/caus')
015500150324     d*---
015600150324     d MaxKey          C                   2
015700150324     d Ascendente      C                   1
015800150324     d Discendente     C                   2
015900150324     d Carattere       C                   6
016000150324     d Put             C                   1
016100150324     d EndPut          C                   2
016200150324     d Get             C                   3
016300150324     d Numerico        C                   8
016400150324     d SflRcd          DS
016402160111     d  Vcau
016403160111     d  Vsoc
016500160111     d  Vfor
016501160114     d  Vtsr
016600160111     d  Vdat
016800160111     d  VcauD
017000160111     d  VForD
017200150908     d  VIMP
017201160111     d  Vdat1g
017202160111     d  Vnff
017203160112     d  Vdft
017204160112     d  Vcont
017900150908     d  OPZKEY
018000150324     d  Selected                      1A
018100150324
018200150324      /COPY QSYSINC/QRPGLESRC,QLGSORT
018300150324     d QLGKL                         16    DIM(MaxKey)
018400150324     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
018500150324     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
018600150324     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
018700150324     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
018800150324      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
018900150324      /COPY QSYSINC/QRPGLESRC,QUSEC
019000970526     C*------------------------------------------------------------------------*
019100970526     C* MAIN LINES
019200970526     C*------------------------------------------------------------------------*
019300970526     C*
019400970526     C* SE NON CI SONO ERRORI, PROSEGUE
019500970526IF  1C     ERRL00        IFNE      '1'
019600970526     C*
019700970526     C* CARICA IL SFILE
019800970526     C                   EXSR      POSSFL
019900970526     C*
020000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
020100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
020200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
020300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
020400970526     C*
020500970526     C* REIMPOSTA ERRORE DI PROCERURA
020600970526     C                   MOVEL     '0'           ERRL00
020700970526     C*
020800970526     C* EMETTE IL SFILE
020900970526     C                   EXSR      OUTSFL
021000970526     C*
021100970526     C* GESTIONE TASTI FUNZIONALI
021200970526     C                   EXSR      FUNCON
021300970526     C*
021400970526E   2C                   ENDDO
021500970527     C*
021600970526E   1C                   ENDIF
021700991108     C*
021800970526     C* FINE PGM
021900970526     C                   SETON                                        LR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F03GES        BEGSR
022400970526     C*
022500970526     C* IMPOSTA "PREMUTO F03"
022600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
022700970526     C*
022800970526     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
023100970526     C*------------------------------------------------------------------------*
023200970526     C     F05GES        BEGSR
023300970526     C*
023400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800150917     C* F10GES - GESTIONE F10 -> INSERIMENTO
023900970526     C*------------------------------------------------------------------------*
024000150917     C     F10GES        BEGSR
024100970526     C*
024200021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
024300150213     C*    DUTLPO        IFNE      'S'
024400150213     C*                  MOVEL     '1'           ERRL00
024500150213     C*                  MOVEL     MSG(3)        DSMSMS
024600150213     C*                  ENDIF
024700000407     C*
024800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024900150917     C*                  MOVEL     'F10'         POP1              3
025000150917     C                   clear                   param                               *RICARICA SFILE
025001150917     c                   movel     param         kpjbu
025100160111     C                   CALL      'FICNH3R'
025200000407     C                   PARM                    KPJBA
025300021211     C                   MOVEL     KPJBU         PARAM
025400000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
025500021211     C                   ENDIF
025600000407     C*
025700970526     C                   ENDSR
025800150324     C*------------------------------------------------------------------------*
025900150909     C* F08GES - GESTIONE F11 -> ORDINAMENTO
026000150324     C*------------------------------------------------------------------------*
026100150909     C     F08GES        BEGSR
026200150324     C*
026300150908     C                   exsr      ordina
026400150324     C*
026500150324     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700991108     C* F12GES - GESTIONE F12 -> RITORNO
026800970526     C*------------------------------------------------------------------------*
026900970526     C     F12GES        BEGSR
027000970526     C*
027100970526     C* IMPOSTA "PREMUTO F12"
027200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
027300970526     C*
027400970526     C                   ENDSR
027500970528     C*------------------------------------------------------------------------*
027600970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
027700970528     C*------------------------------------------------------------------------*
027800970528     C     F21GES        BEGSR
027900970612     C*
028000991117     C* NON GESTITA
028100970528     C*
028200970528     C                   ENDSR
028300970528     C*------------------------------------------------------------------------*
028400970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
028500970528     C*------------------------------------------------------------------------*
028600970528     C     F22GES        BEGSR
028700970528     C*
028800021211     C* NON GESTITA
028900970528     C*
029000970528     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
029300970526     C*------------------------------------------------------------------------*
029400970526     C     F23GES        BEGSR
029500970526     C*
029600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
029700970526     C*
029800970526     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
030100970526     C*------------------------------------------------------------------------*
030200970526     C     F24GES        BEGSR
030300970526     C*
030400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
030500970526     C*
030600970526     C                   ENDSR
030700970526     C*------------------------------------------------------------------------*
030800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
030900970526     C*------------------------------------------------------------------------*
031000970526     C     RUPGES        BEGSR
031100970526     C*
031200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
031300970526     C*
031400970526     C                   ENDSR
031500970526     C*------------------------------------------------------------------------*
031600991108     C* ENTGES - GESTIONE ENTER -> INVIO
031700970526     C*------------------------------------------------------------------------*
031800970526     C     ENTGES        BEGSR
031900991108     C*
032000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032100970526     C                   EXSR      NEWPOS
032200021211     C*
032300021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
032400021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032500021211     C     SWPOSI        IFNE      0
032600021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
032700021211     C                   ELSE
032800021211     C     SFLULT        IFNE      0
032900021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
033000021211     C*
033100021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
033200021211     C     SWMODI        IFNE      0
033300021211     C                   EXSR      POSSFL
033400021211     C                   ENDIF
033500021211     C                   ENDIF
033600021211     C                   ENDIF
033700021211     C                   ENDIF
033800021211     C*
033900021211     C                   ENDSR
034000000420     C*------------------------------------------------------------------------*
034100000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
034200000420     C*------------------------------------------------------------------------*
034300000420     C     O01GES        BEGSR
034400000420     C*
034500000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
034600150908     C*                  MOVEL     VIDksu        Pksu
034700150908     C*                  MOVEL     PARAM         KPJBU
034800150908     C*                  MOVEL     '1'           FINL00                         *FINE PGM
034900000420     C*
035000000420     C                   ENDSR
035100000407     C*------------------------------------------------------------------------*
035200000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
035300000407     C*------------------------------------------------------------------------*
035400000407     C     O02GES        BEGSR
035500000407     C*
035600021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
035700160111     C     DUTLPO        IFNE      'S'
035800160111     C                   MOVEL     '1'           ERRL00
035900160111     C                   MOVEL     MSG(3)        pmsg
036000160111     C                   ENDIF
036100021211     C*
036101160112     C     vnff          IFgt      0
036102160111     C                   MOVEL     '1'           ERRL00
036103160111     C                   MOVEL     MSG(4)        pmsg
036104160111     C                   ENDIF
036105160111     C*
037100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037200160111     C                   move      Vsoc          parsoc
037201160111     C                   move      Vfor          parfor
037202160111     C                   move      Vcau          parcau
037203160111     C                   move      Vdat1g        pardat
037300150917     C                   MOVEL     '2'           paropz
037400150917     C                   movel     param         kpjbu                          *RICARICA SFILE
037500160111     C                   CALL      'FICNH3R'
037600000407     C                   PARM                    KPJBA
037700021211     C                   MOVEL     KPJBU         PARAM
037800021211     C                   ENDIF
037801150922     c                   move      '1'           SWMODI
037900000407     C*
038000000407     C                   ENDSR
038100021211     C*------------------------------------------------------------------------*
038200021211     C* O03GES - GESTIONE OPZIONE 3= COPIA
038300021211     C*------------------------------------------------------------------------*
038400021211     C     O03GES        BEGSR
038500021211     C*
038600021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
038700150213     C*    DUTLPO        IFNE      'S'
038800150213     C*                  MOVEL     '1'           ERRL00
038900150213     C*                  MOVEL     MSG(3)        PMSG
039000150213     C*                  ENDIF
039100021211     C*
039200150908     C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
039300150908     C*                  MOVEL     VIDksu        Pksu
039400150908     C*                  MOVEL     VIDtsc        Ptsc
039500150908     c*                  movel     viddtig       pdti
039600150908     C*                  MOVEL     'O03'         POP1
039700150908     C*                  EXSR      PASPAR                                       *RICARICA SFILE
039800150908     C*                  CALL      'TISIG11R'
039900150908     C*                  PARM                    KPJBA
040000150908     C*                  MOVEL     KPJBU         PARAM
040100150908     C*                  ENDIF
040200021211     C*
040300021211     C                   ENDSR
040400991112     C*------------------------------------------------------------------------*
040500021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
040600991112     C*------------------------------------------------------------------------*
040700991112     C     O04GES        BEGSR
040701160111     C     DUTLPO        IFNE      'S'
040702160111     C                   MOVEL     '1'           ERRL00
040703160111     C                   MOVEL     MSG(3)        PMSG
040704160111     C                   ENDIF
040705160121     C*
040706160121     C     vnff          IFgt      0
040707160121     C                   MOVEL     '1'           ERRL00
040708160121     C                   MOVEL     MSG(4)        pmsg
040709160121     C                   ENDIF
040710160121     C*
041400991112     C*
041500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041600021211     C*
041700021211     C*
041701160111     C                   move      Vsoc          parsoc
041702160111     C                   move      Vfor          parfor
041703160111     C                   move      Vcau          parcau
041704160111     C                   move      Vdat1g        pardat
041706150917     C                   MOVEL     '4'           paropz
041707150917     C                   movel     param         kpjbu                          *RICARICA SFILE
041708160111     C                   CALL      'FICNH3R'
042200000407     C                   PARM                    KPJBA
042300021211     C                   MOVEL     KPJBU         PARAM
042400000411     C                   ENDIF
042401150918     c                   move      '1'           SWMODI
042500991112     C*
042600991112     C                   ENDSR
042700991014     C*------------------------------------------------------------------------*
042800000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
042900970526     C*------------------------------------------------------------------------*
043000970526     C     O05GES        BEGSR
043100000407     C*
043200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043201160111     C                   move      Vsoc          parsoc
043202160111     C                   move      Vfor          parfor
043203160111     C                   move      Vcau          parcau
043204160111     C                   move      Vdat1g        pardat
043205160111     C                   MOVEL     '5'           paropz
043206160111     C                   movel     param         kpjbu                          *RICARICA SFILE
043207160111     C                   CALL      'FICNH3R'
043700000407     C                   PARM                    KPJBA
043800021211     C                   ENDIF
043900000407     C*
044000970526     C                   ENDSR
049300991028     C*------------------------------------------------------------------------*
049400991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
049500991112     C*------------------------------------------------------------------------*
049600991112     C     PASPAR        BEGSR
049700991112     C*
049800000407     C                   MOVEL     PARAM         KPJBU
049900991112     C*
050000991112     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
050300970526     C*------------------------------------------------------------------------*
050400970526     C     NEWPOS        BEGSR
050500970526     C*
050600970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
050700970526     C*         1= RICHIESTO POSIZIONAMENTO
050800970526     C                   Z-ADD     0             SWPOSI            7 0
050900970526     C*
051000991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
051100970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
051200970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
051300970526     C                   Z-ADD     0             SWMODI            1 0
051400970530     C*
051500970530     C* REIMPOSTA INDICATORI DI ERRORE
051600991108     C                   SETOFF                                       808182
051700991108     C                   SETOFF                                       83
051800970526     C*
051900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
052000160111     C     cnhsoc        IFNE      depsoc
052100160111     C     cnhfor        ORNE      depfor
052900970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
053000021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
053100150923     C*                  Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
053200160111     C                   MOVEL     cnhsoc        depsoc                         *DEPOSITI
053300160111     C                   MOVEL     cnhfor        depfor                         *DEPOSITI
054100021211     C                   ENDIF
054200021211     C                   ENDIF
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
054700970526     C*------------------------------------------------------------------------*
054800970528     C     SETFIL        BEGSR
054900970526     C*
055000970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
055100160111     C                   MOVEL     cnhsoc        KEYsoc
055101160111     C                   MOVEL     cnhfor        KEYfor
055200021211     C     WTIPRI        IFEQ      'COD'
055300160111     C     KEYsre        SETLL     tnsre02l                           99
055400021211     C                   ENDIF
055500980107     C*
055600021211     C* LETTURA RECORD
055700021211     C     *IN99         IFEQ      *OFF
055800970526     C                   EXSR      LETFIL
055900021211     C                   ENDIF
056000970526     C*
056100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
056200021211     C     *IN99         IFEQ      *ON
056300970526     C                   Z-ADD     1             SFLEOF
056400021211     C                   ENDIF
056500970526     C*
056600970526     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800970526     C* LETFIL - LETTURA RECORD
056900970526     C*------------------------------------------------------------------------*
057000970526     C     LETFIL        BEGSR
057100970526     C*
057200970526     C* LEGGE FINO A:
057300970526     C*     - FINE FILE
057400970526     C*     - TROVATO RECORD VALIDO
057500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
057600021211     C     *IN99         DOWEQ     *OFF
057700970526     C     WRECOK        ANDEQ     'N'
057800970526     C*
057900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
058000021211     C     WTIPRI        IFEQ      'COD'                                        *PER COD. ARTICOLO
058100160111     C                   READ      tnsre02l                               99
058200021211     C                   ENDIF
058300970528     C*
058400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
058500021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
058600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
058700021211     C                   ELSE
058800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
058900021211     C                   ENDIF
059000021211     C                   ENDDO
059100970526     C*
059200970526     C                   ENDSR
059300970526     C*------------------------------------------------------------------------*
059400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
059500970526     C*------------------------------------------------------------------------*
059600970526     C     CHKREC        BEGSR
059700970526     C*
059800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
059801160111      * causale
059802160111     C     cnhcont       ifeq      'N'
059804160111     C     srenff        andne     0
059805160111     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059807160111     C                   endif
059900150915      * causale
060000160111     C     cnhcau        ifeq      *blank
060100150915     C                   else
060200160111     C     sresre        ifne      cnhcau
060300150915     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
060400150915     C                   endif
060500150915     C                   endif
060600160111     C* FILTRA SE RICHIESTa società
060700160111     C     cnhsoc        ifeq      *blank
060800021211     C                   else
060900160111     C     sresoc        ifne      cnhsoc
061000021211     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
061100021211     C                   endif
061200021211     C                   endif
061300150908      * cliente
061400160111     C     cnhfor        ifeq      *blank
061500150908     C                   else
061600160111     C     srecdf        ifne      cnhfor
061700150908     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
061800150908     C                   endif
061900150908     C                   endif
061901160115      * tipo servizio
061902160115     C     cnhtsr        ifeq      *blank
061903160115     C                   else
061904160115     C     sretsr        ifne      cnhtsr
061905160115     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
061906160115     C                   endif
061907160115     C                   endif
063400150915      * filtra PERIODO DATE
063500160111     C     cnhdat1       ifeq      0
063600150219     C                   else
063700160111     C     sreddc        ifLT      cnhDAT1
063800160111     C     sreddc        orGT      cnhDAT2
063900150219     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
064000150219     C                   endif
064100150909     c                   endif
065600150909      *verifica abilitazioni alle filiali per profilo
065700160111     c*    tnafila       lookup    $pogN                                  60
065800160111     c* n60tnafild       lookup    $pogN                                  60
065900160111     c* n60              movel     'N'           wrecok
066000150325     C*
066100970526     C                   ENDSR
066200150325     C*----------------------------------------------------*
066300150325     C*  CONTROLLO NUMERICITA' CAMPI
066400150325     C*----------------------------------------------------*
066500150325     C     CHKNUM        BEGSR
066600150325     C*
066700150325     C                   IF        PiDecChr = *blanks
066800150325     C                   EVAL      PiDecChr = '.'
066900150325     C                   ENDIF
067000150325     C*
067100150325     C                   CALL(e)   'ISNUMERIC'
067200150325     C                   PARM                    PiStr            30
067300150325     C                   PARM                    PiDecChr          1
067400150325     C                   PARM      *ZEROS        PiVal            30 9
067500150325     C                   PARM      '0'           PiInt             1
067600150325     C                   PARM      '0'           PiNum             1
067700150325     C                   IF        %error
067800150325     C                   EVAL      PiNum=*off
067900150325     C                   ENDIF
068000150325     C*
068100150325     C                   ENDSR
070200970526     C*------------------------------------------------------------------------*
070300000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
070400970526     C*------------------------------------------------------------------------*
070500970526     C     IRESFL        BEGSR
070600970526     C*
070700000407     C                   MOVEL     *blanks       OPZKEY
070800160111     C                   MOVEL     sresre        Vcau
070900160111     C                   MOVEL     sresoc        Vsoc
071000160111     C                   MOVEL     srecdf        Vfor
071400160111     C                   z-add     sretim        VIMP
071401160114     C                   MOVEL     sretsr        Vtsr
071701160111     C                   move      sreddc        Vdat1g
071702160111     c                   clear                   vdat
071703160111     c                   if        sreddc > 0
071704160111     c                   move      sreddc        dataiso
071705160111     c                   move      dataiso       dataeur
071706160111     c                   move      dataeur       vdat
071707160111     c                   endif
071708160111     C                   move      srenff        Vnff
071709160112     c                   clear                   vdft
071710160112     c                   clear                   vcont
071711160112     c                   if        sredft > 0
071712160112     c                   move      sredft        dataiso
071713160112     c                   move      dataiso       dataeur
071714160112     c                   move      dataeur       vdft
071715160112     c                   move      'S'           vcont
071716160112     c                   endif
071717150924      *
071718160111     C                   clear                   Vcaud
071719160111     c                   movel     'SRE'         ktipo
071720160111     c                   movel     sresre        ke1
071721150924     c     ktbe          chain     tntbe01l
071722150924     c                   if        %found(tntbe01l)
071723160111     c                   movel     tbeuni        dsre
071724160111     c                   movel     §sredes       vcaud
071725150924     c                   endif
071800150908      *
073100150202      * cerco i dati anagrafici
073101160111     c                   clear                   vford
073102160111     C     Kfrn          CHAIN     anfrn01l
073103160111     c                   if        %found(anfrn01l)
073104160111     c     frnsogg       chain     ansog01l
073105160111     c                   if        %found(ansog01l)
073106160111     c                   movel     sogdes        vford
073107160111     c                   endif
073108160111     c                   endif
074200970526     C                   ENDSR
074300970526     C*------------------------------------------------------------------------*
074400991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
074500970526     C*------------------------------------------------------------------------*
074600970526     C     CHKPAR        BEGSR
074700150909     c                   move      '0'           w_err
074800150909     c                   setoff                                       808382
074900970528     C*
075000021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
075100021211     C                   MOVEL     'COD'         WTIPRI            3
075200000407     C*
075300970526     C                   ENDSR
075400150915     C*------------------------------------------------------------------------*
075500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
075600970526     C*------------------------------------------------------------------------*
075700970526     C     CHKFOP        BEGSR
075800991112     C*
075900991112     C* OPZIONI
076000970626     C*
076100000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
076200150908     C*    DEPOP0        IFEQ      'P01'
076300150908     C*                  MOVEL     OPD(1)        OPDRIG                         *SCELTA
076400150908     C*                  MOVEL     'S'           OPDATT
076500150908     C*                  MOVEL     OPDRIG        OPD(1)
076600150908     C*                  MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
076700150908     C*                  MOVEL     'N'           OPDATT
076800150908     C*                  MOVEL     OPDRIG        OPD(5)
076900150908     C*                  MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
077000150908     C*                  MOVEL     'N'           fudATT
077100150908     C*                  MOVEL     FUDRIG        FUD(6)
077200150908     C*                  endif
077300000420     C* OPZIONI
077400160111     C     DEPOP0        IFEQ      'P05'
077500150909      *abilitato solo ad interrogare
077700970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
077800970626     C                   MOVEL     'S'           OPDATT
077900970626     C                   MOVEL     OPDRIG        OPD(5)
078000150212     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
078100150212     C                   MOVEL     'N'           OPDATT
078200150212     C                   MOVEL     OPDRIG        OPD(2)
078300150908     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
078400150908     C                   MOVEL     'N'           OPDATT
078500150908     C                   MOVEL     OPDRIG        OPD(4)
079800150908     C                   MOVEL     FUD(10)       FUDRIG                         *NO INSERIMENTO
079900150908     C                   MOVEL     'N'           fudATT
080000150908     C                   MOVEL     FUDRIG        FUD(10)
080100021211     C                   ENDIF
083000000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
083100160111     C     DEPOP0        IFEQ      'P02'
083300000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
083400970626     C                   MOVEL     'S'           OPDATT
083500000407     C                   MOVEL     OPDRIG        OPD(2)
083600150908     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
083700150908     C                   MOVEL     'S'           OPDATT
083800150908     C                   MOVEL     OPDRIG        OPD(4)
083900991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
084000991112     C                   MOVEL     'S'           OPDATT
084001160111     C                   MOVEL     OPDRIG        OPD(5)
085400150908     C                   MOVEL     FUD(10)       FUDRIG                         *INSERIMENTO
085500150908     C                   MOVEL     'S'           FUDATT
085600150908     C                   MOVEL     FUDRIG        FUD(10)
085700021211     C                   ENDIF
085800970626     C*
087200970526     C* GESTIONE OPZIONI A VIDEO
087300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
087400970526     C                   EXSR      OPZGES
087500970526     C*
087600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
087700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
087800970526     C                   EXSR      FUNGES
087900970526     C*
088000970526     C                   ENDSR
088100020917     C*--------------------------------------------------------------------------------------------*
088200020917     C* REPERISCE I DATI UTENTE
088300020917     C*--------------------------------------------------------------------------------------------*
088400020917     C     REPDATIUTE    BEGSR
088500020917     C*
088600020917     C* INIZIALIZZA VARIABILI DI WRK
088700020917     C                   CLEAR                   TIBS34DS
088800020917     C                   CLEAR                   AZUTEDS
088900020917     C                   CLEAR                   DDATIUTE
089000150212     C                   CLEAR                   dute01
089100020917     C*
089200020917     C     *DTAARA       DEFINE    §azute        azuteds
089300020917     C     *DTAARA       DEFINE    §datiute      ddatiute
089400020917     C                   IN(E)     *DTAARA
089500021211     C                   IF        %Error
089600021211     C                   EVAL      I34Tla = 'L'
089700020917     C                   CALL      'TIBS34R'
089800020917     C                   PARM                    Tibs34Ds
089900020917     C                   IN        *DTAARA
090000021211     C                   ENDIF
090100150212
094401160111     C                   MOVEL     'P02'         DEPOP0            3
098000020917     C*
098100020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
098200020917     C                   MOVEL(P)  RSUT          DSFIRS           20
098300020917     C*
098400020917     C                   ENDSR
098500970526     C*------------------------------------------------------------------------*
098600970526     C* *INZSR - OPERAZIONI INIZIALI
098700970526     C*------------------------------------------------------------------------*
098800970526     C     *INZSR        BEGSR
098900970526     C*---
099000970526     C* RICEVIMENTO PARAMETRI
099100970526     C*---
099200970526     C     *ENTRY        PLIST
099300970612     C                   PARM                    KPJBA
099400160111     C                   MOVEL     KPJBU         ficnh1i
099500970526     C*
099501151006     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
099502151006     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
099503151006     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
099504151006     C                   move      WN8           dataeur
099505151006     C                   move      dataeur       dataiso                        *DATA CORRENTE AA/M/
099506151006     C                   move      dataiso       DtaCOR                         *DATA CORRENTE AA/M/
099600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
099700970526     C     XMSGPA        PLIST
099800970526     C                   PARM                    DSMSMS
099900970526     C                   PARM                    DSMSPG
100000970526     C                   PARM                    DSMSKE
100100970526     C                   PARM                    DSMSER
100200970526     C*
100300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
100400970526     C                   CALL      'XKEY'
100500970526     C                   PARM                    DSKEY
100600970526     C*---
100700970526     C* VARIABILI NON RIFERITE AL DATA BASE
100800970526     C*---
100900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
101000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
101100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
101200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
101300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
101400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
101500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
101600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
101700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
101800970526     C*---
101900160111     C     *LIKE         DEFINE    CNHSOC        depSOC
102000160111     C     *LIKE         DEFINE    CNHFOR        depFOR
102100160111     C     *LIKE         DEFINE    CNHDAT1       depDAT1
102101160111     C     *LIKE         DEFINE    CNHDAT2       depDAT2
102102160111     C     *LIKE         DEFINE    CNHCAU        depCAU
102103160115     C     *LIKE         DEFINE    CNHtsr        deptsr
102800991108     C*---
102900970526     C* VARIABILI DI CONTROLLO
103000970526     C*---
103100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
103200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
103300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
103400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
103500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
103600150928     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
103700970526     C*
103800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
103900160111     C                   MOVEL     'FICNH2R'     DSMSPG
104000160111     C                   MOVEL     'FICNH2R'     MSGQUE
104100970526     C                   MOVEL     *BLANKS       MSGKEY
104200970526     C                   MOVEL     *BLANKS       DSMSMS
104300970526     C                   MOVEL     *BLANKS       DSMSKE
104400970526     C                   Z-ADD     *ZEROS        DSMSER
104500970526     C*---
104600970526     C* CHIAVI DI LETTURA
104700970526     C*---
104800150908     C* LETTURA FNTNA02l - COMPLETA
104900160111     C     KEYsre        KLIST
105000160111     C                   KFLD                    KEYsoc            3
105001160111     C                   KFLD                    KEYfor            8
105002160111     C     Kfrn          KLIST
105003160111     C                   KFLD                    vsoc
105004160111     C                   KFLD                    vfor
105005150924     C* LETTURA FNTNA02l - COMPLETA
105006150924     C     Ktbe          KLIST
105007150924     C                   KFLD                    Ktipo             3
105008150924     C                   KFLD                    Ke1              15
105100970527     C* REPERIMENTO DATI UTENTE
105200020917     C                   EXSR      REPDATIUTE
105300970526     C*
105400970526     C* IMPOSTA I PARAMETRI DI INGRESSO
105500970526     C                   EXSR      CHKPAR
105600991115     C*
105700970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
105800970526     C                   EXSR      CHKFOP
105900970526     C*
106000970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
106100150924     C     DEPOP0        IFEQ      'P01'
106200150924     C*                  MOVEL     DEC(3)        VIDING
106300021211     C                   ENDIF
106400021211     C     DEPOP0        IFEQ      'P02'
106500150924     C*                  MOVEL     DEC(1)        VIDING
106600021211     C                   ENDIF
106700021211     C     DEPOP0        IFEQ      'P05'
106800150924     C*                  MOVEL     DEC(2)        VIDING
106900021211     C                   ENDIF
107000970526     C*
107100970526     C                   ENDSR
107200970526     C*------------------------------------------------------------------------*
107300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107500970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
107600970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
107700970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
107800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108000970526     C*------------------------------------------------------------------------*
108100970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
108200970526     C*------------------------------------------------------------------------*
108300970526     C     POSSFL        BEGSR
108400970526     C*
108500970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
108600970526     C                   EXSR      CLRSFL
108700970526     C*
108800970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
108900970526     C                   EXSR      SETFIL
109000970526     C*
109100970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
109200970526     C     SFLEOF        IFEQ      0
109300970526     C                   EXSR      CARSFL
109400970526     C                   END
109500970526     C*
109600970526     C                   ENDSR
109700970526     C*------------------------------------------------------------------------*
109800970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
109900970526     C*------------------------------------------------------------------------*
110000970526     C     ENTATT        BEGSR
110100970526     C*
110200970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
110300970526     C                   Z-ADD     0             OPZOK             1 0
110400970526     C*
110500970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
110600970526     C                   Z-ADD     0             SFLVAR            5 0
110700970526     C*
110800970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
110900970526     C                   Z-ADD     0             SFLERR
111000970526     C*
111100970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
111200970526     C                   Z-ADD     SFLULT        WFLULT
111300970526     C*
111400970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
111500021211     C                   READC     SIG1D05                                98
111600970526     C     *IN98         DOWEQ     '0'
111700970526     C*
111800970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
111900970526     C                   ADD       1             SFLVAR
112000970526     C*
112100970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
112200970526     C     OPZOK         IFEQ      0
112300970526     C*
112400970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
112500970526     C     OPZKEY        IFNE      *BLANKS
112600970526     C                   EXSR      OPZCON
112700970526     C                   END
112800970526     C*
112900970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
113000970526     C*            2. PULIZIA SCELTA IN SFL
113100970526     C     OPZOK         IFEQ      0
113200970526     C                   MOVEL     '0'           *IN41
113300970526     C                   MOVEL     *BLANKS       OPZKEY
113400970526     C                   ELSE
113500970526     C*
113600970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
113700970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
113800970526     C                   MOVEL     '1'           *IN41
113900970526     C                   Z-ADD     SFLULT        SFLERR
114000970526     C                   END
114100970526     C                   ELSE
114200970526     C*
114300970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
114400970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
114500970526     C                   MOVEL     '1'           *IN41
114600970526     C                   END                                                    OPZOK=0
114700970526     C*
114800970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
114900970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
115000970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
115100970526     C     OPZOK         IFNE      0
115200970526     C                   Z-ADD     SFLERR        SFLNBR
115300970526     C                   ELSE
115400970526     C                   Z-ADD     SFLULT        SFLNBR
115500970526     C                   END
115600970526     C*
115700970526     C* AGGIORNO RECORD SFL
115800021211     C                   UPDATE    SIG1D05
115900970526     C*
116000021211     C                   READC     SIG1D05                                98
116100970526     C                   END                                                    DO WHILE
116200970526     C*
116300970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
116400970526     C                   Z-ADD     WFLULT        SFLULT
116500970526     C*
116600970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
116700970526     C     SFLVAR        IFEQ      0
116701150930     c                   EVAL      sflnbr = sflnrr
116800970526     C                   END
116900991112     C*
117000970526     C                   ENDSR
117100970526     C*------------------------------------------------------------------------*
117200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
117300970526     C*------------------------------------------------------------------------*
117400970526     C     CLRSFL        BEGSR
117500970526     C*
117600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
117700970526     C* (0=NON FINE FILE  1=FINE FILE)
117800970526     C                   Z-ADD     0             SFLEOF
117900970526     C*
118000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
118100970526     C                   Z-ADD     0             SFLULT
118200970526     C*
118300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
118400970526     C                   MOVEL     '1'           *IN40
118500021211     C                   WRITE     SIG1D06
118600970526     C                   MOVEL     '0'           *IN40
118700970526     C*
118800970526     C                   ENDSR
118900970526     C*------------------------------------------------------------------------*
119000970526     C* CARSFL - CARICAMENTO SFL
119100970526     C*------------------------------------------------------------------------*
119200970526     C     CARSFL        BEGSR
119300970526     C*
119400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
119500970526     C     SFLULT        IFEQ      0
119600970526     C                   EXSR      SETFIL
119700970526     C                   END
119800970526     C*
119900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
120000970526     C                   Z-ADD     SFLULT        WFLULT
120100970526     C*
120200970526     C* CICLO:
120300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
120400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
120500970526     C                   Z-ADD     1             SFLRIG
120600970526     C     SFLRIG        DOWLE     SFLPAG
120700970526     C     SFLEOF        ANDEQ     0
120800970526     C*
120900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
121000991116     C                   MOVEL     *BLANKS       OPZKEY
121100970526     C*
121200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
121300970526     C                   EXSR      IRESFL
121400970526     C*
121500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
121600970526     C                   ADD       1             SFLULT
121700970526     C*
121800970526     C* SCRITTURA RECORD SFL
121900021211     C                   WRITE     SIG1D05
122000970526     C                   ADD       1             SFLRIG
122100970526     C*
122200970526     C* LETTURA FILE DA CARICARE
122300970526     C                   EXSR      LETFIL
122400970526     C                   END                                                    DO WHILE
122500970526     C*
122600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
122700970526     C     WFLULT        IFNE      SFLULT
122800970526     C                   Z-ADD     WFLULT        SFLNBR
122900970526     C                   ADD       1             SFLNBR
123000970526     C                   END
123100970526     C*
123200970526     C                   ENDSR
123300970526     C*------------------------------------------------------------------------*
123400970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
123500970526     C*------------------------------------------------------------------------*
123600970526     C     OUTSFL        BEGSR
123700970526     C*
123800970526     C* WRITE RECORD TESTATA VIDEO
123900021211     C                   WRITE     SIG1D01
124000970526     C*
124100970526     C* VISUALIZZAZIONE MESSAGGIO
124200970526     C                   EXSR      SNDMSG
124300970526     C*
124400970526     C* WRITE RECORD OPZIONI VIDEO
124500021211     C                   WRITE     SIG1D02
124600970526     C*
124700970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
124800021211     C                   WRITE     SIG1D04
124900970526     C*
125000970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
125100970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
125200970526     C     SFLEOF        IFEQ      0
125300970526     C                   MOVEL     '0'           *IN42
125400970526     C                   ELSE
125500970526     C*
125600970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
125700970526     C                   MOVEL     '1'           *IN42
125800970526     C                   END
125900970526     C*
126000970526     C* WRITE RECORD TASTI FUNZIONALI
126100021211     C                   WRITE     SIG1D09
126200970526     C*
126300970526     C* CASO DI SFL VUOTO
126400970526     C     SFLULT        IFLE      0
126500970526     C*
126600970526     C*    - WRITE RECORD VUOTO
126700021211     C                   WRITE     SIG1D0A
126800021211     C                   READ      SIG1D0A                                99
126900970526     C*
127000970526     C                   ELSE
127100970526     C*
127200970526     C* CASO DI SFL CON RECORDS
127300970526     C*
127400970526     C*    - WRITE RECORD CONTROLLO SFL
127500021211     C                   WRITE     SIG1D06
127600021211     C                   READ      SIG1D06                                99
127700970526     C                   END
127800970526     C*
127900970526     C                   ENDSR
128000970526     C*------------------------------------------------------------------------*
128100970526     C* SNDMSG - MANDA MESSAGGIO
128200970526     C*------------------------------------------------------------------------*
128300970526     C     SNDMSG        BEGSR
128400970526     C*
128500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
128600970526     C                   MOVEL     '1'           *IN40
128700021211     C                   WRITE     SIG1DMC
128800970526     C                   MOVEL     '0'           *IN40
128900970526     C*
129000970526     C* VISUALIZZAZIONE MESSAGGIO
129100970526     C                   Z-ADD     0             MSGULT
129200970526     C                   CALL      'XMSG'        XMSGPA
129300970526     C     DSMSER        IFEQ      0
129400970526     C                   MOVEL     DSMSKE        MSGKEY
129500970526     C                   END
129600970526     C                   Z-ADD     1             MSGULT
129700021211     C                   WRITE     SIG1DMS                                      SFL MESS
129800021211     C                   WRITE     SIG1DMC                                      CONTR MESS
129900970526     C*
130000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
130100970526     C                   MOVEL     *BLANKS       DSMSMS
130200970526     C*
130300970526     C                   ENDSR
130400970526     C*------------------------------------------------------------------------*
130500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
130600970526     C*------------------------------------------------------------------------*
130700970526     C     FUNCON        BEGSR
130800970526     C*
130900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
131000970526     C                   EXSR      FUNVAR
131100970526     C*
131200970526     C* PULISCO CAMPI DI LAVORO
131300970526     C                   Z-ADD     0             I                 5 0
131400970526     C                   Z-ADD     0             W                 5 0
131500970526     C*
131600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
131700970526     C     FUNKEY        IFNE      *BLANKS
131800970526     C     1             DO        24            I
131900970526     C                   MOVEL     FUD(I)        FUDRIG
132000970526     C*
132100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
132200970526     C     FUNKEY        IFEQ      FUDFUN
132300970526     C     FUDATT        ANDEQ     'S'
132400970526     C                   Z-ADD     I             W
132500970526     C                   END
132600970526     C                   END                                                    DO WHILE
132700970526     C*
132800970526     C* FUNZIONE NON ATTIVA (ERRORE)
132900970526     C     W             IFEQ      0
133000970526     C*
133100970526     C* EMISSIONE MESSAGGIO DI ERRORE
133200970526     C                   MOVEL     MSG(2)        DSMSMS
133300970526     C                   END                                                    W=0
133400970526     C                   END                                                    FUNKEY NE *BLANKS
133500970526     C*
133600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
133700970526     C     W             IFNE      0
133800970526     C     FUNKEY        OREQ      *BLANKS
133900970526     C*
134000970526     C     FUNKEY        IFEQ      *BLANKS
134100970526     C                   EXSR      ENTGES
134200970526     C                   END
134300970526     C     FUNKEY        IFEQ      '03'
134400970526     C                   EXSR      F03GES
134500970526     C                   END
134600970526     C     FUNKEY        IFEQ      '05'
134700970526     C                   EXSR      F05GES
134800970526     C                   END
134900150917     C     FUNKEY        IFEQ      '10'
135000150917     C                   EXSR      F10GES
135100970526     C                   END
135500970526     C     FUNKEY        IFEQ      '12'
135600970526     C                   EXSR      F12GES
135700970526     C                   END
135800970528     C     FUNKEY        IFEQ      '21'
135900970528     C                   EXSR      F21GES
136000970528     C                   END
136100970528     C     FUNKEY        IFEQ      '22'
136200970528     C                   EXSR      F22GES
136300970528     C                   END
136400970526     C     FUNKEY        IFEQ      '23'
136500970526     C                   EXSR      F23GES
136600970526     C                   END
136700970526     C     FUNKEY        IFEQ      '24'
136800970526     C                   EXSR      F24GES
136900970526     C                   END
137000970526     C     FUNKEY        IFEQ      'RU'
137100970526     C                   EXSR      RUPGES
137200970526     C                   END
137300970526     C*
137400970526     C                   END
137500970526     C*
137600970526     C                   ENDSR
137700970526     C*------------------------------------------------------------------------*
137800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
137900970526     C*------------------------------------------------------------------------*
138000970526     C     FUNVAR        BEGSR
138100970526     C*
138200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
138300970526     C     KEY           IFEQ      ENTER
138400970526     C                   MOVEL     *BLANKS       FUNKEY
138500970526     C                   END
138600970526     C     KEY           IFEQ      F1
138700970526     C                   MOVEL     '01'          FUNKEY
138800970526     C                   END
138900970526     C     KEY           IFEQ      F2
139000970526     C                   MOVEL     '02'          FUNKEY
139100970526     C                   END
139200970526     C     KEY           IFEQ      F3
139300970526     C                   MOVEL     '03'          FUNKEY
139400970526     C                   END
139500970526     C     KEY           IFEQ      F4
139600970526     C                   MOVEL     '04'          FUNKEY
139700970526     C                   END
139800970526     C     KEY           IFEQ      F5
139900970526     C                   MOVEL     '05'          FUNKEY
140000970526     C                   END
140100970526     C     KEY           IFEQ      F6
140200970526     C                   MOVEL     '06'          FUNKEY
140300970526     C                   END
140400970526     C     KEY           IFEQ      F7
140500970526     C                   MOVEL     '07'          FUNKEY
140600970526     C                   END
140700970526     C     KEY           IFEQ      F8
140800970526     C                   MOVEL     '08'          FUNKEY
140900970526     C                   END
141000970526     C     KEY           IFEQ      F9
141100970526     C                   MOVEL     '09'          FUNKEY
141200970526     C                   END
141300970526     C     KEY           IFEQ      F10
141400970526     C                   MOVEL     '10'          FUNKEY
141500970526     C                   END
141600970526     C     KEY           IFEQ      F11
141700970526     C                   MOVEL     '11'          FUNKEY
141800970526     C                   END
141900970526     C     KEY           IFEQ      F12
142000970526     C                   MOVEL     '12'          FUNKEY
142100970526     C                   END
142200970526     C     KEY           IFEQ      F13
142300970526     C                   MOVEL     '13'          FUNKEY
142400970526     C                   END
142500970526     C     KEY           IFEQ      F14
142600970526     C                   MOVEL     '14'          FUNKEY
142700970526     C                   END
142800970526     C     KEY           IFEQ      F15
142900970526     C                   MOVEL     '15'          FUNKEY
143000970526     C                   END
143100970526     C     KEY           IFEQ      F16
143200970526     C                   MOVEL     '16'          FUNKEY
143300970526     C                   END
143400970526     C     KEY           IFEQ      F17
143500970526     C                   MOVEL     '17'          FUNKEY
143600970526     C                   END
143700970526     C     KEY           IFEQ      F18
143800970526     C                   MOVEL     '18'          FUNKEY
143900970526     C                   END
144000970526     C     KEY           IFEQ      F19
144100970526     C                   MOVEL     '19'          FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      F20
144400970526     C                   MOVEL     '20'          FUNKEY
144500970526     C                   END
144600970526     C     KEY           IFEQ      F21
144700970526     C                   MOVEL     '21'          FUNKEY
144800970526     C                   END
144900970526     C     KEY           IFEQ      F22
145000970526     C                   MOVEL     '22'          FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F23
145300970526     C                   MOVEL     '23'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F24
145600970526     C                   MOVEL     '24'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      ROLLUP
145900970526     C                   MOVEL     'RU'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      HELP
146200970526     C                   MOVEL     'HP'          FUNKEY
146300970526     C                   END
146400970526     C*
146500970526     C                   ENDSR
146600970526     C*------------------------------------------------------------------------*
146700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
146800970526     C*------------------------------------------------------------------------*
146900970526     C     FUNGES        BEGSR
147000970526     C*
147100970526     C* PULISCO CAMPI DI LAVORO
147200970526     C                   Z-ADD     0             W                 5 0
147300970526     C*
147400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
147500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
147600970526     C     1             DO        24            I
147700970526     C                   MOVEL     FUD(I)        FUDRIG
147800970526     C*
147900970526     C* PULIZIA FUNZIONI PRECEDENTI
148000970526     C                   MOVEL     *BLANKS       FUV(I)
148100970526     C*
148200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
148300970526     C* (SALTA F23 E F24)
148400970526     C     FUDATT        IFEQ      'S'
148500970526     C     FUDFUN        ANDNE     '23'
148600970526     C     FUDFUN        ANDNE     '24'
148700970526     C                   ADD       1             W
148800970526     C                   MOVEL     FUDDES        FUV(W)
148900970526     C                   END
149000970526     C                   END                                                    DO WHILE
149100970526     C*
149200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
149300970526     C* ALTRIMENTI DISATTIVO F24
149400970526     C     1             DO        24            I
149500970526     C                   MOVEL     FUD(I)        FUDRIG
149600970526     C*
149700970526     C     FUDFUN        IFEQ      '24'
149800970526     C     W             IFGT      6
149900970526     C                   MOVEL     'S'           FUDATT
150000970526     C                   ELSE
150100970526     C                   MOVEL     *BLANKS       FUDATT
150200970526     C                   END                                                    W GT 8
150300970526     C                   MOVEL     FUDRIG        FUD(I)
150400970526     C                   END
150500970526     C                   END                                                    DO WHILE
150600970526     C*
150700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
150800970526     C     W             DIV       6             I
150900970526     C                   MVR                     Z
151000970526     C*
151100970526     C     Z             IFNE      0
151200970526     C     I             ADD       1             Z
151300970526     C                   ELSE
151400970526     C                   Z-ADD     I             Z
151500970526     C                   END
151600970526     C*
151700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
151800970526     C                   ADD       1             FUNGRU
151900970526     C     FUNGRU        IFGT      Z
152000970526     C     FUNGRU        ORLT      1
152100970526     C                   Z-ADD     1             FUNGRU
152200970526     C                   END
152300970526     C*
152400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
152500970526     C     FUNGRU        IFEQ      1
152600970526     C                   MOVEL     FUV(1)        FUN001
152700970526     C                   MOVEL     FUV(2)        FUN002
152800970526     C                   MOVEL     FUV(3)        FUN003
152900970526     C                   MOVEL     FUV(4)        FUN004
153000970526     C                   MOVEL     FUV(5)        FUN005
153100970526     C                   MOVEL     FUV(6)        FUN006
153200970526     C                   END
153300970526     C*
153400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
153500970526     C     FUNGRU        IFEQ      2
153600970526     C                   MOVEL     FUV(7)        FUN001
153700970526     C                   MOVEL     FUV(8)        FUN002
153800970526     C                   MOVEL     FUV(9)        FUN003
153900970526     C                   MOVEL     FUV(10)       FUN004
154000970526     C                   MOVEL     FUV(11)       FUN005
154100970526     C                   MOVEL     FUV(12)       FUN006
154200970526     C                   END
154300970526     C*
154400970526     C* TERZO GRUPPO DI 6 FUNZIONI
154500970526     C     FUNGRU        IFEQ      3
154600970526     C                   MOVEL     FUV(13)       FUN001
154700970526     C                   MOVEL     FUV(14)       FUN002
154800970526     C                   MOVEL     FUV(15)       FUN003
154900970526     C                   MOVEL     FUV(16)       FUN004
155000970526     C                   MOVEL     FUV(17)       FUN005
155100970526     C                   MOVEL     FUV(18)       FUN006
155200970526     C                   END
155300970526     C*
155400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
155500970526     C     FUNGRU        IFEQ      4
155600970526     C                   MOVEL     FUV(19)       FUN001
155700970526     C                   MOVEL     FUV(20)       FUN002
155800970526     C                   MOVEL     FUV(21)       FUN003
155900970526     C                   MOVEL     FUV(22)       FUN004
156000970526     C                   MOVEL     *BLANKS       FUN005
156100970526     C                   MOVEL     *BLANKS       FUN006
156200970526     C                   END
156300970526     C*
156400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
156500970526     C                   MOVEL     *BLANKS       FUN007
156600970526     C                   MOVEL     *BLANKS       FUN008
156700970526     C*
156800970526     C     1             DO        24            I
156900970526     C                   MOVEL     FUD(I)        FUDRIG
157000970526     C     FUDFUN        IFEQ      '23'
157100970526     C     FUDATT        ANDEQ     'S'
157200970526     C                   MOVEL     FUDDES        FUN007
157300970526     C                   END
157400970526     C     FUDFUN        IFEQ      '24'
157500970526     C     FUDATT        ANDEQ     'S'
157600970526     C                   MOVEL     FUDDES        FUN008
157700970526     C                   END
157800970526     C                   END                                                    DO
157900970526     C*
158000970526     C                   ENDSR
158100970526     C*------------------------------------------------------------------------*
158200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
158300970526     C*------------------------------------------------------------------------*
158400970526     C     OPZGES        BEGSR
158500970526     C*
158600970526     C* PULISCO CAMPI DI LAVORO
158700970526     C                   Z-ADD     0             W                 5 0
158800970526     C*
158900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
159000970526     C* E CARICO TABELLA OPZIONI ATTIVE
159100991115     C     1             DO        24            I
159200970526     C                   MOVEL     OPD(I)        OPDRIG
159300970526     C*
159400970526     C* PULIZIA OPZIONI PRECEDENTI
159500970526     C                   MOVEL     *BLANKS       OPV(I)
159600970526     C*
159700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
159800970526     C     OPDATT        IFEQ      'S'
159900970526     C                   ADD       1             W
160000970526     C                   MOVEL     OPDDES        OPV(W)
160100970526     C                   END
160200970526     C                   END                                                    DO WHILE
160300970526     C*
160400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
160500970526     C* ALTRIMENTI DISATTIVO F23
160600991115     C     1             DO        24            I
160700970526     C                   MOVEL     FUD(I)        FUDRIG
160800970526     C*
160900970526     C     FUDFUN        IFEQ      '23'
161000970526     C     W             IFGT      8
161100970526     C                   MOVEL     'S'           FUDATT
161200970526     C                   ELSE
161300970526     C                   MOVEL     *BLANKS       FUDATT
161400970526     C                   END                                                    W GT 8
161500970526     C                   MOVEL     FUDRIG        FUD(I)
161600970526     C                   END
161700970526     C                   END                                                    DO WHILE
161800970526     C*
161900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
162000970526     C     W             DIV       8             I
162100970526     C                   MVR                     Z                 5 0
162200970526     C*
162300970526     C     Z             IFNE      0
162400970526     C     I             ADD       1             Z
162500970526     C                   ELSE
162600970526     C                   Z-ADD     I             Z
162700970526     C                   END
162800970526     C*
162900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
163000970526     C                   ADD       1             OPZGRU
163100970526     C     OPZGRU        IFGT      Z
163200970526     C     OPZGRU        ORLT      1
163300970526     C                   Z-ADD     1             OPZGRU
163400970526     C                   END
163500970526     C*
163600970526     C* PRIMO GRUPPO DI 8 OPZIONI
163700970526     C     OPZGRU        IFEQ      1
163800970526     C                   MOVEL     OPV(1)        OPZ001
163900970526     C                   MOVEL     OPV(2)        OPZ002
164000970526     C                   MOVEL     OPV(3)        OPZ003
164100970526     C                   MOVEL     OPV(4)        OPZ004
164200970526     C                   MOVEL     OPV(5)        OPZ005
164300970526     C                   MOVEL     OPV(6)        OPZ006
164400970526     C                   MOVEL     OPV(7)        OPZ007
164500970526     C                   MOVEL     OPV(8)        OPZ008
164600970526     C                   END
164700970526     C*
164800970526     C* SECONDO GRUPPO DI 8 OPZIONI
164900970526     C     OPZGRU        IFEQ      2
165000970526     C                   MOVEL     OPV(9)        OPZ001
165100970526     C                   MOVEL     OPV(10)       OPZ002
165200970526     C                   MOVEL     OPV(11)       OPZ003
165300970526     C                   MOVEL     OPV(12)       OPZ004
165400970526     C                   MOVEL     OPV(13)       OPZ005
165500970526     C                   MOVEL     OPV(14)       OPZ006
165600970526     C                   MOVEL     OPV(15)       OPZ007
165700970526     C                   MOVEL     OPV(16)       OPZ008
165800970526     C                   END
165900970526     C*
166000970526     C* TERZO GRUPPO DI 8 OPZIONI
166100970526     C     OPZGRU        IFEQ      3
166200970526     C                   MOVEL     OPV(17)       OPZ001
166300970526     C                   MOVEL     OPV(18)       OPZ002
166400970526     C                   MOVEL     OPV(19)       OPZ003
166500970526     C                   MOVEL     OPV(20)       OPZ004
166600970526     C                   MOVEL     OPV(21)       OPZ005
166700970526     C                   MOVEL     OPV(22)       OPZ006
166800970526     C                   MOVEL     OPV(23)       OPZ007
166900970526     C                   MOVEL     OPV(24)       OPZ008
167000970526     C                   END
167100970526     C*
167200970526     C                   ENDSR
167300970526     C*------------------------------------------------------------------------*
167400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
167500970526     C*------------------------------------------------------------------------*
167600970526     C     OPZCON        BEGSR
167700970526     C*
167800970526     C* PULISCO CAMPI DI LAVORO
167900970526     C                   Z-ADD     0             I                 5 0
168000970526     C                   Z-ADD     0             W                 5 0
168100970526     C*
168200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
168300970526     C     1             DO        24            I
168400970526     C                   MOVEL     OPD(I)        OPDRIG
168500970526     C*
168600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
168700970526     C     OPZKEY        IFEQ      OPDOPZ
168800970526     C     OPZKEY        OREQ      OPDOP1
168900970526     C*
169000970526     C     OPDATT        IFEQ      'S'
169100970526     C                   MOVEL     OPDOPZ        OPZKEY
169200970526     C                   Z-ADD     I             W
169300970526     C                   END
169400970526     C                   END
169500970526     C*
169600970526     C                   END                                                    DO WHILE
169700970526     C*
169800970526     C* OPZIONE NON ATTIVA (ERRORE)
169900970526     C     W             IFEQ      0
170000970526     C*
170100970526     C* OPZIONE NON OK
170200970526     C                   Z-ADD     1             OPZOK
170300970526     C*
170400970526     C* EMISSIONE MESSAGGIO DI ERRORE
170500970526     C                   MOVEL     MSG(1)        DSMSMS
170600970526     C                   ELSE
170700970526     C*
170800970526     C* OPZIONE ATTIVA (PROCEDI ...)
170900970526     C*
171000021211     C                   MOVEL     *BLANKS       PMSG
171100000420     C     OPZKEY        IFEQ      ' 1'
171200000420     C                   EXSR      O01GES
171300000420     C                   END
171400000407     C     OPZKEY        IFEQ      ' 2'
171500000407     C                   EXSR      O02GES
171600000407     C                   END
171700150909     C*    OPZKEY        IFEQ      ' 3'
171800150909     C*                  EXSR      O03GES
171900150909     C*                  END
172000991110     C     OPZKEY        IFEQ      ' 4'
172100991110     C                   EXSR      O04GES
172200991014     C                   END
172300970526     C     OPZKEY        IFEQ      ' 5'
172400970526     C                   EXSR      O05GES
172500970526     C                   END
173200030123     C                   MOVEL     PMSG          DSMSMS
173300021211     C                   END                                                    OPZ.NON ATTIVA
173400970526     C*
173500970526     C                   ENDSR
173600150324      *--------------------------------------------------------------------------------------------*
173700150324      * ordina - riscrive il sfl ordinato
173800150324      *--------------------------------------------------------------------------------------------*
173900150324     C     ordina        BEGSR
174000150324     C                   EVAL      righe = SFLULT
174100150324      * IMPOSTO ENTRATA AD 'A'
174200150324     C                   EVAL      SFLULT= 0
174300150324      *
174400150324     C                   CLEAR                   QLGSCB
174500150324     C                   CLEAR                   QLGSCB00
174600150324
174700150324     ** 2 campi chiave.
174800150324     C                   EVAL      QLGNBRK    = 1
174900150324     c                   select
175000150324     c* ordinamento per prv/rag.sociale destinatario
175100150324     c                   when      w_ord=1
175200150324     **  rag. sociale cliente  2 campo
175300160111     C                   EVAL      QLGSP      = 1 + %SIZE(vsoc)
175400160111     C                   EVAL      QLGSS      = %SIZE(vfor)
175500150925     C                   EVAL      QLGDT      = numerico
175600150324     C                   EVAL      QLGSO      = Ascendente
175700150324     C                   EVAL      QLGKL(1)   = QLGSKL
175702150925     c                   movel     costf81       fud(08)
175800150324     c                   eval      w_ord = 2
175900150324     c* ordinamento per rag.sociale/prv destinatario
176000150324     c                   when      w_ord=2
176100150324     C                   EVAL      QLGSP      = 1
176200160111     C                   EVAL      QLGSS      = %SIZE(vsoc)
176300150324     C                   EVAL      QLGDT      = carattere
176400150324     C                   EVAL      QLGSO      = Ascendente
176500150324     C                   EVAL      QLGKL(1)   = QLGSKL
176502150925     c                   movel     costf82       fud(08)
176600150324     c                   eval      w_ord = 1
176700150324     ** ragione sociale a posizione 1, carattere, 24 byte,
176800150324     c                   endsl
176900150324
177000150324      * Load other sort parameters.
177100150324     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
177200150324     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
177300150324     C                   EVAL      QLGRT     = 8
177400150324     C                   EVAL      QLGOKL    = 80
177500150324     C                   EVAL      QLGLKE    = 16
177600150324     C                   EVAL      QLGLSS    = 290
177700150324
177800150324      * Initialize Sort I/O API fields.
177900150324     C                   EVAL      QLGRL00  = QLGRL
178000150324     C                   EVAL      QLGRC00  = 1
178100150324     C                   CLEAR                   QUSEI
178200150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
178300150324
178400150324      * First step - Initialize the sort routine.
178500150324     C                   CALL      'QLGSORT'
178600150324     C                   PARM                    QLGSCB
178700150324     C                   PARM                    NotUsed
178800150324     C                   PARM                    NotUsed
178900150324     C                   PARM                    SizeList
179000150324     C                   PARM                    ReturnSize
179100150324     C                   PARM                    QUSEC
179200150324
179300150324      * Next step - Write records to I/O routine.
179400150324     C                   EVAL      QLGRT00 = Put
179500150324
179600150324     C                   DO        *hival        cx                4 0
179700150324
179800150324     C     cx            CHAIN     SIG1D05                            50
179900150324     c   50              leave
180000150324     ** Solo le righe con Selected = 'Y' sono riordinate,
180100150324     ** quindi per fare un ordinamento di tutte le righe
180200150324     ** metto 'Y' sempre.
180300150324     C                   EVAL      Selected = 'Y'
180400150324
180500150324     C                   CLEAR                   QUSEI
180600150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
180700150324     C                   CALL      'QLGSRTIO'
180800150324     C                   PARM                    QLGSCB00
180900150324     C                   PARM                    SflRcd
181000150324     C                   PARM                    NotUsed
181100150324     C                   PARM                    SizeList
181200150324     C                   PARM                    NotUsed
181300150324     C                   PARM                    QUSEC
181400150324
181500150324     C                   ENDDO
181600150324
181700150324      * Next step - Signal end of input, clear subfile for reload.
181800150324     C                   EVAL      QLGRT00 = EndPut
181900150324     C                   CLEAR                   QUSEI
182000150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
182100150324
182200150324     C                   CALL      'QLGSRTIO'
182300150324     C                   PARM                    QLGSCB00
182400150324     C                   PARM                    SflRcd
182500150324     C                   PARM                    NotUsed
182600150324     C                   PARM                    SizeList
182700150324     C                   PARM                    NotUsed
182800150324     C                   PARM                    QUSEC
182900150324
183000150324     C                   MOVEL     '1'           *IN40
183100150324     C                   WRITE     SIG1D06
183200150324     C                   MOVEL     '0'           *IN40
183300150324
183400150324      * Final step - Write the records back to the subfile.
183500150324     C                   EVAL      QLGRT00 = Get
183600150324     C                   DO        righe         x                 4 0
183700150324     C                   CLEAR                   QUSEI
183800150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
183900150324     C                   CALL      'QLGSRTIO'
184000150324     C                   PARM                    QLGSCB00
184100150324     C                   PARM                    NotUsed
184200150324     C                   PARM                    SflRcd
184300150324     C                   PARM                    QLGRL00
184400150324     C                   PARM                    NotUsed
184500150324     C                   PARM                    QUSEC
184600150324      *
184700150324     c                   z-add     x             SFLULT
184800150324     C                   WRITE     SIG1D05
184900150324     C                   ENDDO
184901150929     c                   z-add     1             SFLnbr
184902150925     c                   exsr      funges
184903150925     C                   WRITE     SIG1D09
185000150324
185100150324      *
185200150324     C                   ENDSR
185300970526     C*------------------------------------------------------------------------*
185400970526     C* NOTE COSTRUZIONE SCHIERE
185500970526     C*------------------------------------------------------------------------*
185600970526     C*
185700970526     C* -MSG-MESSAGGI
185800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
185900970526     C* (NON TOCCARE MSG,1 E MSG,2)
186000970526     C*
186100970526     C* -CMD-COMANDI
186200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
186300970526     C*
186400970526     C* -OPD-OPZIONI
186500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
186600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
186700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
186800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
186900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
187000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
187100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
187200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
187300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
187400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
187500970526     C*
187600970526     C* -FUD-FUNZIONI
187700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
187800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
187900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
188000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
188100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
188200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
188300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
188400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
188500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
188600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
188700991108     C*
188800991115     C* -STS-CODICI STATI DEI RECORD DI LOG
188900991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
189000991110     C* ================> IMPORTANTE <=================
189100991115     C* per codici di errore in -STS- indicare in -DES- sempre
189200991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
189300991110     C* ERR
189400991108     C*
189500970526     O*------------------------------------------------------------------------*
189600970526** DEC - DECODIFICHE
189700970526    GESTIONE                                                     1
189800000407 INTERROGAZIONE                                                  2
189900970530     SCELTA                                                      3
1900009705264
1901009705265
190200970526** MSG - MESSAGGI
190300150924Opzione non gestita.                                                            1
190400150924Tasto funzionale non gestito.                                                   2
190500150924Utente non abilitato all'inserimento.                                           3
190600160111Rettifica già fatturata non manutenzionabile                                    4
19070016011105                                                                              5
19080016011106                                                                              6
19090016011107                                                                              7
19100016011108                                                                              8
19110016011109                                                                              9
19120015092410                                                                             10
19130015092411                                                                             11
19140015092412                                                                             12
19150015092413                                                                             13
19160015092414                                                                             14
19170015092415                                                                             15
19180015092416                                                                             16
19190015092417                                                                             17
192000150924          18                                                                   18
192100150924          19                                                                   19
192200150924          20                                                                   20
192300970528** CMD - COMANDI CL
192400021211
192500021211
192600991110
192700970528
192800970528
192900970526** OPD - OPZIONI
193000150211 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
193100150202 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
193200150908                   -  -     (S/N)=N                        opz. 03¬
193300150908 4=Cancella        -4 -     (S/N)=S                        opz. 04¬
193400150202 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
193500160111                   -  -     (S/N)=N                        opz. 06¬
193600160111                   -  -     (S/N)=N                        opz. 07¬
193700160111                   -  -     (S/N)=N                        opz. 08¬
193800991110                   -  -     (S/N)=N                        opz. 09¬
193900991110                   -  -     (S/N)=N                        opz. 10¬
194000000407                   -  -     (S/N)=N                        opz. 11¬
194100970526                   -  -     (S/N)=N                        opz. 12¬
194200970526                   -  -     (S/N)=N                        opz. 13¬
194300000407                   -  -     (S/N)=N                        opz. 14¬
194400000407                   -  -     (S/N)=N                        opz. 15¬
194500970526                   -  -     (S/N)=N                        opz. 16¬
194600970526                   -  -     (S/N)=N                        opz. 17¬
194700970526                   -  -     (S/N)=N                        opz. 18¬
194800970526                   -  -     (S/N)=N                        opz. 19¬
194900970526                   -  -     (S/N)=N                        opz. 20¬
195000970526                   -  -     (S/N)=N                        opz. 21¬
195100970526                   -  -     (S/N)=N                        opz. 22¬
195200970526                   -  -     (S/N)=N                        opz. 23¬
195300000407                   -  -     (S/N)=N                        opz. 25¬
195400970526** FUD - FUNZIONI
195500970526                   -        (S/N)=N                        funz.01¬
195600970526                   -        (S/N)=N                        funz.02¬
195700150928                   -        (S/N)=N                        funz.03¬
195800970526                   -        (S/N)=N                        funz.04¬
195900150908                   -        (S/N)=N                        funz.05¬
196000150917                   -        (S/N)=N                        funz.06¬
196100970526                   -        (S/N)=N                        funz.07¬
196200160111                   -        (S/N)=N                        funz.08¬
196300150928F09=Altri dati     -        (S/N)=S                        funz.09¬
196400150908F10=Inserimento    -        (S/N)=S  <== S-FISSO           funz.10¬
196500150325                   -        (S/N)=N                        funz.11¬
196600970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
196700991116                   -        (S/N)=N                        funz.13¬
196800970526                   -        (S/N)=N                        funz.14¬
196900970526                   -        (S/N)=N                        funz.15¬
197000970526                   -        (S/N)=N                        funz.16¬
197100970526                   -        (S/N)=N                        funz.17¬
197200970526                   -        (S/N)=N                        funz.18¬
197300150924 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
197400150924 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
197500000407                   -        (S/N)=N                        funz.21¬
197600991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
197700991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
197800991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
