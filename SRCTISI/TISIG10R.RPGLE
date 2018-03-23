000100970526     H*------------------------------------------------------------------------*
000200150203     H* Gestione file anagrafico e Bancali
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150130     FTISIG10d  CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG1D05:SFLULT)
001000021211     F                                     SFILE(SIG1DMS:MSGULT)
001100970526     F*------------
001200150224     Fazcgb01l  iF   E           K DISK
001300150130     Fazmgb04l  IF   E           K DISK
001400150210     Fazfgb01l  IF   E           K DISK
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
002600970528     D*------------
002700970528     D* COMANDI
002800970528     D*------------
002900970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
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
004500970527     D FUV             S             19    DIM(24)
004600991108     D*------------
004700970526     D*------------------------------------------------------------------------*
004800970526     D* INPUT
004900970526     D*------------------------------------------------------------------------*
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
009000150212     D dute01        E DS                                                       *Utenti
009100150212     d dLat          e ds
009200150212     d TIBS02DS      E DS
009300150212     D*-------------------
009400150212     D TRUL31DS      E DS
009500150212     D TRUL31DS2     E DS
009600150212     d wabi            s                   like(UteAut)
009700150212     d* p.o. abilitati
009800150212     D $pog            s              3    dim(250)
009900150212     D $DIG            s              1    dim(20)
010000150212     D $ARG            s              3    dim(50)
010100970527     D*------------------
010200991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
010300970527     D*------------------
010400970527     D WLBDA8          DS                  INZ
010500970527     D  G08DAT                 1      8  0
010600970527     D  G08INV                 9     16  0
010700970527     D  G08ERR                17     17
010800970527     D  G08TGI                18     22  0
010900970526     D*------------
011000991110     D* DS DI PROCEDURA
011100970526     D*------------
011200970526     D* ARCHITETTURA
011300970526     D*------------
011400970526     D KPJBA         E DS
011500150211     D savKPJBu        s                   like(kpjbu)
011600150202      *
011700150202     d Tibs69DS      E DS                  INZ
011800150202     d DS_cnaco      E DS                  extname(CNACO00F)
011900150202     d DS_cnind      E DS                  extname(CNIND00F)
012000150202     d DS_cnclp      E DS                  extname(CNCLP00F)
012100150202     d DS_fncls      E DS                  extname(FNCLS00F)
012200150203      *
012300021211     D*------------------
012400021211     D* PASSAGGIO PARAMETRI
012500021211     D*------------------
012600021211     D PARAM           DS                  INZ
012700150130     d  pop1                          3
012800150130     D  Pksu                          8
012900150130     D  Ptsc                          1
013000150302     D  Pdti                          8  0
013100150130     D  Pmsg                         80
013200150211     D PARAMb          DS                  INZ
013300150211     d  pop1b                         3
013400150211     D  Pksub                         8
013500150211     D  Pkscb                         1
013600150211     D  Pmsgb                        80
013700150324      *---------------------------------------------------------------------
013800150324      * specifiche per ordinamento forzato sfl
013900150324     d NotUsed         S             16A
014000150324     d ReturnSize      S              9B 0
014100150324     d SizeList        S              9B 0
014200150324     d righe           S              4  0
014300150324     d w_ord           S              1  0         inz(1)
014400150324     d*---
014500150324     d MaxKey          C                   2
014600150324     d Ascendente      C                   1
014700150324     d Discendente     C                   2
014800150324     d Carattere       C                   6
014900150324     d Put             C                   1
015000150324     d EndPut          C                   2
015100150324     d Get             C                   3
015200150324     d Numerico        C                   8
015300150324     d SflRcd          DS
015400150324     d  VIDKSU
015500150324     d  VIDKSUD
015600150324     d  VIDTSC
015700150324     d  VIDDTI
015800150324     d  VIDDTF
015900150324     d  VIDDTE
016000150324     d  VIDFIG
016100150324     d  OPZKEY
016200150324     d  VIDDTIG
016300150324     d  Selected                      1A
016400150324
016500150324      /COPY QSYSINC/QRPGLESRC,QLGSORT
016600150324     d QLGKL                         16    DIM(MaxKey)
016700150324     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
016800150324     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
016900150324     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
017000150324     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
017100150324      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
017200150324      /COPY QSYSINC/QRPGLESRC,QUSEC
017300970526     C*------------------------------------------------------------------------*
017400970526     C* MAIN LINES
017500970526     C*------------------------------------------------------------------------*
017600970526     C*
017700970526     C* SE NON CI SONO ERRORI, PROSEGUE
017800970526IF  1C     ERRL00        IFNE      '1'
017900970526     C*
018000970526     C* CARICA IL SFILE
018100970526     C                   EXSR      POSSFL
018200970526     C*
018300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
018400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
018500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
018600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
018700970526     C*
018800970526     C* REIMPOSTA ERRORE DI PROCERURA
018900970526     C                   MOVEL     '0'           ERRL00
019000970526     C*
019100970526     C* EMETTE IL SFILE
019200970526     C                   EXSR      OUTSFL
019300970526     C*
019400970526     C* GESTIONE TASTI FUNZIONALI
019500970526     C                   EXSR      FUNCON
019600970526     C*
019700970526E   2C                   ENDDO
019800970527     C*
019900970526E   1C                   ENDIF
020000991108     C*
020100970526     C* FINE PGM
020200970526     C                   SETON                                        LR
020300970526     C*------------------------------------------------------------------------*
020400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
020500970526     C*------------------------------------------------------------------------*
020600970526     C     F03GES        BEGSR
020700970526     C*
020800970526     C* IMPOSTA "PREMUTO F03"
020900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F05GES        BEGSR
021600970526     C*
021700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100150204     C* F06GES - GESTIONE F06 -> INSERIMENTO
022200970526     C*------------------------------------------------------------------------*
022300150204     C     F06GES        BEGSR
022400970526     C*
022500021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
022600150213     C*    DUTLPO        IFNE      'S'
022700150213     C*                  MOVEL     '1'           ERRL00
022800150213     C*                  MOVEL     MSG(3)        DSMSMS
022900150213     C*                  ENDIF
023000000407     C*
023100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023200150204     C                   MOVEL     'F06'         POP1
023300000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
023400150130     C                   CALL      'TISIG11R'
023500000407     C                   PARM                    KPJBA
023600021211     C                   MOVEL     KPJBU         PARAM
023700000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
023800021211     C                   ENDIF
023900000407     C*
024000970526     C                   ENDSR
024100150324     C*------------------------------------------------------------------------*
024200150324     C* F11GES - GESTIONE F11 -> ORDINAMENTO
024300150324     C*------------------------------------------------------------------------*
024400150324     C     F11GES        BEGSR
024500150324     C*
024600150325     C*                  exsr      ordina
024700150324     C*
024800150324     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000991108     C* F12GES - GESTIONE F12 -> RITORNO
025100970526     C*------------------------------------------------------------------------*
025200970526     C     F12GES        BEGSR
025300970526     C*
025400970526     C* IMPOSTA "PREMUTO F12"
025500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025600970526     C*
025700970526     C                   ENDSR
025800970528     C*------------------------------------------------------------------------*
025900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
026000970528     C*------------------------------------------------------------------------*
026100970528     C     F21GES        BEGSR
026200970612     C*
026300991117     C* NON GESTITA
026400970528     C*
026500970528     C                   ENDSR
026600970528     C*------------------------------------------------------------------------*
026700970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
026800970528     C*------------------------------------------------------------------------*
026900970528     C     F22GES        BEGSR
027000970528     C*
027100021211     C* NON GESTITA
027200970528     C*
027300970528     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
027600970526     C*------------------------------------------------------------------------*
027700970526     C     F23GES        BEGSR
027800970526     C*
027900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
028000970526     C*
028100970526     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028400970526     C*------------------------------------------------------------------------*
028500970526     C     F24GES        BEGSR
028600970526     C*
028700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028800970526     C*
028900970526     C                   ENDSR
029000970526     C*------------------------------------------------------------------------*
029100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
029200970526     C*------------------------------------------------------------------------*
029300970526     C     RUPGES        BEGSR
029400970526     C*
029500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
029600970526     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900991108     C* ENTGES - GESTIONE ENTER -> INVIO
030000970526     C*------------------------------------------------------------------------*
030100970526     C     ENTGES        BEGSR
030200991108     C*
030300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
030400970526     C                   EXSR      NEWPOS
030500021211     C*
030600021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
030700021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800021211     C     SWPOSI        IFNE      0
030900021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
031000021211     C                   ELSE
031100021211     C     SFLULT        IFNE      0
031200021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
031300021211     C*
031400021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
031500021211     C     SWMODI        IFNE      0
031600021211     C                   EXSR      POSSFL
031700021211     C                   ENDIF
031800021211     C                   ENDIF
031900021211     C                   ENDIF
032000021211     C                   ENDIF
032100021211     C*
032200021211     C                   ENDSR
032300000420     C*------------------------------------------------------------------------*
032400000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
032500000420     C*------------------------------------------------------------------------*
032600000420     C     O01GES        BEGSR
032700000420     C*
032800000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
032900150130     C                   MOVEL     VIDksu        Pksu
033000000420     C                   MOVEL     PARAM         KPJBU
033100000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
033200000420     C*
033300000420     C                   ENDSR
033400000407     C*------------------------------------------------------------------------*
033500000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
033600000407     C*------------------------------------------------------------------------*
033700000407     C     O02GES        BEGSR
033800000407     C*
033900021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
034000150213     C*    DUTLPO        IFNE      'S'
034100150213     C*                  MOVEL     '1'           ERRL00
034200150213     C*                  MOVEL     MSG(3)        pmsg
034300150213     C*                  ENDIF
034400021211     C*
034500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034600150130     C                   MOVEL     VIDksu        Pksu
034700150130     C                   MOVEL     VIDtsc        Ptsc
034800150302     c                   movel     viddtig       pdti
034900000407     C                   MOVEL     'O02'         POP1
035000000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
035100150130     C                   CALL      'TISIG11R'
035200000407     C                   PARM                    KPJBA
035300021211     C                   MOVEL     KPJBU         PARAM
035400021211     C                   ENDIF
035500000407     C*
035600000407     C                   ENDSR
035700021211     C*------------------------------------------------------------------------*
035800021211     C* O03GES - GESTIONE OPZIONE 3= COPIA
035900021211     C*------------------------------------------------------------------------*
036000021211     C     O03GES        BEGSR
036100021211     C*
036200021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
036300150213     C*    DUTLPO        IFNE      'S'
036400150213     C*                  MOVEL     '1'           ERRL00
036500150213     C*                  MOVEL     MSG(3)        PMSG
036600150213     C*                  ENDIF
036700021211     C*
036800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036900150130     C                   MOVEL     VIDksu        Pksu
037000150130     C                   MOVEL     VIDtsc        Ptsc
037100150302     c                   movel     viddtig       pdti
037200021211     C                   MOVEL     'O03'         POP1
037300021211     C                   EXSR      PASPAR                                       *RICARICA SFILE
037400150130     C                   CALL      'TISIG11R'
037500021211     C                   PARM                    KPJBA
037600021211     C                   MOVEL     KPJBU         PARAM
037700021211     C                   ENDIF
037800021211     C*
037900021211     C                   ENDSR
038000991112     C*------------------------------------------------------------------------*
038100021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
038200991112     C*------------------------------------------------------------------------*
038300991112     C     O04GES        BEGSR
038400021211     C*
038500021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
038600150213     C*    DUTLPO        IFNE      'S'
038700150213     C*                  MOVEL     '1'           ERRL00
038800150213     C*                  MOVEL     MSG(3)        PMSG
038900150213     C*                  ENDIF
039000991112     C*
039100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039200021211     C*
039300021211     C* VERIFICO CHE NN VI SIANO PRESENTI MOVIMENTO PER L'ARICOLO CHE SI VUOLE ANNULLARE
039400150130     C                   MOVEL     VIDksu        KEYksu
039500150130     C     KEYMGBP       SETLL     azmgb04l                               55
039600021211     C*
039700021211     C                   IF        *IN55 = *ON
039800030123     C                   MOVEL     MSG(4)        PMSG
039900021211     C                   ELSE
040000150130     C                   MOVEL     VIDksu        Pksu
040100150130     C                   MOVEL     VIDtsc        Ptsc
040200150302     c                   movel     viddtig       pdti
040300000407     C                   MOVEL     'O04'         POP1
040400000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
040500150130     C                   CALL      'TISIG11R'
040600000407     C                   PARM                    KPJBA
040700021211     C                   MOVEL     KPJBU         PARAM
040800021211     C                   ENDIF
040900000411     C                   ENDIF
041000991112     C*
041100991112     C                   ENDSR
041200991014     C*------------------------------------------------------------------------*
041300000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
041400970526     C*------------------------------------------------------------------------*
041500970526     C     O05GES        BEGSR
041600000407     C*
041700021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041800150130     C                   MOVEL     VIDksu        Pksu
041900150130     C                   MOVEL     VIDtsc        Ptsc
042000150302     c                   movel     viddtig       pdti
042100000407     C                   MOVEL     'O05'         POP1
042200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
042300150130     C                   CALL      'TISIG11R'
042400000407     C                   PARM                    KPJBA
042500021211     C                   ENDIF
042600000407     C*
042700970526     C                   ENDSR
042800150211     C*------------------------------------------------------------------------*
042900150211     C* O0FGES - GESTIONE OPZIONE F=VISUALIZZAZIONE abbinati
043000150211     C*------------------------------------------------------------------------*
043100150211     C     O0FGES        BEGSR
043200150211     C*
043300150211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043400150211     C
043500150211     C                   clear                   Pkscb
043600150211     c                   movel     vidksu        pksub
043700150302     c                   if        pop1 = 'P01'
043800150302     C                   MOVEL     'P05'         POP1b
043900150302     c                   else
044000150211     C                   MOVEL     'P02'         POP1b
044100150302     c                   endif
044200150211     C                   movel     kpjbu         savkpjbu                       *RICARICA SFILE
044300150211     C                   movel     paramb        kpjbu
044400150211     C                   CALL      'TISIG12R'
044500150211     C                   PARM                    KPJBA
044600150211     C                   movel     kpjbu         paramb
044700150211     C                   movel     savkpjbu      kpjbu
044800150211     C                   ENDIF
044900150211     C*
045000150211     C                   ENDSR
045100991028     C*------------------------------------------------------------------------*
045200991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
045300991112     C*------------------------------------------------------------------------*
045400991112     C     PASPAR        BEGSR
045500991112     C*
045600000407     C                   MOVEL     PARAM         KPJBU
045700991112     C*
045800991112     C                   ENDSR
045900970526     C*------------------------------------------------------------------------*
046000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
046100970526     C*------------------------------------------------------------------------*
046200970526     C     NEWPOS        BEGSR
046300970526     C*
046400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
046500970526     C*         1= RICHIESTO POSIZIONAMENTO
046600970526     C                   Z-ADD     0             SWPOSI            7 0
046700970526     C*
046800991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
046900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
047000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
047100970526     C                   Z-ADD     0             SWMODI            1 0
047200970530     C*
047300970530     C* REIMPOSTA INDICATORI DI ERRORE
047400991108     C                   SETOFF                                       808182
047500991108     C                   SETOFF                                       83
047600970526     C*
047700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
047800021211     C     VIDCO1        IFNE      depco1
047900150210     C     VIDTs2        ORNE      depts2
048000150210     C     VIDco2        ORNE      depco2
048100150219     C     VIDdt1        ORNE      depdt1
048200150325     C     VIDsrs        ORNE      depsrs
048300970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
048400021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
048500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
048600021211     C                   MOVEL     VIDCO1        depco1                         *DEPOSITI
048700150210     C                   MOVEL     VIDTs2        depts2                         *DEPOSITI
048800150210     C                   MOVEL     VIDco2        depco2                         *DEPOSITI
048900150219     C                   MOVEL     VIDdt1        depdt1                         *DEPOSITI
049000150325     C                   MOVEL     VIDsrs        depsrs
049100021211     C                   ENDIF
049200021211     C                   ENDIF
049300970526     C*
049400970526     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
049700970526     C*------------------------------------------------------------------------*
049800970528     C     SETFIL        BEGSR
049900970526     C*
050000970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
050100150130     C                   MOVEL     VIDCO1        KEYksu
050200021211     C     WTIPRI        IFEQ      'COD'
050300150130     C     KEYcGB        SETLL     azcgb01l                           99
050400021211     C                   ENDIF
050500980107     C*
050600021211     C* LETTURA RECORD
050700021211     C     *IN99         IFEQ      *OFF
050800970526     C                   EXSR      LETFIL
050900021211     C                   ENDIF
051000970526     C*
051100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
051200021211     C     *IN99         IFEQ      *ON
051300970526     C                   Z-ADD     1             SFLEOF
051400021211     C                   ENDIF
051500970526     C*
051600970526     C                   ENDSR
051700970526     C*------------------------------------------------------------------------*
051800970526     C* LETFIL - LETTURA RECORD
051900970526     C*------------------------------------------------------------------------*
052000970526     C     LETFIL        BEGSR
052100970526     C*
052200970526     C* LEGGE FINO A:
052300970526     C*     - FINE FILE
052400970526     C*     - TROVATO RECORD VALIDO
052500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
052600021211     C     *IN99         DOWEQ     *OFF
052700970526     C     WRECOK        ANDEQ     'N'
052800970526     C*
052900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
053000021211     C     WTIPRI        IFEQ      'COD'                                        *PER COD. ARTICOLO
053100150130     C                   READ      azcgb01l                               99
053200021211     C                   ENDIF
053300970528     C*
053400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053500021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
053600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
053700021211     C                   ELSE
053800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
053900021211     C                   ENDIF
054000021211     C                   ENDDO
054100970526     C*
054200970526     C                   ENDSR
054300970526     C*------------------------------------------------------------------------*
054400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
054500970526     C*------------------------------------------------------------------------*
054600970526     C     CHKREC        BEGSR
054700970526     C*
054800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
054900021211     C* FILTRA SE RICHIESTO UN TIPO ARTICOLO
055000150210     C     VIDTs2        ifeq      *blanks
055100021211     C                   else
055200150210     C     cgbTsc        ifne      VIDTs2
055300021211     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
055400021211     C                   endif
055500021211     C                   endif
055600150325      * filtra
055700150210     C     VIDco2        ifeq      *blanks
055800150210     C                   else
055900150219     C     '?'           SCAN      VIDCO2                                 30
056000150219     c                   if        *in30
056100150219     C* RICHIESTO ELENCO ARTICOLI
056200150219     C                   MOVEL     KPJBU         SAVKPJBU
056300150219     C                   MOVEL(P)  'P01'         KPJBU
056400150219     C                   CALL      'TISIG1R'
056500150219     C                   PARM                    KPJBA
056600150219     C                   EVAL      VIDCO2 = %subst(KPJBU:4:15)
056700150219     C                   MOVEL     SAVKPJBU      KPJBU
056800150219     c                   endif
056900150210     C     cgbcod1       ifne      VIDco2
057000150210     C     cgbcod2       andne     VIDco2
057100150210     C     cgbcod3       andne     VIDco2
057200150210     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
057300150210     C                   endif
057400150210     C                   endif
057500991108     C*
057600150219     C     VIDdt1        ifeq      0
057700150219     C                   else
057800150219     c                   if        viddt1g <> g08inv
057900150219     C                   Z-ADD     viddt1        g08dat
058000150219     C                   Z-ADD     *ZEROS        g08inv
058100150219     C                   MOVEL     '0'           G08ERR
058200150219     C                   CALL      'XSRDA8'
058300150219     C                   PARM                    WLBDA8
058400150219     C                   Z-ADD     G08dat        viddt1
058500150219     c                   move      g08inv        viddt1g           8 0
058600150219     c                   endif
058700150219     C     cgbdti        ifgt      VIDdt1g
058800150219     C     cgbdtf        orlt      VIDdt1g
058900150219     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059000150219     C                   endif
059100150219     C                   endif
059200150325     C* RAGIONE SOCIALE DIVERSA DA QUELLA SCELTA
059300150325     C                   if        WRECOK = 'S'
059400150325     C                   EVAL      PiStr=%trim(cgbksu)
059500150325     C                   EXSR      CHKNUM
059600150325     C                   IF        PiInt=*on
059700150325     C                   Z-ADD     PiVal         wcli              7 0
059800150325     C                   ENDIF
059900150325     C                   exsr      dectib
060000150325     C     VIDsrs        ifeq      *blanks
060100150325     C                   else
060200150325     C                   z-add     *zeros        pos               2 0
060300150325     C                   eval      pos = %scan(%trim(VIDsrs):wddksc)
060400150325     C                   if        pos = *zeros
060500150325     C                   movel     'N'           WRECOK
060600150325     C                   endif
060700150325     C                   endif
060800150325     C                   endif
060900150325     C*
061000970526     C                   ENDSR
061100150325     C*----------------------------------------------------*
061200150325     C*  CONTROLLO NUMERICITA' CAMPI
061300150325     C*----------------------------------------------------*
061400150325     C     CHKNUM        BEGSR
061500150325     C*
061600150325     C                   IF        PiDecChr = *blanks
061700150325     C                   EVAL      PiDecChr = '.'
061800150325     C                   ENDIF
061900150325     C*
062000150325     C                   CALL(e)   'ISNUMERIC'
062100150325     C                   PARM                    PiStr            30
062200150325     C                   PARM                    PiDecChr          1
062300150325     C                   PARM      *ZEROS        PiVal            30 9
062400150325     C                   PARM      '0'           PiInt             1
062500150325     C                   PARM      '0'           PiNum             1
062600150325     C                   IF        %error
062700150325     C                   EVAL      PiNum=*off
062800150325     C                   ENDIF
062900150325     C*
063000150325     C                   ENDSR
063100150325     C*------------------------------------------------------------------------*
063200150325     C* dectib - decodifica ragione sociale per vedere se compreso in scansione
063300150325     C*------------------------------------------------------------------------*
063400150325     C     dectib        BEGSR
063500150325     C                   CLEAR                   tibs69ds
063600150325     C                   CLEAR                   ds_cnaco
063700150325     C                   MOVEL     KNSIF         I69SIF
063800150325     C                   Z-ADD     WCLI          I69KAC
063900150325     C                   CALL      'TIBS69R'
064000150325     C                   PARM                    tibs69ds
064100150325     C                   PARM                    DS_cnaco
064200150325     C                   PARM                    DS_cnind
064300150325     C                   PARM                    DS_cnclp
064400150325     C                   PARM                    DS_fncls
064500150325     C     O69ERR        IFNE      '1'
064600150325     C                   MOVEL     ACORAG        wddksc           40
064700150325     C                   ELSE
064800150325     C                   MOVEL     *ALL'*'       Wddksc
064900150325     C                   MOVE      '1'           errl00
065000150325     C                   ENDIF
065100150325     C                   endsr
065200970526     C*------------------------------------------------------------------------*
065300000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
065400970526     C*------------------------------------------------------------------------*
065500970526     C     IRESFL        BEGSR
065600970526     C*
065700000407     C                   MOVEL     *blanks       OPZKEY
065800150130     C                   MOVEL     cgbksu        VIDksu
065900150130     C                   MOVEL     cgbTsc        VIDtsc
066000150202     C                   clear                   VIDksud
066100150202      * cerco i dati anagrafici
066200150325     c*m                 clear                   tibs69ds
066300150325     c*m                 move      vidksu        i69kac
066400150325     c*m                 call      'TIBS69R'
066500150325     c*m                 parm                    tibs69ds
066600150325     c*m                 parm                    ds_cnaco
066700150325     c*m                 parm                    ds_cnind
066800150325     c*m                 parm                    ds_cnclp
066900150325     c*m                 parm                    ds_fncls
067000150202      * ragione sociale
067100150325     c                   eval      vidksud = wddksc
067200150210     C* date
067300150210     c                   if        cgbdti > 0
067400150210     C                   Z-ADD     cgbdti        g08inv
067500150210     C                   Z-ADD     *ZEROS        g08dat
067600150210     C                   MOVEL     '3'           G08ERR
067700150210     C                   CALL      'XSRDA8'
067800150210     C                   PARM                    WLBDA8
067900150210     C                   Z-ADD     G08dat        viddti
068000150302     C                   Z-ADD     G08inv        viddtig
068100150210     c                   endif
068200150210     c                   if        cgbdtf > 0
068300150210     C                   Z-ADD     cgbdtf        g08inv
068400150210     C                   Z-ADD     *ZEROS        g08dat
068500150210     C                   MOVEL     '3'           G08ERR
068600150210     C                   CALL      'XSRDA8'
068700150210     C                   PARM                    WLBDA8
068800150210     C                   Z-ADD     G08dat        viddtf
068900150210     c                   endif
069000150219     C                   Z-ADD     0             viddte
069100150210     c                   if        cgbdte > 0
069200150210     C                   Z-ADD     cgbdte        g08inv
069300150210     C                   Z-ADD     *ZEROS        g08dat
069400150210     C                   MOVEL     '3'           G08ERR
069500150210     C                   CALL      'XSRDA8'
069600150210     C                   PARM                    WLBDA8
069700150210     C                   Z-ADD     G08dat        viddte
069800150210     c                   endif
069900150210     c                   clear                   vidfig
070000150210     c     vidksu        setll     azfgb01l
070100150210     c                   do        *hival
070200150210     c     vidksu        reade     azfgb01l
070300150210     c                   if        %eof(azfgb01l)
070400150210     c                   leave
070500150210     c                   endif
070600150226     c                   if        fgbksc = vidksu or
070700150226     c                             fgbann = 'A'
070800150210     c                   iter
070900150210     c                   endif
071000150210     c                   move      '*'           vidfig
071100150210     c                   leave
071200150210     c                   enddo
071300970526     C                   ENDSR
071400970526     C*------------------------------------------------------------------------*
071500991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
071600970526     C*------------------------------------------------------------------------*
071700970526     C     CHKPAR        BEGSR
071800970528     C*
071900021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
072000021211     C                   MOVEL     'COD'         WTIPRI            3
072100000407     C*
072200970526     C                   ENDSR
072300970526     C*------------------------------------------------------------------------*
072400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
072500970526     C*------------------------------------------------------------------------*
072600970526     C     CHKFOP        BEGSR
072700991112     C*
072800991112     C* OPZIONI
072900970626     C*
073000000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
073100021211     C     DEPOP0        IFEQ      'P01'
073200000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
073300000420     C                   MOVEL     'S'           OPDATT
073400000420     C                   MOVEL     OPDRIG        OPD(1)
073500150225     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
073600150225     C                   MOVEL     'N'           OPDATT
073700150225     C                   MOVEL     OPDRIG        OPD(5)
073800150225     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
073900150225     C                   MOVEL     'N'           OPDATT
074000150225     C                   MOVEL     OPDRIG        OPD(2)
074100150225     C                   MOVEL     OPD(3)        OPDRIG                         *NO MODIFICA
074200150225     C                   MOVEL     'N'           OPDATT
074300150225     C                   MOVEL     OPDRIG        OPD(3)
074400150225     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
074500150225     C*                  MOVEL     'N'           OPDATT
074600150225     C*                  MOVEL     OPDRIG        OPD(4)
074700150225     C                   MOVEL     OPD(6)        OPDRIG                         *NO CANCELLAZIONE
074800150302     C                   MOVEL     'S'           OPDATT
074900150225     C                   MOVEL     OPDRIG        OPD(6)
075000150225     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
075100150302     C                   MOVEL     'N'           fudATT
075200150225     C                   MOVEL     FUDRIG        FUD(6)
075300150212     C                   endif
075400000420     C* OPZIONI
075500150212     C     DEPOP0        IFEQ      'P05'
075600970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
075700970626     C                   MOVEL     'S'           OPDATT
075800970626     C                   MOVEL     OPDRIG        OPD(5)
075900150212     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
076000150212     C                   MOVEL     'N'           OPDATT
076100150212     C                   MOVEL     OPDRIG        OPD(2)
076200150212     C                   MOVEL     OPD(3)        OPDRIG                         *NO MODIFICA
076300150212     C                   MOVEL     'N'           OPDATT
076400150212     C                   MOVEL     OPDRIG        OPD(3)
076500150225     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
076600150225     C*                  MOVEL     'N'           OPDATT
076700150225     C*                  MOVEL     OPDRIG        OPD(4)
076800150212     C                   MOVEL     OPD(6)        OPDRIG                         *NO CANCELLAZIONE
076900150212     C                   MOVEL     'N'           OPDATT
077000150212     C                   MOVEL     OPDRIG        OPD(6)
077100150212     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
077200150212     C                   MOVEL     'N'           fudATT
077300150212     C                   MOVEL     FUDRIG        FUD(6)
077400021211     C                   ENDIF
077500970626     C*
077600000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
077700021211     C     DEPOP0        IFEQ      'P02'
077800000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
077900970626     C                   MOVEL     'S'           OPDATT
078000000407     C                   MOVEL     OPDRIG        OPD(2)
078100021211     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
078200021211     C                   MOVEL     'S'           OPDATT
078300021211     C                   MOVEL     OPDRIG        OPD(3)
078400150225     C*                  MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
078500150225     C*                  MOVEL     'S'           OPDATT
078600150225     C*                  MOVEL     OPDRIG        OPD(4)
078700991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
078800991112     C                   MOVEL     'S'           OPDATT
078900991112     C                   MOVEL     OPDRIG        OPD(5)
079000021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
079100000407     C                   MOVEL     'S'           FUDATT
079200000407     C                   MOVEL     FUDRIG        FUD(6)
079300021211     C                   ENDIF
079400970626     C*
079500970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
079600150130     C*m   DUTLPO        IFNE      'S'                                          *FILIALE
079700150130     C*m                 MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
079800150130     C*m                 MOVEL     'N'           OPDATT
079900150130     C*m                 MOVEL     OPDRIG        OPD(2)
080000150130     C*m                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
080100150130     C*m                 MOVEL     'N'           OPDATT
080200150130     C*m                 MOVEL     OPDRIG        OPD(4)
080300150130     C*m                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
080400150130     C*m                 MOVEL     'N'           OPDATT
080500150130     C*m                 MOVEL     FUDRIG        FUD(6)
080600150130     C*m                 ENDIF
080700970526     C*
080800970526     C* GESTIONE OPZIONI A VIDEO
080900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
081000970526     C                   EXSR      OPZGES
081100970526     C*
081200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
081300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
081400970526     C                   EXSR      FUNGES
081500970526     C*
081600970526     C                   ENDSR
081700020917     C*--------------------------------------------------------------------------------------------*
081800020917     C* REPERISCE I DATI UTENTE
081900020917     C*--------------------------------------------------------------------------------------------*
082000020917     C     REPDATIUTE    BEGSR
082100020917     C*
082200020917     C* INIZIALIZZA VARIABILI DI WRK
082300020917     C                   CLEAR                   TIBS34DS
082400020917     C                   CLEAR                   AZUTEDS
082500020917     C                   CLEAR                   DDATIUTE
082600150212     C                   CLEAR                   dute01
082700020917     C*
082800020917     C     *DTAARA       DEFINE    §azute        azuteds
082900020917     C     *DTAARA       DEFINE    §datiute      ddatiute
083000020917     C                   IN(E)     *DTAARA
083100021211     C                   IF        %Error
083200021211     C                   EVAL      I34Tla = 'L'
083300020917     C                   CALL      'TIBS34R'
083400020917     C                   PARM                    Tibs34Ds
083500020917     C                   IN        *DTAARA
083600021211     C                   ENDIF
083700150212
083800150212     c                   Clear                   wabi
083900150212     c                   Clear                   dLat
084000150212
084100150212      * Verifica errori e autorità profilo
084200150212s   1c                   Select
084300150212      * se ho errori nei dati utente esco dal pgm
084400150212w   1c                   When      DutErr = 'E'
084500150212     c                   seton                                        lr
084600150212     c                   return
084700150212      * se non c'è l'abilitazione
084800150212      * --> se 1° livello, abilitazioni al terminal
084900150212      *     se 2° livello, abilitazioni al punto operativo
085000150212w   1c                   When      UteAut = *Blanks
085100150212if  2c                   If        DutLpo = '1'
085200150212     c                   Eval      wabi   = 'TP'
085300150212e   2c                   EndIf
085400150212if  2c                   If        DutLpo = '2'
085500150212     c                   Eval      wabi   = 'PO'
085600150212e   2c                   EndIf
085700150212if  2c                   If        DutLpo = 'S'
085800150212     c                   Eval      wabi   = 'AZ'
085900150212e   2c                   EndIf
086000150212      * carica le abilitazioni del profilo
086100150212x   1c                   Other
086200150212     c                   Movel     UteFaf        Dute01
086300150212     c                   If        §Utecgb <> *Blanks
086400150212     c                   Eval      wabi = §Utecgb
086500150325     c                   Else
086600150212     c                   Eval      wabi = UteAut
086700150212     c                   EndIf
086800150212e   1c                   EndSl
086900150212      * controllo se ok l'abilitazione dell'utente
087000150212     c                   Clear                   Tibs02ds
087100150212     c                   Eval      T02Mod = 'C'
087200150212     c                   Eval      T02Sif = knsif
087300150212     c                   Eval      T02Cod = 'LAT'
087400150212     c                   Movel(p)  wabi          T02Ke1
087500150212     c                   Call      'TIBS02R'
087600150212     c                   Parm                    kpjba
087700150212     c                   Parm                    Tibs02ds
087800150212if  1c                   If        T02Err = *Blanks
087900150212     c                   Eval      dLat = T02Uni
088000150212e   1c                   EndIf
088100150212      * errore o non abilitato (impossibile xchè controllato nel filtro)
088200150212if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
088300150212     C                   MOVEL     'P05'         DEPOP0
088400150212   x1c                   Else
088500150212      * Reperimento dei P.O. gestibili dall'utente
088600150212     c                   clear                   TRUL31DS
088700150212     c                   clear                   TRUL31DS2
088800150212     c                   eval      I31abi = wabi
088900150212     c                   eval      I31cdi = DUTdis
089000150212     c                   eval      I31car = DUTare
089100150212     c                   eval      I31cpo = DUTpou
089200150212     c                   call      'TRUL31R'
089300150212     c                   parm                    KPJBA
089400150212     c                   parm                    TRUL31DS
089500150212     c                   parm                    TRUL31DS2
089600150212if  2c                   if        O31pog > *zeros
089700150212     c                   movea     O31pog        $pog
089800150212     c                   movea     O31arg        $arg
089900150212     c                   movea     O31dig        $dig
090000150212x   2c                   else
090100150212     c* impossibile perchè controllato nel filtrp
090200150212     c                   seton                                        lr
090300150212     c                   return
090400150212e   2c                   endif
090500150212    1c                   endif
090600020917     C*
090700020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
090800020917     C                   MOVEL(P)  RSUT          DSFIRS           20
090900020917     C*
091000020917     C                   ENDSR
091100970526     C*------------------------------------------------------------------------*
091200970526     C* *INZSR - OPERAZIONI INIZIALI
091300970526     C*------------------------------------------------------------------------*
091400970526     C     *INZSR        BEGSR
091500970526     C*---
091600970526     C* RICEVIMENTO PARAMETRI
091700970526     C*---
091800970526     C     *ENTRY        PLIST
091900970612     C                   PARM                    KPJBA
092000021211     C                   MOVEL     KPJBU         PARAM
092100150202     c                   if        pop1 <> *blank
092200021211     C                   MOVEL     POP1          DEPOP0            3
092300150202     c                   else
092400150202     C                   MOVEL     'P02'         DEPOP0
092500150202     c                   endif
092600970526     C*
092700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
092800970526     C     XMSGPA        PLIST
092900970526     C                   PARM                    DSMSMS
093000970526     C                   PARM                    DSMSPG
093100970526     C                   PARM                    DSMSKE
093200970526     C                   PARM                    DSMSER
093300970526     C*
093400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
093500970526     C                   CALL      'XKEY'
093600970526     C                   PARM                    DSKEY
093700970526     C*---
093800970526     C* VARIABILI NON RIFERITE AL DATA BASE
093900970526     C*---
094000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
094100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
094200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
094300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
094400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
094500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
094600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
094700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
094800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
094900970526     C*---
095000021211     C     *LIKE         DEFINE    VIDCO1        depco1
095100150210     C     *LIKE         DEFINE    VIDts2        depts2
095200150210     C     *LIKE         DEFINE    VIDco2        depco2
095300150219     C     *LIKE         DEFINE    VIDdt1        depdt1
095400150325     C     *LIKE         DEFINE    VIDsrs        depsrs
095500991108     C*---
095600970526     C* VARIABILI DI CONTROLLO
095700970526     C*---
095800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
095900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
096000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
096100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
096200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
096300000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
096400970526     C*
096500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
096600150130     C                   MOVEL     'TISIG10R'    DSMSPG
096700150130     C                   MOVEL     'TISIG10R'    MSGQUE
096800970526     C                   MOVEL     *BLANKS       MSGKEY
096900970526     C                   MOVEL     *BLANKS       DSMSMS
097000970526     C                   MOVEL     *BLANKS       DSMSKE
097100970526     C                   Z-ADD     *ZEROS        DSMSER
097200970526     C*---
097300970526     C* CHIAVI DI LETTURA
097400970526     C*---
097500150130     C* LETTURA azcgb01l - COMPLETA
097600150130     C     KEYcGB        KLIST
097700150130     C                   KFLD                    KEYksu
097800021211     C* LETTURA azmgb02l - PARZIALE
097900021211     C     KEYMGBP       KLIST
098000150204     C                   KFLD                    KEYcli            1
098100150130     C                   KFLD                    KEYksu            8
098200150210     C                   move      'C'           KEYcli            1
098300991108     C*
098400970527     C* REPERIMENTO DATI UTENTE
098500020917     C                   EXSR      REPDATIUTE
098600970526     C*
098700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
098800970526     C                   EXSR      CHKPAR
098900991115     C*
099000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
099100970526     C                   EXSR      CHKFOP
099200970526     C*
099300970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
099400021211     C     DEPOP0        IFEQ      'P01'
099500970530     C                   MOVEL     DEC(3)        VIDING
099600021211     C                   ENDIF
099700021211     C     DEPOP0        IFEQ      'P02'
099800970526     C                   MOVEL     DEC(1)        VIDING
099900021211     C                   ENDIF
100000021211     C     DEPOP0        IFEQ      'P05'
100100970526     C                   MOVEL     DEC(2)        VIDING
100200021211     C                   ENDIF
100300970526     C*
100400970526     C                   ENDSR
100500970526     C*------------------------------------------------------------------------*
100600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
100900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
101000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
101100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101300970526     C*------------------------------------------------------------------------*
101400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
101500970526     C*------------------------------------------------------------------------*
101600970526     C     POSSFL        BEGSR
101700970526     C*
101800970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
101900970526     C                   EXSR      CLRSFL
102000970526     C*
102100970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
102200970526     C                   EXSR      SETFIL
102300970526     C*
102400970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
102500970526     C     SFLEOF        IFEQ      0
102600970526     C                   EXSR      CARSFL
102700970526     C                   END
102800970526     C*
102900970526     C                   ENDSR
103000970526     C*------------------------------------------------------------------------*
103100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
103200970526     C*------------------------------------------------------------------------*
103300970526     C     ENTATT        BEGSR
103400970526     C*
103500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
103600970526     C                   Z-ADD     0             OPZOK             1 0
103700970526     C*
103800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
103900970526     C                   Z-ADD     0             SFLVAR            5 0
104000970526     C*
104100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
104200970526     C                   Z-ADD     0             SFLERR
104300970526     C*
104400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
104500970526     C                   Z-ADD     SFLULT        WFLULT
104600970526     C*
104700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
104800021211     C                   READC     SIG1D05                                98
104900970526     C     *IN98         DOWEQ     '0'
105000970526     C*
105100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
105200970526     C                   ADD       1             SFLVAR
105300970526     C*
105400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
105500970526     C     OPZOK         IFEQ      0
105600970526     C*
105700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
105800970526     C     OPZKEY        IFNE      *BLANKS
105900970526     C                   EXSR      OPZCON
106000970526     C                   END
106100970526     C*
106200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
106300970526     C*            2. PULIZIA SCELTA IN SFL
106400970526     C     OPZOK         IFEQ      0
106500970526     C                   MOVEL     '0'           *IN41
106600970526     C                   MOVEL     *BLANKS       OPZKEY
106700970526     C                   ELSE
106800970526     C*
106900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
107000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
107100970526     C                   MOVEL     '1'           *IN41
107200970526     C                   Z-ADD     SFLULT        SFLERR
107300970526     C                   END
107400970526     C                   ELSE
107500970526     C*
107600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
107700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
107800970526     C                   MOVEL     '1'           *IN41
107900970526     C                   END                                                    OPZOK=0
108000970526     C*
108100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
108200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
108300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
108400970526     C     OPZOK         IFNE      0
108500970526     C                   Z-ADD     SFLERR        SFLNBR
108600970526     C                   ELSE
108700970526     C                   Z-ADD     SFLULT        SFLNBR
108800970526     C                   END
108900970526     C*
109000970526     C* AGGIORNO RECORD SFL
109100021211     C                   UPDATE    SIG1D05
109200970526     C*
109300021211     C                   READC     SIG1D05                                98
109400970526     C                   END                                                    DO WHILE
109500970526     C*
109600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
109700970526     C                   Z-ADD     WFLULT        SFLULT
109800970526     C*
109900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
110000970526     C     SFLVAR        IFEQ      0
110100970526     C                   END
110200991112     C*
110300970526     C                   ENDSR
110400970526     C*------------------------------------------------------------------------*
110500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
110600970526     C*------------------------------------------------------------------------*
110700970526     C     CLRSFL        BEGSR
110800970526     C*
110900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
111000970526     C* (0=NON FINE FILE  1=FINE FILE)
111100970526     C                   Z-ADD     0             SFLEOF
111200970526     C*
111300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
111400970526     C                   Z-ADD     0             SFLULT
111500970526     C*
111600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
111700970526     C                   MOVEL     '1'           *IN40
111800021211     C                   WRITE     SIG1D06
111900970526     C                   MOVEL     '0'           *IN40
112000970526     C*
112100970526     C                   ENDSR
112200970526     C*------------------------------------------------------------------------*
112300970526     C* CARSFL - CARICAMENTO SFL
112400970526     C*------------------------------------------------------------------------*
112500970526     C     CARSFL        BEGSR
112600970526     C*
112700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
112800970526     C     SFLULT        IFEQ      0
112900970526     C                   EXSR      SETFIL
113000970526     C                   END
113100970526     C*
113200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
113300970526     C                   Z-ADD     SFLULT        WFLULT
113400970526     C*
113500970526     C* CICLO:
113600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
113700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
113800970526     C                   Z-ADD     1             SFLRIG
113900970526     C     SFLRIG        DOWLE     SFLPAG
114000970526     C     SFLEOF        ANDEQ     0
114100970526     C*
114200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
114300991116     C                   MOVEL     *BLANKS       OPZKEY
114400970526     C*
114500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
114600970526     C                   EXSR      IRESFL
114700970526     C*
114800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
114900970526     C                   ADD       1             SFLULT
115000970526     C*
115100970526     C* SCRITTURA RECORD SFL
115200021211     C                   WRITE     SIG1D05
115300970526     C                   ADD       1             SFLRIG
115400970526     C*
115500970526     C* LETTURA FILE DA CARICARE
115600970526     C                   EXSR      LETFIL
115700970526     C                   END                                                    DO WHILE
115800970526     C*
115900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
116000970526     C     WFLULT        IFNE      SFLULT
116100970526     C                   Z-ADD     WFLULT        SFLNBR
116200970526     C                   ADD       1             SFLNBR
116300970526     C                   END
116400970526     C*
116500970526     C                   ENDSR
116600970526     C*------------------------------------------------------------------------*
116700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
116800970526     C*------------------------------------------------------------------------*
116900970526     C     OUTSFL        BEGSR
117000970526     C*
117100970526     C* WRITE RECORD TESTATA VIDEO
117200021211     C                   WRITE     SIG1D01
117300970526     C*
117400970526     C* VISUALIZZAZIONE MESSAGGIO
117500970526     C                   EXSR      SNDMSG
117600970526     C*
117700970526     C* WRITE RECORD OPZIONI VIDEO
117800021211     C                   WRITE     SIG1D02
117900970526     C*
118000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
118100021211     C                   WRITE     SIG1D03
118200970526     C*
118300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
118400021211     C                   WRITE     SIG1D04
118500970526     C*
118600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
118700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
118800970526     C     SFLEOF        IFEQ      0
118900970526     C                   MOVEL     '0'           *IN42
119000970526     C                   ELSE
119100970526     C*
119200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
119300970526     C                   MOVEL     '1'           *IN42
119400970526     C                   END
119500970526     C*
119600970526     C* WRITE RECORD TASTI FUNZIONALI
119700021211     C                   WRITE     SIG1D09
119800970526     C*
119900970526     C* CASO DI SFL VUOTO
120000970526     C     SFLULT        IFLE      0
120100970526     C*
120200970526     C*    - WRITE RECORD VUOTO
120300021211     C                   WRITE     SIG1D0A
120400021211     C                   READ      SIG1D0A                                99
120500970526     C*
120600970526     C                   ELSE
120700970526     C*
120800970526     C* CASO DI SFL CON RECORDS
120900970526     C*
121000970526     C*    - WRITE RECORD CONTROLLO SFL
121100021211     C                   WRITE     SIG1D06
121200021211     C                   READ      SIG1D06                                99
121300970526     C                   END
121400970526     C*
121500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
121600021211     C                   READ      SIG1D03                                99
121700970526     C*
121800970526     C                   ENDSR
121900970526     C*------------------------------------------------------------------------*
122000970526     C* SNDMSG - MANDA MESSAGGIO
122100970526     C*------------------------------------------------------------------------*
122200970526     C     SNDMSG        BEGSR
122300970526     C*
122400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
122500970526     C                   MOVEL     '1'           *IN40
122600021211     C                   WRITE     SIG1DMC
122700970526     C                   MOVEL     '0'           *IN40
122800970526     C*
122900970526     C* VISUALIZZAZIONE MESSAGGIO
123000970526     C                   Z-ADD     0             MSGULT
123100970526     C                   CALL      'XMSG'        XMSGPA
123200970526     C     DSMSER        IFEQ      0
123300970526     C                   MOVEL     DSMSKE        MSGKEY
123400970526     C                   END
123500970526     C                   Z-ADD     1             MSGULT
123600021211     C                   WRITE     SIG1DMS                                      SFL MESS
123700021211     C                   WRITE     SIG1DMC                                      CONTR MESS
123800970526     C*
123900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
124000970526     C                   MOVEL     *BLANKS       DSMSMS
124100970526     C*
124200970526     C                   ENDSR
124300970526     C*------------------------------------------------------------------------*
124400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
124500970526     C*------------------------------------------------------------------------*
124600970526     C     FUNCON        BEGSR
124700970526     C*
124800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
124900970526     C                   EXSR      FUNVAR
125000970526     C*
125100970526     C* PULISCO CAMPI DI LAVORO
125200970526     C                   Z-ADD     0             I                 5 0
125300970526     C                   Z-ADD     0             W                 5 0
125400970526     C*
125500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125600970526     C     FUNKEY        IFNE      *BLANKS
125700970526     C     1             DO        24            I
125800970526     C                   MOVEL     FUD(I)        FUDRIG
125900970526     C*
126000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
126100970526     C     FUNKEY        IFEQ      FUDFUN
126200970526     C     FUDATT        ANDEQ     'S'
126300970526     C                   Z-ADD     I             W
126400970526     C                   END
126500970526     C                   END                                                    DO WHILE
126600970526     C*
126700970526     C* FUNZIONE NON ATTIVA (ERRORE)
126800970526     C     W             IFEQ      0
126900970526     C*
127000970526     C* EMISSIONE MESSAGGIO DI ERRORE
127100970526     C                   MOVEL     MSG(2)        DSMSMS
127200970526     C                   END                                                    W=0
127300970526     C                   END                                                    FUNKEY NE *BLANKS
127400970526     C*
127500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
127600970526     C     W             IFNE      0
127700970526     C     FUNKEY        OREQ      *BLANKS
127800970526     C*
127900970526     C     FUNKEY        IFEQ      *BLANKS
128000970526     C                   EXSR      ENTGES
128100970526     C                   END
128200970526     C     FUNKEY        IFEQ      '03'
128300970526     C                   EXSR      F03GES
128400970526     C                   END
128500970526     C     FUNKEY        IFEQ      '05'
128600970526     C                   EXSR      F05GES
128700970526     C                   END
128800150204     C     FUNKEY        IFEQ      '06'
128900150204     C                   EXSR      F06GES
129000970526     C                   END
129100150324     C     FUNKEY        IFEQ      '11'
129200150324     C                   EXSR      F11GES
129300150324     C                   END
129400970526     C     FUNKEY        IFEQ      '12'
129500970526     C                   EXSR      F12GES
129600970526     C                   END
129700970528     C     FUNKEY        IFEQ      '21'
129800970528     C                   EXSR      F21GES
129900970528     C                   END
130000970528     C     FUNKEY        IFEQ      '22'
130100970528     C                   EXSR      F22GES
130200970528     C                   END
130300970526     C     FUNKEY        IFEQ      '23'
130400970526     C                   EXSR      F23GES
130500970526     C                   END
130600970526     C     FUNKEY        IFEQ      '24'
130700970526     C                   EXSR      F24GES
130800970526     C                   END
130900970526     C     FUNKEY        IFEQ      'RU'
131000970526     C                   EXSR      RUPGES
131100970526     C                   END
131200970526     C*
131300970526     C                   END
131400970526     C*
131500970526     C                   ENDSR
131600970526     C*------------------------------------------------------------------------*
131700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
131800970526     C*------------------------------------------------------------------------*
131900970526     C     FUNVAR        BEGSR
132000970526     C*
132100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
132200970526     C     KEY           IFEQ      ENTER
132300970526     C                   MOVEL     *BLANKS       FUNKEY
132400970526     C                   END
132500970526     C     KEY           IFEQ      F1
132600970526     C                   MOVEL     '01'          FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F2
132900970526     C                   MOVEL     '02'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F3
133200970526     C                   MOVEL     '03'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F4
133500970526     C                   MOVEL     '04'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F5
133800970526     C                   MOVEL     '05'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F6
134100970526     C                   MOVEL     '06'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F7
134400970526     C                   MOVEL     '07'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F8
134700970526     C                   MOVEL     '08'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F9
135000970526     C                   MOVEL     '09'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F10
135300970526     C                   MOVEL     '10'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F11
135600970526     C                   MOVEL     '11'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F12
135900970526     C                   MOVEL     '12'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F13
136200970526     C                   MOVEL     '13'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F14
136500970526     C                   MOVEL     '14'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F15
136800970526     C                   MOVEL     '15'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F16
137100970526     C                   MOVEL     '16'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F17
137400970526     C                   MOVEL     '17'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F18
137700970526     C                   MOVEL     '18'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F19
138000970526     C                   MOVEL     '19'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F20
138300970526     C                   MOVEL     '20'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F21
138600970526     C                   MOVEL     '21'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F22
138900970526     C                   MOVEL     '22'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      F23
139200970526     C                   MOVEL     '23'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      F24
139500970526     C                   MOVEL     '24'          FUNKEY
139600970526     C                   END
139700970526     C     KEY           IFEQ      ROLLUP
139800970526     C                   MOVEL     'RU'          FUNKEY
139900970526     C                   END
140000970526     C     KEY           IFEQ      HELP
140100970526     C                   MOVEL     'HP'          FUNKEY
140200970526     C                   END
140300970526     C*
140400970526     C                   ENDSR
140500970526     C*------------------------------------------------------------------------*
140600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
140700970526     C*------------------------------------------------------------------------*
140800970526     C     FUNGES        BEGSR
140900970526     C*
141000970526     C* PULISCO CAMPI DI LAVORO
141100970526     C                   Z-ADD     0             W                 5 0
141200970526     C*
141300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
141400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
141500970526     C     1             DO        24            I
141600970526     C                   MOVEL     FUD(I)        FUDRIG
141700970526     C*
141800970526     C* PULIZIA FUNZIONI PRECEDENTI
141900970526     C                   MOVEL     *BLANKS       FUV(I)
142000970526     C*
142100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
142200970526     C* (SALTA F23 E F24)
142300970526     C     FUDATT        IFEQ      'S'
142400970526     C     FUDFUN        ANDNE     '23'
142500970526     C     FUDFUN        ANDNE     '24'
142600970526     C                   ADD       1             W
142700970526     C                   MOVEL     FUDDES        FUV(W)
142800970526     C                   END
142900970526     C                   END                                                    DO WHILE
143000970526     C*
143100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
143200970526     C* ALTRIMENTI DISATTIVO F24
143300970526     C     1             DO        24            I
143400970526     C                   MOVEL     FUD(I)        FUDRIG
143500970526     C*
143600970526     C     FUDFUN        IFEQ      '24'
143700970526     C     W             IFGT      6
143800970526     C                   MOVEL     'S'           FUDATT
143900970526     C                   ELSE
144000970526     C                   MOVEL     *BLANKS       FUDATT
144100970526     C                   END                                                    W GT 8
144200970526     C                   MOVEL     FUDRIG        FUD(I)
144300970526     C                   END
144400970526     C                   END                                                    DO WHILE
144500970526     C*
144600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
144700970526     C     W             DIV       6             I
144800970526     C                   MVR                     Z
144900970526     C*
145000970526     C     Z             IFNE      0
145100970526     C     I             ADD       1             Z
145200970526     C                   ELSE
145300970526     C                   Z-ADD     I             Z
145400970526     C                   END
145500970526     C*
145600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
145700970526     C                   ADD       1             FUNGRU
145800970526     C     FUNGRU        IFGT      Z
145900970526     C     FUNGRU        ORLT      1
146000970526     C                   Z-ADD     1             FUNGRU
146100970526     C                   END
146200970526     C*
146300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
146400970526     C     FUNGRU        IFEQ      1
146500970526     C                   MOVEL     FUV(1)        FUN001
146600970526     C                   MOVEL     FUV(2)        FUN002
146700970526     C                   MOVEL     FUV(3)        FUN003
146800970526     C                   MOVEL     FUV(4)        FUN004
146900970526     C                   MOVEL     FUV(5)        FUN005
147000970526     C                   MOVEL     FUV(6)        FUN006
147100970526     C                   END
147200970526     C*
147300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
147400970526     C     FUNGRU        IFEQ      2
147500970526     C                   MOVEL     FUV(7)        FUN001
147600970526     C                   MOVEL     FUV(8)        FUN002
147700970526     C                   MOVEL     FUV(9)        FUN003
147800970526     C                   MOVEL     FUV(10)       FUN004
147900970526     C                   MOVEL     FUV(11)       FUN005
148000970526     C                   MOVEL     FUV(12)       FUN006
148100970526     C                   END
148200970526     C*
148300970526     C* TERZO GRUPPO DI 6 FUNZIONI
148400970526     C     FUNGRU        IFEQ      3
148500970526     C                   MOVEL     FUV(13)       FUN001
148600970526     C                   MOVEL     FUV(14)       FUN002
148700970526     C                   MOVEL     FUV(15)       FUN003
148800970526     C                   MOVEL     FUV(16)       FUN004
148900970526     C                   MOVEL     FUV(17)       FUN005
149000970526     C                   MOVEL     FUV(18)       FUN006
149100970526     C                   END
149200970526     C*
149300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
149400970526     C     FUNGRU        IFEQ      4
149500970526     C                   MOVEL     FUV(19)       FUN001
149600970526     C                   MOVEL     FUV(20)       FUN002
149700970526     C                   MOVEL     FUV(21)       FUN003
149800970526     C                   MOVEL     FUV(22)       FUN004
149900970526     C                   MOVEL     *BLANKS       FUN005
150000970526     C                   MOVEL     *BLANKS       FUN006
150100970526     C                   END
150200970526     C*
150300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
150400970526     C                   MOVEL     *BLANKS       FUN007
150500970526     C                   MOVEL     *BLANKS       FUN008
150600970526     C*
150700970526     C     1             DO        24            I
150800970526     C                   MOVEL     FUD(I)        FUDRIG
150900970526     C     FUDFUN        IFEQ      '23'
151000970526     C     FUDATT        ANDEQ     'S'
151100970526     C                   MOVEL     FUDDES        FUN007
151200970526     C                   END
151300970526     C     FUDFUN        IFEQ      '24'
151400970526     C     FUDATT        ANDEQ     'S'
151500970526     C                   MOVEL     FUDDES        FUN008
151600970526     C                   END
151700970526     C                   END                                                    DO
151800970526     C*
151900970526     C                   ENDSR
152000970526     C*------------------------------------------------------------------------*
152100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
152200970526     C*------------------------------------------------------------------------*
152300970526     C     OPZGES        BEGSR
152400970526     C*
152500970526     C* PULISCO CAMPI DI LAVORO
152600970526     C                   Z-ADD     0             W                 5 0
152700970526     C*
152800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
152900970526     C* E CARICO TABELLA OPZIONI ATTIVE
153000991115     C     1             DO        24            I
153100970526     C                   MOVEL     OPD(I)        OPDRIG
153200970526     C*
153300970526     C* PULIZIA OPZIONI PRECEDENTI
153400970526     C                   MOVEL     *BLANKS       OPV(I)
153500970526     C*
153600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
153700970526     C     OPDATT        IFEQ      'S'
153800970526     C                   ADD       1             W
153900970526     C                   MOVEL     OPDDES        OPV(W)
154000970526     C                   END
154100970526     C                   END                                                    DO WHILE
154200970526     C*
154300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
154400970526     C* ALTRIMENTI DISATTIVO F23
154500991115     C     1             DO        24            I
154600970526     C                   MOVEL     FUD(I)        FUDRIG
154700970526     C*
154800970526     C     FUDFUN        IFEQ      '23'
154900970526     C     W             IFGT      8
155000970526     C                   MOVEL     'S'           FUDATT
155100970526     C                   ELSE
155200970526     C                   MOVEL     *BLANKS       FUDATT
155300970526     C                   END                                                    W GT 8
155400970526     C                   MOVEL     FUDRIG        FUD(I)
155500970526     C                   END
155600970526     C                   END                                                    DO WHILE
155700970526     C*
155800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
155900970526     C     W             DIV       8             I
156000970526     C                   MVR                     Z                 5 0
156100970526     C*
156200970526     C     Z             IFNE      0
156300970526     C     I             ADD       1             Z
156400970526     C                   ELSE
156500970526     C                   Z-ADD     I             Z
156600970526     C                   END
156700970526     C*
156800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
156900970526     C                   ADD       1             OPZGRU
157000970526     C     OPZGRU        IFGT      Z
157100970526     C     OPZGRU        ORLT      1
157200970526     C                   Z-ADD     1             OPZGRU
157300970526     C                   END
157400970526     C*
157500970526     C* PRIMO GRUPPO DI 8 OPZIONI
157600970526     C     OPZGRU        IFEQ      1
157700970526     C                   MOVEL     OPV(1)        OPZ001
157800970526     C                   MOVEL     OPV(2)        OPZ002
157900970526     C                   MOVEL     OPV(3)        OPZ003
158000970526     C                   MOVEL     OPV(4)        OPZ004
158100970526     C                   MOVEL     OPV(5)        OPZ005
158200970526     C                   MOVEL     OPV(6)        OPZ006
158300970526     C                   MOVEL     OPV(7)        OPZ007
158400970526     C                   MOVEL     OPV(8)        OPZ008
158500970526     C                   END
158600970526     C*
158700970526     C* SECONDO GRUPPO DI 8 OPZIONI
158800970526     C     OPZGRU        IFEQ      2
158900970526     C                   MOVEL     OPV(9)        OPZ001
159000970526     C                   MOVEL     OPV(10)       OPZ002
159100970526     C                   MOVEL     OPV(11)       OPZ003
159200970526     C                   MOVEL     OPV(12)       OPZ004
159300970526     C                   MOVEL     OPV(13)       OPZ005
159400970526     C                   MOVEL     OPV(14)       OPZ006
159500970526     C                   MOVEL     OPV(15)       OPZ007
159600970526     C                   MOVEL     OPV(16)       OPZ008
159700970526     C                   END
159800970526     C*
159900970526     C* TERZO GRUPPO DI 8 OPZIONI
160000970526     C     OPZGRU        IFEQ      3
160100970526     C                   MOVEL     OPV(17)       OPZ001
160200970526     C                   MOVEL     OPV(18)       OPZ002
160300970526     C                   MOVEL     OPV(19)       OPZ003
160400970526     C                   MOVEL     OPV(20)       OPZ004
160500970526     C                   MOVEL     OPV(21)       OPZ005
160600970526     C                   MOVEL     OPV(22)       OPZ006
160700970526     C                   MOVEL     OPV(23)       OPZ007
160800970526     C                   MOVEL     OPV(24)       OPZ008
160900970526     C                   END
161000970526     C*
161100970526     C                   ENDSR
161200970526     C*------------------------------------------------------------------------*
161300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
161400970526     C*------------------------------------------------------------------------*
161500970526     C     OPZCON        BEGSR
161600970526     C*
161700970526     C* PULISCO CAMPI DI LAVORO
161800970526     C                   Z-ADD     0             I                 5 0
161900970526     C                   Z-ADD     0             W                 5 0
162000970526     C*
162100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
162200970526     C     1             DO        24            I
162300970526     C                   MOVEL     OPD(I)        OPDRIG
162400970526     C*
162500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
162600970526     C     OPZKEY        IFEQ      OPDOPZ
162700970526     C     OPZKEY        OREQ      OPDOP1
162800970526     C*
162900970526     C     OPDATT        IFEQ      'S'
163000970526     C                   MOVEL     OPDOPZ        OPZKEY
163100970526     C                   Z-ADD     I             W
163200970526     C                   END
163300970526     C                   END
163400970526     C*
163500970526     C                   END                                                    DO WHILE
163600970526     C*
163700970526     C* OPZIONE NON ATTIVA (ERRORE)
163800970526     C     W             IFEQ      0
163900970526     C*
164000970526     C* OPZIONE NON OK
164100970526     C                   Z-ADD     1             OPZOK
164200970526     C*
164300970526     C* EMISSIONE MESSAGGIO DI ERRORE
164400970526     C                   MOVEL     MSG(1)        DSMSMS
164500970526     C                   ELSE
164600970526     C*
164700970526     C* OPZIONE ATTIVA (PROCEDI ...)
164800970526     C*
164900021211     C                   MOVEL     *BLANKS       PMSG
165000000420     C     OPZKEY        IFEQ      ' 1'
165100000420     C                   EXSR      O01GES
165200000420     C                   END
165300000407     C     OPZKEY        IFEQ      ' 2'
165400000407     C                   EXSR      O02GES
165500000407     C                   END
165600021211     C     OPZKEY        IFEQ      ' 3'
165700021211     C                   EXSR      O03GES
165800021211     C                   END
165900991110     C     OPZKEY        IFEQ      ' 4'
166000991110     C                   EXSR      O04GES
166100991014     C                   END
166200970526     C     OPZKEY        IFEQ      ' 5'
166300970526     C                   EXSR      O05GES
166400970526     C                   END
166500150211     C     OPZKEY        IFEQ      ' F'
166600150211     C                   EXSR      O0FGES
166700150211     C                   END
166800030123     C                   MOVEL     PMSG          DSMSMS
166900021211     C                   END                                                    OPZ.NON ATTIVA
167000970526     C*
167100970526     C                   ENDSR
167200150324      *--------------------------------------------------------------------------------------------*
167300150324      * ordina - riscrive il sfl ordinato
167400150324      *--------------------------------------------------------------------------------------------*
167500150324     C     ordina        BEGSR
167600150324     C                   EVAL      righe = SFLULT
167700150324      * IMPOSTO ENTRATA AD 'A'
167800150324     C                   EVAL      SFLULT= 0
167900150324      *
168000150324     C                   CLEAR                   QLGSCB
168100150324     C                   CLEAR                   QLGSCB00
168200150324
168300150324     ** 2 campi chiave.
168400150324     C                   EVAL      QLGNBRK    = 1
168500150324     c                   select
168600150324     c* ordinamento per prv/rag.sociale destinatario
168700150324     c                   when      w_ord=1
168800150324     **  rag. sociale cliente  2 campo
168900150324     C                   EVAL      QLGSP      = 1 + %SIZE(vidksu)
169000150324     C                   EVAL      QLGSS      = %SIZE(vidksud)
169100150324     C                   EVAL      QLGDT      = carattere
169200150324     C                   EVAL      QLGSO      = Ascendente
169300150324     C                   EVAL      QLGKL(1)   = QLGSKL
169400150324     c                   eval      w_ord = 2
169500150324     c* ordinamento per rag.sociale/prv destinatario
169600150324     c                   when      w_ord=2
169700150324     C                   EVAL      QLGSP      = 1
169800150324     C                   EVAL      QLGSS      = %SIZE(vidksu)
169900150324     C                   EVAL      QLGDT      = carattere
170000150324     C                   EVAL      QLGSO      = Ascendente
170100150324     C                   EVAL      QLGKL(1)   = QLGSKL
170200150324     c                   eval      w_ord = 1
170300150324     ** ragione sociale a posizione 1, carattere, 24 byte,
170400150324     c                   endsl
170500150324
170600150324      * Load other sort parameters.
170700150324     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
170800150324     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
170900150324     C                   EVAL      QLGRT     = 8
171000150324     C                   EVAL      QLGOKL    = 80
171100150324     C                   EVAL      QLGLKE    = 16
171200150324     C                   EVAL      QLGLSS    = 290
171300150324
171400150324      * Initialize Sort I/O API fields.
171500150324     C                   EVAL      QLGRL00  = QLGRL
171600150324     C                   EVAL      QLGRC00  = 1
171700150324     C                   CLEAR                   QUSEI
171800150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
171900150324
172000150324      * First step - Initialize the sort routine.
172100150324     C                   CALL      'QLGSORT'
172200150324     C                   PARM                    QLGSCB
172300150324     C                   PARM                    NotUsed
172400150324     C                   PARM                    NotUsed
172500150324     C                   PARM                    SizeList
172600150324     C                   PARM                    ReturnSize
172700150324     C                   PARM                    QUSEC
172800150324
172900150324      * Next step - Write records to I/O routine.
173000150324     C                   EVAL      QLGRT00 = Put
173100150324
173200150324     C                   DO        *hival        cx                4 0
173300150324
173400150324     C     cx            CHAIN     SIG1D05                            50
173500150324     c   50              leave
173600150324     ** Solo le righe con Selected = 'Y' sono riordinate,
173700150324     ** quindi per fare un ordinamento di tutte le righe
173800150324     ** metto 'Y' sempre.
173900150324     C                   EVAL      Selected = 'Y'
174000150324
174100150324     C                   CLEAR                   QUSEI
174200150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
174300150324     C                   CALL      'QLGSRTIO'
174400150324     C                   PARM                    QLGSCB00
174500150324     C                   PARM                    SflRcd
174600150324     C                   PARM                    NotUsed
174700150324     C                   PARM                    SizeList
174800150324     C                   PARM                    NotUsed
174900150324     C                   PARM                    QUSEC
175000150324
175100150324     C                   ENDDO
175200150324
175300150324      * Next step - Signal end of input, clear subfile for reload.
175400150324     C                   EVAL      QLGRT00 = EndPut
175500150324     C                   CLEAR                   QUSEI
175600150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
175700150324
175800150324     C                   CALL      'QLGSRTIO'
175900150324     C                   PARM                    QLGSCB00
176000150324     C                   PARM                    SflRcd
176100150324     C                   PARM                    NotUsed
176200150324     C                   PARM                    SizeList
176300150324     C                   PARM                    NotUsed
176400150324     C                   PARM                    QUSEC
176500150324
176600150324     C                   MOVEL     '1'           *IN40
176700150324     C                   WRITE     SIG1D06
176800150324     C                   MOVEL     '0'           *IN40
176900150324
177000150324      * Final step - Write the records back to the subfile.
177100150324     C                   EVAL      QLGRT00 = Get
177200150324     C                   DO        righe         x                 4 0
177300150324     C                   CLEAR                   QUSEI
177400150324     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
177500150324     C                   CALL      'QLGSRTIO'
177600150324     C                   PARM                    QLGSCB00
177700150324     C                   PARM                    NotUsed
177800150324     C                   PARM                    SflRcd
177900150324     C                   PARM                    QLGRL00
178000150324     C                   PARM                    NotUsed
178100150324     C                   PARM                    QUSEC
178200150324      *
178300150324     c                   z-add     x             SFLULT
178400150324     C                   WRITE     SIG1D05
178500150324     C                   ENDDO
178600150324
178700150324      *
178800150324     C                   ENDSR
178900970526     C*------------------------------------------------------------------------*
179000970526     C* NOTE COSTRUZIONE SCHIERE
179100970526     C*------------------------------------------------------------------------*
179200970526     C*
179300970526     C* -MSG-MESSAGGI
179400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
179500970526     C* (NON TOCCARE MSG,1 E MSG,2)
179600970526     C*
179700970526     C* -CMD-COMANDI
179800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
179900970526     C*
180000970526     C* -OPD-OPZIONI
180100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
180200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
180300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
180400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
180500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
180600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
180700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
180800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
180900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
181000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
181100970526     C*
181200970526     C* -FUD-FUNZIONI
181300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
181400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
181500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
181600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
181700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
181800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
181900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
182000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
182100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
182200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
182300991108     C*
182400991115     C* -STS-CODICI STATI DEI RECORD DI LOG
182500991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
182600991110     C* ================> IMPORTANTE <=================
182700991115     C* per codici di errore in -STS- indicare in -DES- sempre
182800991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
182900991110     C* ERR
183000991108     C*
183100970526     O*------------------------------------------------------------------------*
183200970526** DEC - DECODIFICHE
183300970526    GESTIONE                                                     1
183400000407 INTERROGAZIONE                                                  2
183500970530     SCELTA                                                      3
1836009705264
1837009705265
183800970526** MSG - MESSAGGI
183900150202TISIG10R- Opzione non gestita.                                                  1
184000150202TISIG10R- Tasto funzionale non gestito.                                         2
184100150202TISIG10R- Utente non abilitato all'inserimento.                                 3
184200150202TISIG10R- Impossibile annullare, presenti movimenti su tale codice articolo.    4
184300150202TISIG10R- 5                                                                     5
184400150202TISIG10R- 6                                                                     6
184500150202TISIG10R- 7                                                                     7
184600150202TISIG10R- 8                                                                     8
184700150202TISIG10R- 9                                                                     9
184800150202TISIG10R- 10                                                                   10
184900150202TISIG10R- 11                                                                   11
185000150202TISIG10R- 12                                                                   12
185100150202TISIG10R- 13                                                                   13
185200150202TISIG10R- 14                                                                   14
185300150202TISIG10R- 15                                                                   15
185400150202TISIG10R- 16                                                                   16
185500150202TISIG10R- 17                                                                   17
185600150202TISIG10R- 18                                                                   18
185700150202TISIG10R- 19                                                                   19
185800150202TISIG10R- 20                                                                   20
185900970528** CMD - COMANDI CL
186000021211
186100021211
186200991110
186300970528
186400970528
186500970526** OPD - OPZIONI
186600150211 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
186700150202 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
186800150202 3=Copia           -3 -     (S/N)=S                        opz. 03¬
186900150225                   -  -     (S/N)=N                        opz. 04¬
187000150202 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
187100150211 F=Abbinati        -F -     (S/N)=S                        opz. 06¬
187200021211                   -  -     (S/N)=N                        opz. 07¬
187300991110                   -  -     (S/N)=N                        opz. 08¬
187400991110                   -  -     (S/N)=N                        opz. 09¬
187500991110                   -  -     (S/N)=N                        opz. 10¬
187600000407                   -  -     (S/N)=N                        opz. 11¬
187700970526                   -  -     (S/N)=N                        opz. 12¬
187800970526                   -  -     (S/N)=N                        opz. 13¬
187900000407                   -  -     (S/N)=N                        opz. 14¬
188000000407                   -  -     (S/N)=N                        opz. 15¬
188100970526                   -  -     (S/N)=N                        opz. 16¬
188200970526                   -  -     (S/N)=N                        opz. 17¬
188300970526                   -  -     (S/N)=N                        opz. 18¬
188400970526                   -  -     (S/N)=N                        opz. 19¬
188500970526                   -  -     (S/N)=N                        opz. 20¬
188600970526                   -  -     (S/N)=N                        opz. 21¬
188700970526                   -  -     (S/N)=N                        opz. 22¬
188800970526                   -  -     (S/N)=N                        opz. 23¬
188900000407                   -  -     (S/N)=N                        opz. 25¬
189000970526** FUD - FUNZIONI
189100970526                   -        (S/N)=N                        funz.01¬
189200970526                   -        (S/N)=N                        funz.02¬
189300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
189400970526                   -        (S/N)=N                        funz.04¬
189500970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
189600150212F06=Inserimento    -        (S/N)=S                        funz.06¬
189700970526                   -        (S/N)=N                        funz.07¬
189800970527                   -        (S/N)=N                        funz.08¬
189900970527                   -        (S/N)=N                        funz.09¬
190000150204                   -        (S/N)=N  <== S-FISSO           funz.10¬
190100150325                   -        (S/N)=N                        funz.11¬
190200970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
190300991116                   -        (S/N)=N                        funz.13¬
190400970526                   -        (S/N)=N                        funz.14¬
190500970526                   -        (S/N)=N                        funz.15¬
190600970526                   -        (S/N)=N                        funz.16¬
190700970526                   -        (S/N)=N                        funz.17¬
190800970526                   -        (S/N)=N                        funz.18¬
190900970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
191000970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
191100000407                   -        (S/N)=N                        funz.21¬
191200991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
191300991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
191400991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
