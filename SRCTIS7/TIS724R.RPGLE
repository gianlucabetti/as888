000100970526     H*------------------------------------------------------------------------*
000200000808     H* Gestione - codici clienti unificanti
000300970526     H*------------------------------------------------------------------------*
000400000830     H DECEDIT('0,') DATEDIT(*DMY.) EXPROPTS(*RESDECPOS)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991104     FTIS724D   CF   E             WORKSTN INFDS(DEVDS1)
000900991104     F                                     SFILE(S724D05:SFLULT)
001000991104     F                                     SFILE(S724DMS:MSGULT)
001100970526     F*------------
001200991104     Ftivss01l  IF   E           K DISK
001300000131     Ftikun01l  IF   E           K DISK
001400000216     Ftikun03L  IF   E           K DISK
001500020212     F                                     RENAME(tikun000:tikun003)
001600020211     Ftikun06L  IF   E           K DISK
001700020211     F                                     RENAME(tikun000:tikun006)
001800000728     Ftianv01l  IF   E           K DISK
001900020212     Ftntbe01l  IF   E           K DISK
002000021030     Fsgius     IF   E           K DISK
002100970526     D*------------------------------------------------------------------------*
002200970526     D* SCHIERE
002300970526     D*------------------------------------------------------------------------*
002400970526     D*------------
002500970526     D* DECODIFICHE
002600970526     D*------------
002700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* MESSAGGI
003000970526     D*------------
003100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003200970528     D*------------
003300970528     D* COMANDI
003400970528     D*------------
003500970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI OPZIONI
003800970526     D*------------
003900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* DESCRIZIONI TASTI FUNZIONALI
004200970526     D*------------
004300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004400970526     D*------------
004500970526     D* OPZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970526     D OPV             S             19    DIM(24)
004800970526     D*------------
004900970526     D* FUNZIONI DA CARICARE A VIDEO
005000970526     D*------------
005100970527     D FUV             S             19    DIM(24)
005200970526     D*------------------------------------------------------------------------*
005300970526     D* INPUT
005400970526     D*------------------------------------------------------------------------*
005500970526     D*------------
005600970526     D* RIDEFINIZIONE OPZIONI VIDEO
005700970526     D*------------
005800970526     D OPDDS           DS
005900970526     D  OPDOPZ                 1      2
006000970526     D  OPDDE1                 3     19
006100970526     D  OPDOP1                21     22
006200970526     D  OPDFI0                23     34
006300970526     D  OPDATT                35     35
006400970526     D  OPDFI1                36     80
006500970526     D  OPDDES                 1     19
006600970526     D  OPDRIG                 1     80
006700970526     D*------------
006800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006900970526     D*------------
007000970526     D FUDDS           DS
007100970526     D  FUDDE1                 1      1
007200970526     D  FUDFUN                 2      3
007300970526     D  FUDDE2                 4     19
007400970526     D  FUDFI0                20     34
007500970526     D  FUDATT                35     35
007600970526     D  FUDFI1                36     80
007700970526     D  FUDDES                 1     19
007800970526     D  FUDRIG                 1     80
007900970526     D*------------
008000970526     D* DS GESTIONE TASTI FUNZIONALI
008100970526     D*------------
008200970526     D DSKEY         E DS
008300970526     D DEVDS1          DS
008400970526     D  KEY                  369    369
008500970526     D*------------
008600970526     D* DS "XMSG" - GESTIONE MESSAGGI
008700970526     D*------------
008800970526     D DSMSG         E DS
008900970526     D*------------
009000020916     D* DS REPERIMENTO DATI UTENTE
009100970526     D*------------
009200020916     D TIBS34DS      E DS                                                       *Profili utente
009300020916     D DDATIUTE      E DS                                                       *Dati utente
009400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009500020212     D*------------
009600091027     D* DS X LETTURA TABELLE
009700020212     D*------------
009800091027     D dsisv         E DS                  extname(disv)
009900091027     D dstle         E DS                  extname(dtle)
010000970527     D*------------------
010100970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
010200970527     D*------------------
010300970527     D WLBDA8          DS                  INZ
010400970527     D  G08DAT                 1      8  0
010500970527     D  G08INV                 9     16  0
010600970527     D  G08ERR                17     17
010700970527     D  G08TGI                18     22  0
010800970526     D*------------
010900970526     D* DS DI PROCEDURA
011000970526     D*------------
011100001020     D tibds         E DS                  extname(tibs12ds)
011200001020     D tis720        E DS                  extname(tis720ds)
011300060727     D tis732ds      E DS
011400001020     D tibs02DS      E DS
011500970526     D*------------
011600970526     D* ARCHITETTURA
011700970526     D*------------
011800970526     D KPJBA         E DS
011900001020     D*------------
012000001020     D ksc             s              7  0 dim(30)
012100001020     D ksa             s              4    dim(30)
012200020212     D isvkun          s              2    dim(100)
012300001020     D*------------------
012400001020     D* TABELLE "XCLIR"- RICERCA CLIENTI
012500001020     D*------------------
012600001020     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
012700001020     D*------------------
012800001020     DDSXC           e DS                  extname(XCLIRDS)
012900001020     D*------------------
013000001020     D                 DS
013100001020     D dsksn                   1      4p 0
013200001020     D dsksa                   1      4
013300001020     DBS69DS         e ds                  extname(TIBS69DS)
013400001020     DACODS          e ds                  extname(cnaco00f)
013500001020     DINDDS          e ds                  extname(cnind00f)
013600001020     DCLPDS          e ds                  extname(cnclp00f)
013700001020     DCLSDS          e ds                  extname(fncls00f)
013800001020     D*---
013900001020     D* Variabili non riferite al data base
014000001020     D*---
014100001020     D $esikun         S              1                                         *Esistenza KUN
014200001020     D a8              S              8                                         *Alfanumerico 8
014300091027     D sktle           S              2    DIM(20)
014400091027     D jtle            S              5  0                                      *indici
014500970526     C*------------------------------------------------------------------------*
014600970526     C* MAIN LINES
014700970526     C*------------------------------------------------------------------------*
014800970526     C*
014900970526     C* SE NON CI SONO ERRORI, PROSEGUE
015000970526IF  1C     ERRL00        IFNE      '1'
015100970526     C*
015200970526     C* CARICA IL SFILE
015300970526     C                   EXSR      POSSFL
015400970526     C*
015500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015900970526     C*
016000970526     C* REIMPOSTA ERRORE DI PROCERURA
016100970526     C                   MOVEL     '0'           ERRL00
016200970526     C*
016300970526     C* EMETTE IL SFILE
016400970526     C                   EXSR      OUTSFL
016500970526     C*
016600970526     C* GESTIONE TASTI FUNZIONALI
016700970526     C                   EXSR      FUNCON
016800970526     C*
016900970526E   2C                   ENDDO
017000970527     C*
017100970527     C* CI SONO ERRORI IN ENTRATA
017200970527X   1C                   ELSE
017300991104     C                   MOVEL     '1'           s20ERR
017400991104     C                   MOVEL     DSMSMS        s20MSG
017500970526E   1C                   ENDIF
017600970526     C* FINE PGM
017700991210     c                   exsr      finsr
017800970526     C                   SETON                                        LR
017900970526     C*------------------------------------------------------------------------*
018000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018100970526     C*------------------------------------------------------------------------*
018200970526     C     F03GES        BEGSR
018300970526     C*
018400970526     C* IMPOSTA "PREMUTO F03"
018500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018600991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
018700970526     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019100970526     C*------------------------------------------------------------------------*
019200970526     C     F05GES        BEGSR
019300970526     C*
019400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F06GES        BEGSR
020100970526     C*
020200970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
020300020916IF  1C     DUTLPO        IFNE      'S'
020400970526     C                   MOVEL     '1'           ERRL00
020500970526     C                   MOVEL     MSG(3)        DSMSMS
020600970526E   1C                   ENDIF
020700970526     C*
020800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
020900991001     C                   MOVEL     *BLANKS       Dtle
021000991104     C                   MOVEL     *BLANKS       Disv
021100991105     C                   move      *ZEROS        Dksu
021200991105     C                   move      *ZEROS        Dsun
021300970602     C                   MOVEL     DEPOP0        WOP0
021400970529     C                   MOVEL     'F06'         WOP1
021500991104     C                   MOVEL     'TIS725R'     WPGM              8
021600970526     C                   EXSR      LANPGM                                       *LANCIA PGM
021700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021800970526E   1C                   ENDIF
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* F12GES - GESTIONE F12 -> RITORNO
022300970526     C*------------------------------------------------------------------------*
022400970526     C     F12GES        BEGSR
022500970526     C*
022600970526     C* IMPOSTA "PREMUTO F12"
022700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022800991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
022900970526     C*
023000970526     C                   ENDSR
023100970528     C*------------------------------------------------------------------------*
023200970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
023300970528     C*------------------------------------------------------------------------*
023400970528     C     F21GES        BEGSR
023500970612     C*
023600991001     C                   MOVEL     *BLANKS       Dtle
023700991104     C                   MOVEL     *BLANKS       Disv
023800991105     C                   move      *ZEROS        Dksu
023900991105     C                   move      *ZEROS        Dsun
024000970612     C                   MOVEL     DEPOP0        WOP0
024100970612     C                   MOVE      '03'          WOP0
024200970612     C                   MOVEL     'F21'         WOP1
024300000202     C                   MOVEL     'TIS726R'     WPGM              8
024400970612     C                   EXSR      LANPGM
024500970528     C*
024600970528     C                   ENDSR
024700970528     C*------------------------------------------------------------------------*
024800970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
024900970528     C*------------------------------------------------------------------------*
025000970528     C     F22GES        BEGSR
025100970528     C*
025200970528     C                   Z-ADD     80            LENGH            15 5
025300970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
025400970528     C                   PARM                    CMD(1)
025500970528     C                   PARM                    LENGH
025600970528     C*
025700970528     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
026000970526     C*------------------------------------------------------------------------*
026100970526     C     F23GES        BEGSR
026200970526     C*
026300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
026400970526     C*
026500970526     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026800970526     C*------------------------------------------------------------------------*
026900970526     C     F24GES        BEGSR
027000970526     C*
027100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027200970526     C*
027300970526     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
027600970526     C*------------------------------------------------------------------------*
027700970526     C     RUPGES        BEGSR
027800970526     C*
027900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
028000970526     C*
028100970526     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300970526     C* ENTGES - GESTIONE ENTER -> INVIO
028400970526     C*------------------------------------------------------------------------*
028500020212     C     ENTGES        begsr
028600970526     C*
028700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
028800020212     C                   exsr      NEWPOS
028900970526     C*
029000970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
029100020212     C     ERRL00        ifeq      '0'                                          *NO ERRORI
029200020212     C     SWPOSI        ifne      0
029300020212     C                   exsr      POSSFL                                       *RICARICA SFILE
029400020212     C                   else
029500020212     C     SFLULT        ifne      0
029600020212     C                   exsr      ENTATT                                       *GESTIONE ENTER
029700970526     C*
029800970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
029900020212     C     SWMODI        ifne      0
030000020212     C                   exsr      POSSFL
030100020212     C                   endif
030200020212     C                   endif
030300020212     C                   endif
030400020212     C                   endif
030500970526     C*
030600970526     C                   ENDSR
030700970526     C*------------------------------------------------------------------------*
030800970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
030900970526     C*------------------------------------------------------------------------*
031000020212     C     O01GES        begsr
031100970526     C*
031200970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
031300020212     C                   move      vidtle        s20tle
031400991104     C                   move      vidisv        s20isv
031500991105     C                   move      vidksu        s20ksc
031600020212     C                   move      viDsun        s20sun
031700020212     C                   movel     '1'           FINL00                         *FINE PGM
031800970526     C*
031900020226     C                   movel     tis720        kpjbu
032000020226     C*
032100970526     C                   ENDSR
032200970526     C*------------------------------------------------------------------------*
032300970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
032400970526     C*------------------------------------------------------------------------*
032500020212     C     O02GES        begsr
032600970526     C*
032700970527     C* MODIFICA
032800020212     C     ERRL00        ifeq      '0'
032900020212     C                   exsr      movcamp
033000020212     C                   movel     DEPOP0        WOP0
033100020212     C                   movel     'O02'         WOP1
033200020212     C                   movel     'TIS725R'     WPGM
033300020212     C                   exsr      LANPGM
033400020212     C***                z-add     1             swmodi
033500020212     C                   endif
033600970526     C*
033700020212     C                   endsr
033800991014     C*------------------------------------------------------------------------*
033900991014     C* O03GES - GESTIONE OPZIONE 3=copia
034000991014     C*------------------------------------------------------------------------*
034100020212     C     O03GES        begsr
034200991014     C*
034300020212     C* COPIA
034400020212     C     ERRL00        ifeq      '0'
034500020212     C                   exsr      movcamp
034600020212     C                   movel     DEPOP0        WOP0
034700020212     C                   movel     'O03'         WOP1
034800020212     C                   movel     'TIS725R'     WPGM
034900020212     C                   exsr      LANPGM
035000020212     C***                z-add     1             swmodi
035100020212     C                   endif
035200991014     C*
035300020212     C                   endsr
035400970526     C*------------------------------------------------------------------------*
035500021024     C* O04GES - GESTIONE OPZIONE 4=FAI SCADERE (EX ANNULLAMENTO)
035600970526     C*------------------------------------------------------------------------*
035700021024     C     O04GES        BEGSR
035800970526     C*
035900970527     C* ANNULLAMENTO
036000021024IF  1C     ERRL00        IFEQ      '0'
036100021024     c                   exsr      movcamp
036200021024     C                   MOVEL     DEPOP0        WOP0
036300021024     C                   MOVEL     'O04'         WOP1
036400021024     C                   MOVEL     'TIS725R'     WPGM
036500021024     C                   EXSR      LANPGM
036600021024     c                   z-add     1             swmodi
036700021024E   1C                   ENDIF
036800970526     C*
036900021024     C                   ENDSR
037000970526     C*------------------------------------------------------------------------*
037100970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
037200970526     C*------------------------------------------------------------------------*
037300020212     C     O05GES        begsr
037400970526     C*
037500020212     C                   exsr      MOVCAMP
037600020212     C                   movel     DEPOP0        WOP0
037700020212     C                   movel     'O05'         WOP1
037800020212     C                   movel     'TIS725R'     WPGM
037900020212     C                   exsr      LANPGM
038000970526     C*
038100020212     C                   endsr
038200970526     C*------------------------------------------------------------------------*
038300060727     C* O07GES - GESTIONE OPZIONE 7=Richiamo PGM estensinoe abilitazioni ai servizi
038400970526     C*------------------------------------------------------------------------*
038500020212     C     O07GES        begsr
038600970526     C*
038700060727     C* ESTENSIONE ABILITAZIONI AI SERVIZI
038800060727     C     ERRL00        ifeq      '0'                                          *NO ERRORI
038900060727     C                   clear                   tis732ds
039000060731     C                   eval      S32OP0 = DEPOP0
039100060731     C                   eval      S32OP1 = DEPOP1
039200060727     C                   eval      S32F03 = '0'
039300060727     C                   eval      S32F12 = '0'
039400060727     C                   eval      S32ERR = '0'
039500060727     C                   eval      S32MSG = *BLANKS
039600060727     C                   eval      S32SUN = VIDSUN
039700060727     C                   eval      S32ISV = *BLANKS
039800060727     C                   eval      S32TIP = *BLANKS
039900060727     C                   eval      S32KSU = VIDKSU
040000060727     C                   movel     tis732ds      KPJBU
040100060727     C                   CALL      'TIS732R'
040200060727     C                   PARM                    KPJBA
040300060727     C***                z-add     1             SWMODI                     L
040400060727     C                   endif
040500970526     C*
040600020212     C                   endsr
040700991108     C*------------------------------------------------------------------------*
040800001020     C* O08GES - GESTIONE OPZIONE 8=Richiamo PGM regole dft
040900991108     C*------------------------------------------------------------------------*
041000020212     C     O08GES        begsr
041100991108     C*
041200020212     C* REGOLE DI DFT
041300020212     C     ERRL00        ifeq      '0'                                          *NO ERRORI
041400001020     C                   exsr      movcamp
041500020212     C                   movel     DEPOP0        WOP0
041600020212     C                   movel     'O08'         WOP1
041700020212     C                   movel     'TIS720R'     WPGM
041800020212     C                   exsr      LANPGM
041900020212     C***                z-add     1             SWMODI                     L
042000020212     C                   endif
042100991108     C*
042200020212     C                   endsr
042300991108     C*------------------------------------------------------------------------*
042400991108     C* O09GES - GESTIONE OPZIONE 9=Richiamo pgm cliente unificante
042500991108     C*------------------------------------------------------------------------*
042600020212     C     O09GES        begsr
042700991108     C*
042800020212     C* CLIENTE UNIFICANTE
042900020212     C     ERRL00        ifeq      '0'                                          *NO ERRORI
043000020212     C                   movel     VIDtle        Dtle
043100020212     C                   move      VIDksu        Dksu
043200020212     C                   movel     DEPOP0        WOP0
043300020212     C                   movel     'O09'         WOP1
043400020212     C                   movel     'TIBS12R'     WPGM
043500020212     C                   exsr      LANPGM2
043600020212     C***                z-add     1             SWMODI                     L
043700020212     C                   endif
043800991108     C*
043900020212     C                   endsr
044000001020     C*------------------------------------------------------------------------*
044100001020     C* O10GES - GESTIONE OPZIONE 10=Richiamo regole Cliente/Tipo file
044200001020     C*------------------------------------------------------------------------*
044300020212     C     O10GES        begsr
044400001020     C*
044500020212     C* REGOLE PER CLIENTE/TIPO FILE
044600020212     C     ERRL00        ifeq      '0'                                          *NO ERRORI
044700020212     C                   movel     VIDtle        Dtle
044800020212     C                   move      VIDksu        Dksu
044900020212     C                   movel     DEPOP0        WOP0
045000020212     C                   movel     'O10'         WOP1
045100020212     C                   movel     'TIS722R'     WPGM
045200020212     C                   exsr      LANPGM
045300020212     C***                z-add     1             SWMODI                     L
045400020212E   1C                   endif
045500001020     C*
045600020212     C                   endsr
045700001020     C*-----------------------------------------------
045800001020     C* VALORIZZAZIONE CAMPI DA "PASSARE"
045900001020     C*-----------------------------------------------
046000001020     C     MOVCAMP       BEGSR
046100001020     C*
046200991001     C                   MOVEL     VIDtle        Dtle
046300991104     C                   MOVE      VIDisv        Disv
046400001020     C                   MOVE      VIDksu        Dksu
046500991105     C                   MOVE      VIDsun        Dsun
046600001020     C*
046700001020     C                   ENDSR
046800970526     C*------------------------------------------------------------------------*
046900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
047000970526     C*------------------------------------------------------------------------*
047100970526     C     NEWPOS        BEGSR
047200970526     C*
047300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
047400970526     C*         1= RICHIESTO POSIZIONAMENTO
047500970526     C                   Z-ADD     0             SWPOSI            7 0
047600970526     C*
047700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
047800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
047900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
048000970526     C                   Z-ADD     0             SWMODI            1 0
048100970530     C*
048200970530     C* REIMPOSTA INDICATORI DI ERRORE
048300970530     C                   SETOFF                                       808182
048400970530     C                   SETOFF                                       83
048500970526     C*
048600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
048700021113     C                   IF        VIDis2 <> DEPis2 OR
048800021113     C                             VIDsu2 <> DEPsu2 OR
048900021113     C                             VIDis1 <> DEPis1 OR
049000021113     C                             VIDsu1 <> DEPsu1 OR
049100021113     C                             VIDcli <> DEPcli OR
049200021113     C                             VIDsrs <> DEPsrs OR
049300021114     C                             %scan('?':VIDcli) > 0
049400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
049500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
049600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
049700991104     C                   MOVEL     VIDis2        DEPis2                         *DEPOSITI
049800991105     C                   move      VIDsu2        DEPsu2
049900991104     C                   move      VIDis1        DEPis1
050000991105     C                   move      VIDsu1        DEPsu1
050100000216     C                   move      VIDcli        DEPcli
050200010420     C                   move      VIDsrs        DEPsrs
050300970526E   2C                   ENDIF
050400970526E   1C                   ENDIF
050500970526     C*
050600970526     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
050900970526     C*------------------------------------------------------------------------*
051000970528     C     SETFIL        BEGSR
051100970526     C*
051200970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
051300991104     C                   MOVEL     VIDis2        KCEisv
051400991105     C                   move      VIDsu2        KCEsun
051500991001IF  1C     WTIPRI        IFEQ      'UNI'                                        *PER PARTENZA
051600991104     C     KEYvss        SETLL     tivss01l                           99
051700970528E   1C                   ENDIF
051800980107     C*
051900970526     C* LETTURA RECORD
052000970526IF  1C     *IN99         IFEQ      *OFF
052100970526     C                   EXSR      LETFIL
052200970526E   1C                   ENDIF
052300970526     C*
052400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
052500970526IF  1C     *IN99         IFEQ      *ON
052600970526     C                   Z-ADD     1             SFLEOF
052700970526E   1C                   ENDIF
052800970526     C*
052900970526     C                   ENDSR
053000970526     C*------------------------------------------------------------------------*
053100970526     C* LETFIL - LETTURA RECORD
053200970526     C*------------------------------------------------------------------------*
053300970526     C     LETFIL        BEGSR
053400970526     C*
053500970526     C* LEGGE FINO A:
053600970526     C*     - FINE FILE
053700970526     C*     - TROVATO RECORD VALIDO
053800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
053900970526DO  1C     *IN99         DOWEQ     *OFF
054000970526     C     WRECOK        ANDEQ     'N'
054100970526     C*
054200970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
054300991001IF  2C     WTIPRI        IFEQ      'UNI'                                        *PER PARTENZA
054400991104     C                   READ      tivss01l                               99
054500970528E   2C                   ENDIF
054600970528     C*
054700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
054800970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
054900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
055000970530X   2C                   ELSE
055100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
055200970530E   2C                   ENDIF
055300970526E   1C                   ENDDO
055400970526     C*
055500970526     C                   ENDSR
055600970526     C*------------------------------------------------------------------------*
055700010420     C* CHKREC - CONTROLLA VALIDITA' RECORD
055800970526     C*------------------------------------------------------------------------*
055900970526     C     CHKREC        BEGSR
056000970526     C*
056100020212     C                   movel     'S'           WRECOK                         *VALIDITA' RECORD
056200100218     C                   setoff                                       7675
056300970530     C*
056400970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
056500090422     C                   if        WRECOK = 'S'
056600090422     C     vidsu1        ifeq      *zeros
056700090422     C     vidsu1        oreq      *blank
056800090422     C                   else
056900090422     C     vidsu1        ifne      vsssun
057000090422     C                   movel     'N'           WRECOK
057100090422     C                   endif
057200090422     C                   endif
057300090422     C*
057400090422     C*filtra se impostato il tipo servizio
057500090422     C                   if        WRECOK = 'S'
057600090422     C     vidis1        ifeq      *blank
057700090422     C                   else
057800090422     C                   if        %subst(vidis1:1:1) = '*'
057900090422     C                   if        %subst(vidis1:2:1) <>
058000090422     C                             %subst(vssisv:2:1)
058100090422     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
058200090422     C                   endif
058300090422     C                   else
058400090422     C                   if        %subst(vidis1:2:1) = '*'
058500090422     C                   if        %subst(vidis1:1:1) <>
058600090422     C                             %subst(vssisv:1:1)
058700090422     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
058800090422     C                   endif
058900090422     C                   else
059000090422     C                   if        vidis1 <> vssisv
059100090422     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059200090422     C                   endif
059300090422     C                   endif
059400090422     C                   endif
059500090422     C                   endif
059600090422     C                   endif
059700090422     C*
059800090422     C     vidcli        ifeq      *zeros
059900000216     C     vidcli        oreq      *blank
060000020212     C                   else
060100020212     C                   move      vidcli        a8
060200020212     C                   movel     '0'           a8
060300020212     C     a8            ifne      vssksu
060400020212     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
060500020212     C                   endif
060600020212     C                   endif
060700010427     C                   endif
060800020214     C*
060900020214     C* Verifico che il servizio del record cotrrente sia soggetto a controllo unificazioni
061000020214     C     vssisv        lookup    isvkun                                 22
061100020214     C                   if        *in22 = *on
061200000216     C*
061300000216     C* Clienti figli collegati
061400020212     C                   movel     'N'           $esikun                        *non esistono kun
061500020212     C     WRECOK        ifeq      'S'                                          *ALLA DATA
061600020212     C     vsstle        ifne      *blanks                                      *c'è legame
061700020212     C                   movel     vsstle        kkutle
061800020212     C                   movel     *blanks       kkuazp
061900020212     C                   move      vssksu        kkucop
062000020212     C     keykun        setll     tikun01l
062100020212     C     keykun        reade     tikun01l                               98
062200020212     C                   dow       NOT *in98
062300020212     C     kunvat        ifeq      *blanks
062400020212     C     kundsc        iflt      vssdde                                       *non valido
062500020212     C     kundde        orgt      vssdsc
062600020212     C                   else                                                   *valido
062700060802     C                   if        kundsc >= datcor
062800020212     C                   movel     'S'           $esikun                        *esistono kun
062900020212     C                   leave
063000060802     C                   endif
063100020212     C                   endif
063200020212     C                   endif
063300020212     C     KEYkun        reade     tikun01l                               98
063400020212     C                   enddo
063500020212     C     $esikun       ifeq      'N'                                          *non esistono kun
063600020212     C                   seton                                        76        *attenzione
063700020212     C                   endif
063800000216     C*
063900020212     C                   else                                                   *non c'è legame
064000000216     C*
064100020212     C                   move      vssksu        kkucop
064200020212     C     KE3kun        setll     tikun03l
064300020212     C     KE3kun        reade     tikun03l                               98
064400020212     C                   dow       NOT *in98
064500091020     C                   if        kunvat = *blanks AND
064600091020     C                             %subst(kuntle:1:1) = 'W'
064700020212     C     kundsc        iflt      vssdde                                       *non valido
064800020212     C     kundde        orgt      vssdsc
064900020212     C                   else
065000060802     C                   if        kundsc >= datcor
065100020212     C                   movel     'S'           $esikun                        *esistono kun
065200020212     C                   leave
065300060802     C                   endif
065400020212     C                   endif
065500020212     C                   endif
065600020212     C     KEYkun        reade     tikun01l                               98
065700020212     C                   enddo
065800020212     C     $esikun       ifeq      'S'                                          *esistono kun
065900020212     C                   seton                                        76        *attenzione
066000020212     C                   endif
066100020212     C                   endif
066200020212     C                   endif
066300020211     C*
066400091027     C* Che nn sia cliente figlio
066500091117     C*    WRECOK        ifeq      'S'
066600091117     C* Effettuo i controlli seguenti solo se il record è tuttora attivo (x favorire la
066700091117     C* verifica delle anomalie da parte dell'utente)
066800091117     C*                  if        datcor >= vssdde and
066900091117     C*                            datcor <= vssdsc
067000091117     C*                  move(p)   vssksu        kkucof
067100091117     c*
067200091117     c* ciclo x tutti i legami d tipo "internet"
067300091117     c*                  z-add     1             jtle
067400091117     c*                  dow       jtle <= %elem(sktle)
067500091117     c*                  if        sktle(jtle) <> *blanks
067600091117     c*
067700091117     C*                  movel     sktle(jtle)   kkutle
067800091117     C*    ke6kun        setll     tikun06l
067900091117     C*    ke6kun        reade     tikun06l
068000091117     C*                  dow       not %eof(tikun06l)
068100091117     C*    kunvat        ifeq      *blanks                                      *annullati/sospesi
068200091117     C*                  if        kundsc <  vssdde  or                         *non valido
068300091117     C*                            kundde >  vssdsc
068400091117     C*                  else                                                   *valido
068500091117     C*                  if        kundsc >= datcor
068600091117     C*                  seton                                        76
068700091117     C*                  leave
068800091117     C*                  endif
068900091117     C*                  endif
069000091117     C*                  endif
069100091117     C*    ke6kun        reade     tikun06l
069200091117     C*                  enddo
069300091117     c*
069400091117     c*                  else
069500091117     c*                  leave
069600091117     c*                  endif
069700091117     c*
069800091117     c*                  add       1             jtle
069900091117     c*                  enddo
070000091117     C*                  endif
070100091117     C*                  endif
070200091117     C*
070300020212     C                   endif
070400010427     C*
070500010427     C* RAGIONE SOCIALE DIVERSA DA QUELLA SCELTA
070600010427     C                   if        WRECOK = 'S'
070700130719     C                   EVAL      PiStr=%trim(vssksu)
070800130719     C                   EXSR      CHKNUM
070900130719     C                   IF        PiInt=*on
071000130719     C                   Z-ADD     PiVal         wcli              7 0
071100130719     C                   ENDIF
071200010427     C                   exsr      dectib
071300010427     C     VIDsrs        ifeq      *blanks                                      *ALLA DATA
071400010427     C                   else
071500010709     C                   z-add     *zeros        pos               2 0
071600010709     C                   eval      pos = %scan(%trim(VIDsrs):wddksc)
071700010709     C                   if        pos = *zeros
071800010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
071900010427     C                   endif
072000010427     C                   endif
072100010427     C                   endif
072200100218     C*
072300100218     C                   if        WRECOK = 'S'
072400100218     C                   if        VSSDSC < datcor
072500100218     C                   seton                                        75
072600100218     C                   endif
072700100218     C                   endif
072800100218     C*
072900970526     C                   ENDSR
073000970526     C*------------------------------------------------------------------------*
073100970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
073200970526     C*------------------------------------------------------------------------*
073300970526     C     IRESFL        BEGSR
073400970526     C*
073500970526     C                   MOVEL     *BLANKS       OPZKEY
073600991104     C                   Z-ADD     vssDDE        VIHDDE
073700991104     C                   Z-ADD     vssDsc        VIHDsc
073800991104     C                   MOVEL     vsstle        VIDtle
073900991104     C                   MOVEL     vssisv        VIDisv
074000991105     C                   move      vssksu        VIDksu
074100991105     C                   MOVE      vsssun        VIDsun
074200991104     C                   move      vssazp        VIDazp
074300991104     C                   Z-ADD     vssdde        G08INV
074400970618     C                   Z-ADD     *ZEROS        G08DAT
074500970618     C                   MOVEL     '3'           G08ERR
074600970618     C                   CALL      'XSRDA8'
074700970618     C                   PARM                    WLBDA8
074800021030     C***                Z-ADD     G08DAT        VIDDDE
074900991104     C                   Z-ADD     vssDSC        G08INV
075000970618     C                   Z-ADD     *ZEROS        G08DAT
075100970618     C                   MOVEL     '3'           G08ERR
075200970618     C                   CALL      'XSRDA8'
075300970618     C                   PARM                    WLBDA8
075400970618     C                   Z-ADD     G08DAT        VIDDSC
075500021030     C*
075600021030     C* Reperisco l'Access Name presente sul file utenti di Strategi
075700021030     C     vssSUN        chain     SGIUS
075800021030     C                   if        %found(SGIUS)
075900021030     C                   eval      vidNAM = USANAM
076000021127     C                   else
076100021127     C                   eval      vidNAM = *blanks
076200021030     C                   endif
076300021030     C*
076400970528     C* DECODIFICHE
076500010420     C*clienti
076600010420     C                   movel     wddksc        viddep
076700970526     C                   ENDSR
076800991210     C*------------------------------------------------------------------------*
076900991210     C* dectib - Controlla esistenza codice cliente
077000991210     C*------------------------------------------------------------------------*
077100010420     C     dectib        begsr
077200010913     C*
077300010913     C     vssksu        chain     tianv01l
077400010913     C                   if        %found(tianv01l)
077500010913     C                   movel(p)  anvdes        wddksc
077600010913     C                   else
077700991210     C                   CLEAR                   BS69DS
077800991210     C                   CLEAR                   ACODS
077900991210     C                   MOVEL     KNSIF         I69SIF
078000991210     C                   Z-ADD     WCLI          I69KAC
078100991210     C                   CALL      'TIBS69R'
078200991210     C                   PARM                    BS69DS
078300991210     C                   PARM                    ACODS
078400991210     C                   PARM                    INDDS
078500991210     C                   PARM                    CLPDS
078600991210     C                   PARM                    CLSDS
078700991210     C     O69ERR        IFNE      '1'
078800991210     C                   MOVEL     ACORAG        wddksc           40
078900991210     C                   ELSE
079000991210     C                   MOVEL     *ALL'*'       Wddksc
079100010913     C                   MOVE      '1'           errl00
079200021113     C                   MOVEL     'N'           flgKSU
079300010913     C                   ENDIF
079400991210     C                   ENDIF
079500010913     C*
079600010913     C                   endsr
079700991210     C*------------------------------------------------------------------------*
079800991210     C* FINSR - OPERAZIONI FINALI
079900991210     C*------------------------------------------------------------------------*
080000991210     C     FINSR         BEGSR
080100991210     C*
080200991210     C* CHIUDE I FILE DEI PGM CHIAMATI
080300991210     C                   CLEAR                   bs69ds
080400991210     C                   CLEAR                   acods
080500991210     C                   CLEAR                   indds
080600991210     C                   CLEAR                   clpds
080700991210     C                   CLEAR                   clsds
080800991210     C                   MOVEL     'C'           i69tla
080900991210     C                   MOVEL     knsif         i69sif
081000991210     C                   CALL      'TIBS69R'
081100991210     C                   PARM                    bs69ds
081200991210     C                   PARM                    acods
081300991210     C                   PARM                    indds
081400991210     C                   PARM                    clpds
081500991210     C                   PARM                    clsds
081600991210     C*
081700991210     C                   ENDSR
081800970526     C*------------------------------------------------------------------------*
081900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
082000970526     C*------------------------------------------------------------------------*
082100970526     C     CHKPAR        BEGSR
082200970528     C*
082300970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
082400991012     C                   MOVEL     'UNI'         WTIPRI            3
082500991104IF  1C     VIDis2        IFEQ      *BLANKS
082600991105     C     VIDsu2        ANDEQ     *ZEROS
082700991012     C                   MOVEL     'UNI'         WTIPRI
082800970528E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
082900970528     C*
083000970528     C* CONTROLLA IL TIPO RICERCA
083100991012IF  1C     ERRL00        IFEQ      '0'
083200991104IF  2C     VIDis2        IFNE      *blank
083300991015     C                   mOVEL     'UNI'         WTIPRI
083400991104     c                   move      vidis2        wvidis2           2
083500991104     c                   exsr      cntisv
083600991104     c                   move      wvidis2       vidis2
083700991012E   2C                   ENDIF
083800991012E   1C                   ENDIF
083900991012     C*
084000991012IF  1C     ERRL00        IFEQ      '0'
084100991104IF  2C     VIDis1        IFNE      *ZEROS
084200991104IF  2C     VIDis1        andne     *blank
084300090422     C                   if        %scan('*':VIDis1) = 0
084400991104     C                   MOVEL     'UNI'         WTIPRI
084500991104     C                   MOVEL     VIDis1        Wvidis2
084600991104     C                   EXSR      Cntisv
084700991104     C                   MOVEL     Wvidis2       VIdis1
084800090422     C                   endif
084900991012E   2C                   ENDIF
085000991012E   1C                   ENDIF
085100021113     C*
085200021113IF  1C     ERRL00        IFEQ      '0'
085300021113IF  2C     VIDcli        IFNE      *ZEROS
085400021113IF  2C     VIDcli        ANDNE     *blank
085500021113     C                   MOVEL     'UNI'         WTIPRI
085600021114     C                   MOVE(P)   VIDcli        Wksu
085700021113     C                   EXSR      CHKksu
085800021113     C                   IF        flgKSU = 'S'
085900021114     C                   MOVE(P)   Wksu          VIDcli
086000021113     C                   ENDIF
086100021113E   2C                   ENDIF
086200021113E   1C                   ENDIF
086300021113     C*
086400970526     C                   ENDSR
086500991012     C*------------------------------------------------------------------------*
086600000807     C* Cntisv - CONTROLLA il tipo di servizio a cui è abilitato il cliente
086700991012     C*------------------------------------------------------------------------*
086800020212     C     cntisv        begsr
086900020212     C                   clear                   TIBS02DS
087000020212     C*
087100020212     C     '?'           SCAN      wVidis2                                30
087200020212     C*  Ricerca
087300020212     C                   eval      t02tla = 'L'
087400020212    1C                   IF        *IN30 = *ON
087500020212     C                   movel     'R'           t02mod
087600020212     C                   else
087700020212     C                   movel     'C'           t02mod
087800020212     C                   movel(p)  Wvidis2       t02ke1
087900020212     C                   end
088000991104     C                   movel     'ISV'         t02cod
088100020212     C                   movel     knsif         t02sif
088200020212     C                   CALL      'TIBS02R'
088300020212     C                   parm                    KPJBA
088400020212     C                   parm                    TIBS02DS
088500020212     C
088600020212     C*
088700020212    2C                   IF        T02err = *blanks
088800020212     C                   movel     T02ke1        wVidis2
088900020212     C                   else
089000020212     C                   movel     '1'           errl00
089100020212     C                   seton                                        66
089200020212     C                   MOVEL     MSG(12)       DSMSMS
089300020212     C                   ENDIF
089400020212     C
089500020212     C                   endsr
089600991012     C*------------------------------------------------------------------------*
089700991105     C* CHKksu - CONTROLLA codice padre
089800991012     C*------------------------------------------------------------------------*
089900991105     C     CHKksu        BEGSR
090000991012     C*
090100991105     C     *LIKE         DEFINE    Vidsu1        Wksu
090200021113     C                   MOVEL     'S'           flgKSU            1
090300991012     C*
090400991012     C* RICHIESTO ELENCO
090500991012     C                   SETOFF                                           28
090600991105     C     '?'           SCAN      Wksu                                   28
090700991012IF  1C     *IN28         IFEQ      *ON
090800991012     C* RICHIESTO ELENCO CODICI
090900021113     C                   MOVEL     'N'           flgKSU
091000020212     C*clienti
091100020212     C                   CLEAR                   DSXC
091200020924     C                   Z-ADD     DUTKCI        DXCCAP
091300991012     C                   CALL      'XCLIR'
091400991012     C                   PARM                    DSXC
091500991012     C* SCOMPATTA
091600991012     C                   MOVEA     DXCKSC        KSA                        ALF
091700991012DO  2C     1             DO        30            J                 3 0
091800991012     C                   MOVE      KSA(J)        DSKSA                      F
091900991012     C                   Z-ADD     DSKSN         KSC(J)                     M
092000991012E   2C                   ENDDO
092100991012     C                   MOVEL     KSC(1)        WKSC              7
092200991012     C                   MOVEL     '1'           $TESVD            1        TA
092300991015E   1C                   ENDIF
092400991012     C*
092500991012     C* CODICE CLIENTE NUMERICO
092600991012IF  1C     WKSC          IFLT      *ZEROS
092700991012     C                   MOVEL     '1'           WERR              1
092800991012X   1C                   ELSE
092900991012     C*
093000991012     C* SOLO CARATTERI NUMERICI
093100020212     C   28              move      *all'0'       wksu
093200020212     C   28              move      wksc          wksu
093300991012     C                   SETOFF                                       28
093400991105     C                   TESTN                   Wksu                 28
093500991012IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
093600991012     C                   MOVEL     '1'           ERRL00
093700991012X   1C                   ELSE                                                   *E'NUMERO DA TESTN
093800991105     C                   MOVE      Wksu          W001              1
093900991012IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
094000991012     C                   MOVEL     '1'           ERRL00
094100991012X   2C                   ELSE                                                   *NUMERO
094200991012     C*
094300991012     C* CONTROLLA ESISTENZA CODICE
094400021114     C                   MOVEL     'S'           flgKSU
094500991210     C                   MOVE      Wksu          wcli
094600991210     C                   exsr      dectib
094700991012E   2C                   ENDIF
094800991012E   1C                   ENDIF
094900991012E   1C                   ENDIF
095000991012     C*
095100991012     C                   ENDSR
095200970526     C*------------------------------------------------------------------------*
095300970526     C* LANPGM - LANCIA UN PROGRAMMA
095400970526     C*------------------------------------------------------------------------*
095500970526     C     LANPGM        BEGSR
095600970526     C*
095700970526     C* IMPOSTA LA DS DI PROCEDURA
095800991104     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
095900991104     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
096000991104     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
096100991104     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
096200991104     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
096300101103     C                   MOVEL     *BLANKS       s20TIP                         *TOPO FILE
096400991104     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
096500001020     C                   MOVEL     Dtle          s20tle
096600001020     C                   MOVEL     Disv          s20isv                         *TIPO SERVIZIO
096700001020     C                   MOVE      Dksu          s20ksc                         *CLIENTE UNIFICANTE
096800001020     C                   MOVE      Dsun          S20sun                         *UTENTE STRATEGI
096900970526     C*
097000060727     C                   MOVEL     tis720        kpjbu
097100060727     C                   CALL      WPGM
097200060727     C                   PARM                    KPJBA
097300991104     C*                  PARM                    tibds
097400100914     C                   MOVEL     kpjbu         tis720
097500970526     C*
097600970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
097700001020     C     s20F03        IFEQ      '1'                                          *PREMUTO F03
097800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
097900970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
098000001020     C                   ELSE                                                   *NON PREMUTO F03
098100991104     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
098200001020     C                   ENDIF
098300970526     C*
098400970526     C                   ENDSR
098500991108     C*------------------------------------------------------------------------*
098600001020     C* LANPGM2- LANCIA un *PGM alternativo per ds non di procedura
098700991108     C*------------------------------------------------------------------------*
098800991108     C     LANPGM2       BEGSR
098900991108     C*
099000001020     C* IMPOSTA LA DS "NON" DI PROCEDURA
099100001020     C                   CLEAR                   tibds
099200991108     C                   MOVEL     WOP0          k12OP0                         *LIVELLO PROCEDURA
099300991108     C                   MOVEL     WOP1          k12OP1                         *LIVELLO PGM
099400991108     C                   MOVEL     '0'           k12F03                         *PREMUTO F03=FINE
099500991108     C                   MOVEL     '0'           k12F12                         *PREMUTO F12=RITORNO
099600991108     C                   MOVEL     '0'           k12ERR                         *'1'=ERRORE
099700991108     C                   MOVEL     *BLANKS       k12MSG                         *MESSAGGIO DI RITORN
099800991108     C                   MOVEL     Dtle          k12tle                         *PARTENZA/ARRIVO
099900001020     C*                  MOVE      *zeros        k12cop                         *TERMINAL PARTENZA
100000001020     C                   MOVE(p)   Dksu          k12cop                         *TERMINAL PARTENZA
100100991108     C*
100200001020     C                   MOVEL     tibds         kpjbu
100300991108     C                   CALL      WPGM
100400991108     C                   PARM                    KPJBA
100500100914     C                   MOVEL     kpjbu         tibds
100600991108     C*
100700991108     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
100800001020     C     k12F03        IFEQ      '1'                                          *PREMUTO F03
100900991108     C                   MOVEL     '1'           ERRL00                         *ERRORE
101000991108     C                   MOVEL     '1'           F03L00                         *FINE PGM
101100001020     C                   ELSE                                                   *NON PREMUTO F03
101200991108     C                   MOVEL     k12MSG        DSMSMS                         *MESSAGGIO DI RITORN
101300001020     C                   ENDIF
101400001020     C*
101500991108     C                   ENDSR
101600001020     C*--------------------------------------------------------------------------------------------*
101700091027     C* CARTAB - carica le tabelle occorrenti
101800001020     C*--------------------------------------------------------------------------------------------*
101900001020     C     CARTAB        BEGSR
102000020212     C*----------
102100020212     C* Tipi servizi soggetti a controllo delle abilitazioni
102200020212     C*----------
102300020212     C                   z-add     *zeros        x                 3 0
102400020212     C                   eval      tbecod = 'ISV'
102500091027     C     tbecod        setll     tntbe01l
102600091027     C     tbecod        reade     tntbe01l
102700020212     C                   dow       not %eof(tntbe01l)
102800091027     C                   movel     tbeuni        dsisv
102900020212     C                   if        §isvuni  = 'S'
103000020212     C                   add       1             x
103100020212     C                   eval      isvkun(x) = tbeke1
103200020212     C                   endif
103300091027     C     tbecod        reade     tntbe01l
103400020212     C                   enddo
103500091027     c*---
103600091027     c* tipi legami "internet"
103700091027     c*---
103800091027     c                   z-add     *zeros        jtle
103900091027     c                   movel     'TLE'         tbecod
104000091027     c     tbecod        setll     tntbe01l
104100091027     c     tbecod        reade     tntbe01l
104200091027     c                   dow       not %eof(tntbe01l)
104300091027     c                   if        tbeatb = *blanks
104400091027     c                   eval      dstle = tbeuni
104500091027     c* solo legami d tipo "internet"
104600091027     c                   if        §TLEGRP = 'W'
104700091027     c                   add       1             jtle
104800091027     c                   eval      sktle(jtle) = tbeke1
104900091027     c                   endif
105000091027     c                   endif
105100091027     c     tbecod        reade     tntbe01l
105200091027     c                   enddo
105300020212     C*
105400991001     C                   ENDSR
105500970526     C*------------------------------------------------------------------------*
105600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
105700970526     C*------------------------------------------------------------------------*
105800970526     C     CHKFOP        BEGSR
105900091110     C*
106000091110     C* OPZIONI SOLO X PROFILI "EDP*"
106100091110     C                   IF        %subst(KNMUS:1:3) = 'EDP'
106200091110     C                   MOVEL     OPD(7)        OPDRIG                         *EXT ABILITAZIONI
106300091110     C                   MOVEL     'S'           OPDATT
106400091110     C                   MOVEL     OPDRIG        OPD(7)
106500091110     C                   MOVEL     OPD(8)        OPDRIG                         *REGOLE DEFAULT
106600091110     C                   MOVEL     'S'           OPDATT
106700091110     C                   MOVEL     OPDRIG        OPD(8)
106800091111     C                   MOVEL     OPD(10)       OPDRIG                         *CLIENTE/TIPO FILE
106900091111     C                   MOVEL     'S'           OPDATT
107000091111     C                   MOVEL     OPDRIG        OPD(10)
107100091110     C                   ENDIF
107200970626     C*
107300970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
107400970626IF  1C     DEPOP0        IFEQ      'P01'
107500970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
107600970626     C                   MOVEL     'S'           OPDATT
107700970626     C                   MOVEL     OPDRIG        OPD(1)
107800970626X   1C                   ELSE
107900970626     C* OPZIONI
108000000830     C                   MOVEL     OPD(5)        OPDRIG                         *VISUALIZZAZIONE
108100970626     C                   MOVEL     'S'           OPDATT
108200970626     C                   MOVEL     OPDRIG        OPD(5)
108300091110     C***  DUTLPO        IFEQ      'S'
108400091110     C                   MOVEL     OPD(9)        OPDRIG                         *ABILITAZIONI
108500091110     C                   MOVEL     'S'           OPDATT
108600091110     C                   MOVEL     OPDRIG        OPD(9)
108700091111     C                   MOVEL     OPD(10)       OPDRIG                         *CLIENTE/TIPO FILE
108800091111     C                   MOVEL     'S'           OPDATT
108900091111     C                   MOVEL     OPDRIG        OPD(10)
109000050222     C***                ENDIF
109100970626     C* FUNZIONI
109200970626E   1C                   ENDIF
109300970626     C*
109400970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
109500970626IF  1C     DEPOP0        IFEQ      'P02'
109600051013     C     DUTLPO        ANDEQ     'S'                                          *SOLO SEDE
109700970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
109800970626     C                   MOVEL     'S'           OPDATT
109900970626     C                   MOVEL     OPDRIG        OPD(2)
110000991014     C                   MOVEL     OPD(3)        OPDRIG                         *MODIFICA
110100991014     C                   MOVEL     'S'           OPDATT
110200991014     C                   MOVEL     OPDRIG        OPD(3)
110300021024     C                   MOVEL     OPD(4)        OPDRIG                         *FAR SCADERE
110400021024     C                   MOVEL     'S'           OPDATT
110500021024     C                   MOVEL     OPDRIG        OPD(4)
110600970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
110700970626     C                   MOVEL     'S'           FUDATT
110800970626     C                   MOVEL     FUDRIG        FUD(6)
110900970626E   1C                   ENDIF
111000970526     C*
111100970526     C* GESTIONE OPZIONI A VIDEO
111200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
111300970526     C                   EXSR      OPZGES
111400970526     C*
111500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
111600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
111700970526     C                   EXSR      FUNGES
111800970526     C*
111900970526     C                   ENDSR
112000020916     C*--------------------------------------------------------------------------------------------*
112100020916     C* REPERISCE I DATI UTENTE
112200020916     C*--------------------------------------------------------------------------------------------*
112300020916     C     REPDATIUTE    BEGSR
112400020916     C*
112500020916     C* INIZIALIZZA VARIABILI DI WRK
112600020916     C                   CLEAR                   TIBS34DS
112700020916     C                   CLEAR                   AZUTEDS
112800020916     C                   CLEAR                   DDATIUTE
112900020916     C*
113000020916     C     *DTAARA       DEFINE    §azute        azuteds
113100020916     C     *DTAARA       DEFINE    §datiute      ddatiute
113200020916     C                   IN(E)     *DTAARA
113300020916if  1C                   IF        %Error
113400020916     c                   EVAL      I34Tla = 'L'
113500020916     C                   CALL      'TIBS34R'
113600020916     C                   PARM                    Tibs34Ds
113700020916     C                   IN        *DTAARA
113800020916e   1C                   ENDIF
113900020916     C*
114000020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
114100020916     C                   MOVEL(P)  RSUT          DSFIRS           20
114200020916     C*
114300020916     C                   ENDSR
114400130719
114500130719
114600130719
114700130719     C*----------------------------------------------------*
114800130719     C*  CONTROLLO NUMERICITA' CAMPI
114900130719     C*----------------------------------------------------*
115000130719     C     CHKNUM        BEGSR
115100130719     C*
115200130719     C                   IF        PiDecChr = *blanks
115300130719     C                   EVAL      PiDecChr = '.'
115400130719     C                   ENDIF
115500130719     C*
115600130719     C                   CALL(e)   'ISNUMERIC'
115700130719     C                   PARM                    PiStr            30
115800130719     C                   PARM                    PiDecChr          1
115900130719     C                   PARM      *ZEROS        PiVal            30 9
116000130719     C                   PARM      '0'           PiInt             1
116100130719     C                   PARM      '0'           PiNum             1
116200130719     C                   IF        %error
116300130719     C                   EVAL      PiNum=*off
116400130719     C                   ENDIF
116500130719     C*
116600130719     C                   ENDSR
116700130719     C***
116800130719
116900130719
117000130719
117100970526     C*------------------------------------------------------------------------*
117200970526     C* *INZSR - OPERAZIONI INIZIALI
117300970526     C*------------------------------------------------------------------------*
117400970526     C     *INZSR        BEGSR
117500970526     C*---
117600970526     C* RICEVIMENTO PARAMETRI
117700970526     C*---
117800970526     C     *ENTRY        PLIST
117900970612     C                   PARM                    KPJBA
118000020916     C                   MOVEL     KPJBU         TIS720
118100970526     C*
118200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
118300970526     C     XMSGPA        PLIST
118400970526     C                   PARM                    DSMSMS
118500970526     C                   PARM                    DSMSPG
118600970526     C                   PARM                    DSMSKE
118700970526     C                   PARM                    DSMSER
118800970526     C*
118900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
119000970526     C                   CALL      'XKEY'
119100970526     C                   PARM                    DSKEY
119200970526     C*---
119300970526     C* VARIABILI NON RIFERITE AL DATA BASE
119400970526     C*---
119500970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
119600970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
119700970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
119800970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
119900970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
120000970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
120100970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
120200970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
120300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
120400991104     C     *LIKE         DEFINE    s20OP0        DEPOP0                         *DI DEPOSITO
120500991104     C     *LIKE         DEFINE    s20OP1        DEPOP1
120600991104     C     *LIKE         DEFINE    s20OP0        WOP0                           *DI LAVORO
120700991104     C     *LIKE         DEFINE    s20OP1        WOP1
120800991104     C     *LIKE         DEFINE    s20tle        Dtle
120900991105     C     *LIKE         DEFINE    s20ksc        Dksu
121000991104     C     *LIKE         DEFINE    s20isv        Disv
121100991105     C     *LIKE         DEFINE    S20sun        Dsun
121200970526     C*---
121300991104     C     *LIKE         DEFINE    s20isv        Depis2
121400991105     C     *LIKE         DEFINE    s20sun        Depsu2
121500991105     C     *LIKE         DEFINE    s20sun        Depsu1
121600991104     C     *LIKE         DEFINE    vidisv        Depis1
121700010420     C     *LIKE         DEFINE    vidsrs        Depsrs
121800000216     C     *LIKE         DEFINE    vidcli        Depcli
121900991104     C     *LIKE         DEFINE    s20isv        kceisv
122000991105     C     *LIKE         DEFINE    s20sun        kcesun
122100000131     C*---
122200000131     C     *LIKE         DEFINE    kuntle        kkutle
122300000131     C     *LIKE         DEFINE    kunazp        kkuazp
122400000131     C     *LIKE         DEFINE    kuncop        kkucop
122500020212     C     *LIKE         DEFINE    kuncof        kkucof
122600020212     C     *LIKE         DEFINE    kundde        kkudde
122700970526     C* VARIABILI DI CONTROLLO
122800970526     C*---
122900970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
123000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
123100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
123200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
123300970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
123400970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
123500970526     C*
123600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
123700991104     C                   MOVEL     'TIS724R'     DSMSPG
123800991104     C                   MOVEL     'TIS724R'     MSGQUE
123900970526     C                   MOVEL     *BLANKS       MSGKEY
124000970526     C                   MOVEL     *BLANKS       DSMSMS
124100970526     C                   MOVEL     *BLANKS       DSMSKE
124200970526     C                   Z-ADD     *ZEROS        DSMSER
124300970526     C*---
124400970526     C* VARIABILI RIFERITE AL DATA BASE
124500970526     C*---
124600970526     C*---
124700970526     C* CHIAVI DI LETTURA
124800970526     C*---
124900991104     C* LETTURA tivss01L -
125000991104     C     KEYvss        KLIST
125100991105     C                   KFLD                    KCEsun                         *TERMINAL PARTENZA
125200991104     C                   KFLD                    KCEisv                         *PARTENZA/ARRIVO
125300000131     C* LETTURA tikun01L -
125400000131     C     KEYkun        KLIST
125500000131     C                   KFLD                    kkutle                         *TERMINAL PARTENZA
125600000131     C                   KFLD                    kkuazp                         *PARTENZA/ARRIVO
125700000131     C                   KFLD                    kkucop                         *PARTENZA/ARRIVO
125800000216     C* LETTURA tikun03L -
125900000216     C     KE3kun        KLIST                                                  *tikun03l
126000000216     C                   KFLD                    kkucop                          -cliente padre
126100020211     C* LETTURA tikun06L
126200020212     C     ke6kun        KLIST
126300020212     C                   KFLD                    kkucof                         *CODICE CLI FIGLIO
126400020212     C                   KFLD                    kkutle                         *TIPO LEGAME
126500970527     C*
126600970527     C* REPERIMENTO DATI UTENTE
126700020916     C                   EXSR      REPDATIUTE
126800970527     C*
126900970527     C* CARICA LE TABELLE OCCORRENTI
127000970527     C                   EXSR      CARTAB
127100970526     C*
127200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
127300991104     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
127400991104     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
127500970526     C*
127600970526     C* CALCOLA LA DATA CORRENTE
127700970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
127800970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
127900970526     C                   Z-ADD     WN8           G08DAT
128000970526     C                   Z-ADD     *ZEROS        G08INV
128100970526     C                   MOVEL     '0'           G08ERR
128200970526     C                   CALL      'XSRDA8'
128300970526     C                   PARM                    WLBDA8
128400970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
128500970526     C*
128600970526     C* IMPOSTA I PARAMETRI DI INGRESSO
128700020212     C                   if        s20isv <> *blank
128800020226     C                   move      s20isv        vidis1
128900020212     C                   else
129000020226     C                   MOVE      *blank        VIDis1                         *PARTENZA/ARRIVO
129100020226     C                   endif
129200000607     C                   move      s20sun        VIDsu2                         *CODICE STRATEGI
129300000607     C                   move      s20ksc        VIDcli                         *CODICE CLI PADRE
129400970526     C                   EXSR      CHKPAR
129500970526IF  1C     ERRL00        IFEQ      '1'
129600991104     C                   MOVEL     '1'           s20ERR
129700991104     C                   MOVEL     DSMSMS        s20MSG
129800970526X   1C                   ELSE
129900991105     C                   move      VIDsu2        DEPsu2
130000991105     C                   move      VIDsu1        DEPsu1
130100000216     C                   move      VIDcli        DEPcli
130200991104     C                   move      VIDis1        DEPis1
130300970526E   1C                   ENDIF
130400970526     C*
130500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
130600970526     C                   EXSR      CHKFOP
130700970526     C*
130800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
130900970530IF  1C     DEPOP0        IFEQ      'P01'
131000970530     C                   MOVEL     DEC(3)        VIDING
131100970530E   1C                   ENDIF
131200970530IF  1C     DEPOP0        IFEQ      'P02'
131300970526     C                   MOVEL     DEC(1)        VIDING
131400970530E   1C                   ENDIF
131500970530IF  1C     DEPOP0        IFEQ      'P05'
131600970526     C                   MOVEL     DEC(2)        VIDING
131700970526E   1C                   ENDIF
131800970526     C*
131900970526     C                   ENDSR
132000970526     C*------------------------------------------------------------------------*
132100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
132200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
132300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
132400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
132500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
132600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
132700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
132800970526     C*------------------------------------------------------------------------*
132900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
133000970526     C*------------------------------------------------------------------------*
133100970526     C     POSSFL        BEGSR
133200970526     C*
133300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
133400970526     C                   EXSR      CLRSFL
133500970526     C*
133600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
133700970526     C                   EXSR      SETFIL
133800970526     C*
133900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
134000970526     C     SFLEOF        IFEQ      0
134100970526     C                   EXSR      CARSFL
134200970526     C                   END
134300970526     C*
134400970526     C                   ENDSR
134500970526     C*------------------------------------------------------------------------*
134600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
134700970526     C*------------------------------------------------------------------------*
134800970526     C     ENTATT        BEGSR
134900970526     C*
135000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
135100970526     C                   Z-ADD     0             OPZOK             1 0
135200970526     C*
135300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
135400970526     C                   Z-ADD     0             SFLVAR            5 0
135500970526     C*
135600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
135700970526     C                   Z-ADD     0             SFLERR
135800970526     C*
135900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
136000970526     C                   Z-ADD     SFLULT        WFLULT
136100970526     C*
136200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
136300991104     C                   READC     S724D05                                98
136400970526     C     *IN98         DOWEQ     '0'
136500970526     C*
136600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
136700970526     C                   ADD       1             SFLVAR
136800970526     C*
136900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
137000970526     C     OPZOK         IFEQ      0
137100970526     C*
137200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
137300970526     C     OPZKEY        IFNE      *BLANKS
137400970526     C                   EXSR      OPZCON
137500970526     C                   END
137600970526     C*
137700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
137800970526     C*            2. PULIZIA SCELTA IN SFL
137900970526     C     OPZOK         IFEQ      0
138000970526     C                   MOVEL     '0'           *IN41
138100970526     C                   MOVEL     *BLANKS       OPZKEY
138200970526     C                   ELSE
138300970526     C*
138400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
138500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
138600970526     C                   MOVEL     '1'           *IN41
138700970526     C                   Z-ADD     SFLULT        SFLERR
138800970526     C                   END
138900970526     C                   ELSE
139000970526     C*
139100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
139200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
139300970526     C                   MOVEL     '1'           *IN41
139400970526     C                   END                                                    OPZOK=0
139500970526     C*
139600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
139700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
139800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
139900970526     C     OPZOK         IFNE      0
140000970526     C                   Z-ADD     SFLERR        SFLNBR
140100970526     C                   ELSE
140200970526     C                   Z-ADD     SFLULT        SFLNBR
140300970526     C                   END
140400970526     C*
140500970526     C* AGGIORNO RECORD SFL
140600020212     C                   EVAL      *IN76 = HIN76
140700991104     C                   UPDATE    S724D05
140800970526     C*
140900991104     C                   READC     S724D05                                98
141000970526     C                   END                                                    DO WHILE
141100970526     C*
141200970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
141300970526     C                   Z-ADD     WFLULT        SFLULT
141400970526     C*
141500970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
141600970526     C     SFLVAR        IFEQ      0
141700970526     C*---                 MOVEL'1'       FINL00
141800970526     C                   END
141900970526     C*
142000970526     C                   ENDSR
142100970526     C*------------------------------------------------------------------------*
142200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
142300970526     C*------------------------------------------------------------------------*
142400970526     C     CLRSFL        BEGSR
142500970526     C*
142600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
142700970526     C* (0=NON FINE FILE  1=FINE FILE)
142800970526     C                   Z-ADD     0             SFLEOF
142900970526     C*
143000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
143100970526     C                   Z-ADD     0             SFLULT
143200970526     C*
143300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
143400970526     C                   MOVEL     '1'           *IN40
143500991104     C                   WRITE     S724D06
143600970526     C                   MOVEL     '0'           *IN40
143700970526     C*
143800970526     C                   ENDSR
143900970526     C*------------------------------------------------------------------------*
144000970526     C* CARSFL - CARICAMENTO SFL
144100970526     C*------------------------------------------------------------------------*
144200970526     C     CARSFL        BEGSR
144300970526     C*
144400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
144500970526     C     SFLULT        IFEQ      0
144600970526     C                   EXSR      SETFIL
144700970526     C                   END
144800970526     C*
144900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
145000970526     C                   Z-ADD     SFLULT        WFLULT
145100970526     C*
145200970526     C* CICLO:
145300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
145400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
145500970526     C                   Z-ADD     1             SFLRIG
145600970526     C     SFLRIG        DOWLE     SFLPAG
145700970526     C     SFLEOF        ANDEQ     0
145800970526     C*
145900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
146000970526     C                   MOVEL     *BLANKS       OPZKEY
146100970526     C*
146200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
146300970526     C                   EXSR      IRESFL
146400970526     C*
146500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
146600970526     C                   ADD       1             SFLULT
146700970526     C*
146800970526     C* SCRITTURA RECORD SFL
146900020212     C                   EVAL      HIN76 = *IN76
147000991104     C                   WRITE     S724D05
147100970526     C                   ADD       1             SFLRIG
147200970526     C*
147300970526     C* LETTURA FILE DA CARICARE
147400970526     C                   EXSR      LETFIL
147500970526     C                   END                                                    DO WHILE
147600970526     C*
147700970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
147800970526     C     WFLULT        IFNE      SFLULT
147900970526     C                   Z-ADD     WFLULT        SFLNBR
148000970526     C                   ADD       1             SFLNBR
148100970526     C                   END
148200970526     C*
148300970526     C                   ENDSR
148400970526     C*------------------------------------------------------------------------*
148500970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
148600970526     C*------------------------------------------------------------------------*
148700970526     C     OUTSFL        BEGSR
148800970526     C*
148900970526     C* WRITE RECORD TESTATA VIDEO
149000991104     C                   WRITE     S724D01
149100970526     C*
149200970526     C* VISUALIZZAZIONE MESSAGGIO
149300970526     C                   EXSR      SNDMSG
149400970526     C*
149500970526     C* WRITE RECORD OPZIONI VIDEO
149600991104     C                   WRITE     S724D02
149700970526     C*
149800970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
149900991104     C                   WRITE     S724D03
150000970526     C*
150100970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
150200991104     C                   WRITE     S724D04
150300970526     C*
150400970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
150500970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
150600970526     C     SFLEOF        IFEQ      0
150700970526     C                   MOVEL     '0'           *IN42
150800970526     C                   ELSE
150900970526     C*
151000970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
151100970526     C                   MOVEL     '1'           *IN42
151200970526     C                   END
151300970526     C*
151400970526     C* WRITE RECORD TASTI FUNZIONALI
151500991104     C                   WRITE     S724D09
151600970526     C*
151700970526     C* CASO DI SFL VUOTO
151800970526     C     SFLULT        IFLE      0
151900970526     C*
152000970526     C*    - WRITE RECORD VUOTO
152100991104     C                   WRITE     S724D0A
152200991104     C                   READ      S724D0A                                99
152300970526     C*
152400970526     C                   ELSE
152500970526     C*
152600970526     C* CASO DI SFL CON RECORDS
152700970526     C*
152800970526     C*    - WRITE RECORD CONTROLLO SFL
152900991104     C                   WRITE     S724D06
153000991104     C                   READ      S724D06                                99
153100970526     C                   END
153200970526     C*
153300970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
153400991104     C                   READ      S724D03                                99
153500970526     C*
153600970526     C                   ENDSR
153700970526     C*------------------------------------------------------------------------*
153800970526     C* SNDMSG - MANDA MESSAGGIO
153900970526     C*------------------------------------------------------------------------*
154000970526     C     SNDMSG        BEGSR
154100970526     C*
154200970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
154300970526     C                   MOVEL     '1'           *IN40
154400991104     C                   WRITE     S724DMC
154500970526     C                   MOVEL     '0'           *IN40
154600970526     C*
154700970526     C* VISUALIZZAZIONE MESSAGGIO
154800970526     C                   Z-ADD     0             MSGULT
154900970526     C                   CALL      'XMSG'        XMSGPA
155000970526     C     DSMSER        IFEQ      0
155100970526     C                   MOVEL     DSMSKE        MSGKEY
155200970526     C                   END
155300970526     C                   Z-ADD     1             MSGULT
155400991104     C                   WRITE     S724DMS                                      SFL MESS
155500991104     C                   WRITE     S724DMC                                      CONTR MESS
155600970526     C*
155700970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
155800970526     C                   MOVEL     *BLANKS       DSMSMS
155900970526     C*
156000970526     C                   ENDSR
156100970526     C*------------------------------------------------------------------------*
156200970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
156300970526     C*------------------------------------------------------------------------*
156400970526     C     FUNCON        BEGSR
156500970526     C*
156600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
156700970526     C                   EXSR      FUNVAR
156800970526     C*
156900970526     C* PULISCO CAMPI DI LAVORO
157000970526     C                   Z-ADD     0             I                 5 0
157100970526     C                   Z-ADD     0             W                 5 0
157200970526     C*
157300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
157400970526     C     FUNKEY        IFNE      *BLANKS
157500970526     C     1             DO        24            I
157600970526     C                   MOVEL     FUD(I)        FUDRIG
157700970526     C*
157800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
157900970526     C     FUNKEY        IFEQ      FUDFUN
158000970526     C     FUDATT        ANDEQ     'S'
158100970526     C                   Z-ADD     I             W
158200970526     C                   END
158300970526     C                   END                                                    DO WHILE
158400970526     C*
158500970526     C* FUNZIONE NON ATTIVA (ERRORE)
158600970526     C     W             IFEQ      0
158700970526     C*
158800970526     C* EMISSIONE MESSAGGIO DI ERRORE
158900970526     C                   MOVEL     MSG(2)        DSMSMS
159000970526     C                   END                                                    W=0
159100970526     C                   END                                                    FUNKEY NE *BLANKS
159200970526     C*
159300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
159400970526     C     W             IFNE      0
159500970526     C     FUNKEY        OREQ      *BLANKS
159600970526     C*
159700970526     C     FUNKEY        IFEQ      *BLANKS
159800970526     C                   EXSR      ENTGES
159900970526     C                   END
160000970526     C     FUNKEY        IFEQ      '03'
160100970526     C                   EXSR      F03GES
160200970526     C                   END
160300970526     C     FUNKEY        IFEQ      '05'
160400970526     C                   EXSR      F05GES
160500970526     C                   END
160600970526     C     FUNKEY        IFEQ      '06'
160700970526     C                   EXSR      F06GES
160800970526     C                   END
160900970526     C     FUNKEY        IFEQ      '12'
161000970526     C                   EXSR      F12GES
161100970526     C                   END
161200970528     C     FUNKEY        IFEQ      '21'
161300970528     C                   EXSR      F21GES
161400970528     C                   END
161500970528     C     FUNKEY        IFEQ      '22'
161600970528     C                   EXSR      F22GES
161700970528     C                   END
161800970526     C     FUNKEY        IFEQ      '23'
161900970526     C                   EXSR      F23GES
162000970526     C                   END
162100970526     C     FUNKEY        IFEQ      '24'
162200970526     C                   EXSR      F24GES
162300970526     C                   END
162400970526     C     FUNKEY        IFEQ      'RU'
162500970526     C                   EXSR      RUPGES
162600970526     C                   END
162700970526     C*
162800970526     C                   END
162900970526     C*
163000970526     C                   ENDSR
163100970526     C*------------------------------------------------------------------------*
163200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
163300970526     C*------------------------------------------------------------------------*
163400970526     C     FUNVAR        BEGSR
163500970526     C*
163600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
163700970526     C     KEY           IFEQ      ENTER
163800970526     C                   MOVEL     *BLANKS       FUNKEY
163900970526     C                   END
164000970526     C     KEY           IFEQ      F1
164100970526     C                   MOVEL     '01'          FUNKEY
164200970526     C                   END
164300970526     C     KEY           IFEQ      F2
164400970526     C                   MOVEL     '02'          FUNKEY
164500970526     C                   END
164600970526     C     KEY           IFEQ      F3
164700970526     C                   MOVEL     '03'          FUNKEY
164800970526     C                   END
164900970526     C     KEY           IFEQ      F4
165000970526     C                   MOVEL     '04'          FUNKEY
165100970526     C                   END
165200970526     C     KEY           IFEQ      F5
165300970526     C                   MOVEL     '05'          FUNKEY
165400970526     C                   END
165500970526     C     KEY           IFEQ      F6
165600970526     C                   MOVEL     '06'          FUNKEY
165700970526     C                   END
165800970526     C     KEY           IFEQ      F7
165900970526     C                   MOVEL     '07'          FUNKEY
166000970526     C                   END
166100970526     C     KEY           IFEQ      F8
166200970526     C                   MOVEL     '08'          FUNKEY
166300970526     C                   END
166400970526     C     KEY           IFEQ      F9
166500970526     C                   MOVEL     '09'          FUNKEY
166600970526     C                   END
166700970526     C     KEY           IFEQ      F10
166800970526     C                   MOVEL     '10'          FUNKEY
166900970526     C                   END
167000970526     C     KEY           IFEQ      F11
167100970526     C                   MOVEL     '11'          FUNKEY
167200970526     C                   END
167300970526     C     KEY           IFEQ      F12
167400970526     C                   MOVEL     '12'          FUNKEY
167500970526     C                   END
167600970526     C     KEY           IFEQ      F13
167700970526     C                   MOVEL     '13'          FUNKEY
167800970526     C                   END
167900970526     C     KEY           IFEQ      F14
168000970526     C                   MOVEL     '14'          FUNKEY
168100970526     C                   END
168200970526     C     KEY           IFEQ      F15
168300970526     C                   MOVEL     '15'          FUNKEY
168400970526     C                   END
168500970526     C     KEY           IFEQ      F16
168600970526     C                   MOVEL     '16'          FUNKEY
168700970526     C                   END
168800970526     C     KEY           IFEQ      F17
168900970526     C                   MOVEL     '17'          FUNKEY
169000970526     C                   END
169100970526     C     KEY           IFEQ      F18
169200970526     C                   MOVEL     '18'          FUNKEY
169300970526     C                   END
169400970526     C     KEY           IFEQ      F19
169500970526     C                   MOVEL     '19'          FUNKEY
169600970526     C                   END
169700970526     C     KEY           IFEQ      F20
169800970526     C                   MOVEL     '20'          FUNKEY
169900970526     C                   END
170000970526     C     KEY           IFEQ      F21
170100970526     C                   MOVEL     '21'          FUNKEY
170200970526     C                   END
170300970526     C     KEY           IFEQ      F22
170400970526     C                   MOVEL     '22'          FUNKEY
170500970526     C                   END
170600970526     C     KEY           IFEQ      F23
170700970526     C                   MOVEL     '23'          FUNKEY
170800970526     C                   END
170900970526     C     KEY           IFEQ      F24
171000970526     C                   MOVEL     '24'          FUNKEY
171100970526     C                   END
171200970526     C     KEY           IFEQ      ROLLUP
171300970526     C                   MOVEL     'RU'          FUNKEY
171400970526     C                   END
171500970526     C     KEY           IFEQ      HELP
171600970526     C                   MOVEL     'HP'          FUNKEY
171700970526     C                   END
171800970526     C*
171900970526     C                   ENDSR
172000970526     C*------------------------------------------------------------------------*
172100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
172200970526     C*------------------------------------------------------------------------*
172300970526     C     FUNGES        BEGSR
172400970526     C*
172500970526     C* PULISCO CAMPI DI LAVORO
172600970526     C                   Z-ADD     0             W                 5 0
172700970526     C*
172800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
172900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
173000970526     C     1             DO        24            I
173100970526     C                   MOVEL     FUD(I)        FUDRIG
173200970526     C*
173300970526     C* PULIZIA FUNZIONI PRECEDENTI
173400970526     C                   MOVEL     *BLANKS       FUV(I)
173500970526     C*
173600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
173700970526     C* (SALTA F23 E F24)
173800970526     C     FUDATT        IFEQ      'S'
173900970526     C     FUDFUN        ANDNE     '23'
174000970526     C     FUDFUN        ANDNE     '24'
174100970526     C                   ADD       1             W
174200970526     C                   MOVEL     FUDDES        FUV(W)
174300970526     C                   END
174400970526     C                   END                                                    DO WHILE
174500970526     C*
174600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
174700970526     C* ALTRIMENTI DISATTIVO F24
174800970526     C     1             DO        24            I
174900970526     C                   MOVEL     FUD(I)        FUDRIG
175000970526     C*
175100970526     C     FUDFUN        IFEQ      '24'
175200970526     C     W             IFGT      6
175300970526     C                   MOVEL     'S'           FUDATT
175400970526     C                   ELSE
175500970526     C                   MOVEL     *BLANKS       FUDATT
175600970526     C                   END                                                    W GT 8
175700970526     C                   MOVEL     FUDRIG        FUD(I)
175800970526     C                   END
175900970526     C                   END                                                    DO WHILE
176000970526     C*
176100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
176200970526     C     W             DIV       6             I
176300970526     C                   MVR                     Z
176400970526     C*
176500970526     C     Z             IFNE      0
176600970526     C     I             ADD       1             Z
176700970526     C                   ELSE
176800970526     C                   Z-ADD     I             Z
176900970526     C                   END
177000970526     C*
177100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
177200970526     C                   ADD       1             FUNGRU
177300970526     C     FUNGRU        IFGT      Z
177400970526     C     FUNGRU        ORLT      1
177500970526     C                   Z-ADD     1             FUNGRU
177600970526     C                   END
177700970526     C*
177800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
177900970526     C     FUNGRU        IFEQ      1
178000970526     C                   MOVEL     FUV(1)        FUN001
178100970526     C                   MOVEL     FUV(2)        FUN002
178200970526     C                   MOVEL     FUV(3)        FUN003
178300970526     C                   MOVEL     FUV(4)        FUN004
178400970526     C                   MOVEL     FUV(5)        FUN005
178500970526     C                   MOVEL     FUV(6)        FUN006
178600970526     C                   END
178700970526     C*
178800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
178900970526     C     FUNGRU        IFEQ      2
179000970526     C                   MOVEL     FUV(7)        FUN001
179100970526     C                   MOVEL     FUV(8)        FUN002
179200970526     C                   MOVEL     FUV(9)        FUN003
179300970526     C                   MOVEL     FUV(10)       FUN004
179400970526     C                   MOVEL     FUV(11)       FUN005
179500970526     C                   MOVEL     FUV(12)       FUN006
179600970526     C                   END
179700970526     C*
179800970526     C* TERZO GRUPPO DI 6 FUNZIONI
179900970526     C     FUNGRU        IFEQ      3
180000970526     C                   MOVEL     FUV(13)       FUN001
180100970526     C                   MOVEL     FUV(14)       FUN002
180200970526     C                   MOVEL     FUV(15)       FUN003
180300970526     C                   MOVEL     FUV(16)       FUN004
180400970526     C                   MOVEL     FUV(17)       FUN005
180500970526     C                   MOVEL     FUV(18)       FUN006
180600970526     C                   END
180700970526     C*
180800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
180900970526     C     FUNGRU        IFEQ      4
181000970526     C                   MOVEL     FUV(19)       FUN001
181100970526     C                   MOVEL     FUV(20)       FUN002
181200970526     C                   MOVEL     FUV(21)       FUN003
181300970526     C                   MOVEL     FUV(22)       FUN004
181400970526     C                   MOVEL     *BLANKS       FUN005
181500970526     C                   MOVEL     *BLANKS       FUN006
181600970526     C                   END
181700970526     C*
181800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
181900970526     C                   MOVEL     *BLANKS       FUN007
182000970526     C                   MOVEL     *BLANKS       FUN008
182100970526     C*
182200970526     C     1             DO        24            I
182300970526     C                   MOVEL     FUD(I)        FUDRIG
182400970526     C     FUDFUN        IFEQ      '23'
182500970526     C     FUDATT        ANDEQ     'S'
182600970526     C                   MOVEL     FUDDES        FUN007
182700970526     C                   END
182800970526     C     FUDFUN        IFEQ      '24'
182900970526     C     FUDATT        ANDEQ     'S'
183000970526     C                   MOVEL     FUDDES        FUN008
183100970526     C                   END
183200970526     C                   END                                                    DO
183300970526     C*
183400970526     C                   ENDSR
183500970526     C*------------------------------------------------------------------------*
183600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
183700970526     C*------------------------------------------------------------------------*
183800970526     C     OPZGES        BEGSR
183900970526     C*
184000970526     C* PULISCO CAMPI DI LAVORO
184100970526     C                   Z-ADD     0             W                 5 0
184200970526     C*
184300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
184400970526     C* E CARICO TABELLA OPZIONI ATTIVE
184500970526     C     1             DO        24            I
184600970526     C                   MOVEL     OPD(I)        OPDRIG
184700970526     C*
184800970526     C* PULIZIA OPZIONI PRECEDENTI
184900970526     C                   MOVEL     *BLANKS       OPV(I)
185000970526     C*
185100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
185200970526     C     OPDATT        IFEQ      'S'
185300970526     C                   ADD       1             W
185400970526     C                   MOVEL     OPDDES        OPV(W)
185500970526     C                   END
185600970526     C                   END                                                    DO WHILE
185700970526     C*
185800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
185900970526     C* ALTRIMENTI DISATTIVO F23
186000970526     C     1             DO        24            I
186100970526     C                   MOVEL     FUD(I)        FUDRIG
186200970526     C*
186300970526     C     FUDFUN        IFEQ      '23'
186400970526     C     W             IFGT      8
186500970526     C                   MOVEL     'S'           FUDATT
186600970526     C                   ELSE
186700970526     C                   MOVEL     *BLANKS       FUDATT
186800970526     C                   END                                                    W GT 8
186900970526     C                   MOVEL     FUDRIG        FUD(I)
187000970526     C                   END
187100970526     C                   END                                                    DO WHILE
187200970526     C*
187300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
187400970526     C     W             DIV       8             I
187500970526     C                   MVR                     Z                 5 0
187600970526     C*
187700970526     C     Z             IFNE      0
187800970526     C     I             ADD       1             Z
187900970526     C                   ELSE
188000970526     C                   Z-ADD     I             Z
188100970526     C                   END
188200970526     C*
188300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
188400970526     C                   ADD       1             OPZGRU
188500970526     C     OPZGRU        IFGT      Z
188600970526     C     OPZGRU        ORLT      1
188700970526     C                   Z-ADD     1             OPZGRU
188800970526     C                   END
188900970526     C*
189000970526     C* PRIMO GRUPPO DI 8 OPZIONI
189100970526     C     OPZGRU        IFEQ      1
189200970526     C                   MOVEL     OPV(1)        OPZ001
189300970526     C                   MOVEL     OPV(2)        OPZ002
189400970526     C                   MOVEL     OPV(3)        OPZ003
189500970526     C                   MOVEL     OPV(4)        OPZ004
189600970526     C                   MOVEL     OPV(5)        OPZ005
189700970526     C                   MOVEL     OPV(6)        OPZ006
189800970526     C                   MOVEL     OPV(7)        OPZ007
189900970526     C                   MOVEL     OPV(8)        OPZ008
190000970526     C                   END
190100970526     C*
190200970526     C* SECONDO GRUPPO DI 8 OPZIONI
190300970526     C     OPZGRU        IFEQ      2
190400970526     C                   MOVEL     OPV(9)        OPZ001
190500970526     C                   MOVEL     OPV(10)       OPZ002
190600970526     C                   MOVEL     OPV(11)       OPZ003
190700970526     C                   MOVEL     OPV(12)       OPZ004
190800970526     C                   MOVEL     OPV(13)       OPZ005
190900970526     C                   MOVEL     OPV(14)       OPZ006
191000970526     C                   MOVEL     OPV(15)       OPZ007
191100970526     C                   MOVEL     OPV(16)       OPZ008
191200970526     C                   END
191300970526     C*
191400970526     C* TERZO GRUPPO DI 8 OPZIONI
191500970526     C     OPZGRU        IFEQ      3
191600970526     C                   MOVEL     OPV(17)       OPZ001
191700970526     C                   MOVEL     OPV(18)       OPZ002
191800970526     C                   MOVEL     OPV(19)       OPZ003
191900970526     C                   MOVEL     OPV(20)       OPZ004
192000970526     C                   MOVEL     OPV(21)       OPZ005
192100970526     C                   MOVEL     OPV(22)       OPZ006
192200970526     C                   MOVEL     OPV(23)       OPZ007
192300970526     C                   MOVEL     OPV(24)       OPZ008
192400970526     C                   END
192500970526     C*
192600970526     C                   ENDSR
192700970526     C*------------------------------------------------------------------------*
192800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
192900970526     C*------------------------------------------------------------------------*
193000970526     C     OPZCON        BEGSR
193100970526     C*
193200970526     C* PULISCO CAMPI DI LAVORO
193300970526     C                   Z-ADD     0             I                 5 0
193400970526     C                   Z-ADD     0             W                 5 0
193500970526     C*
193600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
193700970526     C     1             DO        24            I
193800970526     C                   MOVEL     OPD(I)        OPDRIG
193900970526     C*
194000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
194100970526     C     OPZKEY        IFEQ      OPDOPZ
194200970526     C     OPZKEY        OREQ      OPDOP1
194300970526     C*
194400970526     C     OPDATT        IFEQ      'S'
194500970526     C                   MOVEL     OPDOPZ        OPZKEY
194600970526     C                   Z-ADD     I             W
194700970526     C                   END
194800970526     C                   END
194900970526     C*
195000970526     C                   END                                                    DO WHILE
195100970526     C*
195200970526     C* OPZIONE NON ATTIVA (ERRORE)
195300970526     C     W             IFEQ      0
195400970526     C*
195500970526     C* OPZIONE NON OK
195600970526     C                   Z-ADD     1             OPZOK
195700970526     C*
195800970526     C* EMISSIONE MESSAGGIO DI ERRORE
195900970526     C                   MOVEL     MSG(1)        DSMSMS
196000970526     C                   ELSE
196100970526     C*
196200970526     C* OPZIONE ATTIVA (PROCEDI ...)
196300970526     C*
196400970526     C     OPZKEY        IFEQ      ' 1'
196500970526     C                   EXSR      O01GES
196600970526     C                   END
196700970526     C     OPZKEY        IFEQ      ' 2'
196800970526     C                   EXSR      O02GES
196900970526     C                   END
197000991014     C     OPZKEY        IFEQ      ' 3'
197100991014     C                   EXSR      O03GES
197200991014     C                   END
197300021024     C     OPZKEY        IFEQ      ' 4'
197400021024     C                   EXSR      O04GES
197500021024     C                   END
197600970526     C     OPZKEY        IFEQ      ' 5'
197700970526     C                   EXSR      O05GES
197800970526     C                   END
197900060727     C     OPZKEY        IFEQ      ' 7'
198000060727     C                   EXSR      O07GES
198100060727     C                   END
198200991108     C     OPZKEY        IFEQ      ' 8'
198300991108     C                   EXSR      O08GES
198400991108     C                   END
198500991108     C     OPZKEY        IFEQ      ' 9'
198600991108     C                   EXSR      O09GES
198700991108     C                   END
198800001020     C     OPZKEY        IFEQ      '10'
198900001020     C                   EXSR      O10GES
199000001020     C                   END
199100970526     C                   END                                                    OPZ.NON ATTIVA
199200970526     C*
199300970526     C                   ENDSR
199400970526     C*------------------------------------------------------------------------*
199500970526     C* NOTE COSTRUZIONE SCHIERE
199600970526     C*------------------------------------------------------------------------*
199700970526     C*
199800970526     C* -MSG-MESSAGGI
199900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
200000970526     C* (NON TOCCARE MSG,1 E MSG,2)
200100970526     C*
200200970526     C* -CMD-COMANDI
200300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
200400970526     C*
200500970526     C* -OPD-OPZIONI
200600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
200700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
200800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
200900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
201000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
201100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
201200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
201300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
201400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
201500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
201600970526     C*
201700970526     C* -FUD-FUNZIONI
201800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
201900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
202000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
202100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
202200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
202300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
202400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
202500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
202600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
202700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
202800970526     O*------------------------------------------------------------------------*
202900970526** DEC - DECODIFICHE
203000970526    GESTIONE                                                     1
203100970526INTERROGAZIONE                                                   2
203200970530     SCELTA                                                      3
2033009705264
2034009705265
203500970526** MSG - MESSAGGI
203600991104TIS724R- Opzione non gestita                                                   1
203700991104TIS724R- Tasto funzionale non gestito                                          2
203800991104TIS724R- Non abilitato alla gestione di questo CAP                             3
203900991104TIS724R- Record annullato. Consentito solo il ripristino                       4
204000991104TIS724R- Record non annullato. Impossibile ripristinare                        5
204100991104TIS724R- Accettato solo un parametro di inizio lista                           6
204200991104TIS724R- 7                                                                     7
204300991104TIS724R- 8                                                                     8
204400991104TIS724R- 9                                                                     9
204500991104TIS724R- 10                                                                   10
204600991104TIS724R- 11
204700991104TIS724R- Codice tipo servizio                                                 12
204800991104TIS724R- 13                                                                   13
204900991104TIS724R- 14                                                                   14
205000991104TIS724R- 15                                                                   15
205100991104TIS724R- 16                                                                   16
205200991104TIS724R- 17                                                                   17
205300991104TIS724R- 18                                                                   18
205400991104TIS724R- 19                                                                   19
205500991104TIS724R- 20                                                                   20
205600970528** CMD - COMANDI CL
205700970528WRKSPLF
205800970528
205900991108
206000970528
206100970528
206200970526** OPD - OPZIONI
206300970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
206400970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
206500991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
206600021024 4=Fai scadere     -4 -     (S/N)=N                        opz. 04¬
206700970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
206800970526                   -6 -     (S/N)=N                        opz. 06¬
206900060727 7=Ext Abilitaz.   -7 -     (S/N)=N                        opz. 07¬
207000991108 8=Regole Dft      -8 -     (S/N)=N                        opz. 08¬
207100991108 9=Cliente Unific. -9 -     (S/N)=N                        opz. 09¬
20720000102010=Regole Cli/TipoF-10-     (S/N)=N                        opz. 10¬
207300970526                   -  -     (S/N)=N                        opz. 11¬
207400970526                   -  -     (S/N)=N                        opz. 12¬
207500970526                   -  -     (S/N)=N                        opz. 13¬
207600970526                   -  -     (S/N)=N                        opz. 14¬
207700970526                   -  -     (S/N)=N                        opz. 15¬
207800970526                   -  -     (S/N)=N                        opz. 16¬
207900970526                   -  -     (S/N)=N                        opz. 17¬
208000970526                   -  -     (S/N)=N                        opz. 18¬
208100970526                   -  -     (S/N)=N                        opz. 19¬
208200970526                   -  -     (S/N)=N                        opz. 20¬
208300970526                   -  -     (S/N)=N                        opz. 21¬
208400970526                   -  -     (S/N)=N                        opz. 22¬
208500970526                   -  -     (S/N)=N                        opz. 23¬
208600970526                   -  -     (S/N)=N                        opz. 24¬
208700970526** FUD - FUNZIONI
208800970526                   -        (S/N)=N                        funz.01¬
208900970526                   -        (S/N)=N                        funz.02¬
209000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
209100970526                   -        (S/N)=N                        funz.04¬
209200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
209300970526F06=Inserimento    -        (S/N)=N                        funz.06¬
209400970526                   -        (S/N)=N                        funz.07¬
209500970527                   -        (S/N)=N                        funz.08¬
209600970527                   -        (S/N)=N                        funz.09¬
209700970527                   -        (S/N)=N                        funz.10¬
209800970526                   -        (S/N)=N                        funz.11¬
209900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
210000970526                   -        (S/N)=N                        funz.13¬
210100970526                   -        (S/N)=N                        funz.14¬
210200970526                   -        (S/N)=N                        funz.15¬
210300970526                   -        (S/N)=N                        funz.16¬
210400970526                   -        (S/N)=N                        funz.17¬
210500970526                   -        (S/N)=N                        funz.18¬
210600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
210700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
210800050408F21=Stampa elenco  -        (S/N)=N                        funz.21¬
210900050408F22=Gestione stampe-        (S/N)=N                        funz.22¬
211000970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
211100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
