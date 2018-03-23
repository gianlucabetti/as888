000100021024      * FNLV40R  *----------------------------------------------------*
000200021024      *------------------*                                            *
000300021024      *        PROGRAMMA INTERATTIVO DI REVISIONE/INSERIMENTO         *
000400021024      *           TESTI LETTERE    TIPO STAMPA: COMPRESSO             *
000500060912
000600060912      * ? ATTENZIONE!!!!! ?
000700060912      *? ?  il pgm è stato modificato per gestire i testi in lingua   *
000800060912      *? ?  e per poterli copiare ma dato che per ora i testi che     *
000900060912      *? ?  servivano in lingua li abbiamo fatti come pgm ho oscurato *
001000060912      *? ?  il tutto dalle videate utilizzando un indicatore          *
001100060912      *? ?  IN15 no lingua                                            *
001200060912
001300021024      *---------------------------------------------------------------*
001400930108     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
001500021024     FFNTXT01L  UF A E           K DISK
001600021024     FTABEL00F  IF   E           K DISK
001700060606     fazlin03l  if   e           k disk
001800021024     FFNLV40D   CF   E             WORKSTN INFDS(LV40DS)
001900941215     F                                     SFILE(LV40S02:RT)
002000941215     F                                     SFILE(LV40S03:RT)
002100021024      *-----------------------------------------------------------------
002200940308     D TES             S             37    DIM(1) CTDATA PERRCD(1)
002300021025     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
002400021024      *
002500930406     D DS1H          E DS
002600021024     D DFNTXTH       E DS
002700021024     D FNLV41DS      E DS
002800021024     D CNCR80        E DS
002900021024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000940209     D DS1I          E DS
003100021024     D  SKI                   26     88    DIM(21)
003200921214     D KPJBA         E DS
003300940208     D  PROF                   1      3
003400021024      *
003500021024     D $PO                            3  0
003600060606
003700060606     d p37iso2         s                   like(liniso2)
003800060606     d p37desi         s                   like(lindesita)
003900060607     d savvidlin       s                   like(linfntxt)
004000060607     d savcoplin       s                   like(linfntxt)
004100060607     d comlin          s                   like(txtlin)
004200060606
004300021024      *
004400021024      * LISTA PARAMETRI PER RICHIAMO PGM MANUTENZIONE TESTI
004500900730     D PARAM1          DS
004600901024     D  FLG1                   1      1
004700900730     D  S08TTX                 2      3
004800060607     D  vidLIN                 4      5
004900060607     D**S08FNZ                 5      5
005000901212     D  COPTTX                 6      7
005100940222     D*
005200941215     D* RICHIAMO PGM GESTIONE OPZIONI STAMPA TESTO     - FNLV42R -
005300940222     D PARAM3          DS
005400940222     D  PA3TTX                 1      2
005500940308     D*
005600941215     D LV40DS          DS
005700940218     D  NRG                  370    370
005800940218     D  NCL                  371    371
005900940218     D  PRIMA                378    379B 0
006000940218     D*
006100940218     D CURSOR          DS
006200940218     D  RIRI                   1      2B 0
006300940218     D  R$$                    2      2
006400940218     D  COCO                   3      4B 0
006500940218     D  C$$                    4      4
006600940308     D                 DS
006700940308     D  S08LET                 1      1
006800940308     D  S08RIB                 2      2
006900940308     D  S08CR1                 3     77
007000940308     D* RIGA SUBFILE PER COMPRESSO
007100940308     D  S08CR2                78    112
007200940308     D  WRIGA                  1    112
007300940308     D* RIGA SUBFILE PER NORMALE
007400940308     D  WRIGA2                 1     77
007500940308     D* RIGA DEL TESTO
007600940308     D  RIGAT                  3    112
007700921214     C*---------------------------------------------------------------*
007800921214     C* INDICATORI USATI                                              *
007900921214     C*---------------------------------------------------------------*
008000940308     C* 01 - POSIZIONAMENTO CURSORE
008100940308     C* 03 - TESTO NON GESTIBILE
008200930406     C* 04 - ROLLUP
008300930107     C* 05 - OFF - ALMENO UNA WRITE SUL SFILE
008400941215     C* 10 - ON ---> TESTO NORMALE      OFF ---> TESTO IN COMPRESSO
008500060607      * 12 - ON ---> Sede               OFF ---> Filiale
008600940301     C* 13 - PROGRAMMA RICHIAMATO
008700021024     C* 21 - TIPO TESTO NON PRESENTE IN FLTXT00F O TESTO ANNULLATO
008800921214     C* 22 - OFF - MODIFICA TESTO; ON - IMMISSIONE
008900921214     C* 23 - DI COMODO
009000921214     C* 24 - TIPO TESTO DA CUI COPIARE ERRATO
009100930105     C* 27 - ABILITAZIONE CMD16-ANNULLAMENTO TESTO
009200021025     c* 28 - Visualizza messaggio a video
009300021024     C* 30 - DI COMODO
009400940210     C* 35 - LOKUP ANDATA A BUON FINE
009500921214     C* 40 - ERRORE SE RIGHE IN CALCE PIU DI DUE
009600921214     C* 41 - ERRORE SE TIPO RIGA NON IMMESSO
009700940210     C* 42 - ERRORE SE PROFILO NON VALIDO PER LA GESTIONE DEL TESTO
009800940210     C* 43 - ERRORE MESSAGGIO DI AVVERTIMENTO
009900021025     c* 44 - Errore descrizione testo
010000060607      * 45 - Lingua testo
010100060607      * 46 - Lingua testo da cui copiare
010200921214     C* 72 - OFF - SFLCLR; ON - SFLDSP E SFLDSPCTL
010300921214     C* 77 - AGGANCIO RIGA SFILE E LETTURA FLTXT00F
010400921214     C* 90 - GENERICO DI ERRORE
010500921214     C*---------------------------------------------------------------*
010600921214     C*                    MAIN                                       *
010700921214     C* PROGRAMMA PRINCIPALE MANUTENZIONE TESTI LETTERE COMMERCIALE   *
010800921214     C*---------------------------------------------------------------*
010900921214     C*
011000921214     C* DEFINIZIONE KLIST E VARIABILI
011100921214     C                   EXSR      INIZIO
011200921214     C*
011300940301     C* 13 ON  - P R O G R A M M A   R I C H I A M A T O
011400940301IF  1C     FLG1          IFNE      ' '
011500940301     C                   SETON                                        13
011600901212     C                   GOTO      RICH2
011700930105E   1C                   ENDIF
011800930107     C*
011900921214     C     RICH1         TAG
012000060607     C**!!!              MOVE      'I'           S08LIN
012100881121     C                   MOVE      '  '          S08TTX
012200901212     C                   MOVE      '  '          COPTTX
012300940210     C                   MOVEL     ' '           S08FTS
012400940210     C                   MOVEL     ' '           UNO               1
012500060606     c                   eval      vidlin = 'it'
012600060606     c                   eval      coplin = 'it'
012700921104     C*
012800921214     C* EMISSIONE I FORMATO - RICHIESTA TESTO DA MANUTENERE
012900921214     C     EMET1         TAG
013000941215     C                   EXFMT     LV40D01
013100021025     C                   CLEAR                   VIDMSG
013200021025     c                   Eval      *in28 = *off
013300021025     c                   Setoff                                       2124
013400021025     c                   Setoff                                       4042
013500060607     c                   Setoff                                       4546
013600921214     C*
013700901023     C* CMD3 - FINE LAVORO
013800901023     C   KC              GOTO      FINE
013900940308     C*
014000940308     C     RICH2         TAG
014100940308     C*
014200940308     C* C O N T R O L L I
014300940308     C                   EXSR      CONTR1
014400940308     C   90              GOTO      EMET1
014500940215     C*
014600940216     C     SOPRA         TAG
014700940308     C**
014800940309     C* T E S T O   N O R M A L E
014900021024    1C                   if        S08FTS = *blanks
015000021024     C                   eval      *in10 = *on
015100940308     C                   MOVEL     '1'           UNO
015200940309     C                   Z-ADD     18            WRIG
015300940308     C                   ELSE
015400940309     C* T E S T O   C O M P R E S S O
015500021024     C                   eval      *in10 = *off
015600021024     C                   clear                   UNO
015700940309     C                   Z-ADD     9             WRIG              5 0
015800940308     C                   ENDIF
015900940308     C*
016000940216     C                   EXSR      REVIS
016100940308     C* 19 ON- MODIFICATO TIPO STAMPA TESTO (NORM O COMPRESSO)
016200940216     C   19              GOTO      SOPRA
016300940216     C*
016400901025     C* SE PGM RICHIAMATO ESCO SUBITO
016500921214IF  1C     FLG1          IFEQ      ' '
016600921214     C                   GOTO      RICH1
016700930105E   1C                   ENDIF
016800940301     C*
016900921214     C     FINE          TAG
017000021024     C                   eval      *inLR = *ON
017100021024      *---------------------------------------------------------------*
017200021024      *                    CONTR1                                     *
017300021024      * CONTROLLI FORMATO 1                                           *
017400021024      *---------------------------------------------------------------*
017500940308     C     CONTR1        BEGSR
017600021024      *
017700021024     C                   eval      *in90 = *off
017800060607     c                   eval      *in45 = *off
017900021024     C                   clear                   Ds1H
018000021024      *
018100021024      * SE ? RICERCA TESTO LETTERA
018200940308     C     '?'           SCAN      S08TTX                                 90
018300021024      *
018400940308    1C     *IN90         IFEQ      *ON
018500021024     C                   clear                   S08TTX
018600021024      * PROFILO UTENTE
018700021024     C                   clear                   Fnlv41ds
018800021024     C                   MOVEL     PROF          I41Pro
018900021024      * I42prv = 'G' ===> PROVENGO DA MANUTENZIONE TESTI
019000021024     C                   MOVEL     'G'           I41Prv
019100021024     C                   MOVEL     '1'           I41MOD
019200021024     C                   CALL      'FNLV41R'
019300021024     C                   PARM                    FNLV41DS
019400021024     C     O41LET        IFNE      *blanks
019500021024     C                   MOVEL     O41LET        S08TTX
019600940308     C                   GOTO      ENDCT1
019700021024E   2C                   ENDIF
019800021024E   1C                   ENDIF
019900060606
020000060606      * ricerca lingua testo lettera
020100060606     c                   if        %scan('?':vidlin) > 0
020200060606     c                   clear                   p37iso2
020300060606     c                   clear                   p37desi
020400060606     c                   call      'TRUL37R'
020500060606     c                   parm                    p37iso2
020600060606     c                   parm                    p37desi
020700060606     c                   eval      vidlin = p37iso2
020800060606     c     vidlin        chain     azlin03l
020900060607     c                   eval      *in45 = *on
021000060606     c                   eval      *in90 = *on
021100060606     c                   leavesr
021200060606     c                   endif
021300060606
021400940308     C*
021500940308     C* SE ? RICERCA TESTO LETTERA DA CUI COPIARE
021600940308     C     '?'           SCAN      COPTTX                                 90
021700940308     C*
021800940308    1C     *IN90         IFEQ      *ON
021900021024      * I42prv = ' ' ===> CARICO TUTTI I TESTI
022000021024     C                   CLEAR                   Fnlv41ds
022100021024     C                   MOVEL     '1'           I41MOD
022200021024     C                   CALL      'FNLV41R'
022300021024     C                   PARM                    FNLV41DS
022400021024    2C     O41LET        IFNE      *blanks
022500021024     C                   MOVEL     O41LET        COPTTX
022600940308     C                   GOTO      ENDCT1
022700021024E   2C                   ENDIF
022800021024E   1C                   ENDIF
022900060606
023000060606      * ricerca lingua testo lettera da cui copiare
023100060606     c                   if        %scan('?':coplin) > 0
023200060606     c                   clear                   p37iso2
023300060606     c                   clear                   p37desi
023400060606     c                   call      'TRUL37R'
023500060606     c                   parm                    p37iso2
023600060606     c                   parm                    p37desi
023700060606     c                   eval      coplin = p37iso2
023800060606     c     coplin        chain     azlin03l
023900060607     c                   eval      *in46 = *on
024000060606     c                   eval      *in90 = *on
024100060606     c                   leavesr
024200060606     c                   endif
024300940308     C*
024400940308     C* CONTROLLO SE IMMESSO TIPO TESTO DA CUI COPIARE
024500940308IF  1C     COPTTX        IFNE      *BLANKS
024600021024     C                   CLEAR                   Fnlv41ds
024700021024     C                   MOVEL     COPTTX        I41LET
024800021024     C                   MOVEL     '2'           I41MOD
024900021024     C                   CALL      'FNLV41R'
025000021024     C                   PARM                    FNLV41DS
025100021024     C                   if        O41ERR =  'E'
025200021025     C                   SETON                                        249028
025300021025     C                   Eval      VIDMSG = msg(3)
025400021024     C                   GOTO      ENDCT1
025500021024     C                   endif
025600051027     c* se testo html errore --> non gestibile da questo pgm
025700051027     C                   MOVEL     O41DTA        DS1H
025800051027     c                   if        §1html = 'S'
025900051027     C                   SETON                                        249028
026000051027     C                   Eval      VIDMSG = msg(5)
026100051027     C                   GOTO      ENDCT1
026200051027     c                   endif
026300060607      * Se sede
026400060607     c                   if        *in12
026500060607      * lingua obbligatoria
026600060607     c                   if        coplin = *blanks
026700060607     c                   eval      *in28 = *on
026800060607     c                   eval      *in46 = *on
026900060607     c                   eval      *in90 = *on
027000060607     c                   eval      vidmsg = msg(6)
027100060607     c                   leavesr
027200060607     c                   endif
027300060607      * lingua controllo
027400060607     c     coplin        chain     azlin03l
027500060607     c                   if        not %found(azlin03l)
027600060607     c                   eval      *in28 = *on
027700060607     c                   eval      *in46 = *on
027800060607     c                   eval      *in90 = *on
027900060607     c                   eval      vidmsg = msg(6)
028000060607     c                   leavesr
028100060607     c                   endif
028200060607      * se non è attiva per i testi errore
028300060607     c                   if        linfntxt = '0'
028400060607     c                   eval      *in28 = *on
028500060607     c                   eval      *in46 = *on
028600060607     c                   eval      *in90 = *on
028700060607     c                   eval      vidmsg = msg(7)
028800060607     c                   leavesr
028900060607     c                   endif
029000060607      * se ho scelto di stampare in lingua il testo deve essere gestito
029100060607      * in lingua
029200060607     c                   if        coplin <> 'it' and §1hlin = *blanks
029300060607     c                   eval      *in28 = *on
029400060607     c                   eval      *in46 = *on
029500060607     c                   eval      *in90 = *on
029600060607     c                   eval      vidmsg = msg(7)
029700060607     c                   leavesr
029800060607     c                   endif
029900060607     c                   eval      savcoplin = linfntxt
030000060607     c                   endif
030100060607     c  n12              eval      savcoplin = 'I'
030200021024E   1C                   ENDIF
030300940308     C*
030400021024      * CONTROLLO ESISTENZA TESTO IN TABELLA '1H' O FILE TESTI
030500021024     C                   CLEAR                   Fnlv41ds
030600021024     C                   MOVEL     S08TTX        I41LET
030700021024     C                   MOVEL     '2'           I41MOD
030800021024     C                   CALL      'FNLV41R'
030900021024     C                   PARM                    FNLV41DS
031000021024     C     O41ERR        IFEQ      'E'
031100021025     C                   SETON                                        219028
031200021025     C                   Eval      VIDMSG = msg(1)
031300021024     C                   GOTO      ENDCT1
031400021025     c                   ELSE
031500021024     C                   MOVEL     O41DTA        DS1H
031600051027     c* se testo html errore --> non gestibile da questo pgm
031700051027     c                   if        §1html = 'S'
031800051027     C                   SETON                                        219028
031900051027     C                   Eval      VIDMSG = msg(5)
032000051027     C                   GOTO      ENDCT1
032100051027     c                   endif
032200021024     C                   ENDIF
032300940308     C*
032400940308     C* DESCRIZIONE TIPO TESTO
032500940308     C                   MOVEL     §1HDES        S08DES
032600940308     C                   MOVEL     §1HFTS        S08FTS
032700940308     C*
032800940308     C* CONTROLLO SE PROFILO VALIDO PER LA GESTIONE DEL TESTO RICHIESTO
032900940308     C                   MOVEL     '1I'          COD
033000940308     C                   MOVEL     *BLANKS       KEY
033100940308     C                   MOVEL     §1HFGT        KEY
033200021024     C     KTAB          CHAIN     TABEL                              31
033300940308     C  N31TBLFLG        IFEQ      ' '
033400940308     C                   MOVEL     TBLUNI        DS1I
033500940308     C*
033600940308     C     '999'         LOOKUP    SKI                                    35
033700940308     C  N35PROF          LOOKUP    SKI                                    35
033800021024     C  N35              eval      *in31 = *on
033900940308     C                   ELSE
034000021024     C                   eval      *in31 = *on
034100940308     C                   ENDIF
034200940308     C*
034300021025     C   31              SETON                                        429028
034400021025     C   31              Eval      vidmsg = msg(2)
034500940308     C   31              GOTO      ENDCT1
034600940308     C*
034700021024     C                   eval      *in03 = *off
034800021024      * sono in un p.o. e
034900021024      * TESTO GESTIBILE SOLO IN SEDE: CAMPI PROTETTI + CMD6 INIBITO
035000021024      * sono in sede e
035100021024      * TESTO GESTIBILE SOLO NEI P.O.: CAMPI PROTETTI + CMD6 INIBITO
035200021024     C                   IF        (simfel <> *zeros and §1HFGE = 'S')  or
035300021024     C                             (simfel = *zeros and §1HFGE = 'F')
035400021024     C                   eval       *in03 = *on
035500940308E   1C                   ENDIF
035600021024      *
035700021024      * SE TESTO NON GESTIBILE NON POSSO COPIARE
035800021024     C                   IF        *in03 = *on  and copttx <> *blanks
035900021024     C                   eval       *in40 = *on
036000021024     C                   eval       *in90 = *on
036100021025     C                   eval       *in28 = *on
036200021025     c                   Eval      vidmsg = msg(4)
036300940308     C                   GOTO      ENDCT1
036400940308     C                   ENDIF
036500060607      * lingua obbligatoria
036600060607     c                   if        vidlin = *blanks
036700060607     c                   eval      *in28 = *on
036800060607     c                   eval      *in45 = *on
036900060607     c                   eval      *in90 = *on
037000060607     c                   eval      vidmsg = msg(6)
037100060607     c                   leavesr
037200060607     c                   endif
037300060607      * lingua controllo
037400060607     c     vidlin        chain     azlin03l
037500060607     c                   if        not %found(azlin03l)
037600060607     c                   eval      *in28 = *on
037700060607     c                   eval      *in45 = *on
037800060607     c                   eval      *in90 = *on
037900060607     c                   eval      vidmsg = msg(6)
038000060607     c                   leavesr
038100060607     c                   endif
038200060607      * se non è attiva per i testi errore
038300060607     c                   if        linfntxt = '0'
038400060607     c                   eval      *in28 = *on
038500060607     c                   eval      *in45 = *on
038600060607     c                   eval      *in90 = *on
038700060607     c                   eval      vidmsg = msg(7)
038800060607     c                   leavesr
038900060607     c                   endif
039000060607      * se ho scelto di stampare in lingua il testo deve essere gestito
039100060607      * in lingua
039200060607     c                   if        vidlin <> 'it' and §1hlin = *blanks
039300060607     c                   eval      *in28 = *on
039400060607     c                   eval      *in45 = *on
039500060607     c                   eval      *in90 = *on
039600060607     c                   eval      vidmsg = msg(7)
039700060607     c                   leavesr
039800060607     c                   endif
039900060607     c                   eval      savvidlin = linfntxt
040000940308     C*
040100940308     C     ENDCT1        ENDSR
040200940215     C*
040300921214     C*---------------------------------------------------------------*
040400921214     C*                    REVIS                                      *
040500921214     C* ROUTINE DI EMISSIONE SFILE E AGGIORNAMENTO MODIFICHE TESTO    *
040600921214     C*---------------------------------------------------------------*
040700921214     C     REVIS         BEGSR
040800021024      *
040900021024      * SE ESISTE IL TESTO LO CARICO ALTRIMENTI LO EMETTO VUOTO
041000930107     C                   EXSR      CARICA
041100021024      * eseguo COPIA TESTO se ho caricato il testo "base"
041200021024      * e se il testo da cui copiare è pieno e non annullato
041300921214IF  1C   22COPTTX        IFNE      *BLANKS
041400930107     C                   EXSR      CARIC2
041500921214X   1C                   ELSE
041600021024      *   SE NON VOGLIO COPIARE
041700940308     C                   Z-ADD     1             WDA               5 0
041800930104     C                   EXSR      AZZTES
041900930105E   1C                   ENDIF
042000021024      *
042100930104     C                   Z-ADD     1             RT
042200021024      * EMISSIONE SFILE
042300921214     C     EMET2         TAG
042400941215     C                   WRITE     LV40Z04
042500941215     C  N13              WRITE     LV40Z05
042600941215     C   13              WRITE     LV40Z06
042700941215     C  N10              EXFMT     LV40C02
042800941215     C   10              EXFMT     LV40C03
042900021024      *
043000021024      * CMD12 - FINE LAVORO
043100021024     C                   if        *inKL = *on
043200021024     C                   EXSR      CTRF12
043300021024     C                   GOTO      FINR
043400021024     c                   endif
043500021024      *
043600021024      * ROLLUP
043700921214IF  1C     *IN04         IFEQ      '1'
043800940308     C                   SETON                                        01
043900870610     C                   Z-ADD     UT            RT
044000940308     C                   Z-ADD     1             WDA               5 0
044100870610     C                   EXSR      AZZTES
044200940308     C     UT            SUB       WRIG          RT
044300940308     C                   ADD       1             RT
044400901213     C                   GOTO      EMET2
044500930105E   1C                   ENDIF
044600930108     C* INSERIMENTO RIGA
044700021024     c                   if        *inKI = *on
044800021024     C  N10              EXSR      CTRIMM
044900021024     C   10              EXSR      CTRIM2
045000021024     C                   endif
045100870610     C*
045200930108     C* CONTROLLO DATI II FORMATO VIDEO
045300930104     C                   EXSR      CONTR
045400021025
045500021025      * Se F6 devo controllare che la descrizione del testo
045600021025      * sia stata inserita
045700021025     c                   If        *InKf and s08des = *blanks
045800021025     c                   Eval      *In90 = *On
045900021025     c                   Eval      *In44 = *On
046000021025     c                   Eval      *InKf = *Off
046100021025     c                   EndIf
046200930108     C* SE ERRORI
046300930107     C   90              GOTO      EMET2
046400921214     C*
046500940222     C* CMD6  - AGGIORNAMENTO TESTO
046600940216     C   19
046700940216     COR KF              EXSR      AGGIO
046800930105     C*
046900940308     C* CMD14 - OPZIONI STAMPA TESTO
047000021024     C                   if        *INKN = *ON
047100940222     C                   MOVEL     S08TTX        PA3TTX
047200940222     C                   MOVEL     PARAM3        KPJBU
047300941215     C                   CALL      'FNLV42R'
047400940222     C                   PARM                    KPJBA
047500940222     C                   ENDIF
047600940222     C*
047700921214     C* CMD16 - ANNULLAMENTO TESTO
047800930104     C   KQ              EXSR      ANNUL
047900940308     C*
048000940308     C* ENTER - RIMANE NELLA SECONDA VIDEATA
048100940308     C     *INKF         IFEQ      '0'
048200940308     C     *INKQ         ANDEQ     '0'
048300940308     C     *IN19         ANDEQ     '0'
048400940308     C     *IN90         OREQ      *ON
048500940308     C                   GOTO      EMET2
048600940308E   1C                   ENDIF
048700921214     C*
048800921214     C     FINR          ENDSR
048900921214     C*---------------------------------------------------------------*
049000921214     C*                    AZZTES                                     *
049100921214     C* ROUTINE DI CARICAMENTO SFILE VUOTO PER INSERIMENTO -TESTATA-  *
049200921214     C*---------------------------------------------------------------*
049300870610     C     AZZTES        BEGSR
049400021024      *
049500940309     C  N10              CLEAR                   WRIGA
049600940309     C   10              CLEAR                   WRIGA2
049700930406     C*
049800940308DO  1C     WDA           DO        WRIG
049900930406     C                   ADD       1             RT
050000930406     C*
050100941215     C  N10              WRITE     LV40S02
050200941215     C   10              WRITE     LV40S03
050300021024     C                   eval      *in01 = *off
050400930406E   1C                   ENDDO
050500930406     C                   Z-ADD     RT            UT                4 0
050600930406     C*
050700870610     C                   ENDSR
050800921214     C*---------------------------------------------------------------*
050900921214     C*                    CARICA                                     *
051000921214     C* ROUTINE DI CARICAMENTO SFILE PER MODIFICA TESTO LETTERE       *
051100921214     C*---------------------------------------------------------------*
051200870610     C     CARICA        BEGSR
051300921214     C*
051400021024     C                   SETOFF                                       2223
051500021024     C                   SETOFF                                       2719
051600940308     C* PULIZIA SFILE
051700940308     C                   SETON                                        7201
051800941215     C  N10              WRITE     LV40C02
051900941215     C   10              WRITE     LV40C03
052000940308     C                   SETOFF                                       72
052100940308     C*
052200940308     C                   Z-ADD     0             RT                4 0
052300021024      *
052400930107     C                   MOVEL     S08TTX        COMTTX
052500060607     c                   move      savvidlin     comlin
052600021024      * CARICO TESTO
052700021024     C                   move      '0'           $Copia            1
052800940308     C                   EXSR      CARTES
052900021024      *
053000021024      * MODIFICA (22-OFF)  INSERIMENTO (22-ON)
053100021024    1C                   if        RT = *zeros
053200021024     C                   eval      *in22 = *on
053300021024   x1C                   ELSE
053400021024      * ABILITO CMD16-ANNULLAMENTO TESTO  SOLO SE NON RICHIAMATO
053500021024     c                   If         *in13 = *off  and  *in03 = *off
053600021024     C                   eval      *in27 = *on
053700021024     c                   Endif
053800021024      *
053900940308     C     RT            DIV       WRIG          WNUM              5 0
054000940308     C                   MVR                     WDA
054100940308     C                   ADD       1             WDA
054200940308     C                   EXSR      AZZTES
054300021024    1C                   ENDIF
054400930107     C*
054500870610     C                   Z-ADD     RT            UT
054600940308     C* MI SALVO FLAG DI COMPRESSO
054700940308     C                   MOVEL     S08FTS        SAVFTS            1
054800930406     C*
054900870610     C                   ENDSR
055000930107     C*---------------------------------------------------------------*
055100930107     C*                    CARIC2                                     *
055200930107     C* ROUTINE DI CARICAMENTO SFILE PER TESTO DA CUI COPIARE         *
055300930107     C*---------------------------------------------------------------*
055400930107     C     CARIC2        BEGSR
055500930107     C*
055600021024     c                   eval      *in23 = *off
055700021024     c                   eval      *in27 = *off
055800930108     C*
055900930107     C                   MOVEL     COPTTX        COMTTX
056000060607     c                   move      savcoplin     comlin
056100940308     C* CARICO TESTO
056200021024      *
056300021024     C                   move      '1'           $Copia            1
056400940308     C                   EXSR      CARTES
056500021024      *
056600021024      * SE TESTO DA CUI COPIARE ANNULLATO, EMETTO SFILE VUOTO
056700930107IF  1C     RT            IFEQ      0
056800940308     C                   Z-ADD     1             WDA               5 0
056900930107     C                   EXSR      AZZTES
057000930107E   1C                   ENDIF
057100021024      *
057200930107     C                   Z-ADD     RT            UT
057300021024      *
057400930107     C                   ENDSR
057500021024      *---------------------------------------------------------------*
057600021024      *                    CARTES                                     *
057700021024      * CARICO TESTO IN SUBFILE                                       *
057800021024      *---------------------------------------------------------------*
057900940308     C     CARTES        BEGSR
058000021024      *
058100021024      * Verifico quale testo devo caricare: se esiste FnTxt per simfel
058200021024      * uso questo altrimenti cerco testo sede
058300021024     C                   move      simfel        $po
058400021024     C     Ke2Txt        setll     FnTxt01l                               30
058500021024     C                   IF        *in30 = *off and simfel <> *zeros
058600021024     C                   move      *zeros        $po
058700021024     C     Ke2Txt        setll     FnTxt01l                               30
058800021024     C                   endif
058900021024      *
059000021024      * Se stò copiando da altro testo o sono in un p.o. e ho caricato
059100021024      *  il testo master (TXTPOL = 0) pulisco la descrizione a video
059200021024     C                   IF        $Copia = '1' or
059300021024     C                              ($po = *zeros  and  simfel <> *zeros)
059400021024     C                   eval      S08Des = *blanks
059500021024     C                   ENDIF
059600021024      *
059700021024      * Se ho trovato un record elaboro
059800021024    1c                   IF        *IN30 = *on
059900021024      *
060000021024      * N.B.: LE DUE RIGHE DI FONDO PAGINA (TIPO 'F') SONO NUMERATE
060100021024      *       AL CONTRARIO, PERCIO' DEVO LEGGERLE SEPARATAMENTE
060200940908     C                   Z-ADD     2             TXTNRR
060300021024     C     KE1TXT        CHAIN(N)  FNTXT01L                           23
060400021024    2C                   IF        *in23 = *off  and TXTatb = *blanks
060500940908     C                   ADD       1             RT
060600940908     C                   MOVEL     TXTRIB        S08RIB
060700940908     C                   MOVEL     TXTRIG        S08CR1
060800940908     C  N10              MOVE      TXTRIG        S08CR2
060900940908     C                   MOVE      TXTPOS        S08LET
061000941215     C  N10              WRITE     LV40S02
061100941215     C   10              WRITE     LV40S03
061200021024    2C                   ENDIF
061300021024      *
061400940908     C                   Z-ADD     1             TXTNRR
061500021024     C     KE1TXT        CHAIN(N)  FNTXT01L                           23
061600021024    2C                   IF        *in23 = *off  and TXTatb = *blanks
061700940908     C                   ADD       1             RT
061800940908     C                   MOVEL     TXTRIB        S08RIB
061900940908     C                   MOVEL     TXTRIG        S08CR1
062000940908     C  N10              MOVE      TXTRIG        S08CR2
062100940908     C                   MOVE      TXTPOS        S08LET
062200941215     C  N10              WRITE     LV40S02
062300941215     C   10              WRITE     LV40S03
062400021024    2C                   ENDIF
062500021024      *
062600021024      * QUINDI LEGGO IL RESTO DELLA LETTERA DALLA RIGA 3 IN POI
062700940908     C                   Z-ADD     3             TXTNRR
062800021024     C     KE1TXT        SETLL     FNTXT01L
062900021024     C     KEYTXT        READE(N)  FNTXT01L                               23
063000021024    2C     *IN23         DOWEQ     '0'
063100021024      *
063200021024      * CARICO SOLAMENTE LE RIGHE NON ANNULLATE
063300021024    3C     TXTATB        IFEQ      *BLANKS
063400940308     C                   ADD       1             RT
063500940908     C                   MOVEL     TXTRIB        S08RIB
063600940908     C                   MOVEL     TXTRIG        S08CR1
063700940908     C  N10              MOVE      TXTRIG        S08CR2
063800940908     C                   MOVE      TXTPOS        S08LET
063900941215     C  N10              WRITE     LV40S02
064000941215     C   10              WRITE     LV40S03
064100021024    3C                   ENDIF
064200940308     C*
064300021024     C     KEYTXT        READE(N)  FNTXT01L                               23
064400021024    2C                   ENDDO
064500021024      *
064600021024    1C                   ENDIF
064700021024E   1 *
064800940308     C                   ENDSR
064900021024      *---------------------------------------------------------------*
065000021024      *                    CTRF12                                     *
065100021024      * se per Cod. Testo + simfel ESISTE solo RECORD H lo DELETO     *
065200021024      * E' il caso in cui entro in immissione, richiamo FNLV42R per   *
065300021024      *  impostare i parametri (quindi write del rcd "H"), e poi esco *
065400021024      *  da FNLV40R senza salvare                                     *
065500021024      *---------------------------------------------------------------*
065600021024     C     CTRF12        BEGSR
065700021024      *
065800021024     c                   eval      *in30 = *off
065900021024     C                   MOVEL     S08TTX        COMTTX
066000021024     C                   MOVEL     simfel        $po
066100021024      *
066200021024     C     KE2TXT        setll     FnTXT01l
066300021024     C     KE2TXT        reade(N)  FnTXT01l
066400021024     c                   dow       not %EOF(FNTXT01L)
066500021024     c                   IF        TXTPOS <> 'H'
066600021024     c                   eval      *in30 = *on
066700021024     c                   leave
066800021024     c                   ENDIF
066900021024     C     KE2TXT        reade(N)  FnTXT01l
067000021024     c                   enddo
067100021024      *
067200021024     C                   IF        *in30 = *off
067300021025     C     KE2TXT        delete    FnTxt000
067400021024     c                   ENDIF
067500021024      *
067600021024     C                   ENDSR
067700921214     C*---------------------------------------------------------------*
067800921214     C*                    CTRIMM                                     *
067900930108     C* ROUTINE DI CONTROLLO INSERIMENTO 'I' DI "IMMISSIONE RIGA"     *
068000940308     C*                   PER TESTO COMPRESSO
068100921214     C*---------------------------------------------------------------*
068200901213     C     CTRIMM        BEGSR
068300940309     C*
068400940309     C                   MOVE      NRG           R$$
068500940222     C*
068600940309     C     RIRI          IFGE      4
068700940309     C     RIRI          ANDLE     21
068800940309     C*
068900940222     C                   Z-ADD     0             $RIG              5 0
069000940222     C     RIRI          SUB       4             $RIG
069100940222     C     $RIG          DIV       2             $RIG
069200940222     C                   ADD       PRIMA         $RIG
069300940222     C*
069400940218     C* SE VIENE PREMUTO IL CMD9 AL DI FUORI DEL SFL NON FACCIO NULLA
069500940309     C     $RIG          IFGE      0
069600921214     C*
069700901213     C                   ADD       1             UT
069800901213     C                   Z-ADD     UT            RT
069900940309     C                   CLEAR                   WRIGA
070000941215     C                   WRITE     LV40S02
070100901213     C                   SUB       1             RT
070200901213     C*
070300021024DO  1C     RT            DOWGT     $RIG
070400941215     C     RT            CHAIN     LV40S02                            23
070500940218DO  2C  N23              DO
070600940308     C                   MOVEL     WRIGA         COMRIG          112
070700901213     C                   ADD       1             RT
070800941215     C     RT            CHAIN     LV40S02                            23
070900940218DO  3C  N23              DO
071000940308     C                   MOVEL     COMRIG        WRIGA
071100941215     C                   UPDATE    LV40S02
071200940218E   3C                   ENDDO
071300940218E   2C                   ENDDO
071400901213     C*
071500901213     C                   SUB       2             RT
071600940218E   1C                   ENDDO
071700901213     C*
071800940222     C                   ADD       1             RT
071900941215     C     RT            CHAIN     LV40S02                            23
072000940309     C                   CLEAR                   WRIGA
072100941215     C  N23              UPDATE    LV40S02
072200940309     C*
072300940309     C                   Z-ADD     RT            PRIMA
072400940309     C                   ENDIF
072500940309     C                   ENDIF
072600901213     C*
072700901213     C                   ENDSR
072800940308     C*---------------------------------------------------------------*
072900940308     C*                    CTRIM2                                     *
073000940308     C* ROUTINE DI CONTROLLO INSERIMENTO 'I' DI "IMMISSIONE RIGA"     *
073100940308     C*                   PER TESTO NORMALE
073200940308     C*---------------------------------------------------------------*
073300940308     C     CTRIM2        BEGSR
073400940309     C*
073500940309     C* MUOVO IL NUMERO DELLA RIGA DOVE E' POSIZIONATO IL CURSORE
073600940309     C*   IN R$$ PER TRASFORMARLO DA ESADECIMALE IN BINARIO
073700940309     C                   MOVE      NRG           R$$
073800940309     C*
073900940309     C     RIRI          IFGE      4
074000940309     C     RIRI          ANDLE     21
074100940308     C*
074200940308     C* PRIMA = NUMERO RELATIVO DI RECORD DELLA PRIMA RIGA DEL SFL
074300940308     C*         DELLA PAGINA VISUALIZZATA
074400940308     C                   Z-ADD     PRIMA         $RIG              5 0
074500940308     C* AGGIUNGENDO IL NUMERO DELLA RIGA DOVE E' POSIZIONATO IL CURSORE
074600940308     C*   AL NUMERO DELLA PRIMA RIGA DEL SFL TROVO IL NUMERO DELLA
074700940308     C*   RIGA VUOTA DA INSERIRE
074800940308     C                   ADD       RIRI          $RIG
074900940308     C* SOTTRAGGO TUTTE LE RIGHE CHE SEPARANO IL SFL DALLA PRIMA RIGA
075000940308     C*   DELLO SCHERMO (COMPRESA LA PRIMA RIGA DEL SFL)
075100940308     C                   SUB       4             $RIG
075200940308     C* SE VIENE PREMUTO IL CMD9 AL DI FUORI DEL SFL NON FACCIO NULLA
075300940309     C     $RIG          IFGE      0
075400940308     C*
075500940308     C                   ADD       1             UT
075600940308     C                   Z-ADD     UT            RT
075700940309     C                   CLEAR                   WRIGA2
075800941215     C                   WRITE     LV40S03
075900940308     C                   SUB       1             RT
076000940308     C*
076100940308DO  1C     RT            DOWGT     $RIG
076200941215     C     RT            CHAIN     LV40S03                            23
076300940308DO  2C  N23              DO
076400940308     C                   MOVEL     WRIGA2        COMRIG
076500940308     C                   ADD       1             RT
076600941215     C     RT            CHAIN     LV40S03                            23
076700940308DO  3C  N23              DO
076800940308     C                   MOVEL     COMRIG        WRIGA2
076900941215     C                   UPDATE    LV40S03
077000940308E   3C                   ENDDO
077100940308E   2C                   ENDDO
077200940308     C*
077300940308     C                   SUB       2             RT
077400940308E   1C                   ENDDO
077500940308     C*
077600940308     C                   ADD       1             RT
077700941215     C     RT            CHAIN     LV40S03                            23
077800940309     C                   CLEAR                   WRIGA2
077900941215     C  N23              UPDATE    LV40S03
078000940309     C*
078100940309     C                   Z-ADD     RT            PRIMA
078200940309     C                   ENDIF
078300940309     C                   ENDIF
078400940308     C*
078500940308     C                   ENDSR
078600921214     C*---------------------------------------------------------------*
078700921214     C*                    CONTR                                      *
078800921214     C* ROUTINE DI CONTROLLO TESTO LETTERA                            *
078900921214     C*---------------------------------------------------------------*
079000901026     C     CONTR         BEGSR
079100021024      *
079200940308     C                   SETOFF                                       9001
079300940309     C                   Z-ADD     PRIMA         SAVPRI
079400940216     C*
079500901026     C                   Z-ADD     0             CONT
079600901026     C                   Z-ADD     1             RT
079700941215     C  N10RT            CHAIN     LV40S02                            77
079800941215     C   10RT            CHAIN     LV40S03                            77
079900901026     C*
080000921214DO  1C  N77              DO        *HIVAL
080100921214     C*
080200921214IF  2C     S08LET        IFEQ      'F'
080300901026     C                   ADD       1             CONT              1 0
080400930105E   2C                   ENDIF
080500921214     C*
080600901026     C* SONO AMMESSE SOLO 2 RIGHE IN CALCE
080700921214IF  2C     CONT          IFGT      2
080800901026     C                   SETON                                        9040
080900901026     C                   GOTO      ENDCTR
081000930105E   2C                   ENDIF
081100921214     C*
081200921214     C* CONTROLLO SE IMMESSO RIGA LETTERA E SE IMMESSO TIPO DI RIGA
081300940308IF  2C  N10S08LET        IFEQ      *BLANKS
081400940308IF  3C     RIGAT         ANDNE     *BLANKS
081500901026     C                   SETON                                        9041
081600901026     C                   GOTO      ENDCTR
081700930105E   2C                   ENDIF
081800940308     C*
081900940308     C* CONTROLLO SE IMMESSO RIGA LETTERA E SE IMMESSO TIPO DI RIGA
082000940308IF  2C   10S08LET        IFEQ      *BLANKS
082100940308     C     S08CR1        ANDNE     *BLANKS
082200940308     C                   SETON                                        9041
082300940308     C                   GOTO      ENDCTR
082400940308E   2C                   ENDIF
082500940308     C*
082600941215     C  N10              UPDATE    LV40S02
082700941215     C   10              UPDATE    LV40S03
082800901026     C*
082900901026     C                   ADD       1             RT
083000941215     C  N10RT            CHAIN     LV40S02                            77
083100941215     C   10RT            CHAIN     LV40S03                            77
083200921214     C*
083300901026     C     ENDCTR        TAG
083400930105     C  N90
083500930105E   1CANN77              ENDDO
083600940308     C* UPDATO PER ERRORE
083700941215     C   90
083800941215     CANN10              UPDATE    LV40S02
083900941215     C   90
084000941215     CAN 10              UPDATE    LV40S03
084100921214     C*
084200901026     C* SE NESSUN ERRORE RIEMETTO DALLA PRIMA VIDEATA
084300940309     C  N90              Z-ADD     SAVPRI        RT
084400940308     C*
084500940308     C* CONTROLLO SE CAMBIATO TIPO STAMPA TESTO(NORMALE O COMPRESSO)
084600940308     C  N90S08FTS        IFNE      SAVFTS
084700940308     C*
084800940308     C     UNO           IFEQ      ' '
084900940308     C                   SETON                                        4390
085000940308     C                   MOVEL     '1'           UNO
085100940308     C                   GOTO      ENDCON
085200940308     C                   ENDIF
085300940308     C*
085400940308     C                   MOVEL     S08FTS        SAVFTS
085500940308     C                   SETON                                        19
085600940308     C                   ENDIF
085700901026     C*
085800940216     C     ENDCON        ENDSR
085900921214     C*---------------------------------------------------------------*
086000921214     C*                    AGGIO                                      *
086100021024     C* ROUTINE DI AGGIORNAMENTO FILE TESTI (FNTXT00F)                *
086200921214     C*---------------------------------------------------------------*
086300870610     C     AGGIO         BEGSR
086400870610     C*
086500021024     C                   eval      *in05 = *off
086600021024     C                   eval      *in77 = *off
086700021024     c                   clear                   dfntxth
086800021024     C                   clear                   ALF1              1
086900921214     C*
087000921215     C                   Z-ADD     3             NRR               3 0
087100921215     C                   Z-ADD     2             CONT
087200921215     C                   Z-ADD     1             RT
087300941215     C  N10RT            CHAIN     LV40S02                            77
087400941215     C   10RT            CHAIN     LV40S03                            77
087500021024      *
087600021024      * Cancello il testo dal file
087700021024      *  Imposto flag se trovato almeno un rcd
087800021024      *  Salvo dati riga "H"
087900901212     C                   MOVEL     S08TTX        COMTTX
088000021024     C                   MOVEL     simfel        $po
088100060607     c                   move      savvidlin     comlin
088200021024     C     KEYTXT        SETLL     FNTXT01L
088300021024     C     KEYTXT        READE     FNTXT01L                               23
088400021024      *
088500930107DO  1C     *IN23         DOWEQ     '0'
088600930107     C                   MOVEL     'S'           ALF1
088700021024     c                   if        txtpos = 'H'
088800021024     c                   movel     txtrig        dfntxth
088900021024     c                   endif
089000021024     C                   DELETE    FNTXT000
089100021024     C     KEYTXT        READE     FNTXT01L                               23
089200930107E   1C                   ENDDO
089300021024      *
089400021024      * IMPOSTO CAMPI STANDARD PER SCRITTURA TXT
089500021024     C                   z-add     simfel        TXTPOL
089600021024     C                   clear                   TXTRIG
089700940308     C                   MOVE      S08TTX        TXTLET
089800060607     C**!!!              MOVE      S08LIN        TXTLIN
089900060607     c                   move      comlin        txtlin
090000021024     C                   clear                   TXTFTR
090100021024     C                   clear                   TXTDTR
090200021024     C                   MOVEL     000           TXTFLT
090300940308     C                   MOVEL     §1HFTT        TXTFTT
090400021024     C                   clear                   TXTATB
090500021024      *
090600021024      * LETTURA SFL per scrivere file testi
090700021024      * accendo IN05 se scrivo almeno un record
090800921214DO  1C     *IN77         DOWEQ     '0'
090900921214     C*
091000021024    2c                   if        S08LET <> 'A' and S08LET <> *blanks
091100870611     C                   MOVE      S08LET        TXTPOS
091200921214     C*
091300921214     C* LE RIGHE IN CALCE LE METTO NELLE PRIME DUE POSIZIONI
091400021024IF  3C                   if        S08LET = 'F'
091500901026     C                   Z-ADD     CONT          TXTNRR
091600901026     C                   SUB       1             CONT
091700021024X   3C                   else
091800901026     C                   Z-ADD     NRR           TXTNRR
091900901026     C                   ADD       1             NRR
092000021024E   3C                   endif
092100901026     C*
092200940308     C                   MOVEL     S08CR1        TXTRIG
092300940308     C  N10              MOVE      S08CR2        TXTRIG
092400870610     C                   MOVEL     S08RIB        TXTRIB
092500021024     C                   eval      *in05 = *on
092600021024     C                   WRITE     FNTXT000
092700930105E   2C                   ENDIF
092800910424     C*
092900870610     C                   ADD       1             RT
093000941215     C  N10RT            CHAIN     LV40S02                            77
093100941215     C   10RT            CHAIN     LV40S03                            77
093200930105E   1C                   ENDDO
093300021024      *
093400021024     C                   SELECT
093500021024      * Se sono in sede e il testo è da trasmettere
093600021024      *  quando viene annullato interamente scrivo un record
093700021024      *  annullato per la trasmissione
093800021024     C                   WHEN      SIMFEL = *zeros and §1HFTT <> *blanks
093900021024     c                             and ALF1 = 'S' and *in05 = *off
094000021024     C                   movel     'C'           TXTPOS
094100021024     C                   z-add     3             TXTNRR
094200021024     C                   movel     'A'           TXTATB
094300021024     C                   clear                   TXTRIB
094400021024     C                   clear                   TXTRIG
094500021025     C                   WRITE     FNTXT000
094600021024      * Se non annullo e ho scritto almeno un record scrivo la riga "H"
094700021025      *  Impostando i parametri se non esisteva
094800021024     C                   WHEN      *in05 = *on
094900021025     C                   clear                   TXTNRR
095000021024     C                   clear                   TXTRIB
095100021024     C                   movel     'H'           TXTPOS
095200021025     c                   If        §TXDES = *blanks
095300021025     c                   movel     §1hPva        §txPva
095400021025     c                   movel     §1HFv1        §TxSip
095500021025     c                   movel     §1HFv2        §TxSic
095600021025     c                   movel     §1HFv3        §TxScc
095700021025     c                   movel     §1HFv4        §TxSca
095800021025     c                   movel     §1HFv5        §TxStc
095900021025     c                   movel     §1HFv6        §TxSdc
096000021025     c                   movel     §1HFv7        §TxSpf
096100021025     c                   movel     §1HFv8        §TxSnp
096200021025     c                   movel     §1HFv9        §TxSri
096300021025     c                   Endif
096400021025     c                   movel     S08Fts        §txFts
096500021025     C                   movel     S08des        §txdes
096600021025     C                   movel(P)  DfntxtH       TXTRIG
096700021025     C                   WRITE     FNTXT000
096800021024     c                   ENDSL
096900021024      *
097000021024      *
097100870610     C                   ENDSR
097200921214     C*---------------------------------------------------------------*
097300921214     C*                    ANNUL                                      *
097400921214     C* ROUTINE DI ANNULLAMENTO TESTO                                 *
097500921214     C*---------------------------------------------------------------*
097600921214     C     ANNUL         BEGSR
097700921214     C*
097800930105     C* ANNULLO TUTTE LE RIGHE DEL TESTO
097900921215     C*
098000921215     C                   MOVEL     S08TTX        COMTTX
098100021024     C                   MOVEL     simfel        $po
098200060607     c                   move      savvidlin     comlin
098300021024     C     KEYTXT        SETLL     FNTXT01L
098400021024     C     KEYTXT        READE     FNTXT01L                               77
098500921215     C*
098600930107DO  1C     *IN77         DOWEQ     '0'
098700021024     C                   DELETE    FNTXT000
098800021024     C     KEYTXT        READE     FNTXT01L                               77
098900930105E   1C                   ENDDO
099000021024     C* Se sono in sede
099100930107     C* SCRIVO UNA RIGA VUOTA DA TRASMETTERE
099200021024IF  1C                   if        simfel = *zeros and §1HFTT <> *blanks
099300930107     C                   MOVE      S08TTX        TXTLET
099400930107     C                   MOVEL     'C'           TXTPOS
099500930107     C                   Z-ADD     3             TXTNRR
099600930107     C                   MOVEL     *BLANKS       TXTRIB
099700930107     C                   MOVEL     *BLANKS       TXTRIG
099800060607     C**!!!              MOVE      S08LIN        TXTLIN
099900060607     c                   move      comlin        txtlin
100000930107     C                   MOVEL     §1HFTT        TXTFTT
100100930402     C*
100200021024     C                   MOVEL     000           TXTFLT
100300930107     C                   MOVEL     *BLANKS       TXTFTR
100400930107     C                   Z-ADD     *ZEROS        TXTDTR
100500930107     C                   MOVEL     'A'           TXTATB
100600930402     C*
100700021024     C                   WRITE     FNTXT000
100800930402E   1C                   ENDIF
100900921215     C*
101000921214     C                   ENDSR
101100921214     C*---------------------------------------------------------------*
101200921214     C*                    INIZIO                                     *
101300921214     C* ROUTINE DI DEFINIZIONE KLIST E VARIABILI                      *
101400921214     C*---------------------------------------------------------------*
101500921214     C     INIZIO        BEGSR
101600921214     C*
101700921214     C     *ENTRY        PLIST
101800921214     C                   PARM                    KPJBA
101900921214     C                   MOVEL     KPJBU         PARAM1
102000921214     C*
102100940218     C                   Z-ADD     0             RIRI
102200940308     C                   MOVE      TES(1)        VIDTES
102300921214     C*
102400921214     C                   Z-ADD     1             CODUT
102500921214     C                   CALL      'X§PARUT'
102600921214     C                   PARM                    UT§DSE
102700921214     C                   MOVEL     REC80         CNCR80
102800921214     C                   MOVEL     RAGUT         DESDIT
102900060607      * controllo se sono in sede
103000060607     c                   eval      *in12 = (simfel = 0)
103100060912      * ? CHIODO!!!!! ?
103200060912      * ? accendo l'indicatore 15 in modo da non gestire la lingua nei testi
103300060912     c                   eval      *in15 = *on
103400921214     C*
103500921214     C* KLIST RICERCA TESTO LETTERA
103600921214     C     KEYTXT        KLIST
103700021024     C                   KFLD                    COMTTX
103800021024     C                   KFLD                    $PO
103900060607     C**!!!              KFLD                    S08LIN
104000060607     c                   kfld                    comlin
104100940908     C     KE1TXT        KLIST
104200021024     C                   KFLD                    COMTTX
104300021024     C                   KFLD                    $PO
104400060607     C**!!!              KFLD                    S08LIN
104500060607     c                   kfld                    comlin
104600940908     C                   KFLD                    TXTNRR
104700021024     C     KE2TXT        KLIST
104800021024     C                   KFLD                    COMTTX
104900021024     C                   KFLD                    $PO
105000060607     c                   kfld                    comlin
105100021024     C     KEYTXT2       KLIST
105200021024     C                   KFLD                    COMTTX            2
105300021024     C                   KFLD                    $PO
105400060607     C**!!!              KFLD                    S08LIN
105500060607     c                   kfld                    comlin
105600021024     C                   KFLD                    $TPORIG           1
105700921214     C*
105800921214     C* AGGANCIO RECORD TABELLA
105900921214     C     KTAB          KLIST
106000921214     C                   KFLD                    CODUT
106100921214     C                   KFLD                    COD               2
106200921214     C                   KFLD                    KEY               8
106300921214     C*
106400921214     C* DEFINIZIOMNE VARIABILI
106500921214     C     *LIKE         DEFINE    TBLKUT        §KUT
106600921214     C     *LIKE         DEFINE    TBLCOD        §COD
106700921214     C     *LIKE         DEFINE    TBLKEY        §KEY
106800940309     C     *LIKE         DEFINE    PRIMA         SAVPRI
106900921214     C*
107000921214     C                   ENDSR
107100930108     C*---------------------------------------------------------------*
107200940302**
107300940302 ***   GESTIONE  TESTI  LETTERE   ***
107400021025**  MSG
107500021025Tipo di Testo Errato Mancante o Annullato                                     1
107600021025Profilo non abilitato alla gestione del Testo richiesto !!                    2
107700021025Tipo Testo da cui copiare errato                                              3
107800021025Copia testo non ammessa se testo non gestibile                                4
107900051027Tipo di testo "HTML" non utilizzabile
108000060607Lingua errata                                                                 6
108100060607Testo non gestito in lingua                                                   7
