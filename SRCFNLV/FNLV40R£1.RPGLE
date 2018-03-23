000100021024      * FNLV40R  *----------------------------------------------------*
000200021024      *------------------*                                            *
000300021024      *        PROGRAMMA INTERATTIVO DI REVISIONE/INSERIMENTO         *
000400021024      *           TESTI LETTERE    TIPO STAMPA: COMPRESSO             *
000500021024      *---------------------------------------------------------------*
000600930108     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000700021024     FFNTXT01L  UF A E           K DISK
000800021024     FTABEL00F  IF   E           K DISK
000900021024     FFNLV40D   CF   E             WORKSTN INFDS(LV40DS)
001000941215     F                                     SFILE(LV40S02:RT)
001100941215     F                                     SFILE(LV40S03:RT)
001200021024      *-----------------------------------------------------------------
001300940308     D TES             S             37    DIM(1) CTDATA PERRCD(1)
001400021025     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001500021024      *
001600930406     D DS1H          E DS
001700021024     D DFNTXTH       E DS
001800021024     D FNLV41DS      E DS
001900021024     D CNCR80        E DS
002000021024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100940209     D DS1I          E DS
002200021024     D  SKI                   26     88    DIM(21)
002300921214     D KPJBA         E DS
002400940208     D  PROF                   1      3
002500021024      *
002600021024     D $PO                            3  0
002700021024      *
002800021024      * LISTA PARAMETRI PER RICHIAMO PGM MANUTENZIONE TESTI
002900900730     D PARAM1          DS
003000901024     D  FLG1                   1      1
003100900730     D  S08TTX                 2      3
003200900730     D  S08LIN                 4      4
003300900730     D  S08FNZ                 5      5
003400901212     D  COPTTX                 6      7
003500940222     D*
003600941215     D* RICHIAMO PGM GESTIONE OPZIONI STAMPA TESTO     - FNLV42R -
003700940222     D PARAM3          DS
003800940222     D  PA3TTX                 1      2
003900940308     D*
004000941215     D LV40DS          DS
004100940218     D  NRG                  370    370
004200940218     D  NCL                  371    371
004300940218     D  PRIMA                378    379B 0
004400940218     D*
004500940218     D CURSOR          DS
004600940218     D  RIRI                   1      2B 0
004700940218     D  R$$                    2      2
004800940218     D  COCO                   3      4B 0
004900940218     D  C$$                    4      4
005000940308     D                 DS
005100940308     D  S08LET                 1      1
005200940308     D  S08RIB                 2      2
005300940308     D  S08CR1                 3     77
005400940308     D* RIGA SUBFILE PER COMPRESSO
005500940308     D  S08CR2                78    112
005600940308     D  WRIGA                  1    112
005700940308     D* RIGA SUBFILE PER NORMALE
005800940308     D  WRIGA2                 1     77
005900940308     D* RIGA DEL TESTO
006000940308     D  RIGAT                  3    112
006100921214     C*---------------------------------------------------------------*
006200921214     C* INDICATORI USATI                                              *
006300921214     C*---------------------------------------------------------------*
006400940308     C* 01 - POSIZIONAMENTO CURSORE
006500940308     C* 03 - TESTO NON GESTIBILE
006600930406     C* 04 - ROLLUP
006700930107     C* 05 - OFF - ALMENO UNA WRITE SUL SFILE
006800941215     C* 10 - ON ---> TESTO NORMALE      OFF ---> TESTO IN COMPRESSO
006900940301     C* 13 - PROGRAMMA RICHIAMATO
007000021024     C* 21 - TIPO TESTO NON PRESENTE IN FLTXT00F O TESTO ANNULLATO
007100921214     C* 22 - OFF - MODIFICA TESTO; ON - IMMISSIONE
007200921214     C* 23 - DI COMODO
007300921214     C* 24 - TIPO TESTO DA CUI COPIARE ERRATO
007400930105     C* 27 - ABILITAZIONE CMD16-ANNULLAMENTO TESTO
007500021025     c* 28 - Visualizza messaggio a video
007600021024     C* 30 - DI COMODO
007700940210     C* 35 - LOKUP ANDATA A BUON FINE
007800921214     C* 40 - ERRORE SE RIGHE IN CALCE PIU DI DUE
007900921214     C* 41 - ERRORE SE TIPO RIGA NON IMMESSO
008000940210     C* 42 - ERRORE SE PROFILO NON VALIDO PER LA GESTIONE DEL TESTO
008100940210     C* 43 - ERRORE MESSAGGIO DI AVVERTIMENTO
008200021025     c* 44 - Errore descrizione testo
008300921214     C* 72 - OFF - SFLCLR; ON - SFLDSP E SFLDSPCTL
008400921214     C* 77 - AGGANCIO RIGA SFILE E LETTURA FLTXT00F
008500921214     C* 90 - GENERICO DI ERRORE
008600921214     C*---------------------------------------------------------------*
008700921214     C*                    MAIN                                       *
008800921214     C* PROGRAMMA PRINCIPALE MANUTENZIONE TESTI LETTERE COMMERCIALE   *
008900921214     C*---------------------------------------------------------------*
009000921214     C*
009100921214     C* DEFINIZIONE KLIST E VARIABILI
009200921214     C                   EXSR      INIZIO
009300921214     C*
009400940301     C* 13 ON  - P R O G R A M M A   R I C H I A M A T O
009500940301IF  1C     FLG1          IFNE      ' '
009600940301     C                   SETON                                        13
009700901212     C                   GOTO      RICH2
009800930105E   1C                   ENDIF
009900930107     C*
010000921214     C     RICH1         TAG
010100901023     C                   MOVE      'I'           S08LIN
010200881121     C                   MOVE      '  '          S08TTX
010300901212     C                   MOVE      '  '          COPTTX
010400940210     C                   MOVEL     ' '           S08FTS
010500940210     C                   MOVEL     ' '           UNO               1
010600921104     C*
010700921214     C* EMISSIONE I FORMATO - RICHIESTA TESTO DA MANUTENERE
010800921214     C     EMET1         TAG
010900941215     C                   EXFMT     LV40D01
011000021025     C                   CLEAR                   VIDMSG
011100021025     c                   Eval      *in28 = *off
011200021025     c                   Setoff                                       2124
011300021025     c                   Setoff                                       4042
011400921214     C*
011500901023     C* CMD3 - FINE LAVORO
011600901023     C   KC              GOTO      FINE
011700940308     C*
011800940308     C     RICH2         TAG
011900940308     C*
012000940308     C* C O N T R O L L I
012100940308     C                   EXSR      CONTR1
012200940308     C   90              GOTO      EMET1
012300940215     C*
012400940216     C     SOPRA         TAG
012500940308     C**
012600940309     C* T E S T O   N O R M A L E
012700021024    1C                   if        S08FTS = *blanks
012800021024     C                   eval      *in10 = *on
012900940308     C                   MOVEL     '1'           UNO
013000940309     C                   Z-ADD     18            WRIG
013100940308     C                   ELSE
013200940309     C* T E S T O   C O M P R E S S O
013300021024     C                   eval      *in10 = *off
013400021024     C                   clear                   UNO
013500940309     C                   Z-ADD     9             WRIG              5 0
013600940308     C                   ENDIF
013700940308     C*
013800940216     C                   EXSR      REVIS
013900940308     C* 19 ON- MODIFICATO TIPO STAMPA TESTO (NORM O COMPRESSO)
014000940216     C   19              GOTO      SOPRA
014100940216     C*
014200901025     C* SE PGM RICHIAMATO ESCO SUBITO
014300921214IF  1C     FLG1          IFEQ      ' '
014400921214     C                   GOTO      RICH1
014500930105E   1C                   ENDIF
014600940301     C*
014700921214     C     FINE          TAG
014800021024     C                   eval      *inLR = *ON
014900021024      *---------------------------------------------------------------*
015000021024      *                    CONTR1                                     *
015100021024      * CONTROLLI FORMATO 1                                           *
015200021024      *---------------------------------------------------------------*
015300940308     C     CONTR1        BEGSR
015400021024      *
015500021024     C                   eval      *in90 = *off
015600021024     C                   clear                   Ds1H
015700021024      *
015800021024      * SE ? RICERCA TESTO LETTERA
015900940308     C     '?'           SCAN      S08TTX                                 90
016000021024      *
016100940308    1C     *IN90         IFEQ      *ON
016200021024     C                   clear                   S08TTX
016300021024      * PROFILO UTENTE
016400021024     C                   clear                   Fnlv41ds
016500021024     C                   MOVEL     PROF          I41Pro
016600021024      * I42prv = 'G' ===> PROVENGO DA MANUTENZIONE TESTI
016700021024     C                   MOVEL     'G'           I41Prv
016800021024     C                   MOVEL     '1'           I41MOD
016900021024     C                   CALL      'FNLV41R'
017000021024     C                   PARM                    FNLV41DS
017100021024     C     O41LET        IFNE      *blanks
017200021024     C                   MOVEL     O41LET        S08TTX
017300940308     C                   GOTO      ENDCT1
017400021024E   2C                   ENDIF
017500021024E   1C                   ENDIF
017600940308     C*
017700940308     C* SE ? RICERCA TESTO LETTERA DA CUI COPIARE
017800940308     C     '?'           SCAN      COPTTX                                 90
017900940308     C*
018000940308    1C     *IN90         IFEQ      *ON
018100021024      * I42prv = ' ' ===> CARICO TUTTI I TESTI
018200021024     C                   CLEAR                   Fnlv41ds
018300021024     C                   MOVEL     '1'           I41MOD
018400021024     C                   CALL      'FNLV41R'
018500021024     C                   PARM                    FNLV41DS
018600021024    2C     O41LET        IFNE      *blanks
018700021024     C                   MOVEL     O41LET        COPTTX
018800940308     C                   GOTO      ENDCT1
018900021024E   2C                   ENDIF
019000021024E   1C                   ENDIF
019100940308     C*
019200940308     C* CONTROLLO SE IMMESSO TIPO TESTO DA CUI COPIARE
019300940308IF  1C     COPTTX        IFNE      *BLANKS
019400021024     C                   CLEAR                   Fnlv41ds
019500021024     C                   MOVEL     COPTTX        I41LET
019600021024     C                   MOVEL     '2'           I41MOD
019700021024     C                   CALL      'FNLV41R'
019800021024     C                   PARM                    FNLV41DS
019900021024     C                   if        O41ERR =  'E'
020000021025     C                   SETON                                        249028
020100021025     C                   Eval      VIDMSG = msg(3)
020200021024     C                   GOTO      ENDCT1
020300021024     C                   endif
020400051027     c* se testo html errore --> non gestibile da questo pgm
020500051027     C                   MOVEL     O41DTA        DS1H
020600051027     c                   if        §1html = 'S'
020700051027     C                   SETON                                        249028
020800051027     C                   Eval      VIDMSG = msg(5)
020900051027     C                   GOTO      ENDCT1
021000051027     c                   endif
021100021024E   1C                   ENDIF
021200940308     C*
021300021024      * CONTROLLO ESISTENZA TESTO IN TABELLA '1H' O FILE TESTI
021400021024     C                   CLEAR                   Fnlv41ds
021500021024     C                   MOVEL     S08TTX        I41LET
021600021024     C                   MOVEL     '2'           I41MOD
021700021024     C                   CALL      'FNLV41R'
021800021024     C                   PARM                    FNLV41DS
021900021024     C     O41ERR        IFEQ      'E'
022000021025     C                   SETON                                        219028
022100021025     C                   Eval      VIDMSG = msg(1)
022200021024     C                   GOTO      ENDCT1
022300021025     c                   ELSE
022400021024     C                   MOVEL     O41DTA        DS1H
022500051027     c* se testo html errore --> non gestibile da questo pgm
022600051027     c                   if        §1html = 'S'
022700051027     C                   SETON                                        219028
022800051027     C                   Eval      VIDMSG = msg(5)
022900051027     C                   GOTO      ENDCT1
023000051027     c                   endif
023100021024     C                   ENDIF
023200940308     C*
023300940308     C* DESCRIZIONE TIPO TESTO
023400940308     C                   MOVEL     §1HDES        S08DES
023500940308     C                   MOVEL     §1HFTS        S08FTS
023600940308     C*
023700940308     C* CONTROLLO SE PROFILO VALIDO PER LA GESTIONE DEL TESTO RICHIESTO
023800940308     C                   MOVEL     '1I'          COD
023900940308     C                   MOVEL     *BLANKS       KEY
024000940308     C                   MOVEL     §1HFGT        KEY
024100021024     C     KTAB          CHAIN     TABEL                              31
024200940308     C  N31TBLFLG        IFEQ      ' '
024300940308     C                   MOVEL     TBLUNI        DS1I
024400940308     C*
024500940308     C     '999'         LOOKUP    SKI                                    35
024600940308     C  N35PROF          LOOKUP    SKI                                    35
024700021024     C  N35              eval      *in31 = *on
024800940308     C                   ELSE
024900021024     C                   eval      *in31 = *on
025000940308     C                   ENDIF
025100940308     C*
025200021025     C   31              SETON                                        429028
025300021025     C   31              Eval      vidmsg = msg(2)
025400940308     C   31              GOTO      ENDCT1
025500940308     C*
025600021024     C                   eval      *in03 = *off
025700021024      * sono in un p.o. e
025800021024      * TESTO GESTIBILE SOLO IN SEDE: CAMPI PROTETTI + CMD6 INIBITO
025900021024      * sono in sede e
026000021024      * TESTO GESTIBILE SOLO NEI P.O.: CAMPI PROTETTI + CMD6 INIBITO
026100021024     C                   IF        (simfel <> *zeros and §1HFGE = 'S')  or
026200021024     C                             (simfel = *zeros and §1HFGE = 'F')
026300021024     C                   eval       *in03 = *on
026400940308E   1C                   ENDIF
026500021024      *
026600021024      * SE TESTO NON GESTIBILE NON POSSO COPIARE
026700021024     C                   IF        *in03 = *on  and copttx <> *blanks
026800021024     C                   eval       *in40 = *on
026900021024     C                   eval       *in90 = *on
027000021025     C                   eval       *in28 = *on
027100021025     c                   Eval      vidmsg = msg(4)
027200940308     C                   GOTO      ENDCT1
027300940308     C                   ENDIF
027400940308     C*
027500940308     C     ENDCT1        ENDSR
027600940215     C*
027700921214     C*---------------------------------------------------------------*
027800921214     C*                    REVIS                                      *
027900921214     C* ROUTINE DI EMISSIONE SFILE E AGGIORNAMENTO MODIFICHE TESTO    *
028000921214     C*---------------------------------------------------------------*
028100921214     C     REVIS         BEGSR
028200021024      *
028300021024      * SE ESISTE IL TESTO LO CARICO ALTRIMENTI LO EMETTO VUOTO
028400930107     C                   EXSR      CARICA
028500021024      * eseguo COPIA TESTO se ho caricato il testo "base"
028600021024      * e se il testo da cui copiare è pieno e non annullato
028700921214IF  1C   22COPTTX        IFNE      *BLANKS
028800930107     C                   EXSR      CARIC2
028900921214X   1C                   ELSE
029000021024      *   SE NON VOGLIO COPIARE
029100940308     C                   Z-ADD     1             WDA               5 0
029200930104     C                   EXSR      AZZTES
029300930105E   1C                   ENDIF
029400021024      *
029500930104     C                   Z-ADD     1             RT
029600021024      * EMISSIONE SFILE
029700921214     C     EMET2         TAG
029800941215     C                   WRITE     LV40Z04
029900941215     C  N13              WRITE     LV40Z05
030000941215     C   13              WRITE     LV40Z06
030100941215     C  N10              EXFMT     LV40C02
030200941215     C   10              EXFMT     LV40C03
030300021024      *
030400021024      * CMD12 - FINE LAVORO
030500021024     C                   if        *inKL = *on
030600021024     C                   EXSR      CTRF12
030700021024     C                   GOTO      FINR
030800021024     c                   endif
030900021024      *
031000021024      * ROLLUP
031100921214IF  1C     *IN04         IFEQ      '1'
031200940308     C                   SETON                                        01
031300870610     C                   Z-ADD     UT            RT
031400940308     C                   Z-ADD     1             WDA               5 0
031500870610     C                   EXSR      AZZTES
031600940308     C     UT            SUB       WRIG          RT
031700940308     C                   ADD       1             RT
031800901213     C                   GOTO      EMET2
031900930105E   1C                   ENDIF
032000930108     C* INSERIMENTO RIGA
032100021024     c                   if        *inKI = *on
032200021024     C  N10              EXSR      CTRIMM
032300021024     C   10              EXSR      CTRIM2
032400021024     C                   endif
032500870610     C*
032600930108     C* CONTROLLO DATI II FORMATO VIDEO
032700930104     C                   EXSR      CONTR
032800021025
032900021025      * Se F6 devo controllare che la descrizione del testo
033000021025      * sia stata inserita
033100021025     c                   If        *InKf and s08des = *blanks
033200021025     c                   Eval      *In90 = *On
033300021025     c                   Eval      *In44 = *On
033400021025     c                   Eval      *InKf = *Off
033500021025     c                   EndIf
033600930108     C* SE ERRORI
033700930107     C   90              GOTO      EMET2
033800921214     C*
033900940222     C* CMD6  - AGGIORNAMENTO TESTO
034000940216     C   19
034100940216     COR KF              EXSR      AGGIO
034200930105     C*
034300940308     C* CMD14 - OPZIONI STAMPA TESTO
034400021024     C                   if        *INKN = *ON
034500940222     C                   MOVEL     S08TTX        PA3TTX
034600940222     C                   MOVEL     PARAM3        KPJBU
034700941215     C                   CALL      'FNLV42R'
034800940222     C                   PARM                    KPJBA
034900940222     C                   ENDIF
035000940222     C*
035100921214     C* CMD16 - ANNULLAMENTO TESTO
035200930104     C   KQ              EXSR      ANNUL
035300940308     C*
035400940308     C* ENTER - RIMANE NELLA SECONDA VIDEATA
035500940308     C     *INKF         IFEQ      '0'
035600940308     C     *INKQ         ANDEQ     '0'
035700940308     C     *IN19         ANDEQ     '0'
035800940308     C     *IN90         OREQ      *ON
035900940308     C                   GOTO      EMET2
036000940308E   1C                   ENDIF
036100921214     C*
036200921214     C     FINR          ENDSR
036300921214     C*---------------------------------------------------------------*
036400921214     C*                    AZZTES                                     *
036500921214     C* ROUTINE DI CARICAMENTO SFILE VUOTO PER INSERIMENTO -TESTATA-  *
036600921214     C*---------------------------------------------------------------*
036700870610     C     AZZTES        BEGSR
036800021024      *
036900940309     C  N10              CLEAR                   WRIGA
037000940309     C   10              CLEAR                   WRIGA2
037100930406     C*
037200940308DO  1C     WDA           DO        WRIG
037300930406     C                   ADD       1             RT
037400930406     C*
037500941215     C  N10              WRITE     LV40S02
037600941215     C   10              WRITE     LV40S03
037700021024     C                   eval      *in01 = *off
037800930406E   1C                   ENDDO
037900930406     C                   Z-ADD     RT            UT                4 0
038000930406     C*
038100870610     C                   ENDSR
038200921214     C*---------------------------------------------------------------*
038300921214     C*                    CARICA                                     *
038400921214     C* ROUTINE DI CARICAMENTO SFILE PER MODIFICA TESTO LETTERE       *
038500921214     C*---------------------------------------------------------------*
038600870610     C     CARICA        BEGSR
038700921214     C*
038800021024     C                   SETOFF                                       2223
038900021024     C                   SETOFF                                       2719
039000940308     C* PULIZIA SFILE
039100940308     C                   SETON                                        7201
039200941215     C  N10              WRITE     LV40C02
039300941215     C   10              WRITE     LV40C03
039400940308     C                   SETOFF                                       72
039500940308     C*
039600940308     C                   Z-ADD     0             RT                4 0
039700021024      *
039800930107     C                   MOVEL     S08TTX        COMTTX
039900021024      * CARICO TESTO
040000021024     C                   move      '0'           $Copia            1
040100940308     C                   EXSR      CARTES
040200021024      *
040300021024      * MODIFICA (22-OFF)  INSERIMENTO (22-ON)
040400021024    1C                   if        RT = *zeros
040500021024     C                   eval      *in22 = *on
040600021024   x1C                   ELSE
040700021024      * ABILITO CMD16-ANNULLAMENTO TESTO  SOLO SE NON RICHIAMATO
040800021024     c                   If         *in13 = *off  and  *in03 = *off
040900021024     C                   eval      *in27 = *on
041000021024     c                   Endif
041100021024      *
041200940308     C     RT            DIV       WRIG          WNUM              5 0
041300940308     C                   MVR                     WDA
041400940308     C                   ADD       1             WDA
041500940308     C                   EXSR      AZZTES
041600021024    1C                   ENDIF
041700930107     C*
041800870610     C                   Z-ADD     RT            UT
041900940308     C* MI SALVO FLAG DI COMPRESSO
042000940308     C                   MOVEL     S08FTS        SAVFTS            1
042100930406     C*
042200870610     C                   ENDSR
042300930107     C*---------------------------------------------------------------*
042400930107     C*                    CARIC2                                     *
042500930107     C* ROUTINE DI CARICAMENTO SFILE PER TESTO DA CUI COPIARE         *
042600930107     C*---------------------------------------------------------------*
042700930107     C     CARIC2        BEGSR
042800930107     C*
042900021024     c                   eval      *in23 = *off
043000021024     c                   eval      *in27 = *off
043100930108     C*
043200930107     C                   MOVEL     COPTTX        COMTTX
043300940308     C* CARICO TESTO
043400021024      *
043500021024     C                   move      '1'           $Copia            1
043600940308     C                   EXSR      CARTES
043700021024      *
043800021024      * SE TESTO DA CUI COPIARE ANNULLATO, EMETTO SFILE VUOTO
043900930107IF  1C     RT            IFEQ      0
044000940308     C                   Z-ADD     1             WDA               5 0
044100930107     C                   EXSR      AZZTES
044200930107E   1C                   ENDIF
044300021024      *
044400930107     C                   Z-ADD     RT            UT
044500021024      *
044600930107     C                   ENDSR
044700021024      *---------------------------------------------------------------*
044800021024      *                    CARTES                                     *
044900021024      * CARICO TESTO IN SUBFILE                                       *
045000021024      *---------------------------------------------------------------*
045100940308     C     CARTES        BEGSR
045200021024      *
045300021024      * Verifico quale testo devo caricare: se esiste FnTxt per simfel
045400021024      * uso questo altrimenti cerco testo sede
045500021024     C                   move      simfel        $po
045600021024     C     Ke2Txt        setll     FnTxt01l                               30
045700021024     C                   IF        *in30 = *off and simfel <> *zeros
045800021024     C                   move      *zeros        $po
045900021024     C     Ke2Txt        setll     FnTxt01l                               30
046000021024     C                   endif
046100021024      *
046200021024      * Se stò copiando da altro testo o sono in un p.o. e ho caricato
046300021024      *  il testo master (TXTPOL = 0) pulisco la descrizione a video
046400021024     C                   IF        $Copia = '1' or
046500021024     C                              ($po = *zeros  and  simfel <> *zeros)
046600021024     C                   eval      S08Des = *blanks
046700021024     C                   ENDIF
046800021024      *
046900021024      * Se ho trovato un record elaboro
047000021024    1c                   IF        *IN30 = *on
047100021024      *
047200021024      * N.B.: LE DUE RIGHE DI FONDO PAGINA (TIPO 'F') SONO NUMERATE
047300021024      *       AL CONTRARIO, PERCIO' DEVO LEGGERLE SEPARATAMENTE
047400940908     C                   Z-ADD     2             TXTNRR
047500021024     C     KE1TXT        CHAIN(N)  FNTXT01L                           23
047600021024    2C                   IF        *in23 = *off  and TXTatb = *blanks
047700940908     C                   ADD       1             RT
047800940908     C                   MOVEL     TXTRIB        S08RIB
047900940908     C                   MOVEL     TXTRIG        S08CR1
048000940908     C  N10              MOVE      TXTRIG        S08CR2
048100940908     C                   MOVE      TXTPOS        S08LET
048200941215     C  N10              WRITE     LV40S02
048300941215     C   10              WRITE     LV40S03
048400021024    2C                   ENDIF
048500021024      *
048600940908     C                   Z-ADD     1             TXTNRR
048700021024     C     KE1TXT        CHAIN(N)  FNTXT01L                           23
048800021024    2C                   IF        *in23 = *off  and TXTatb = *blanks
048900940908     C                   ADD       1             RT
049000940908     C                   MOVEL     TXTRIB        S08RIB
049100940908     C                   MOVEL     TXTRIG        S08CR1
049200940908     C  N10              MOVE      TXTRIG        S08CR2
049300940908     C                   MOVE      TXTPOS        S08LET
049400941215     C  N10              WRITE     LV40S02
049500941215     C   10              WRITE     LV40S03
049600021024    2C                   ENDIF
049700021024      *
049800021024      * QUINDI LEGGO IL RESTO DELLA LETTERA DALLA RIGA 3 IN POI
049900940908     C                   Z-ADD     3             TXTNRR
050000021024     C     KE1TXT        SETLL     FNTXT01L
050100021024     C     KEYTXT        READE(N)  FNTXT01L                               23
050200021024    2C     *IN23         DOWEQ     '0'
050300021024      *
050400021024      * CARICO SOLAMENTE LE RIGHE NON ANNULLATE
050500021024    3C     TXTATB        IFEQ      *BLANKS
050600940308     C                   ADD       1             RT
050700940908     C                   MOVEL     TXTRIB        S08RIB
050800940908     C                   MOVEL     TXTRIG        S08CR1
050900940908     C  N10              MOVE      TXTRIG        S08CR2
051000940908     C                   MOVE      TXTPOS        S08LET
051100941215     C  N10              WRITE     LV40S02
051200941215     C   10              WRITE     LV40S03
051300021024    3C                   ENDIF
051400940308     C*
051500021024     C     KEYTXT        READE(N)  FNTXT01L                               23
051600021024    2C                   ENDDO
051700021024      *
051800021024    1C                   ENDIF
051900021024E   1 *
052000940308     C                   ENDSR
052100021024      *---------------------------------------------------------------*
052200021024      *                    CTRF12                                     *
052300021024      * se per Cod. Testo + simfel ESISTE solo RECORD H lo DELETO     *
052400021024      * E' il caso in cui entro in immissione, richiamo FNLV42R per   *
052500021024      *  impostare i parametri (quindi write del rcd "H"), e poi esco *
052600021024      *  da FNLV40R senza salvare                                     *
052700021024      *---------------------------------------------------------------*
052800021024     C     CTRF12        BEGSR
052900021024      *
053000021024     c                   eval      *in30 = *off
053100021024     C                   MOVEL     S08TTX        COMTTX
053200021024     C                   MOVEL     simfel        $po
053300021024      *
053400021024     C     KE2TXT        setll     FnTXT01l
053500021024     C     KE2TXT        reade(N)  FnTXT01l
053600021024     c                   dow       not %EOF(FNTXT01L)
053700021024     c                   IF        TXTPOS <> 'H'
053800021024     c                   eval      *in30 = *on
053900021024     c                   leave
054000021024     c                   ENDIF
054100021024     C     KE2TXT        reade(N)  FnTXT01l
054200021024     c                   enddo
054300021024      *
054400021024     C                   IF        *in30 = *off
054500021025     C     KE2TXT        delete    FnTxt000
054600021024     c                   ENDIF
054700021024      *
054800021024     C                   ENDSR
054900921214     C*---------------------------------------------------------------*
055000921214     C*                    CTRIMM                                     *
055100930108     C* ROUTINE DI CONTROLLO INSERIMENTO 'I' DI "IMMISSIONE RIGA"     *
055200940308     C*                   PER TESTO COMPRESSO
055300921214     C*---------------------------------------------------------------*
055400901213     C     CTRIMM        BEGSR
055500940309     C*
055600940309     C                   MOVE      NRG           R$$
055700940222     C*
055800940309     C     RIRI          IFGE      4
055900940309     C     RIRI          ANDLE     21
056000940309     C*
056100940222     C                   Z-ADD     0             $RIG              5 0
056200940222     C     RIRI          SUB       4             $RIG
056300940222     C     $RIG          DIV       2             $RIG
056400940222     C                   ADD       PRIMA         $RIG
056500940222     C*
056600940218     C* SE VIENE PREMUTO IL CMD9 AL DI FUORI DEL SFL NON FACCIO NULLA
056700940309     C     $RIG          IFGE      0
056800921214     C*
056900901213     C                   ADD       1             UT
057000901213     C                   Z-ADD     UT            RT
057100940309     C                   CLEAR                   WRIGA
057200941215     C                   WRITE     LV40S02
057300901213     C                   SUB       1             RT
057400901213     C*
057500021024DO  1C     RT            DOWGT     $RIG
057600941215     C     RT            CHAIN     LV40S02                            23
057700940218DO  2C  N23              DO
057800940308     C                   MOVEL     WRIGA         COMRIG          112
057900901213     C                   ADD       1             RT
058000941215     C     RT            CHAIN     LV40S02                            23
058100940218DO  3C  N23              DO
058200940308     C                   MOVEL     COMRIG        WRIGA
058300941215     C                   UPDATE    LV40S02
058400940218E   3C                   ENDDO
058500940218E   2C                   ENDDO
058600901213     C*
058700901213     C                   SUB       2             RT
058800940218E   1C                   ENDDO
058900901213     C*
059000940222     C                   ADD       1             RT
059100941215     C     RT            CHAIN     LV40S02                            23
059200940309     C                   CLEAR                   WRIGA
059300941215     C  N23              UPDATE    LV40S02
059400940309     C*
059500940309     C                   Z-ADD     RT            PRIMA
059600940309     C                   ENDIF
059700940309     C                   ENDIF
059800901213     C*
059900901213     C                   ENDSR
060000940308     C*---------------------------------------------------------------*
060100940308     C*                    CTRIM2                                     *
060200940308     C* ROUTINE DI CONTROLLO INSERIMENTO 'I' DI "IMMISSIONE RIGA"     *
060300940308     C*                   PER TESTO NORMALE
060400940308     C*---------------------------------------------------------------*
060500940308     C     CTRIM2        BEGSR
060600940309     C*
060700940309     C* MUOVO IL NUMERO DELLA RIGA DOVE E' POSIZIONATO IL CURSORE
060800940309     C*   IN R$$ PER TRASFORMARLO DA ESADECIMALE IN BINARIO
060900940309     C                   MOVE      NRG           R$$
061000940309     C*
061100940309     C     RIRI          IFGE      4
061200940309     C     RIRI          ANDLE     21
061300940308     C*
061400940308     C* PRIMA = NUMERO RELATIVO DI RECORD DELLA PRIMA RIGA DEL SFL
061500940308     C*         DELLA PAGINA VISUALIZZATA
061600940308     C                   Z-ADD     PRIMA         $RIG              5 0
061700940308     C* AGGIUNGENDO IL NUMERO DELLA RIGA DOVE E' POSIZIONATO IL CURSORE
061800940308     C*   AL NUMERO DELLA PRIMA RIGA DEL SFL TROVO IL NUMERO DELLA
061900940308     C*   RIGA VUOTA DA INSERIRE
062000940308     C                   ADD       RIRI          $RIG
062100940308     C* SOTTRAGGO TUTTE LE RIGHE CHE SEPARANO IL SFL DALLA PRIMA RIGA
062200940308     C*   DELLO SCHERMO (COMPRESA LA PRIMA RIGA DEL SFL)
062300940308     C                   SUB       4             $RIG
062400940308     C* SE VIENE PREMUTO IL CMD9 AL DI FUORI DEL SFL NON FACCIO NULLA
062500940309     C     $RIG          IFGE      0
062600940308     C*
062700940308     C                   ADD       1             UT
062800940308     C                   Z-ADD     UT            RT
062900940309     C                   CLEAR                   WRIGA2
063000941215     C                   WRITE     LV40S03
063100940308     C                   SUB       1             RT
063200940308     C*
063300940308DO  1C     RT            DOWGT     $RIG
063400941215     C     RT            CHAIN     LV40S03                            23
063500940308DO  2C  N23              DO
063600940308     C                   MOVEL     WRIGA2        COMRIG
063700940308     C                   ADD       1             RT
063800941215     C     RT            CHAIN     LV40S03                            23
063900940308DO  3C  N23              DO
064000940308     C                   MOVEL     COMRIG        WRIGA2
064100941215     C                   UPDATE    LV40S03
064200940308E   3C                   ENDDO
064300940308E   2C                   ENDDO
064400940308     C*
064500940308     C                   SUB       2             RT
064600940308E   1C                   ENDDO
064700940308     C*
064800940308     C                   ADD       1             RT
064900941215     C     RT            CHAIN     LV40S03                            23
065000940309     C                   CLEAR                   WRIGA2
065100941215     C  N23              UPDATE    LV40S03
065200940309     C*
065300940309     C                   Z-ADD     RT            PRIMA
065400940309     C                   ENDIF
065500940309     C                   ENDIF
065600940308     C*
065700940308     C                   ENDSR
065800921214     C*---------------------------------------------------------------*
065900921214     C*                    CONTR                                      *
066000921214     C* ROUTINE DI CONTROLLO TESTO LETTERA                            *
066100921214     C*---------------------------------------------------------------*
066200901026     C     CONTR         BEGSR
066300021024      *
066400940308     C                   SETOFF                                       9001
066500940309     C                   Z-ADD     PRIMA         SAVPRI
066600940216     C*
066700901026     C                   Z-ADD     0             CONT
066800901026     C                   Z-ADD     1             RT
066900941215     C  N10RT            CHAIN     LV40S02                            77
067000941215     C   10RT            CHAIN     LV40S03                            77
067100901026     C*
067200921214DO  1C  N77              DO        *HIVAL
067300921214     C*
067400921214IF  2C     S08LET        IFEQ      'F'
067500901026     C                   ADD       1             CONT              1 0
067600930105E   2C                   ENDIF
067700921214     C*
067800901026     C* SONO AMMESSE SOLO 2 RIGHE IN CALCE
067900921214IF  2C     CONT          IFGT      2
068000901026     C                   SETON                                        9040
068100901026     C                   GOTO      ENDCTR
068200930105E   2C                   ENDIF
068300921214     C*
068400921214     C* CONTROLLO SE IMMESSO RIGA LETTERA E SE IMMESSO TIPO DI RIGA
068500940308IF  2C  N10S08LET        IFEQ      *BLANKS
068600940308IF  3C     RIGAT         ANDNE     *BLANKS
068700901026     C                   SETON                                        9041
068800901026     C                   GOTO      ENDCTR
068900930105E   2C                   ENDIF
069000940308     C*
069100940308     C* CONTROLLO SE IMMESSO RIGA LETTERA E SE IMMESSO TIPO DI RIGA
069200940308IF  2C   10S08LET        IFEQ      *BLANKS
069300940308     C     S08CR1        ANDNE     *BLANKS
069400940308     C                   SETON                                        9041
069500940308     C                   GOTO      ENDCTR
069600940308E   2C                   ENDIF
069700940308     C*
069800941215     C  N10              UPDATE    LV40S02
069900941215     C   10              UPDATE    LV40S03
070000901026     C*
070100901026     C                   ADD       1             RT
070200941215     C  N10RT            CHAIN     LV40S02                            77
070300941215     C   10RT            CHAIN     LV40S03                            77
070400921214     C*
070500901026     C     ENDCTR        TAG
070600930105     C  N90
070700930105E   1CANN77              ENDDO
070800940308     C* UPDATO PER ERRORE
070900941215     C   90
071000941215     CANN10              UPDATE    LV40S02
071100941215     C   90
071200941215     CAN 10              UPDATE    LV40S03
071300921214     C*
071400901026     C* SE NESSUN ERRORE RIEMETTO DALLA PRIMA VIDEATA
071500940309     C  N90              Z-ADD     SAVPRI        RT
071600940308     C*
071700940308     C* CONTROLLO SE CAMBIATO TIPO STAMPA TESTO(NORMALE O COMPRESSO)
071800940308     C  N90S08FTS        IFNE      SAVFTS
071900940308     C*
072000940308     C     UNO           IFEQ      ' '
072100940308     C                   SETON                                        4390
072200940308     C                   MOVEL     '1'           UNO
072300940308     C                   GOTO      ENDCON
072400940308     C                   ENDIF
072500940308     C*
072600940308     C                   MOVEL     S08FTS        SAVFTS
072700940308     C                   SETON                                        19
072800940308     C                   ENDIF
072900901026     C*
073000940216     C     ENDCON        ENDSR
073100921214     C*---------------------------------------------------------------*
073200921214     C*                    AGGIO                                      *
073300021024     C* ROUTINE DI AGGIORNAMENTO FILE TESTI (FNTXT00F)                *
073400921214     C*---------------------------------------------------------------*
073500870610     C     AGGIO         BEGSR
073600870610     C*
073700021024     C                   eval      *in05 = *off
073800021024     C                   eval      *in77 = *off
073900021024     c                   clear                   dfntxth
074000021024     C                   clear                   ALF1              1
074100921214     C*
074200921215     C                   Z-ADD     3             NRR               3 0
074300921215     C                   Z-ADD     2             CONT
074400921215     C                   Z-ADD     1             RT
074500941215     C  N10RT            CHAIN     LV40S02                            77
074600941215     C   10RT            CHAIN     LV40S03                            77
074700021024      *
074800021024      * Cancello il testo dal file
074900021024      *  Imposto flag se trovato almeno un rcd
075000021024      *  Salvo dati riga "H"
075100901212     C                   MOVEL     S08TTX        COMTTX
075200021024     C                   MOVEL     simfel        $po
075300021024     C     KEYTXT        SETLL     FNTXT01L
075400021024     C     KEYTXT        READE     FNTXT01L                               23
075500021024      *
075600930107DO  1C     *IN23         DOWEQ     '0'
075700930107     C                   MOVEL     'S'           ALF1
075800021024     c                   if        txtpos = 'H'
075900021024     c                   movel     txtrig        dfntxth
076000021024     c                   endif
076100021024     C                   DELETE    FNTXT000
076200021024     C     KEYTXT        READE     FNTXT01L                               23
076300930107E   1C                   ENDDO
076400021024      *
076500021024      * IMPOSTO CAMPI STANDARD PER SCRITTURA TXT
076600021024     C                   z-add     simfel        TXTPOL
076700021024     C                   clear                   TXTRIG
076800940308     C                   MOVE      S08TTX        TXTLET
076900940308     C                   MOVE      S08LIN        TXTLIN
077000021024     C                   clear                   TXTFTR
077100021024     C                   clear                   TXTDTR
077200021024     C                   MOVEL     000           TXTFLT
077300940308     C                   MOVEL     §1HFTT        TXTFTT
077400021024     C                   clear                   TXTATB
077500021024      *
077600021024      * LETTURA SFL per scrivere file testi
077700021024      * accendo IN05 se scrivo almeno un record
077800921214DO  1C     *IN77         DOWEQ     '0'
077900921214     C*
078000021024    2c                   if        S08LET <> 'A' and S08LET <> *blanks
078100870611     C                   MOVE      S08LET        TXTPOS
078200921214     C*
078300921214     C* LE RIGHE IN CALCE LE METTO NELLE PRIME DUE POSIZIONI
078400021024IF  3C                   if        S08LET = 'F'
078500901026     C                   Z-ADD     CONT          TXTNRR
078600901026     C                   SUB       1             CONT
078700021024X   3C                   else
078800901026     C                   Z-ADD     NRR           TXTNRR
078900901026     C                   ADD       1             NRR
079000021024E   3C                   endif
079100901026     C*
079200940308     C                   MOVEL     S08CR1        TXTRIG
079300940308     C  N10              MOVE      S08CR2        TXTRIG
079400870610     C                   MOVEL     S08RIB        TXTRIB
079500021024     C                   eval      *in05 = *on
079600021024     C                   WRITE     FNTXT000
079700930105E   2C                   ENDIF
079800910424     C*
079900870610     C                   ADD       1             RT
080000941215     C  N10RT            CHAIN     LV40S02                            77
080100941215     C   10RT            CHAIN     LV40S03                            77
080200930105E   1C                   ENDDO
080300021024      *
080400021024     C                   SELECT
080500021024      * Se sono in sede e il testo è da trasmettere
080600021024      *  quando viene annullato interamente scrivo un record
080700021024      *  annullato per la trasmissione
080800021024     C                   WHEN      SIMFEL = *zeros and §1HFTT <> *blanks
080900021024     c                             and ALF1 = 'S' and *in05 = *off
081000021024     C                   movel     'C'           TXTPOS
081100021024     C                   z-add     3             TXTNRR
081200021024     C                   movel     'A'           TXTATB
081300021024     C                   clear                   TXTRIB
081400021024     C                   clear                   TXTRIG
081500021025     C                   WRITE     FNTXT000
081600021024      * Se non annullo e ho scritto almeno un record scrivo la riga "H"
081700021025      *  Impostando i parametri se non esisteva
081800021024     C                   WHEN      *in05 = *on
081900021025     C                   clear                   TXTNRR
082000021024     C                   clear                   TXTRIB
082100021024     C                   movel     'H'           TXTPOS
082200021025     c                   If        §TXDES = *blanks
082300021025     c                   movel     §1hPva        §txPva
082400021025     c                   movel     §1HFv1        §TxSip
082500021025     c                   movel     §1HFv2        §TxSic
082600021025     c                   movel     §1HFv3        §TxScc
082700021025     c                   movel     §1HFv4        §TxSca
082800021025     c                   movel     §1HFv5        §TxStc
082900021025     c                   movel     §1HFv6        §TxSdc
083000021025     c                   movel     §1HFv7        §TxSpf
083100021025     c                   movel     §1HFv8        §TxSnp
083200021025     c                   movel     §1HFv9        §TxSri
083300021025     c                   Endif
083400021025     c                   movel     S08Fts        §txFts
083500021025     C                   movel     S08des        §txdes
083600021025     C                   movel(P)  DfntxtH       TXTRIG
083700021025     C                   WRITE     FNTXT000
083800021024     c                   ENDSL
083900021024      *
084000021024      *
084100870610     C                   ENDSR
084200921214     C*---------------------------------------------------------------*
084300921214     C*                    ANNUL                                      *
084400921214     C* ROUTINE DI ANNULLAMENTO TESTO                                 *
084500921214     C*---------------------------------------------------------------*
084600921214     C     ANNUL         BEGSR
084700921214     C*
084800930105     C* ANNULLO TUTTE LE RIGHE DEL TESTO
084900921215     C*
085000921215     C                   MOVEL     S08TTX        COMTTX
085100021024     C                   MOVEL     simfel        $po
085200021024     C     KEYTXT        SETLL     FNTXT01L
085300021024     C     KEYTXT        READE     FNTXT01L                               77
085400921215     C*
085500930107DO  1C     *IN77         DOWEQ     '0'
085600021024     C                   DELETE    FNTXT000
085700021024     C     KEYTXT        READE     FNTXT01L                               77
085800930105E   1C                   ENDDO
085900021024     C* Se sono in sede
086000930107     C* SCRIVO UNA RIGA VUOTA DA TRASMETTERE
086100021024IF  1C                   if        simfel = *zeros and §1HFTT <> *blanks
086200930107     C                   MOVE      S08TTX        TXTLET
086300930107     C                   MOVEL     'C'           TXTPOS
086400930107     C                   Z-ADD     3             TXTNRR
086500930107     C                   MOVEL     *BLANKS       TXTRIB
086600930107     C                   MOVEL     *BLANKS       TXTRIG
086700930107     C                   MOVE      S08LIN        TXTLIN
086800930107     C                   MOVEL     §1HFTT        TXTFTT
086900930402     C*
087000021024     C                   MOVEL     000           TXTFLT
087100930107     C                   MOVEL     *BLANKS       TXTFTR
087200930107     C                   Z-ADD     *ZEROS        TXTDTR
087300930107     C                   MOVEL     'A'           TXTATB
087400930402     C*
087500021024     C                   WRITE     FNTXT000
087600930402E   1C                   ENDIF
087700921215     C*
087800921214     C                   ENDSR
087900921214     C*---------------------------------------------------------------*
088000921214     C*                    INIZIO                                     *
088100921214     C* ROUTINE DI DEFINIZIONE KLIST E VARIABILI                      *
088200921214     C*---------------------------------------------------------------*
088300921214     C     INIZIO        BEGSR
088400921214     C*
088500921214     C     *ENTRY        PLIST
088600921214     C                   PARM                    KPJBA
088700921214     C                   MOVEL     KPJBU         PARAM1
088800921214     C*
088900940218     C                   Z-ADD     0             RIRI
089000940308     C                   MOVE      TES(1)        VIDTES
089100921214     C*
089200921214     C                   Z-ADD     1             CODUT
089300921214     C                   CALL      'X§PARUT'
089400921214     C                   PARM                    UT§DSE
089500921214     C                   MOVEL     REC80         CNCR80
089600921214     C                   MOVEL     RAGUT         DESDIT
089700921214     C*
089800921214     C* KLIST RICERCA TESTO LETTERA
089900921214     C     KEYTXT        KLIST
090000021024     C                   KFLD                    COMTTX
090100021024     C                   KFLD                    $PO
090200940908     C                   KFLD                    S08LIN
090300940908     C     KE1TXT        KLIST
090400021024     C                   KFLD                    COMTTX
090500021024     C                   KFLD                    $PO
090600940908     C                   KFLD                    S08LIN
090700940908     C                   KFLD                    TXTNRR
090800021024     C     KE2TXT        KLIST
090900021024     C                   KFLD                    COMTTX
091000021024     C                   KFLD                    $PO
091100021024     C     KEYTXT2       KLIST
091200021024     C                   KFLD                    COMTTX            2
091300021024     C                   KFLD                    $PO
091400021024     C                   KFLD                    S08LIN
091500021024     C                   KFLD                    $TPORIG           1
091600921214     C*
091700921214     C* AGGANCIO RECORD TABELLA
091800921214     C     KTAB          KLIST
091900921214     C                   KFLD                    CODUT
092000921214     C                   KFLD                    COD               2
092100921214     C                   KFLD                    KEY               8
092200921214     C*
092300921214     C* DEFINIZIOMNE VARIABILI
092400921214     C     *LIKE         DEFINE    TBLKUT        §KUT
092500921214     C     *LIKE         DEFINE    TBLCOD        §COD
092600921214     C     *LIKE         DEFINE    TBLKEY        §KEY
092700940309     C     *LIKE         DEFINE    PRIMA         SAVPRI
092800921214     C*
092900921214     C                   ENDSR
093000930108     C*---------------------------------------------------------------*
093100940302**
093200940302 ***   GESTIONE  TESTI  LETTERE   ***
093300021025**  MSG
093400021025Tipo di Testo Errato Mancante o Annullato                                     1
093500021025Profilo non abilitato alla gestione del Testo richiesto !!                    2
093600021025Tipo Testo da cui copiare errato                                              3
093700021025Copia testo non ammessa se testo non gestibile                                4
093800051027Tipo di testo "HTML" non utilizzabile
