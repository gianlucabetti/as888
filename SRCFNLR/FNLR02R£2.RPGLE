000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940711      * FNLR02R *----------------------------------------------------*
000300940711      *         - INTERROGAZIONI FOGLI VARI
000400900515      *--------------------------------------------------------------*
000500950615      *  IL PGM RICONOSCE DI ESSERE RICHIAMATO SE IL PARAMETRO DLRRIC
000600950615      *  E' PIENO: SE C'E' "S" NON VIENE MOSTRATA LA PRIMA VIDEATA
000700950615      *  CON LA RICHIESTA PARAMETRI; SE C'E' "§" VIENE MOSTRATA LA
000800950615      *  PRIMA VIDEATA CON LA RICHIESTA PARAMETRI.
000900950615      *--------------------------------------------------------------*
001000940711     FFNLR01D   CF   E             WORKSTN
001100940711     F                                     SFILE(LR01SF2:NR2)
001200010509     F                                     SFILE(LR01SF3:NR3)
001300911004     FTABEL00F  IF   E           K DISK
001400070112     FFNBRV01L  IF   E           K DISK
001500920806     FAZORG01L  IF   E           K DISK
001600940711     FFNFVV01L  IF   E           K DISK
001700940711     FFNFVV02L  IF   E           K DISK
001800940711     F                                     RENAME(FNFVV000:FNFVV002)
001900940711     FFNFVV03L  IF   E           K DISK
002000940711     F                                     RENAME(FNFVV000:FNFVV003)
002100020905     fFnfvv08l  if   e           k Disk    Rename(Fnfvv000:Fnfvv008)
002200020910     FFNARB78L  IF   E           K DISK
002300010510     FFNORM06L  IF   E           K DISK
002400020423     FFiar601L  IF   E           K DISK
002500051114     FFiAR901L  IF   E           K DISK
002600911004      *
002700040518     d $FV             s                   like(FVVnfv) dim(600)                F.V. da FIMS03R
002800070404     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
002900031111      *
003000940711     D TIN             S              1    DIM(30)                              TIPO INV
003100940711     D DIN             S             10    DIM(30)                              DESCR TIP.INV
003200900515      *
003300940711     D PGM             S             10    DIM(2) CTDATA PERRCD(1)              PGM RICHIAMANTI
003400940711     D TES             S             34    DIM(2) CTDATA PERRCD(1)              TESTATE
003500050512     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG DI ERRORE
003600911119      *
003700920806     D DEC             S             12    DIM(6)                               DESCR CATEGOR
003800941027     D L1              S              3  0 DIM(30)                              FIL GESTITE
003900020611
004000050512     d fgspkm          s                   like(§ogpkm)
004100050512     d savfgs          s                   like(v0cfgs)
004200050512     d savin08         s              1
004300050512     d kfnvv           s                   like(arbndc)
004400020910     d Wifp            s                   Like(Arbifp)
004500020611     d wbar            s              1    inz('0')
004600020611     d wppt            s              1    inz('0')
004700020611
004800031111      *
004900031111     d Status         sds           333
005000031111     d   SDSprm          *parms
005100900515      *
005200900514     D KPJBA         E DS
005300070329     D KPJBUs          s                   like(KPJBU)
005400070329     D fidg31ds      E DS
005500920806     D TBLKEY          DS
005600920806     D  ALF1                   1      1
005700900516      *
005800940802      *
005900940802     D* DS STANDARD FOGLI VARI
006000940802     D DSLR01        E DS
006100940802     D*
006200920806     D DS7N          E DS
006300920806     D  §7NDE2                 1     10
006400941027     D* DS PER TRUL06R - CARICAMENTO £X
006500941027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006600941027     D  LIN                    1     90  0
006700941027     D                                     DIM(30)                              FIL. COMODO
006800050512     D og150         E DS
006900070329     D og146         E DS
007000050512     D DFVVSPN       E DS
007100050512     D CNCR80        E DS
007200920806     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007300020423     d YEURDS        E DS                  EXTNAME(YEURCODS)
007400900515      *
007500940711     D***
007600940711     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007700940711     D***
007800900515     D WLBDAT          DS
007900940711     D  G02DAT                 1      8  0
008000940711     D  G02INV                 9     16  0
008100940711     D  G02ERR                17     17
008200940711     D  G02TGI                18     22  0
008300950112     D PARA04          DS
008400950112     D  P04NFV                 1      5  0
008500950112     D  P04LNP                 6      8  0
008600950112     D  P04LNA                 9     11  0
008700950112     D  P04NRS                12     13  0
008800950112     D  P04NCD                14     20  0
008900950112     D  P04NCA                21     27  0
009000950112     D  P04NPS                28     29  0
009100950112     D  P04NPG                30     30  0
009200950112     D  P04FLG                31     31
009300950112     D  P04PGM                32     38
009400950112     D  P04FGS                39     41  0
009500101202     d* bolla
009600101202     d  p04bolp               42     44  0
009700101202     d  p04bors               45     46  0
009800101202     d  p04bosp               47     53  0
009900101202     d  p04boan               54     57  0
010000101202     D  P04ERR                58     58
010100101202     d
010200940712     D***
010300940712     D* COSTANTI
010400940712     D***
010500940712     D CANNUL          C                   CONST('  Annullato  ')
010600940712     D CBLOCC          C                   CONST('  Bloccato   ')
010700910319      *---------------------------------------------------------------*
010800910319      * INDICATORI USATI
010900910319      *---------------------------------------------------------------*
011000940712      * 01    = PGM RICHIAMATO
011100940712      * 02    = CATEGORIA INVENTARIO 3
011200941027      * 03/05 = USATI IN GESTIONE FOGLI VARI
011300940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
011400941027      * 07    = EMETTO LA SCRITTA : ESISTONO SPUNTE SUL FOGLIO
011500050512      * 08    = p.o. gestione con picking messaggerie
011600050512      * 09    = riemetto video per cambio p.o. gestione anche senza errori
011700010509      * 14    = RICERCA DI FOGLI CATEGORIA '4'
011800031111      * 19    = Richiamato per visualizzazione f.v. non immessi in FIMS03D
011900031111      *         (non viene richiesta alcuna opzione).
012000940712      * 20/29 = SU SFL STD
012100050512      * 38/46 = ERRORE
012200940712      * 52    = RECORD ANNULLATO
012300940712      * 53    = FOGLIO CHIUSO
012400010515      * 56/58 = USATO COME ERRORE NEL FILE VIDEO
012500010515      * 60/61 = USATO COME ERRORE NEL FILE VIDEO
012600910319      * 75/77 = ERRORE SUI CAMPI
012700940712      * 88    = DI COMODO
012800940712      * 90    = ERRORE GENERICO
012900010515      * 95    = USATO COME ERRORE NEL FILE VIDEO
013000010515      * 96    = RECORD DA CARICARE IN SUBFILE
013100940712      * 98    = INTERROGAZIONE
013200940712      * 99    = PROTEZIONE CAMPI
013300910319      *--------------------------------------------------------------*
013400000000     C     *ENTRY        PLIST
013500000000     C                   PARM                    KPJBA
013600031111     c                   parm                    $FV
013700031111      *
013800940802     C                   MOVEL     KPJBU         DSLR01
013900900516      *
014000000000     C                   Z-ADD     1             CODUT
014100920806     C                   CALL      'X§PARUT'
014200920806     C                   PARM                    UT§DSE
014300940711     C                   MOVEL     RAGUT         V0CDSA
014400920806     C                   MOVEL     REC80         CNCR80
014500900515      *
014600900515      *---------------------------------------------------------------*
014700900515      * FLUSSO PRINCIPALE                                             *
014800900515      *---------------------------------------------------------------*
014900920806     C*
015000940711     C* CARICO TABELLE
015100940711     C                   EXSR      CARTAB
015200920806     C* PER L'INTERROGAZIONE
015300941027     C* LA FILIALE APERTURA FOGLIO E' SEMPRE ABILITATA
015400941027     C                   SETON                                        990498
015500940712     C*
015600940712     C* LA PULIZIA LA ESEGUO UNA VOLTA SOLA ALL'INIZIO
015700940712     C                   EXSR      INZD00
015800940712     C*
015900040310     C* 01 ON - PROGRAMMA RICHIAMATO
016000950615     C     DLRRIC        IFNE      ' '
016100940711     C* PRENDO LA TESTATA DAL NOME DEL PROGRAMMA
016200940712     C                   Z-ADD     1             X                 2 0
016300940802     C     DLRPGM        LOOKUP    PGM(X)                                 30
016400940711     C   30              MOVEL     TES(X)        V0CTES
016500941027     C* ACCENDO GLI INDICATORI DI PGM RICHIAMATO
016600941027     C                   SETON                                        01
016700031111      * Condiziono l'emissione del campo "1=Selezione" nei sfl
016800031111     c     SDSprm        comp      1                                  19
016900031112      * Ripulisco eventuale flag di ritorno per FIMS03R
017000031112      * (se FNLR02R richiamato per sapere se ci sono distinte aperte
017100031112      *  non inserite nel piano a video di FIMS03R)
017200031112     c                   if        DLRtfv = '3'
017300031112     c                   clear                   DLRfs1
017400031112     c                   endif
017500940711     C* IMPOSTO I CAMPI
017600940802     C                   MOVEL     DLRPGM        V0CPGM
017700940802     C                   MOVEL     DLRGDA        V0CDIN
017800940802     C                   MOVEL     DLRGAL        V0CDFI
017900940802     C                   MOVEL     DLRTFV        V0CTFV
018000940802     C                   MOVEL     DLRNPG        V0CNPG
018100940802     C                   MOVEL     DLRFGS        V0CFGS
018200050512     c                   exsr      chainfgs
018300940802     C                   MOVEL     DLRNFV        V0CNFV
018400950615     C*
018500950615     C* VEDO SE VISUALIZZARE LA PRIMA VIDEATA (SCELTA PARAMETRI) O NO
018600950615     C     DLRRIC        IFNE      '§'                                          '§' SI 1° VIDEO
018700020612     c                   move      'N'           v0impo
018800020612     c                   eval      v0ctpb = 'N'
018900031113if  2c                   if        SDSprm > 1
019000031113     c                   eval      V0ctpb = 'T'
019100031113e   2c                   endif
019200950615     C                   GOTO      FOR01
019300950615     C                   ENDIF
019400911010     C                   END
019500911004     C*
019600900515      *          =============
019700900509     C     INIZIO        TAG
019800900515      *          =============
019900950615     C                   CLEAR                   V0CNFV
020000050512     C                   CLEAR                   V0Ctpm
020100020423     c                   move      'N'           v0impo
020200020612     c                   eval      v0ctpb = 'N'
020300911010     C*
020400900515     C     FMT00         TAG
020500911004     C                   SETOFF                                       02
020600950615     C                   EXFMT     LR01D00
020700941027     C*
020800941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
020900050512     C                   SETOFF                                       432809
021000050512     C                   SETOFF                                       46
021100941027     C*
021200910319     C* CMD3  - FINE LAVORO
021300910319     C   KC              GOTO      FINE                                         F3=FINE
021400940711     C*
021500950615     C     FOR01         TAG
021600940711     C* CONTROLLO CAMPI
021700940711     C                   EXSR      CTRD00
021800941027     C*
021900050512     c   09
022000050512     Cor 90              GOTO      FMT00
022100070329     C*
022200070329      * Se categoria foglio 4 devo chiamare il nuovo programma
022300070329     C*  FIDG31R
022400070329     c                   If        v0cNPG = '4'
022500070329      *
022600070329     C*  Controlla Con il P.O. la data abilitazione
022700070329     C     v0cFGS        CHAIN     AZORG01l
022800070329    1C                   IF        %found(azorg01l)
022900070329     C                   MOVEL     ORGde6        og146
023000070329     c                   if             §ogdda <> *blank and §ogdda <> *all'0'
023100070329      * se c'è una data in organigramma e siamo partiti in produzione
023200070329     c                   move      §ogdda        data_org          8 0
023300070406     c                   if        data_org <> 20391231 and dateu8 >= data_org
023400070329     C*
023500070329     C                   Z-ADD     DATinW        FDGGDA
023600070329     C                   Z-ADD     DATfiW        FDGGAL
023700070329     C                   move      V0CNPG        FDGNPG
023800070403     C                   move      'I'           FDGRIC
023900070329     C                   Z-ADD     V0CNFV        FDGNFV
024000070329     C                   Z-ADD     V0CFGS        FDGFGS
024100070329     C                   move      V0CTFV        FDGTFV
024200160309     C                   move      V0CTpm        FDGtdis
024300070329     C                   move      'T'           FDGTPV
024400070329     c                   eval      kpjbus = kpjbu
024500070404     c                   clear                   $FV6
024600070404     c                   do        600           yy                3 0
024700070404if  2c                   if        SDSprm > 1
024800070404     c                   z-add     $FV(yy)       $FV6(yy)
024900070404     c                   else
025000070404     c                   z-add     999999        $FV6(yy)
025100070404     c                   end
025200070404     c                   enddo
025300070329     C                   MOVEL(p)  FIDG31DS      KPJBU
025400070329     c                   call      'FIDG31R'
025500070329     c                   parm                    kpjba
025600070404     c                   parm                    $FV6
025700070329     c                   movel     kpjbu         fidg31ds
025800070419     C                   eval      dlrERR  =  FDGerr
025900070419     C                   eval      dlrFS1  =  FDGfs1
026000070419     C                   eval      V0CNFV  =  FDGNFV
026100070329     C                   eval      DLRNFV  =  FDGNFV
026200070329     c                   eval      kpjbu = kpjbus
026300070329     C*
026400070329     c                   if        DLRRIC = *blank
026500070329     C                   GOTO      FMT00
026600070329     c                   else
026700070329     C                   GOTO      Fine
026800070329     c                   end
026900070329     C*
027000070406     c                   endIf
027100070329     c                   end
027200070329     c                   end
027300070329    1C                   ENDIF
027400911010      **
027500940802      ** NON IMMESSO FOGLIO
027600940712     C     V0CNFV        IFEQ      0
027700010509     C     V0CNPG        COMP      '4'                                    14
027800900517     C                   EXSR      RICERC
027900940711     C*
028000911010      * NON CI SONO FV DA VISUALIZZARE
028100940712     C     *IN48         IFEQ      *ON
028200940712     C* PGM RICHIAMATO
028300940712     C     *IN01         IFEQ      *ON
028400940802     C                   MOVEL     '9'           DLRERR
028500940712     C                   GOTO      FINE
028600940712     C                   ELSE
028700940712     C* PGM NON RICHIAMATO
028800940712     C                   GOTO      FMT00
028900940712     C                   ENDIF
029000940712     C                   ENDIF
029100900522      *
029200010510     C     *IN14         IFEQ      *OFF
029300940712     C                   WRITE     LR01RC2
029400911015     C     FORCT         TAG
029500940712     C                   EXFMT     LR01CT2
029600010510     C                   ENDIF
029700010510     C     *IN14         IFEQ      *ON
029800010510     C                   WRITE     LR01RC3
029900010510     C     FORCT1        TAG
030000010510     C                   EXFMT     LR01CT3
030100010510     C                   ENDIF
030200910319      * CMD12 - RITORNO
030300940712     C   KL
030400940712     CANN01              GOTO      INIZIO
030500940712     C   KL
030600940712     CAN 01              GOTO      FINE
030700900522      *          =============
030800900522     C     LEGGI         TAG
030900900522      *          =============
031000010510     C  N14              READC     LR01SF2                                29
031100010510     C   14              READC     LR01SF3                                29
031200911015     C*
031300911015     C* FINE LETTURA SE NON SCELTE EFFETTUATE RIMANGO NEL SUBFILE
031400940712     C   29SCE           IFEQ      1
031500940712     C  N01              GOTO      INIZIO
031600940712     C   01              GOTO      FINE
031700911015     C                   ELSE
031800010510     C  N14              GOTO      FORCT
031900010510     C   14              GOTO      FORCT1
032000911015     C                   END
032100910204     C*
032200010510     C  N14V2CSCE        IFNE      '1'
032300910204     C                   GOTO      LEGGI
032400910204     C                   END
032500010510     C   14V3CSCE        IFNE      '1'
032600010510     C                   GOTO      LEGGI
032700010510     C                   END
032800900522      *
032900911015     C                   Z-ADD     1             SCE               1 0
033000910423      *
033100941027      * SE RICHIAMATO VADO A FINE PGM
033200940712     C     *IN01         IFEQ      *ON
033300010510     C  N14              Z-ADD     V2CNFV        DLRNFV
033400010510     C   14              Z-ADD     V3CNFV        DLRNFV
033500950615     C                   MOVEL     V0CNPG        DLRNPG
033600940712     C                   GOTO      FINE
033700940712     C                   ELSE
033800940712     C*
033900010510     C  N14              Z-ADD     V2CNFV        V0CNFV
034000010510     C  N14              Z-ADD     V2CFGS        V0CFGS
034100010510     C   14              Z-ADD     V3CNFV        V0CNFV
034200050512     C** 14              Z-ADD     V3CFGS        V0CFGS
034300940712     C     KFVV          CHAIN     FNFVV000                           30
034400910423      *
034500910319      * VISUALIZZO FOGLIO VIAGGIO
034600940712     C  N30              EXSR      VISNFV
034700910319      * CONTROLLO SE ALTRA SELEZIONE
034800910319     C                   GOTO      LEGGI
034900940712     C                   ENDIF
035000911010      **
035100911010      ** IMMESSO FOGLIO VIAGGIO
035200910319     C                   ELSE
035300910319      * VISUALIZZO
035400910319     C                   EXSR      VISNFV
035500910319      *
035600910319     C                   GOTO      INIZIO
035700900517     C                   END
035800900515      *
035900000000     C     FINE          TAG
036000940802     C                   MOVEL     DSLR01        KPJBU
036100000000     C                   SETON                                        LR
036200920806      *-----------------------------------------------------***********
036300920806      * CARICO TABELLE                                         CARTAB
036400920806      *-----------------------------------------------------***********
036500920806     C     CARTAB        BEGSR
036600940711     C***
036700940711     C* CARICO TIPI INVENTARIO
036800940711     C***
036900940711     C                   MOVEL     *BLANKS       VIDSP1
037000940711     C                   MOVEL     *BLANKS       DESSP1
037100940711     C                   MOVEL     *BLANKS       VIDSP2
037200940711     C                   MOVEL     *BLANKS       DESSP2
037300940711     C                   MOVEL     *BLANKS       VIDSP3
037400940711     C                   MOVEL     *BLANKS       DESSP3
037500940711     C                   MOVEL     *BLANKS       VIDSP4
037600940711     C                   MOVEL     *BLANKS       DESSP4
037700940711     C                   MOVEL     *BLANKS       VIDSP5
037800940711     C                   MOVEL     *BLANKS       DESSP5
037900940711     C                   MOVEL     *BLANKS       VIDSP6
038000940711     C                   MOVEL     *BLANKS       DESSP6
038100940711     C*
038200940711     C                   MOVEL     '7B'          COD
038300940711     C                   Z-ADD     1             I                 2 0
038400940711     C     KTAB          SETLL     TABEL
038500940711     C     KTAB          READE     TABEL                                  30
038600940711     C*
038700940711     C     *IN30         DOWEQ     '0'
038800940711     C                   MOVEL     TBLKEY        TIN(I)
038900940711     C                   MOVEL     TBLUNI        DIN(I)
039000940711     C*
039100940711     C                   ADD       1             I
039200940711     C     KTAB          READE     TABEL                                  30
039300940711     C                   END
039400940711     C* EMETTO A VIDEO I PRIMI 6
039500940711     C     TIN(1)        IFNE      *BLANKS
039600940711     C                   MOVEL     TIN(1)        VIDSP1
039700940711     C                   MOVE      ' ='          VIDSP1
039800940711     C                   MOVEL     DIN(1)        DESSP1
039900940711     C     TIN(2)        IFNE      *BLANKS
040000940711     C                   MOVEL     TIN(2)        VIDSP2
040100940711     C                   MOVE      ' ='          VIDSP2
040200940711     C                   MOVEL     DIN(2)        DESSP2
040300940711     C     TIN(3)        IFNE      *BLANKS
040400940711     C                   MOVEL     TIN(3)        VIDSP3
040500940711     C                   MOVE      ' ='          VIDSP3
040600940711     C                   MOVEL     DIN(3)        DESSP3
040700940711     C     TIN(4)        IFNE      *BLANKS
040800940711     C                   MOVEL     TIN(4)        VIDSP4
040900940711     C                   MOVE      ' ='          VIDSP4
041000940711     C                   MOVEL     DIN(4)        DESSP4
041100940711     C     TIN(5)        IFNE      *BLANKS
041200940711     C                   MOVEL     TIN(5)        VIDSP5
041300940711     C                   MOVE      ' ='          VIDSP5
041400940711     C                   MOVEL     DIN(5)        DESSP5
041500940711     C     TIN(6)        IFNE      *BLANKS
041600940711     C                   MOVEL     TIN(6)        VIDSP6
041700940711     C                   MOVE      ' ='          VIDSP6
041800940711     C                   MOVEL     DIN(6)        DESSP6
041900940711     C                   END
042000940711     C                   END
042100940711     C                   END
042200940711     C                   END
042300940711     C                   END
042400940711     C                   END
042500940711     C***
042600940711     C* VEDO SE SONO SIMFEL O REMOTO
042700940711     C***
042800020513     c     simtpp        ifeq      '2'
042900020513     c     simtpp        oreq      *blanks
043000020513     c                   movel     simpou        v0cfgs
043100940711     C                   SETON                                        05
043200940711     C                   ELSE
043300940711     C                   MOVEL     SIMFEL        V0CFGS
043400940711     C                   END
043500050512     c                   exsr      chainfgs
043600050512     c
043700940711     C***
043800940711     C* CARICO CATEGORIE FOGLI
043900940711     C***
044000940711     C                   MOVEL     '7N'          COD
044100940711     C                   Z-ADD     0             I
044200940711     C     KTAB          SETLL     TABEL
044300940711     C     KTAB          READE     TABEL                                  31
044400940711     C*
044500940711     C     *IN31         DOWEQ     '0'
044600940711     C                   MOVEL     TBLUNI        DS7N
044700940711     C* SOLO SE ABILITATA PER FOGLI VARI
044800940711     C     §7NFFV        IFEQ      'S'
044900940711     C     §7NUTI        ANDNE     '9'
045000940711     C                   ADD       1             I
045100940711     C                   MOVEL     ' ='          DEC(I)
045200940711     C                   MOVEL     ALF1          DEC(I)
045300940711     C                   MOVE      §7NDE2        DEC(I)
045400940711     C     I             IFEQ      6
045500940711     C                   SETON                                        31
045600940711     C                   END
045700940711     C                   END
045800940711     C*
045900940711     C  N31KTAB          READE     TABEL                                  31
046000940711     C                   END
046100940711     C*
046200940711     C* IMPOSTO LE DESCRIZIONI A VIDEO
046300940711     C     DEC(1)        IFNE      *BLANKS
046400940711     C                   MOVEL     DEC(1)        DESCA1
046500940711     C     DEC(2)        IFNE      *BLANKS
046600940711     C                   MOVEL     DEC(2)        DESCA2
046700940711     C     DEC(3)        IFNE      *BLANKS
046800940711     C                   MOVEL     DEC(3)        DESCA3
046900940711     C     DEC(4)        IFNE      *BLANKS
047000940711     C                   MOVEL     DEC(4)        DESCA4
047100940711     C     DEC(5)        IFNE      *BLANKS
047200940711     C                   MOVEL     DEC(5)        DESCA5
047300940711     C     DEC(6)        IFNE      *BLANKS
047400940711     C                   MOVEL     DEC(6)        DESCA6
047500940711     C                   END
047600940711     C                   END
047700940711     C                   END
047800940711     C                   END
047900940711     C                   END
048000940711     C                   END
048100940711     C***
048200940711     C* TESTATA
048300940711     C***
048400940712     C                   MOVE      TES(1)        V0CTES
048500941027     C***
048600941027     C* CARICO TABELLA FILIALI GESTITE £1
048700941027     C***
048800941027     C                   CLEAR                   DSUL06
048900941027     C                   MOVE      '£1'          D06COD
049000941027     C                   MOVEL     SIMFEL        D06KEY
049100941027     C                   MOVEL     'L'           D06TLA
049200941027     C                   MOVEL     DSUL06        KPJBU
049300941027     C*
049400941027     C                   CALL      'TRUL06R'
049500941027     C                   PARM                    KPJBA
049600941027     C                   MOVEL     KPJBU         DSUL06
049700941027     C                   MOVEA     LIN           L1
049800920806     C                   ENDSR
049900050512      *-----------------------------------------------------***********
050000050512     c* Impostazione iniziale p.o. gestione
050100050512      *-----------------------------------------------------***********
050200050512     c     CHAINFGS      BEGSR
050300050512     C     V0CFGS        CHAIN     AZORG                              31
050400050512     C  N31              MOVEL     ORGDES        V1DFGS
050500050512     C  N31              MOVEL     ORGDES        V0dFGS
050600050512     c  n31              movel     orgdf0        og150
050700050512     c   31              clear                   og150
050800050512     c   31              clear                   v0dfgs
050900050512     c   31              clear                   v1dfgs
051000050512     c* p.o. con piking messaggerie
051100050512     c                   if        §ogpkm='S'
051200050512     c                   seton                                        08
051300050512     c                   endif
051400050512     c
051500050512     c* salvo campi di comodo
051600050512     c                   eval      fgspkm=§ogpkm
051700050512     c                   eval      savfgs=v0cfgs
051800050512     c                   eval      savin08=*in08
051900050512     c                   ENDSR
052000900515      *-----------------------------------------------------***********
052100940712      * PULIZIA CAMPI VIDEO                                 *  INZD00 *
052200900515      *-----------------------------------------------------***********
052300940712     C     INZD00        BEGSR
052400900515      *
052500950119     C                   MOVEL     'FNLR02R'     V0CPGM
052600940711     C                   MOVEL     ' '           V0CNPG
052700940711     C                   Z-ADD     0             V0CNFV
052800940711     C                   Z-ADD     0             V0CDIN
052900940711     C                   Z-ADD     31129999      V0CDFI
053000940711     C                   MOVEL     *BLANKS       V0CTFV
053100900515      *
053200900514     C                   ENDSR
053300900515      *-----------------------------------------------------***********
053400900517      * RICERCA E SCELTA FOGLIO DI VIAGGIO                  *  RICERC *
053500900515      *-----------------------------------------------------***********
053600900517     C     RICERC        BEGSR
053700941027     C*
053800010509     C  N14              Z-ADD     0             NR2               4 0
053900010509     C   14              Z-ADD     0             NR3               4 0
054000900517      *
054100900517      * RIEMPIO IL SFL
054200040310if  1c                   if        DLRtfv <> '3'
054300010509     C     *IN14         IFEQ      *OFF
054400900517     C                   Z-ADD     1             NR2
054500941229     C                   SETOFF                                       211310
054600940712     C                   WRITE     LR01CT2
054700940712     C                   SETON                                        202123
054800010509     C                   ENDIF
054900010509     C     *IN14         IFEQ      *ON
055000010509     C                   Z-ADD     1             NR3
055100010509     C                   SETOFF                                       211310
055200010509     C                   WRITE     LR01CT3
055300010509     C                   SETON                                        202123
055400010509     C                   ENDIF
055500040310e   1c                   endif
055600911205     C*
055700940711     C* 13 ON - RICHIESTI SOLO FOGLI APERTI
055800940712     C     V0CTFV        IFEQ      '2'
055900040112     c     V0Ctfv        oreq      '3'
056000911205     C                   SETON                                        13
056100911205     C                   END
056200941229     C* 10 ON - IMMESSO FILIALE GESTIONE
056300941229     C     V0CFGS        IFGT      0
056400941229     C                   SETON                                        10
056500941229     C                   END
056600900517      *
056700020905     c                   Select
056800020905     c                   When      *In10 and Not *In13
056900020905     c     Kdat1         setll     fnfvv008
057000020905     c                   When      *In10 and *In13
057100020905     C     KFVV3         SETLL     FNFVV003
057200020905     c                   When      Not *In10 and *In13
057300020905     C     *LOVAL        SETLL     FNFVV003
057400020905     c                   Other
057500020905     c     Kdat          setll     fnfvv002
057600020905     c                   EndSl
057700911205     C*
057800020905     c                   Select
057900020905     c                   When      *In10 and Not *In13
058000020905     c     Kdat2         Reade     Fnfvv008                               88
058100020905     c                   When      *In10 and *In13
058200020905     C     KFVV3         READE     FNFVV003                               88
058300020905     c                   When      Not *In10 and *In13
058400020905     C                   READ      FNFVV003                               88
058500020905     c                   Other
058600020905     c     Wnpg          Reade     Fnfvv002                               88
058700020905     c                   EndSl
058800910319      *
058900040310do  1C     *IN88         DOWEQ     *OFF
059000910319      *
059100040310if  2C     FVVDFV        IFLE      DATFIW
059200900529      *
059300010515     C                   SETOFF                                       96
059400911205      *
059500911205     C   13FVVDFV        IFGE      DATINW
059600010515     C                   SETON                                        96
059700911205     C                   END
059800900529      *
059900910319      * SELEZIONATI SOLO I CHIUSI
060000940712     C     V0CTFV        IFEQ      '1'
060100910319     C     FVVFCF        ANDNE     ' '
060200010515     C                   SETON                                        96
060300900529     C                   END
060400900529      *
060500940712     C     V0CTFV        IFEQ      ' '
060600010515     C                   SETON                                        96
060700900529     C                   END
060800941027     C*
060900940711     C* CONTROLLO CHE SIA FOGLIO DI FILIALE IN GESTIONE
061000040310if  3C   96V0CFGS        IFNE      0
061100010515     C   96FVVFGS        IFNE      V0CFGS
061200010515     C                   SETOFF                                       96
061300941027     C                   ENDIF
061400941027     C*
061500040310x   3C                   ELSE
061600941027     C*
061700941027     C* CARICO SOLO LE FILIALE NON DELL'AREA
061800941027     C     FVVFGS        LOOKUP    L1                                     30
061900010515     C   30              SETOFF                                       96
062000040310e   3C                   ENDIF
062100020611
062200020612      * richiesta la parzializzazione per tipo bolla
062300020612     c                   if        *in14 and v0ctpb <> 'N'
062400020611     c                             and *in96 = *on
062500020612     c                   exsr      rictpb
062600020611     c                   endif
062700031111
062800031111      * richiamato da FIMS03R, che passa i F.V. già inseriti
062900040310if  3c                   if        SDSprm > 1
063000031111     c                   eval      *in30  = *off
063100031111     c     FVVnfv        lookup    $FV                                    30
063200040310if  4c                   if        *in30  = *on
063300031111     c                   eval      *in96  = *off
063400040310e   4c                   endif
063500040310e   3c                   endif
063600031112
063700031112      * richiamato SOLO per sapere se esistono distinte aperte
063800031112      *   non inserito nel piano di cui sono stati passati i F.V.
063900040310if  3c                   if            SDSprm > 1
064000031112     c                             and DLRtfv = '3'
064100040310     c                             and *in96  = *on
064200031112     c                   eval      DLRfs1 = *on
064300031112     c                   goto      FINE
064400040310e   3c                   endif
064500050512     c* Parzializzazione per tipo zone consegna parcel/messaggerie
064600050512     c                   movel     fvvspn        dfvvspn
064700050512     c
064800050512     c                   if        *in08 and v0ctpm<>' '
064900050512     c                   if        (v0ctpm='M' and §fvvtpm<>'M') or
065000050512     c                             (v0ctpm='P' and §fvvtpm ='M')
065100050512     c                   eval      *in96  = *off
065200050512     c                   endif
065300050512     c                   endif
065400910319      *
065500010515      * 96 ON - CARICO RECORD
065600040310if  3C     *IN96         IFEQ      *ON
065700900529      *
065800010509     C  N14              CLEAR                   V2CSCE
065900010509     C  N14              CLEAR                   V2CANN
066000010509     C  N14              CLEAR                   V2CFCF
066100010509     C   14              CLEAR                   V3CSCE
066200010509     C   14              CLEAR                   V3CANN
066300010509     C   14              CLEAR                   V3CFCF
066400010510     C   14              CLEAR                   V3CTPD
066500050512     C   14              CLEAR                   V3CTPM
066600940712     C* GIRO DATA FOGLIO
066700940712     C                   MOVE      FVVDFV        G02INV
066800940712     C                   MOVEL     '3'           G02ERR
066900940712     C                   CALL      'XSRDA8'
067000940712     C                   PARM                    WLBDAT
067100940712     C*
067200010509     C  N14              Z-ADD     G02DAT        V2CDFV
067300010510     C   14              Z-ADD     G02DAT        V3CDFV
067400900517      *
067500010509     C  N14              Z-ADD     FVVNFV        V2CNFV
067600010509     C  N14              MOVEL     FVVDSF        V2CDSF
067700010509     C   14              Z-ADD     FVVNFV        V3CNFV
067800010509     C   14              MOVEL     FVVDSF        V3CDSF
067900911003      * TIPO INVENTARIO
068000040310if  4C     *IN14         IFEQ      *OFF
068100940712     C                   MOVEL     *BLANKS       V2DINV
068200940712     C     FVVSPG        IFNE      ' '
068300940712     C                   Z-ADD     1             I
068400940712     C     FVVSPG        LOOKUP    TIN(I)                                 30
068500940712     C   30              MOVEL     DIN(I)        V2DINV
068600940712     C  N30              MOVEL     FVVSPG        V2DINV
068700940712     C                   ENDIF
068800040310e   4C                   ENDIF
068900940712     C* FILIALE GESTIONE
069000010509     C  N14              MOVEL     FVVFGS        V2CFGS
069100050512     C** 14              MOVEL     FVVFGS        V3CFGS
069200940712     C     FVVFGS        CHAIN     AZORG                              30
069300010509     C  N14
069400010509     CANN30              MOVEL     ORGDES        V2DFGS
069500010509     C  N14
069600010509     CAN 30              MOVEL     *BLANKS       V2DFGS
069700010509      *
069800020611      * se richiesto tipo foglio "4"
069900040310if  4C                   IF        *in14 = *ON
070000050512     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
070100050512     c                   if        *in08
070200050512     c                   if        §fvvtpm='M'
070300050512     c                   movel     'Mes'         v3ctpm
070400050512     c                   endif
070500050512     c                   if        §fvvtpm<>'M'
070600050512     c                   movel     'Prc'         v3ctpm
070700050512     c                   endif
070800050512     c                   endif
070900050512     c
071000020611      * Flag tipo distinta (solo consegna, orm o entrambi)
071100010510     C                   CLEAR                   WBOL
071200010510     C                   CLEAR                   WORM
071300050512     c                   Z-Add     v0cfgs        Wifp
071400020910     C                   Z-ADD     V3CNFV        kfnvv
071500010510     C                   Z-ADD     V3CNFV        W0CNFV
071600020910     c     Karb78        Setll     Fnarb78l
071700040310if  5c                   if        %equal(fnarb78l)
071800010510     c                   movel     'S'           wbol              1
071900040310x   5c                   else
072000070112     C     KBRV          SETLL     FNBRV01L
072100070112     c                   if        %equal(fNbrv01l)
072200010510     c                   movel     'S'           wbol              1
072300010510     c                   endif
072400040310e   5c                   endif
072500010510     C     KORM          SETLL     FNORM06L
072600040310do  5c                   do        *hival
072700010511     C     KORM          reade     FNORM06L
072800010511     c                   if        %eof(fnorm06l)
072900010511     c                   leave
073000010511     c                   endif
073100010511     c                   if        ormatb = *blanks and ormfao < 999
073200010510     C                   move      'S'           worm              1
073300010510     c                   endif
073400040310e   5c                   enddo
073500010510     C                   if        wbol = 'S' and worm <> 'S'
073600020611     C                   movel     'CON'         v3ctpd
073700010510     C                   endif
073800010510     C                   if        wbol <> 'S' and worm = 'S'
073900020611     C                   movel     'ORM'         v3ctpd
074000010510     C                   endif
074100010510     C                   if        wbol = 'S' and worm = 'S'
074200020611     C                   movel     'C/O'         v3ctpd
074300010510     C                   endif
074400020612      * Calcolo importi se non già fatto prima
074500040310if  5c                   if        v0ctfv = '2' and v0impo = 'S'
074600020612     c                   if        v0ctpb = 'N'
074700020423     c                   exsr      totalizzo
074800020423     c                   z-add     w3imas        v3imas
074900020423     c                   z-add     w3imca        v3imca
075000020612     c                   endif
075100040310x   5c                   else
075200020423     c                   z-add     0             v3imas
075300020423     c                   z-add     0             v3imca
075400040310e   5c                   endif
075500020611
075600020612      * Tipo bolla
075700020612     c                   clear                   v3ctpb
075800040310if  5c                   if        v0ctpb <> 'N'
075900020611     c                   select
076000020611     c                   when      wbar = *on  and wppt = *off
076100020612     c                   eval      v3ctpb = 'BAR'
076200020611     c                   when      wbar = *off and wppt = *on
076300020612     c                   eval      v3ctpb = 'PPT'
076400020611     c                   when      wbar = *on  and wppt = *on
076500020612     c                   eval      v3ctpb = 'B/P'
076600020611     c                   endsl
076700040310e   5c                   endif
076800020611
076900040310e   4C                   ENDIF
077000911008      *
077100911008      * ANNULLATO
077200910319     C     FVVATB        IFEQ      '*'
077300010509     C  N14              MOVEL     'ANN'         V2CANN
077400010509     C   14              MOVEL     'ANN'         V3CANN
077500900517     C                   END
077600961016      * DATI IN RICEZIONE
077700040310if  4C     FVVFSC        IFEQ      'B'
077800010509     C  N14              MOVE      'DR'          V2CFCF
077900010509     C   14              MOVE      'DR'          V3CFCF
078000040310x   4C                   ELSE
078100040310if  5C     FVVFCF        IFNE      ' '
078200940712      * CHIUSO
078300940712     C     FVVFCF        IFEQ      'S'
078400010509     C  N14              MOVEL     'C '          V2CFCF
078500010509     C   14              MOVEL     'C '          V3CFCF
078600940712     C                   ELSE
078700940712     C* BLOCCATO
078800010509     C  N14              MOVEL     'BL'          V2CFCF
078900010509     C   14              MOVEL     'BL'          V3CFCF
079000010509     C                   ENDIF
079100040310e   5C                   ENDIF
079200040310e   4C                   ENDIF
079300900517      *
079400040310if  4c                   if        DLRtfv <> '3'
079500010509     C  N14              WRITE     LR01SF2
079600010509     C  N14              ADD       1             NR2
079700010509     C   14              WRITE     LR01SF3
079800010509     C   14              ADD       1             NR3
079900040310e   4c                   endif
080000900517      *
080100040310e   3C                   ENDif
080200910319      *
080300020905     c                   Select
080400020905     c                   When      *In10 and Not *In13
080500020905     c     Kdat2         Reade     Fnfvv008                               88
080600020905     c                   When      *In10 and *In13
080700020905     C     KFVV3         READE     FNFVV003                               88
080800020905     c                   When      Not *In10 and *In13
080900020905     C                   READ      FNFVV003                               88
081000020905     c                   Other
081100020905     c     Wnpg          Reade     Fnfvv002                               88
081200020905     c                   EndSl
081300900517      *
081400040310x   2C                   ELSE
081500911205     C  N13              SETON                                        88
081600941229     C   10
081700941229     CAN 13KFVV3         READE     FNFVV003                               88
081800941229     C  N10
081900941229     CAN 13              READ      FNFVV003                               88
082000040310e   2C                   ENDif
082100910319      *
082200040310e   1C                   ENDdo
082300900517      *
082400900517      * NON TROVATI FOGLI IN INTERVALLO RICHIESTO
082500040630     C**n14NR2           IFEQ      1
082600040630     C  n14NR2           IFLE      1
082700920806     C                   SETON                                        48
082800900517     C                   END
082900040630     C***14NR3           IFEQ      1
083000040630     C   14NR3           IFLE      1
083100010517     C                   SETON                                        48
083200010517     C                   END
083300900517      *
083400920806     C                   ENDSR
083500020611      *-----------------------------------------------------
083600020612      * RICERCA PER TIPO BOLLA
083700020611      *-----------------------------------------------------
083800020612     c     rictpb        begsr
083900020611
084000020611     c                   eval      wbar = *off
084100020611     c                   eval      wppt = *off
084200020612     c                   clear                   w3imca           13 3
084300020612     c                   clear                   w3imas           13 3
084400020611
084500020910     c                   Z-Add     fvvfgs        wifp
084600020611     c                   z-add     fvvnfv        kfnvv
084700020910     c     karb78        setll     fnarb78l
084800020910 b1  c                   if        not %equal(fnarb78l)
084900020612     c                   if        v0ctpb <> 'T'
085000020611     c                   eval      *in96 = *off
085100020612     c                   endif
085200020612 x1  c                   else
085300020612 b2  c                   do        *hival
085400020910     c     karb78        reade     fnarb78l
085500020910     c                   if        %eof(fnarb78l)
085600020611     c                   leave
085700020611     c                   endif
085800020611     c                   if        arbtsp <> 'P'
085900020611     c                   eval      wbar = *on
086000020611     c                   else
086100020611     c                   eval      wppt = *on
086200020611     c                   endif
086300020612
086400020612      * se richiesto ormai che ci sono totalizzo gli assegnati
086500020612 b3  c                   if        v0ctfv = '2' and v0impo = 'S'
086600020612     c                   if        arbica = *blank or arbica = 'R'
086700020612     c     kimpo         chain     fiar601l                           71
086800020612     c  n71              add       ar6ift        w3imas
086900020612     c                   endif
087000020612      * totalizzo i contrassegni
087100020612 b4  c                   if        arbicc = *blank or arbicc = 'R'
087200051114     c     kimpo         chain     fiar901l                           71
087300020612 b5  c                   if        not *in71
087400020612 b6  c                   if        ar9vca = 'EUR'
087500020612     c                   add       ar9cas        w3imca
087600020612 x6  c                   else
087700020612      *se la divisa non è EUR traduco con yeurco l'importo
087800020612     c                   CLEAR                   YEURDS
087900020612     c                   MOVEL     ar9vca        YECDVI
088000020612     c                   Z-ADD     ar9cas        YECIMI
088100020612     c                   MOVEL     'EUR'         YECDVO
088200020612     c                   MOVEL     'H'           YECTAR
088300020612     c                   CALL      'YEURCO'
088400020612     c                   PARM                    YEURDS
088500020612     c                   add       YECIMO        W3imca
088600020612 e6  c                   endif
088700020612 e5  c                   endif
088800020612 e4  c                   endif
088900020612
089000020612 x3  c                   else
089100020612     c                   z-add     0             v3imas
089200020612     c                   z-add     0             v3imca
089300020612 e3  c                   endif
089400020612
089500020612 e2  c                   enddo
089600020611
089700020611     c                   select
089800020612     c                   when      v0ctpb = 'B' and wppt = *on
089900020611     c                   eval      *in96 = *off
090000020612     c                   when      v0ctpb = 'P' and wbar = *on
090100020611     c                   eval      *in96 = *off
090200020612     c                   when      v0ctpb = 'M' and (wbar = *off or wppt = *off)
090300020611     c                   eval      *in96 = *off
090400020611     c                   endsl
090500020612 e1  c                   endif
090600020611
090700020611     c                   endsr
090800900517      *-----------------------------------------------------***********
090900940711      * CONTROLLI FORMATO 0                                 *  CTRD00 *
091000900517      *-----------------------------------------------------***********
091100940711     C     CTRD00        BEGSR
091200050512     C                   SETOFF                                       9008
091300941027     C*
091400941027     C* SE IMMESSA DEVE ESSERE FILIALE VALIDA
091500050512     c                   clear                   fgspkm
091600050512     c                   clear                   v0dfgs
091700941027     C     V0CFGS        IFNE      0
091800941027     C     V0CFGS        CHAIN     AZORG                              30
091900941027     C     *IN30         IFEQ      *ON
092000941027     C                   MOVEL     MSG(1)        V0CMSG
092100941027     C                   SETON                                        439028
092200941027     C                   GOTO      ENDCT0
092300050512     c                   else
092400050512     c                   movel     orgdf0        og150
092500050512     c                   movel     §ogpkm        fgspkm
092600050512     c                   movel     orgdes        v0dfgs
092700050512     c
092800050512     c* P.o. con picking, messaggerie: visualizzo parzializzazione
092900050512     c                   if        fgspkm='S'
093000050512     c                   seton                                        08
093100050512    1C                   ENDif
093200050512    1C                   END
093300941027    1C                   END
093400050512     c
093500050512     c* Filiale apertura obbligatoria se categoria 4
093600050512     c                   if        v0cnpg='4' and v0cfgs=0
093700050512     C                   MOVEL     MSG(2)        V0CMSG
093800050512     C                   SETON                                        439028
093900050512     C                   GOTO      ENDCT0
094000050512     c                   endif
094100050512     c
094200050512     c* riemetto video 1 senza errori se per cambio p.o. gestione, cambia
094300050512     c*  l'indicatore del picking messaggerie S/N
094400050512     c                   if        v0cfgs<>savfgs
094500050512     c                   if        (savin08='0' AND *IN08)   OR
094600050512     c                             (savin08='1' and not *in08)
094700050512     c                   seton                                        09
094800050512     c                   endif
094900050512     c                   eval      savfgs=v0cfgs
095000050512     c                   eval      savin08=*in08
095100050512     c                   endif
095200910319      *
095300910319      * CATEGORIA FOGLIO VIAGGIO OBBLIGATORIA
095400940712    1C     V0CNPG        IFEQ      ' '
095500910319     C                   SETON                                        4290
095600940711     C                   GOTO      ENDCT0
095700940712    1C                   END
095800920806     C* RICERCA IN TABELLA
095900940712    1C     V0CNPG        IFEQ      '?'
096000920806     C                   MOVEL     CODUT         §KUT
096100920806     C                   MOVEL     '7N'          §COD
096200940712     C                   MOVE      ' '           V0CNPG
096300920806     C                   MOVEL     *BLANKS       §KEY
096400920806     C                   CALL      'X§TABER'
096500920806     C                   PARM                    §KUT              1 0
096600920806     C                   PARM                    §COD              2
096700920806     C                   PARM                    §KEY              8
096800920806     C                   PARM                    §DES             30
096900940712     C                   MOVEL     §KEY          V0CNPG
097000920806     C                   SETON                                        90
097100940711     C                   GOTO      ENDCT0
097200940712    1C                   END
097300920806     C* CONTROLLO
097400920806     C                   MOVEL     '7N'          COD
097500920806     C                   MOVEL     *BLANKS       KEY
097600940712     C                   MOVEL     V0CNPG        KEY
097700920806     C* INESISTENTE
097800920806     C     KTAB2         CHAIN     TABEL                              42
097900920806     C* ANNULLATA
098000940712    1C  N42TBLFLG        IFNE      ' '
098100920806     C                   SETON                                        42
098200920806     C                   ELSE
098300920806     C                   MOVEL     TBLUNI        DS7N
098400920806     C* NON E' CATEGORIA DI FOGLI VARI
098500940712    2C     §7NFFV        IFNE      'S'
098600920806     C                   SETON                                        42
098700940712    2C                   END
098800940712    1C                   END
098900920806     C*
099000920806     C   42              SETON                                        90
099100940711     C   42              GOTO      ENDCT0
099200940712     C                   MOVEL     §7NDES        V1DNPG
099300940713     C                   MOVEL     §7NDES        V0DNPG
099400940712     C                   MOVEL     V0CNPG        WNPG
099500900517      *
099600900517      * CONTROLLO DATA INIZIALE
099700900517      *
099800940712    1C     V0CDIN        IFNE      0
099900940712     C                   MOVE      V0CDIN        G02DAT
100000900517     C                   MOVEL     *BLANK        G02ERR
100100940711     C                   CALL      'XSRDA8'
100200900517     C                   PARM                    WLBDAT
100300940712    2C     G02ERR        IFEQ      '1'
100400900517     C                   SETON                                        76  90
100500940711     C                   GOTO      ENDCT0
100600940712    2C                   END
100700940711     C*
100800940711     C                   Z-ADD     G02INV        DATINW
100900940712     C                   Z-ADD     G02DAT        V0CDIN
101000940711     C*
101100940712   X1C                   ELSE
101200940711     C*
101300940711     C                   Z-ADD     0             DATINW
101400940712    1C                   END
101500900517      *
101600900517      * CONTROLLO DATA FINALE
101700900517      *
101800940712    1C     V0CDFI        IFEQ      0
101900940712     C                   Z-ADD     31129999      V0CDFI
102000940712     C                   Z-ADD     99991231      DATFIW
102100940712   X1C                   ELSE
102200900529      *
102300940712     C                   MOVE      V0CDFI        G02DAT
102400900517     C                   MOVEL     *BLANK        G02ERR
102500940712     C                   CALL      'XSRDA8'
102600900517     C                   PARM                    WLBDAT
102700940712    2C     G02ERR        IFEQ      '1'
102800900517     C                   SETON                                        77  90
102900940711     C                   GOTO      ENDCT0
103000940712    2C                   END
103100940711     C*
103200940711     C                   Z-ADD     G02INV        DATFIW
103300940712     C                   Z-ADD     G02DAT        V0CDFI
103400900517      *
103500900517      * DATA INIZIALE MAGGIORE DATA FINALE
103600940712    2C     DATINW        IFGT      DATFIW
103700900517     C                   SETON                                        75  90
103800940711     C                   GOTO      ENDCT0
103900940712    2C                   END
104000940712    1C                   END
104100940711     C*
104200940711      * NUMERO FOGLIO
104300940711     C*
104400940712    1C     V0CNFV        IFGT      0
104500940711     C* FOGLIO VIAGGIO
104600940712     C     KFVV          CHAIN     FNFVV000                           38
104700940711     C*
104800940711     C* VEDO SE E' FOGLIO DI FILIALE GESTIBILE
104900940712     C* FOGLIO INESISTENTE
105000941027    2C     *IN38         IFEQ      *ON
105100940712     C                   SETON                                          90
105200940712     C                   GOTO      ENDCT0
105300940712    2C                   ENDIF
105400940712    1C                   ENDIF
105500050512     c* Per visualizzare gli importi rchiedere tipi fogli = 2
105600050512     c                   if        v0impo='S' and v0ctfv<>'2'
105700050512     C                   MOVEL     MSG(3)        V0CMSG
105800050512     C                   SETON                                        469028
105900050512     C                   GOTO      ENDCT0
106000050512     c                   endif
106100940711     C*
106200940711     C     ENDCT0        ENDSR
106300900515      *-----------------------------------------------------***********
106400900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
106500900515      *-----------------------------------------------------***********
106600900515     C     RIEMPI        BEGSR
106700900515      *
106800940712     C                   SETOFF                                       525306
106900050512     C                   SETOFF                                       07
107000940712     C* FILIALE GESTIONE
107100940712     C     FVVFGS        CHAIN     AZORG                              30
107200940713     C  N30              MOVEL     ORGDES        V1DFGS
107300940712     C* DATA FOGLIO
107400910319     C                   MOVE      FVVDFV        G02INV
107500910319     C                   MOVE      *ZEROS        G02DAT
107600940712     C                   MOVEL     '3'           G02ERR
107700940712     C                   CALL      'XSRDA8'
107800910319     C                   PARM                    WLBDAT
107900940712     C                   Z-ADD     G02DAT        V1CDFV
108000910319      *
108100940712     C                   MOVE      FVVDSF        V1CDSF
108200050512     C*                  MOVE      FVVSPN        V1CSPN
108300050512     C                   MOVEl     FVVSPN        dfvvspn
108400050512     c                   movel     §fvvspn       v1cspn
108500940712     C                   MOVEL     FVVSPG        V1CSPG
108600050512     C* FOGLIO INVENTARIO
108700940712     C     FVVNPG        IFEQ      3
108800940712     C                   SETON                                        02
108900940712     C                   END
109000911003     C*
109100910319      * VEDO SE ANNULLATO
109200910319     C     FVVATB        IFEQ      '*'
109300940712     C                   SETON                                        5206
109400940712     C                   MOVEL     CANNUL        V1DTFV
109500910319     C                   END
109600910319      *
109700910319      * VEDO SE CHIUSO
109800910319     C     FVVFCF        IFNE      *BLANKS
109900940712     C     FVVFCF        IFEQ      'S'
110000910319     C                   SETON                                        53
110100940712     C                   ELSE
110200940712     C                   SETON                                        06
110300940712     C                   MOVEL     CBLOCC        V1DTFV
110400940712     C                   ENDIF
110500940712     C                   ENDIF
110600941027     C*
110700941027     C* 07 ON ESISTONO SPUNTE SUL FOGLIO
110800000114     C                   Z-ADD     V0CNFV        W0CNFV
110900070112     C     KBRV          SETLL     FNBRV000                               07     ON=TROVATO
111000900515     C                   ENDSR
111100910319      *-----------------------------------------------------***********
111200910319      * VISUALIZZO FOGLIO VIAGGIO                             VISNFV
111300910319      *-----------------------------------------------------***********
111400910319     C     VISNFV        BEGSR
111500910319      *
111600910319     C* RIEMPO FORMATO
111700910319     C                   EXSR      RIEMPI
111800910319      *
111900950112     C     FMT01         TAG
112000940712     C                   EXFMT     LR01D01
112100950112     C*
112200950112     C* CMD21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
112300950112     C     *INKV         IFEQ      *ON
112400950112     C                   CLEAR                   PARA04
112500950112     C                   Z-ADD     V0CNFV        P04NFV
112600950112     C                   Z-ADD     99            P04NRS
112700950112     C                   Z-ADD     99            P04NPS
112800950112     C                   MOVEL     V0CNPG        P04NPG
112900950112     C                   MOVEL     '1'           P04FLG
113000950112     C                   MOVEL     'FNLR01R'     P04PGM
113100950112     C                   Z-ADD     V0CFGS        P04FGS
113200950112     C                   MOVEL     PARA04        KPJBU
113300950112     C                   CALL      'FNLR04R'
113400950112     C                   PARM                    KPJBA
113500950112     C                   GOTO      FMT01
113600950112     C                   ENDIF
113700910319      *
113800910319     C     ENDVIS        ENDSR
113900020423      *--------------------------------------------
114000020423     c     totalizzo     begsr
114100020423      *--------------------------------------------
114200020423     c                   clear                   w3imca           13 3
114300020423     c                   clear                   w3imas           13 3
114400020910     c     Karb78        setll     fnarb78l
114500020423     c                   do        *hival
114600020910     c     Karb78        reade     fnarb78l                               70
114700020423     c   70              leave
114800020423      * totalizzo gli assegnati
114900020423     c                   if        arbica = *blank or arbica = 'R'
115000020423     c     kimpo         chain     fiar601l                           71
115100020423     c  n71              add       ar6ift        w3imas
115200020423     c                   endif
115300020423      * totalizzo i contrassegni
115400020423     c                   if        arbicc = *blank or arbicc = 'R'
115500051114     c     kimpo         chain     fiar901l                           71
115600020423     c                   if        not *in71
115700020423     c                   if        ar9vca = 'EUR'
115800020423     c                   add       ar9cas        w3imca
115900020423     c                   else
116000020423      *se la divisa non è EUR traduco con yeurco l'importo
116100020423     c                   CLEAR                   YEURDS
116200020423     c                   MOVEL     ar9vca        YECDVI
116300020423     c                   Z-ADD     ar9cas        YECIMI
116400020423     c                   MOVEL     'EUR'         YECDVO
116500020423     c                   MOVEL     'H'           YECTAR
116600020423     c                   CALL      'YEURCO'
116700020423     c                   PARM                    YEURDS
116800020423     c                   add       YECIMO        W3imca
116900020423     c                   endif
117000020423     c                   endif
117100020423     c                   endif
117200020423     c                   enddo
117300020423     c                   endsr
117400020423      *--------------------------------------------
117500940711      * INZSR INIZIALE                                         INZSR
117600940711      *-----------------------------------------------------***********
117700940711     C     *INZSR        BEGSR
117800940711     C***
117900940711      * CHIAVI                                                        *
118000940711     C***
118100940711     C     KFVV          KLIST
118200940711     C                   KFLD                    WNPG
118300940711     C                   KFLD                    V0CNFV
118400941027     C                   KFLD                    V0CFGS
118500020910      * Fnarb
118600020910     c     Karb78        Klist
118700020910     c                   Kfld                    Wifp
118800020910     c                   Kfld                    Kfnvv
118900000114     C*
119000000114     C     KBRV          KLIST
119100000114     C                   KFLD                    WNPG
119200000114     C                   KFLD                    W0CNFV
119300000114     C                   KFLD                    V0CFGS
119400940711      *
119500010510     C     KORM          KLIST
119600010510     C                   KFLD                    V0CFGS
119700010510     C                   KFLD                    W0CNFV
119800010510      *
119900940711     C     KDAT          KLIST
120000940711     C                   KFLD                    WNPG
120100940711     C                   KFLD                    DATINW
120200020905
120300020905     c     Kdat1         Klist
120400020905     c                   Kfld                    V0cfgs
120500020905     c                   Kfld                    Wnpg
120600020905     c                   Kfld                    Datinw
120700020905
120800020905     c     Kdat2         Klist
120900020905     c                   Kfld                    V0cfgs
121000020905     c                   Kfld                    Wnpg
121100940711      *
121200940711     C     KTAB2         KLIST
121300940711     C                   KFLD                    CODUT
121400940711     C                   KFLD                    COD               2
121500940711     C                   KFLD                    KEY               8
121600940711     C     KTAB          KLIST
121700940711     C                   KFLD                    CODUT
121800940711     C                   KFLD                    COD               2
121900940711     C     KFVV3         KLIST
122000940711     C                   KFLD                    V0CFGS
122100940711     C                   KFLD                    WNPG
122200020423     C     Kimpo         Klist
122300020423     C                   Kfld                    arbaas
122400020423     C                   Kfld                    arblnp
122500020423     C                   Kfld                    arbnrs
122600020423     C                   Kfld                    arbnsp
122700940711     C* LIKE DEFN
122800940711     C     *LIKE         DEFINE    FVVDFV        DATINW
122900940711     C     *LIKE         DEFINE    FVVDFV        DATFIW
123000940711     C     *LIKE         DEFINE    FVVNPG        WNPG
123100000114     C     *LIKE         DEFINE    BRVNFV        W0CNFV
123200070329     C***
123300070329     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
123400070329     C                   TIME                    W0140            14 0
123500070329     C                   MOVE      W0140         UDATE8            8 0
123600070329     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
123700070329     C*
123800070329     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
123900070329     C                   MOVEL     *BLANK        G02ERR
124000070329     C                   CALL      'XSRDA8'
124100070329     C                   PARM                    WLBDAT
124200070329     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
124300070329      *
124400070329      *  Se richiamata da Azione/Menù
124500070329     c                   CLEAR                   se_EDP            1
124600070329     c                   if        %subst(Knmus:1:3) = 'EDP'
124700070329     c                   MOVE      'S'           se_EDP
124800070329     c                   end
124900940712     C*
125000940712     C                   ENDSR
125100940712     C*
125200941027**  PGM RICHIAMANTI
125300940711FNLR02R
125400940711FNLR25R
125500940711**  TES
125600920805 ** INTERROGAZIONE  FOGLI  VARI **
125700920709 ** ABBINAMENTO  FOGLI  VIAGGIO **
125800941027**
125900941027Filiale in gestione errata o mancante
126000050512Punto Operativo apertura Foglio obbligatorio per categoria 4
126100050512Importi visualizzabili soltanto richiedendo Tipo fogli=2 non chiusi
