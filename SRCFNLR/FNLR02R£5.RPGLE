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
003200160929     D UIN             S              1    DIM(30)                              DESCR TIP.INV
003300160929     D dbin            S              3    DIM(30)                              DESCR TIP.INV
003400900515      *
003500940711     D PGM             S             10    DIM(2) CTDATA PERRCD(1)              PGM RICHIAMANTI
003600940711     D TES             S             34    DIM(2) CTDATA PERRCD(1)              TESTATE
003700050512     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG DI ERRORE
003800911119      *
003900920806     D DEC             S             12    DIM(6)                               DESCR CATEGOR
004000941027     D L1              S              3  0 DIM(30)                              FIL GESTITE
004100020611
004200050512     d fgspkm          s                   like(§ogpkm)
004300050512     d savfgs          s                   like(v0cfgs)
004400050512     d savin08         s              1
004500050512     d kfnvv           s                   like(arbndc)
004600020910     d Wifp            s                   Like(Arbifp)
004700020611     d wbar            s              1    inz('0')
004800020611     d wppt            s              1    inz('0')
004900020611
005000031111      *
005100031111     d Status         sds           333
005200031111     d   SDSprm          *parms
005300900515      *
005400900514     D KPJBA         E DS
005500070329     D KPJBUs          s                   like(KPJBU)
005600070329     D fidg31ds      E DS
005700920806     D TBLKEY          DS
005800920806     D  ALF1                   1      1
005900900516      *
006000940802      *
006100940802     D* DS STANDARD FOGLI VARI
006200940802     D DSLR01        E DS
006300940802     D*
006400920806     D DS7N          E DS
006500920806     D  §7NDE2                 1     10
006600941027     D* DS PER TRUL06R - CARICAMENTO £X
006700941027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006800941027     D  LIN                    1     90  0
006900941027     D                                     DIM(30)                              FIL. COMODO
007000050512     D og150         E DS
007100070329     D og146         E DS
007200050512     D DFVVSPN       E DS
007300050512     D CNCR80        E DS
007400920806     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007500020423     d YEURDS        E DS                  EXTNAME(YEURCODS)
007600160929     d
007700160929     d ds7b          E DS
007800161125     d
007900161125     d TRULVPODS     e ds
008000161125     D  skFilOk               16    765    dim(250)
008100900515      *
008200940711     D***
008300940711     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008400940711     D***
008500900515     D WLBDAT          DS
008600940711     D  G02DAT                 1      8  0
008700940711     D  G02INV                 9     16  0
008800940711     D  G02ERR                17     17
008900940711     D  G02TGI                18     22  0
009000950112     D PARA04          DS
009100950112     D  P04NFV                 1      5  0
009200950112     D  P04LNP                 6      8  0
009300950112     D  P04LNA                 9     11  0
009400950112     D  P04NRS                12     13  0
009500950112     D  P04NCD                14     20  0
009600950112     D  P04NCA                21     27  0
009700950112     D  P04NPS                28     29  0
009800950112     D  P04NPG                30     30  0
009900950112     D  P04FLG                31     31
010000950112     D  P04PGM                32     38
010100950112     D  P04FGS                39     41  0
010200101202     d* bolla
010300101202     d  p04bolp               42     44  0
010400101202     d  p04bors               45     46  0
010500101202     d  p04bosp               47     53  0
010600101202     d  p04boan               54     57  0
010700101202     D  P04ERR                58     58
010800101202     d
010900940712     D***
011000940712     D* COSTANTI
011100940712     D***
011200940712     D CANNUL          C                   CONST('  Annullato  ')
011300940712     D CBLOCC          C                   CONST('  Bloccato   ')
011400910319      *---------------------------------------------------------------*
011500910319      * INDICATORI USATI
011600910319      *---------------------------------------------------------------*
011700940712      * 01    = PGM RICHIAMATO
011800940712      * 02    = CATEGORIA INVENTARIO 3
011900941027      * 03/05 = USATI IN GESTIONE FOGLI VARI
012000940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
012100941027      * 07    = EMETTO LA SCRITTA : ESISTONO SPUNTE SUL FOGLIO
012200050512      * 08    = p.o. gestione con picking messaggerie
012300050512      * 09    = riemetto video per cambio p.o. gestione anche senza errori
012400161125      * 10    = per filiale gestione
012500010509      * 14    = RICERCA DI FOGLI CATEGORIA '4'
012600031111      * 19    = Richiamato per visualizzazione f.v. non immessi in FIMS03D
012700031111      *         (non viene richiesta alcuna opzione).
012800940712      * 20/29 = SU SFL STD
012900050512      * 38/46 = ERRORE
013000161125      * 50/51 = abilitazioni per tipo IPG
013100940712      * 52    = RECORD ANNULLATO
013200940712      * 53    = FOGLIO CHIUSO
013300010515      * 56/58 = USATO COME ERRORE NEL FILE VIDEO
013400010515      * 60/61 = USATO COME ERRORE NEL FILE VIDEO
013500910319      * 75/77 = ERRORE SUI CAMPI
013600940712      * 88    = DI COMODO
013700940712      * 90    = ERRORE GENERICO
013800010515      * 95    = USATO COME ERRORE NEL FILE VIDEO
013900010515      * 96    = RECORD DA CARICARE IN SUBFILE
014000940712      * 98    = INTERROGAZIONE
014100940712      * 99    = PROTEZIONE CAMPI
014200910319      *--------------------------------------------------------------*
014300000000     C     *ENTRY        PLIST
014400000000     C                   PARM                    KPJBA
014500031111     c                   parm                    $FV
014600031111      *
014700940802     C                   MOVEL     KPJBU         DSLR01
014800900516      *
014900000000     C                   Z-ADD     1             CODUT
015000920806     C                   CALL      'X§PARUT'
015100920806     C                   PARM                    UT§DSE
015200940711     C                   MOVEL     RAGUT         V0CDSA
015300920806     C                   MOVEL     REC80         CNCR80
015400900515      *
015500900515      *---------------------------------------------------------------*
015600900515      * FLUSSO PRINCIPALE                                             *
015700900515      *---------------------------------------------------------------*
015800920806     C*
015900940711     C* CARICO TABELLE
016000940711     C                   EXSR      CARTAB
016100920806     C* PER L'INTERROGAZIONE
016200941027     C* LA FILIALE APERTURA FOGLIO E' SEMPRE ABILITATA
016300941027     C                   SETON                                        990498
016400940712     C*
016500940712     C* LA PULIZIA LA ESEGUO UNA VOLTA SOLA ALL'INIZIO
016600940712     C                   EXSR      INZD00
016700940712     C*
016800040310     C* 01 ON - PROGRAMMA RICHIAMATO
016900950615     C     DLRRIC        IFNE      ' '
017000940711     C* PRENDO LA TESTATA DAL NOME DEL PROGRAMMA
017100940712     C                   Z-ADD     1             X                 2 0
017200940802     C     DLRPGM        LOOKUP    PGM(X)                                 30
017300940711     C   30              MOVEL     TES(X)        V0CTES
017400941027     C* ACCENDO GLI INDICATORI DI PGM RICHIAMATO
017500941027     C                   SETON                                        01
017600031111      * Condiziono l'emissione del campo "1=Selezione" nei sfl
017700031111     c     SDSprm        comp      1                                  19
017800031112      * Ripulisco eventuale flag di ritorno per FIMS03R
017900031112      * (se FNLR02R richiamato per sapere se ci sono distinte aperte
018000031112      *  non inserite nel piano a video di FIMS03R)
018100031112     c                   if        DLRtfv = '3'
018200031112     c                   clear                   DLRfs1
018300031112     c                   endif
018400940711     C* IMPOSTO I CAMPI
018500940802     C                   MOVEL     DLRPGM        V0CPGM
018600940802     C                   MOVEL     DLRGDA        V0CDIN
018700940802     C                   MOVEL     DLRGAL        V0CDFI
018800940802     C                   MOVEL     DLRTFV        V0CTFV
018900940802     C                   MOVEL     DLRNPG        V0CNPG
019000940802     C                   MOVEL     DLRFGS        V0CFGS
019100050512     c                   exsr      chainfgs
019200940802     C                   MOVEL     DLRNFV        V0CNFV
019300950615     C*
019400950615     C* VEDO SE VISUALIZZARE LA PRIMA VIDEATA (SCELTA PARAMETRI) O NO
019500950615     C     DLRRIC        IFNE      '§'                                          '§' SI 1° VIDEO
019600020612     c                   move      'N'           v0impo
019700020612     c                   eval      v0ctpb = 'N'
019800031113if  2c                   if        SDSprm > 1
019900031113     c                   eval      V0ctpb = 'T'
020000031113e   2c                   endif
020100950615     C                   GOTO      FOR01
020200950615     C                   ENDIF
020300911010     C                   END
020400911004     C*
020500900515      *          =============
020600900509     C     INIZIO        TAG
020700900515      *          =============
020800950615     C                   CLEAR                   V0CNFV
020900050512     C                   CLEAR                   V0Ctpm
021000020423     c                   move      'N'           v0impo
021100020612     c                   eval      v0ctpb = 'N'
021200911010     C*
021300900515     C     FMT00         TAG
021400911004     C                   SETOFF                                       02
021500950615     C                   EXFMT     LR01D00
021600941027     C*
021700941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
021800050512     C                   SETOFF                                       432809
021900050512     C                   SETOFF                                       46
022000941027     C*
022100910319     C* CMD3  - FINE LAVORO
022200910319     C   KC              GOTO      FINE                                         F3=FINE
022300940711     C*
022400950615     C     FOR01         TAG
022500940711     C* CONTROLLO CAMPI
022600940711     C                   EXSR      CTRD00
022700941027     C*
022800050512     c   09
022900050512     Cor 90              GOTO      FMT00
023000070329     C*
023100070329      * Se categoria foglio 4 devo chiamare il nuovo programma
023200070329     C*  FIDG31R
023300070329     c                   If        v0cNPG = '4'
023400070329      *
023500070329     C*  Controlla Con il P.O. la data abilitazione
023600070329     C     v0cFGS        CHAIN     AZORG01l
023700070329    1C                   IF        %found(azorg01l)
023800070329     C                   MOVEL     ORGde6        og146
023900070329     c                   if             §ogdda <> *blank and §ogdda <> *all'0'
024000070329      * se c'è una data in organigramma e siamo partiti in produzione
024100070329     c                   move      §ogdda        data_org          8 0
024200070406     c                   if        data_org <> 20391231 and dateu8 >= data_org
024300070329     C*
024400070329     C                   Z-ADD     DATinW        FDGGDA
024500070329     C                   Z-ADD     DATfiW        FDGGAL
024600070329     C                   move      V0CNPG        FDGNPG
024700070403     C                   move      'I'           FDGRIC
024800070329     C                   Z-ADD     V0CNFV        FDGNFV
024900070329     C                   Z-ADD     V0CFGS        FDGFGS
025000070329     C                   move      V0CTFV        FDGTFV
025100160309     C                   move      V0CTpm        FDGtdis
025200070329     C                   move      'T'           FDGTPV
025300070329     c                   eval      kpjbus = kpjbu
025400070404     c                   clear                   $FV6
025500070404     c                   do        600           yy                3 0
025600070404if  2c                   if        SDSprm > 1
025700070404     c                   z-add     $FV(yy)       $FV6(yy)
025800070404     c                   else
025900070404     c                   z-add     999999        $FV6(yy)
026000070404     c                   end
026100070404     c                   enddo
026200070329     C                   MOVEL(p)  FIDG31DS      KPJBU
026300070329     c                   call      'FIDG31R'
026400070329     c                   parm                    kpjba
026500070404     c                   parm                    $FV6
026600070329     c                   movel     kpjbu         fidg31ds
026700070419     C                   eval      dlrERR  =  FDGerr
026800070419     C                   eval      dlrFS1  =  FDGfs1
026900070419     C                   eval      V0CNFV  =  FDGNFV
027000070329     C                   eval      DLRNFV  =  FDGNFV
027100070329     c                   eval      kpjbu = kpjbus
027200070329     C*
027300070329     c                   if        DLRRIC = *blank
027400070329     C                   GOTO      FMT00
027500070329     c                   else
027600070329     C                   GOTO      Fine
027700070329     c                   end
027800070329     C*
027900070406     c                   endIf
028000070329     c                   end
028100070329     c                   end
028200070329    1C                   ENDIF
028300911010      **
028400940802      ** NON IMMESSO FOGLIO
028500940712     C     V0CNFV        IFEQ      0
028600010509     C     V0CNPG        COMP      '4'                                    14
028700900517     C                   EXSR      RICERC
028800940711     C*
028900911010      * NON CI SONO FV DA VISUALIZZARE
029000940712     C     *IN48         IFEQ      *ON
029100940712     C* PGM RICHIAMATO
029200940712     C     *IN01         IFEQ      *ON
029300940802     C                   MOVEL     '9'           DLRERR
029400940712     C                   GOTO      FINE
029500940712     C                   ELSE
029600940712     C* PGM NON RICHIAMATO
029700940712     C                   GOTO      FMT00
029800940712     C                   ENDIF
029900940712     C                   ENDIF
030000900522      *
030100010510     C     *IN14         IFEQ      *OFF
030200940712     C                   WRITE     LR01RC2
030300911015     C     FORCT         TAG
030400940712     C                   EXFMT     LR01CT2
030500010510     C                   ENDIF
030600010510     C     *IN14         IFEQ      *ON
030700010510     C                   WRITE     LR01RC3
030800010510     C     FORCT1        TAG
030900010510     C                   EXFMT     LR01CT3
031000010510     C                   ENDIF
031100910319      * CMD12 - RITORNO
031200940712     C   KL
031300940712     CANN01              GOTO      INIZIO
031400940712     C   KL
031500940712     CAN 01              GOTO      FINE
031600900522      *          =============
031700900522     C     LEGGI         TAG
031800900522      *          =============
031900010510     C  N14              READC     LR01SF2                                29
032000010510     C   14              READC     LR01SF3                                29
032100911015     C*
032200911015     C* FINE LETTURA SE NON SCELTE EFFETTUATE RIMANGO NEL SUBFILE
032300940712     C   29SCE           IFEQ      1
032400940712     C  N01              GOTO      INIZIO
032500940712     C   01              GOTO      FINE
032600911015     C                   ELSE
032700010510     C  N14              GOTO      FORCT
032800010510     C   14              GOTO      FORCT1
032900911015     C                   END
033000910204     C*
033100010510     C  N14V2CSCE        IFNE      '1'
033200910204     C                   GOTO      LEGGI
033300910204     C                   END
033400010510     C   14V3CSCE        IFNE      '1'
033500010510     C                   GOTO      LEGGI
033600010510     C                   END
033700900522      *
033800911015     C                   Z-ADD     1             SCE               1 0
033900910423      *
034000941027      * SE RICHIAMATO VADO A FINE PGM
034100940712     C     *IN01         IFEQ      *ON
034200010510     C  N14              Z-ADD     V2CNFV        DLRNFV
034300010510     C   14              Z-ADD     V3CNFV        DLRNFV
034400950615     C                   MOVEL     V0CNPG        DLRNPG
034500940712     C                   GOTO      FINE
034600940712     C                   ELSE
034700940712     C*
034800010510     C  N14              Z-ADD     V2CNFV        V0CNFV
034900010510     C  N14              Z-ADD     V2CFGS        V0CFGS
035000010510     C   14              Z-ADD     V3CNFV        V0CNFV
035100050512     C** 14              Z-ADD     V3CFGS        V0CFGS
035200940712     C     KFVV          CHAIN     FNFVV000                           30
035300910423      *
035400910319      * VISUALIZZO FOGLIO VIAGGIO
035500940712     C  N30              EXSR      VISNFV
035600910319      * CONTROLLO SE ALTRA SELEZIONE
035700910319     C                   GOTO      LEGGI
035800940712     C                   ENDIF
035900911010      **
036000911010      ** IMMESSO FOGLIO VIAGGIO
036100910319     C                   ELSE
036200910319      * VISUALIZZO
036300910319     C                   EXSR      VISNFV
036400910319      *
036500910319     C                   GOTO      INIZIO
036600900517     C                   END
036700900515      *
036800000000     C     FINE          TAG
036900940802     C                   MOVEL     DSLR01        KPJBU
037000000000     C                   SETON                                        LR
037100920806      *-----------------------------------------------------***********
037200920806      * CARICO TABELLE                                         CARTAB
037300920806      *-----------------------------------------------------***********
037400920806     C     CARTAB        BEGSR
037500161125     c                   setoff                                       5051
037600161125     c
037700161125     c* carico le filiali abilitate all'IPG
037800161125     c                   clear                   trulvpods
037900161125     c                   eval      ivpoke1 = 'DECOFIIPG'
038000161125     c                   call      'TRULVPOR'
038100161125     c                   parm                    trulvpods
038200161125     c
038300161125     c                   if        %lookup('999':skFilOk)>0
038400161125     c                   seton                                        50
038500161125     c                   endif
038600161125     C*
038700161125     C                   MOVEL     '7B'          COD
038800161125     C                   Z-ADD     1             I                 2 0
038900161125     C     KTAB          SETLL     TABEL
039000161125     C     KTAB          READE     TABEL                                  30
039100161125     C*
039200161125     C     *IN30         DOWEQ     '0'
039300161125     C                   MOVEL     TBLUNI        DS7B
039400161125     C                   MOVEL     TBLKEY        TIN(I)
039500161125     C                   MOVEL     §7bdes        DIN(I)
039600161125     C                   MOVE      §7Buti        uIN(I)
039700161125     C                   MOVE      §7Bdesb       dbIN(I)
039800161125     C*
039900161125     C                   ADD       1             I
040000161125     C     KTAB          READE     TABEL                                  30
040100161125     C                   END
040200161125     c
040300161125     C***
040400161125     C* CARICO I TIPI INVENTARIO
040500161125     C***
040600161125     c                   exsr      car_inv
040700940711     C***
040800160929     C* VEDO SE SONO SIMFEL O 2 liv
040900940711     C***
041000020513     c     simtpp        ifeq      '2'
041100020513     c     simtpp        oreq      *blanks
041200020513     c                   movel     simpou        v0cfgs
041300940711     C                   SETON                                        05
041400940711     C                   ELSE
041500940711     C                   MOVEL     SIMFEL        V0CFGS
041600940711     C                   END
041700050512     c                   exsr      chainfgs
041800050512     c
041900940711     C***
042000940711     C* CARICO CATEGORIE FOGLI
042100940711     C***
042200940711     C                   MOVEL     '7N'          COD
042300940711     C                   Z-ADD     0             I
042400940711     C     KTAB          SETLL     TABEL
042500940711     C     KTAB          READE     TABEL                                  31
042600940711     C*
042700940711     C     *IN31         DOWEQ     '0'
042800940711     C                   MOVEL     TBLUNI        DS7N
042900940711     C* SOLO SE ABILITATA PER FOGLI VARI
043000940711     C     §7NFFV        IFEQ      'S'
043100940711     C     §7NUTI        ANDNE     '9'
043200940711     C                   ADD       1             I
043300940711     C                   MOVEL     ' ='          DEC(I)
043400940711     C                   MOVEL     ALF1          DEC(I)
043500940711     C                   MOVE      §7NDE2        DEC(I)
043600940711     C     I             IFEQ      6
043700940711     C                   SETON                                        31
043800940711     C                   END
043900940711     C                   END
044000940711     C*
044100940711     C  N31KTAB          READE     TABEL                                  31
044200940711     C                   END
044300940711     C*
044400940711     C* IMPOSTO LE DESCRIZIONI A VIDEO
044500940711     C     DEC(1)        IFNE      *BLANKS
044600940711     C                   MOVEL     DEC(1)        DESCA1
044700940711     C     DEC(2)        IFNE      *BLANKS
044800940711     C                   MOVEL     DEC(2)        DESCA2
044900940711     C     DEC(3)        IFNE      *BLANKS
045000940711     C                   MOVEL     DEC(3)        DESCA3
045100940711     C     DEC(4)        IFNE      *BLANKS
045200940711     C                   MOVEL     DEC(4)        DESCA4
045300940711     C     DEC(5)        IFNE      *BLANKS
045400940711     C                   MOVEL     DEC(5)        DESCA5
045500940711     C     DEC(6)        IFNE      *BLANKS
045600940711     C                   MOVEL     DEC(6)        DESCA6
045700940711     C                   END
045800940711     C                   END
045900940711     C                   END
046000940711     C                   END
046100940711     C                   END
046200940711     C                   END
046300940711     C***
046400940711     C* TESTATA
046500940711     C***
046600940712     C                   MOVE      TES(1)        V0CTES
046700941027     C***
046800941027     C* CARICO TABELLA FILIALI GESTITE £1
046900941027     C***
047000941027     C                   CLEAR                   DSUL06
047100941027     C                   MOVE      '£1'          D06COD
047200941027     C                   MOVEL     SIMFEL        D06KEY
047300941027     C                   MOVEL     'L'           D06TLA
047400941027     C                   MOVEL     DSUL06        KPJBU
047500941027     C*
047600941027     C                   CALL      'TRUL06R'
047700941027     C                   PARM                    KPJBA
047800941027     C                   MOVEL     KPJBU         DSUL06
047900941027     C                   MOVEA     LIN           L1
048000920806     C                   ENDSR
048100161125      *-----------------------------------------------------***********
048200161125      *  Carico i tipi inventario
048300161125      *-----------------------------------------------------***********
048400161125     c     Car_inv       BEGSR
048500161125     C                   MOVEL     *BLANKS       VIDSP1
048600161125     C                   MOVEL     *BLANKS       DESSP1
048700161125     C                   MOVEL     *BLANKS       VIDSP2
048800161125     C                   MOVEL     *BLANKS       DESSP2
048900161125     C                   MOVEL     *BLANKS       VIDSP3
049000161125     C                   MOVEL     *BLANKS       DESSP3
049100161125     C                   MOVEL     *BLANKS       VIDSP4
049200161125     C                   MOVEL     *BLANKS       DESSP4
049300161125     C                   MOVEL     *BLANKS       VIDSP5
049400161125     C                   MOVEL     *BLANKS       DESSP5
049500161125     C                   MOVEL     *BLANKS       VIDSP6
049600161125     C                   MOVEL     *BLANKS       DESSP6
049700161125
049800161125     C* EMETTO A VIDEO I PRIMI 6
049900161125     c                   z-add     1             I
050000161125     c                   dow       tin(i)<>*blanks
050100161125     C     TIN(I)        IFNE      *BLANKS
050200161125     c     uin(i)        andne     'N'
050300161125     c                   if        *in50 or
050400161125     c                             *in51 or (not *in51 and uin(I)<>'1' )
050500161125     c                   select
050600161125     c                   when      vidsp1=*blanks
050700161125     C                   MOVEL     TIN(I)        VIDSP1
050800161125     C                   MOVE      ' ='          VIDSP1
050900161125     C                   MOVEL     DIN(I)        DESSP1
051000161125     c                   when      vidsp2=*blanks
051100161125     C                   MOVEL     TIN(I)        VIDSP2
051200161125     C                   MOVE      ' ='          VIDSP2
051300161125     C                   MOVEL     DIN(I)        DESSP2
051400161125     c                   when      vidsp3=*blanks
051500161125     C                   MOVEL     TIN(I)        VIDSP3
051600161125     C                   MOVE      ' ='          VIDSP3
051700161125     C                   MOVEL     DIN(I)        DESSP3
051800161125     c                   when      vidsp4=*blanks
051900161125     C                   MOVEL     TIN(I)        VIDSP4
052000161125     C                   MOVE      ' ='          VIDSP4
052100161125     C                   MOVEL     DIN(I)        DESSP4
052200161125     c                   when      vidsp5=*blanks
052300161125     C                   MOVEL     TIN(I)        VIDSP5
052400161125     C                   MOVE      ' ='          VIDSP5
052500161125     C                   MOVEL     DIN(I)        DESSP5
052600161125     c                   when      vidsp6=*blanks
052700161125     C                   MOVEL     TIN(I)        VIDSP6
052800161125     C                   MOVE      ' ='          VIDSP6
052900161125     C                   MOVEL     DIN(I)        DESSP6
053000161125     c                   leave
053100161125     c                   endsl
053200161125     c                   endif
053300161125     c                   endif
053400161125     c
053500161125     c                   add       1             I
053600161125
053700161125     C                   ENDdo
053800161125     c                   ENDSR
053900050512      *-----------------------------------------------------***********
054000050512     c* Impostazione iniziale p.o. gestione
054100050512      *-----------------------------------------------------***********
054200050512     c     CHAINFGS      BEGSR
054300050512     C     V0CFGS        CHAIN     AZORG                              31
054400050512     C  N31              MOVEL     ORGDES        V1DFGS
054500050512     C  N31              MOVEL     ORGDES        V0dFGS
054600050512     c  n31              movel     orgdf0        og150
054700050512     c   31              clear                   og150
054800050512     c   31              clear                   v0dfgs
054900050512     c   31              clear                   v1dfgs
055000050512     c* p.o. con piking messaggerie
055100050512     c                   if        §ogpkm='S'
055200050512     c                   seton                                        08
055300050512     c                   endif
055400050512     c
055500050512     c* salvo campi di comodo
055600050512     c                   eval      fgspkm=§ogpkm
055700050512     c                   eval      savfgs=v0cfgs
055800050512     c                   eval      savin08=*in08
055900050512     c                   ENDSR
056000900515      *-----------------------------------------------------***********
056100940712      * PULIZIA CAMPI VIDEO                                 *  INZD00 *
056200900515      *-----------------------------------------------------***********
056300940712     C     INZD00        BEGSR
056400900515      *
056500950119     C                   MOVEL     'FNLR02R'     V0CPGM
056600940711     C                   MOVEL     ' '           V0CNPG
056700940711     C                   Z-ADD     0             V0CNFV
056800940711     C                   Z-ADD     0             V0CDIN
056900940711     C                   Z-ADD     31129999      V0CDFI
057000940711     C                   MOVEL     *BLANKS       V0CTFV
057100900515      *
057200900514     C                   ENDSR
057300900515      *-----------------------------------------------------***********
057400900517      * RICERCA E SCELTA FOGLIO DI VIAGGIO                  *  RICERC *
057500900515      *-----------------------------------------------------***********
057600900517     C     RICERC        BEGSR
057700941027     C*
057800010509     C  N14              Z-ADD     0             NR2               4 0
057900010509     C   14              Z-ADD     0             NR3               4 0
058000900517      *
058100900517      * RIEMPIO IL SFL
058200040310if  1c                   if        DLRtfv <> '3'
058300010509     C     *IN14         IFEQ      *OFF
058400900517     C                   Z-ADD     1             NR2
058500941229     C                   SETOFF                                       211310
058600940712     C                   WRITE     LR01CT2
058700940712     C                   SETON                                        202123
058800010509     C                   ENDIF
058900010509     C     *IN14         IFEQ      *ON
059000010509     C                   Z-ADD     1             NR3
059100010509     C                   SETOFF                                       211310
059200010509     C                   WRITE     LR01CT3
059300010509     C                   SETON                                        202123
059400010509     C                   ENDIF
059500040310e   1c                   endif
059600911205     C*
059700940711     C* 13 ON - RICHIESTI SOLO FOGLI APERTI
059800940712     C     V0CTFV        IFEQ      '2'
059900040112     c     V0Ctfv        oreq      '3'
060000911205     C                   SETON                                        13
060100911205     C                   END
060200941229     C* 10 ON - IMMESSO FILIALE GESTIONE
060300941229     C     V0CFGS        IFGT      0
060400941229     C                   SETON                                        10
060500941229     C                   END
060600900517      *
060700020905     c                   Select
060800020905     c                   When      *In10 and Not *In13
060900020905     c     Kdat1         setll     fnfvv008
061000020905     c                   When      *In10 and *In13
061100020905     C     KFVV3         SETLL     FNFVV003
061200020905     c                   When      Not *In10 and *In13
061300020905     C     *LOVAL        SETLL     FNFVV003
061400020905     c                   Other
061500020905     c     Kdat          setll     fnfvv002
061600020905     c                   EndSl
061700911205     C*
061800020905     c                   Select
061900020905     c                   When      *In10 and Not *In13
062000020905     c     Kdat2         Reade     Fnfvv008                               88
062100020905     c                   When      *In10 and *In13
062200020905     C     KFVV3         READE     FNFVV003                               88
062300020905     c                   When      Not *In10 and *In13
062400020905     C                   READ      FNFVV003                               88
062500020905     c                   Other
062600020905     c     Wnpg          Reade     Fnfvv002                               88
062700020905     c                   EndSl
062800910319      *
062900040310do  1C     *IN88         DOWEQ     *OFF
063000910319      *
063100040310if  2C     FVVDFV        IFLE      DATFIW
063200900529      *
063300010515     C                   SETOFF                                       96
063400911205      *
063500911205     C   13FVVDFV        IFGE      DATINW
063600010515     C                   SETON                                        96
063700911205     C                   END
063800900529      *
063900910319      * SELEZIONATI SOLO I CHIUSI
064000940712     C     V0CTFV        IFEQ      '1'
064100910319     C     FVVFCF        ANDNE     ' '
064200010515     C                   SETON                                        96
064300900529     C                   END
064400900529      *
064500940712     C     V0CTFV        IFEQ      ' '
064600010515     C                   SETON                                        96
064700900529     C                   END
064800941027     C*
064900940711     C* CONTROLLO CHE SIA FOGLIO DI FILIALE IN GESTIONE
065000040310if  3C   96V0CFGS        IFNE      0
065100010515     C   96FVVFGS        IFNE      V0CFGS
065200010515     C                   SETOFF                                       96
065300941027     C                   ENDIF
065400941027     C*
065500040310x   3C                   ELSE
065600941027     C*
065700941027     C* CARICO SOLO LE FILIALE NON DELL'AREA
065800941027     C     FVVFGS        LOOKUP    L1                                     30
065900010515     C   30              SETOFF                                       96
066000040310e   3C                   ENDIF
066100020611
066200020612      * richiesta la parzializzazione per tipo bolla
066300020612     c                   if        *in14 and v0ctpb <> 'N'
066400020611     c                             and *in96 = *on
066500020612     c                   exsr      rictpb
066600020611     c                   endif
066700031111
066800031111      * richiamato da FIMS03R, che passa i F.V. già inseriti
066900040310if  3c                   if        SDSprm > 1
067000031111     c                   eval      *in30  = *off
067100031111     c     FVVnfv        lookup    $FV                                    30
067200040310if  4c                   if        *in30  = *on
067300031111     c                   eval      *in96  = *off
067400040310e   4c                   endif
067500040310e   3c                   endif
067600031112
067700031112      * richiamato SOLO per sapere se esistono distinte aperte
067800031112      *   non inserito nel piano di cui sono stati passati i F.V.
067900040310if  3c                   if            SDSprm > 1
068000031112     c                             and DLRtfv = '3'
068100040310     c                             and *in96  = *on
068200031112     c                   eval      DLRfs1 = *on
068300031112     c                   goto      FINE
068400040310e   3c                   endif
068500050512     c* Parzializzazione per tipo zone consegna parcel/messaggerie
068600050512     c                   movel     fvvspn        dfvvspn
068700050512     c
068800050512     c                   if        *in08 and v0ctpm<>' '
068900050512     c                   if        (v0ctpm='M' and §fvvtpm<>'M') or
069000050512     c                             (v0ctpm='P' and §fvvtpm ='M')
069100050512     c                   eval      *in96  = *off
069200050512     c                   endif
069300050512     c                   endif
069400910319      *
069500010515      * 96 ON - CARICO RECORD
069600040310if  3C     *IN96         IFEQ      *ON
069700900529      *
069800010509     C  N14              CLEAR                   V2CSCE
069900010509     C  N14              CLEAR                   V2CANN
070000010509     C  N14              CLEAR                   V2CFCF
070100010509     C   14              CLEAR                   V3CSCE
070200010509     C   14              CLEAR                   V3CANN
070300010509     C   14              CLEAR                   V3CFCF
070400010510     C   14              CLEAR                   V3CTPD
070500050512     C   14              CLEAR                   V3CTPM
070600940712     C* GIRO DATA FOGLIO
070700940712     C                   MOVE      FVVDFV        G02INV
070800940712     C                   MOVEL     '3'           G02ERR
070900940712     C                   CALL      'XSRDA8'
071000940712     C                   PARM                    WLBDAT
071100940712     C*
071200010509     C  N14              Z-ADD     G02DAT        V2CDFV
071300010510     C   14              Z-ADD     G02DAT        V3CDFV
071400900517      *
071500010509     C  N14              Z-ADD     FVVNFV        V2CNFV
071600010509     C  N14              MOVEL     FVVDSF        V2CDSF
071700010509     C   14              Z-ADD     FVVNFV        V3CNFV
071800010509     C   14              MOVEL     FVVDSF        V3CDSF
071900911003      * TIPO INVENTARIO
072000040310if  4C     *IN14         IFEQ      *OFF
072100940712     C                   MOVEL     *BLANKS       V2DINV
072200160929     C                   Z-ADD     1             I
072300940712     C     FVVSPG        IFNE      ' '
072400161006     c***                if        fvvfpp<>*blanks
072500161006     c***                eval      I=%lookup(fvvfpp:tin)
072600161006     C***                MOVEL     dbiN(I)       V2DINV
072700161006     c***                else
072800160929     c                   eval      I=%lookup(fvvspg:tin)
072900160929     C                   MOVEL     dbin(I)       V2DINV
073000161006     c***                endif
073100940712     C                   ENDIF
073200040310e   4C                   ENDIF
073300940712     C* FILIALE GESTIONE
073400010509     C  N14              MOVEL     FVVFGS        V2CFGS
073500940712     C     FVVFGS        CHAIN     AZORG                              30
073600010509     C  N14
073700010509     CANN30              MOVEL     ORGDES        V2DFGS
073800010509     C  N14
073900010509     CAN 30              MOVEL     *BLANKS       V2DFGS
074000010509      *
074100020611      * se richiesto tipo foglio "4"
074200040310if  4C                   IF        *in14 = *ON
074300050512     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
074400050512     c                   if        *in08
074500050512     c                   if        §fvvtpm='M'
074600050512     c                   movel     'Mes'         v3ctpm
074700050512     c                   endif
074800050512     c                   if        §fvvtpm<>'M'
074900050512     c                   movel     'Prc'         v3ctpm
075000050512     c                   endif
075100050512     c                   endif
075200050512     c
075300020611      * Flag tipo distinta (solo consegna, orm o entrambi)
075400010510     C                   CLEAR                   WBOL
075500010510     C                   CLEAR                   WORM
075600050512     c                   Z-Add     v0cfgs        Wifp
075700020910     C                   Z-ADD     V3CNFV        kfnvv
075800010510     C                   Z-ADD     V3CNFV        W0CNFV
075900020910     c     Karb78        Setll     Fnarb78l
076000040310if  5c                   if        %equal(fnarb78l)
076100010510     c                   movel     'S'           wbol              1
076200040310x   5c                   else
076300070112     C     KBRV          SETLL     FNBRV01L
076400070112     c                   if        %equal(fNbrv01l)
076500010510     c                   movel     'S'           wbol              1
076600010510     c                   endif
076700040310e   5c                   endif
076800010510     C     KORM          SETLL     FNORM06L
076900040310do  5c                   do        *hival
077000010511     C     KORM          reade     FNORM06L
077100010511     c                   if        %eof(fnorm06l)
077200010511     c                   leave
077300010511     c                   endif
077400010511     c                   if        ormatb = *blanks and ormfao < 999
077500010510     C                   move      'S'           worm              1
077600010510     c                   endif
077700040310e   5c                   enddo
077800010510     C                   if        wbol = 'S' and worm <> 'S'
077900020611     C                   movel     'CON'         v3ctpd
078000010510     C                   endif
078100010510     C                   if        wbol <> 'S' and worm = 'S'
078200020611     C                   movel     'ORM'         v3ctpd
078300010510     C                   endif
078400010510     C                   if        wbol = 'S' and worm = 'S'
078500020611     C                   movel     'C/O'         v3ctpd
078600010510     C                   endif
078700020612      * Calcolo importi se non già fatto prima
078800040310if  5c                   if        v0ctfv = '2' and v0impo = 'S'
078900020612     c                   if        v0ctpb = 'N'
079000020423     c                   exsr      totalizzo
079100020423     c                   z-add     w3imas        v3imas
079200020423     c                   z-add     w3imca        v3imca
079300020612     c                   endif
079400040310x   5c                   else
079500020423     c                   z-add     0             v3imas
079600020423     c                   z-add     0             v3imca
079700040310e   5c                   endif
079800020611
079900020612      * Tipo bolla
080000020612     c                   clear                   v3ctpb
080100040310if  5c                   if        v0ctpb <> 'N'
080200020611     c                   select
080300020611     c                   when      wbar = *on  and wppt = *off
080400020612     c                   eval      v3ctpb = 'BAR'
080500020611     c                   when      wbar = *off and wppt = *on
080600020612     c                   eval      v3ctpb = 'PPT'
080700020611     c                   when      wbar = *on  and wppt = *on
080800020612     c                   eval      v3ctpb = 'B/P'
080900020611     c                   endsl
081000040310e   5c                   endif
081100020611
081200040310e   4C                   ENDIF
081300911008      *
081400911008      * ANNULLATO
081500910319     C     FVVATB        IFEQ      '*'
081600010509     C  N14              MOVEL     'ANN'         V2CANN
081700010509     C   14              MOVEL     'ANN'         V3CANN
081800900517     C                   END
081900961016      * DATI IN RICEZIONE
082000040310if  4C     FVVFSC        IFEQ      'B'
082100010509     C  N14              MOVE      'DR'          V2CFCF
082200010509     C   14              MOVE      'DR'          V3CFCF
082300040310x   4C                   ELSE
082400040310if  5C     FVVFCF        IFNE      ' '
082500940712      * CHIUSO
082600940712     C     FVVFCF        IFEQ      'S'
082700010509     C  N14              MOVEL     'C '          V2CFCF
082800010509     C   14              MOVEL     'C '          V3CFCF
082900940712     C                   ELSE
083000940712     C* BLOCCATO
083100010509     C  N14              MOVEL     'BL'          V2CFCF
083200010509     C   14              MOVEL     'BL'          V3CFCF
083300010509     C                   ENDIF
083400040310e   5C                   ENDIF
083500040310e   4C                   ENDIF
083600900517      *
083700040310if  4c                   if        DLRtfv <> '3'
083800010509     C  N14              WRITE     LR01SF2
083900010509     C  N14              ADD       1             NR2
084000010509     C   14              WRITE     LR01SF3
084100010509     C   14              ADD       1             NR3
084200040310e   4c                   endif
084300900517      *
084400040310e   3C                   ENDif
084500910319      *
084600020905     c                   Select
084700020905     c                   When      *In10 and Not *In13
084800020905     c     Kdat2         Reade     Fnfvv008                               88
084900020905     c                   When      *In10 and *In13
085000020905     C     KFVV3         READE     FNFVV003                               88
085100020905     c                   When      Not *In10 and *In13
085200020905     C                   READ      FNFVV003                               88
085300020905     c                   Other
085400020905     c     Wnpg          Reade     Fnfvv002                               88
085500020905     c                   EndSl
085600900517      *
085700040310x   2C                   ELSE
085800911205     C  N13              SETON                                        88
085900941229     C   10
086000941229     CAN 13KFVV3         READE     FNFVV003                               88
086100941229     C  N10
086200941229     CAN 13              READ      FNFVV003                               88
086300040310e   2C                   ENDif
086400910319      *
086500040310e   1C                   ENDdo
086600900517      *
086700900517      * NON TROVATI FOGLI IN INTERVALLO RICHIESTO
086800040630     C**n14NR2           IFEQ      1
086900040630     C  n14NR2           IFLE      1
087000920806     C                   SETON                                        48
087100900517     C                   END
087200040630     C***14NR3           IFEQ      1
087300040630     C   14NR3           IFLE      1
087400010517     C                   SETON                                        48
087500010517     C                   END
087600900517      *
087700920806     C                   ENDSR
087800020611      *-----------------------------------------------------
087900020612      * RICERCA PER TIPO BOLLA
088000020611      *-----------------------------------------------------
088100020612     c     rictpb        begsr
088200020611
088300020611     c                   eval      wbar = *off
088400020611     c                   eval      wppt = *off
088500020612     c                   clear                   w3imca           13 3
088600020612     c                   clear                   w3imas           13 3
088700020611
088800020910     c                   Z-Add     fvvfgs        wifp
088900020611     c                   z-add     fvvnfv        kfnvv
089000020910     c     karb78        setll     fnarb78l
089100020910 b1  c                   if        not %equal(fnarb78l)
089200020612     c                   if        v0ctpb <> 'T'
089300020611     c                   eval      *in96 = *off
089400020612     c                   endif
089500020612 x1  c                   else
089600020612 b2  c                   do        *hival
089700020910     c     karb78        reade     fnarb78l
089800020910     c                   if        %eof(fnarb78l)
089900020611     c                   leave
090000020611     c                   endif
090100020611     c                   if        arbtsp <> 'P'
090200020611     c                   eval      wbar = *on
090300020611     c                   else
090400020611     c                   eval      wppt = *on
090500020611     c                   endif
090600020612
090700020612      * se richiesto ormai che ci sono totalizzo gli assegnati
090800020612 b3  c                   if        v0ctfv = '2' and v0impo = 'S'
090900020612     c                   if        arbica = *blank or arbica = 'R'
091000020612     c     kimpo         chain     fiar601l                           71
091100020612     c  n71              add       ar6ift        w3imas
091200020612     c                   endif
091300020612      * totalizzo i contrassegni
091400020612 b4  c                   if        arbicc = *blank or arbicc = 'R'
091500051114     c     kimpo         chain     fiar901l                           71
091600020612 b5  c                   if        not *in71
091700020612 b6  c                   if        ar9vca = 'EUR'
091800020612     c                   add       ar9cas        w3imca
091900020612 x6  c                   else
092000020612      *se la divisa non è EUR traduco con yeurco l'importo
092100020612     c                   CLEAR                   YEURDS
092200020612     c                   MOVEL     ar9vca        YECDVI
092300020612     c                   Z-ADD     ar9cas        YECIMI
092400020612     c                   MOVEL     'EUR'         YECDVO
092500020612     c                   MOVEL     'H'           YECTAR
092600020612     c                   CALL      'YEURCO'
092700020612     c                   PARM                    YEURDS
092800020612     c                   add       YECIMO        W3imca
092900020612 e6  c                   endif
093000020612 e5  c                   endif
093100020612 e4  c                   endif
093200020612
093300020612 x3  c                   else
093400020612     c                   z-add     0             v3imas
093500020612     c                   z-add     0             v3imca
093600020612 e3  c                   endif
093700020612
093800020612 e2  c                   enddo
093900020611
094000020611     c                   select
094100020612     c                   when      v0ctpb = 'B' and wppt = *on
094200020611     c                   eval      *in96 = *off
094300020612     c                   when      v0ctpb = 'P' and wbar = *on
094400020611     c                   eval      *in96 = *off
094500020612     c                   when      v0ctpb = 'M' and (wbar = *off or wppt = *off)
094600020611     c                   eval      *in96 = *off
094700020611     c                   endsl
094800020612 e1  c                   endif
094900020611
095000020611     c                   endsr
095100900517      *-----------------------------------------------------***********
095200940711      * CONTROLLI FORMATO 0                                 *  CTRD00 *
095300900517      *-----------------------------------------------------***********
095400940711     C     CTRD00        BEGSR
095500050512     C                   SETOFF                                       9008
095600941027     C*
095700941027     C* SE IMMESSA DEVE ESSERE FILIALE VALIDA
095800050512     c                   clear                   fgspkm
095900050512     c                   clear                   v0dfgs
096000941027     C     V0CFGS        IFNE      0
096100941027     C     V0CFGS        CHAIN     AZORG                              30
096200941027     C     *IN30         IFEQ      *ON
096300941027     C                   MOVEL     MSG(1)        V0CMSG
096400941027     C                   SETON                                        439028
096500941027     C                   GOTO      ENDCT0
096600050512     c                   else
096700050512     c                   movel     orgdf0        og150
096800050512     c                   movel     §ogpkm        fgspkm
096900050512     c                   movel     orgdes        v0dfgs
097000050512     c
097100050512     c* P.o. con picking, messaggerie: visualizzo parzializzazione
097200050512     c                   if        fgspkm='S'
097300050512     c                   seton                                        08
097400050512    1C                   ENDif
097500050512    1C                   END
097600941027    1C                   END
097700050512     c
097800050512     c* Filiale apertura obbligatoria se categoria 4
097900050512     c                   if        v0cnpg='4' and v0cfgs=0
098000050512     C                   MOVEL     MSG(2)        V0CMSG
098100050512     C                   SETON                                        439028
098200050512     C                   GOTO      ENDCT0
098300050512     c                   endif
098400050512     c
098500050512     c* riemetto video 1 senza errori se per cambio p.o. gestione, cambia
098600050512     c*  l'indicatore del picking messaggerie S/N
098700050512     c                   if        v0cfgs<>savfgs
098800050512     c                   if        (savin08='0' AND *IN08)   OR
098900050512     c                             (savin08='1' and not *in08)
099000050512     c                   seton                                        09
099100050512     c                   endif
099200050512     c                   eval      savfgs=v0cfgs
099300050512     c                   eval      savin08=*in08
099400050512     c                   endif
099500910319      *
099600910319      * CATEGORIA FOGLIO VIAGGIO OBBLIGATORIA
099700940712    1C     V0CNPG        IFEQ      ' '
099800910319     C                   SETON                                        4290
099900940711     C                   GOTO      ENDCT0
100000940712    1C                   END
100100920806     C* RICERCA IN TABELLA
100200940712    1C     V0CNPG        IFEQ      '?'
100300920806     C                   MOVEL     CODUT         §KUT
100400920806     C                   MOVEL     '7N'          §COD
100500940712     C                   MOVE      ' '           V0CNPG
100600920806     C                   MOVEL     *BLANKS       §KEY
100700920806     C                   CALL      'X§TABER'
100800920806     C                   PARM                    §KUT              1 0
100900920806     C                   PARM                    §COD              2
101000920806     C                   PARM                    §KEY              8
101100920806     C                   PARM                    §DES             30
101200940712     C                   MOVEL     §KEY          V0CNPG
101300920806     C                   SETON                                        90
101400940711     C                   GOTO      ENDCT0
101500940712    1C                   END
101600920806     C* CONTROLLO
101700920806     C                   MOVEL     '7N'          COD
101800920806     C                   MOVEL     *BLANKS       KEY
101900940712     C                   MOVEL     V0CNPG        KEY
102000920806     C* INESISTENTE
102100920806     C     KTAB2         CHAIN     TABEL                              42
102200920806     C* ANNULLATA
102300940712    1C  N42TBLFLG        IFNE      ' '
102400920806     C                   SETON                                        42
102500920806     C                   ELSE
102600920806     C                   MOVEL     TBLUNI        DS7N
102700920806     C* NON E' CATEGORIA DI FOGLI VARI
102800940712    2C     §7NFFV        IFNE      'S'
102900920806     C                   SETON                                        42
103000940712    2C                   END
103100940712    1C                   END
103200920806     C*
103300920806     C   42              SETON                                        90
103400940711     C   42              GOTO      ENDCT0
103500940712     C                   MOVEL     §7NDES        V1DNPG
103600940713     C                   MOVEL     §7NDES        V0DNPG
103700940712     C                   MOVEL     V0CNPG        WNPG
103800900517      *
103900900517      * CONTROLLO DATA INIZIALE
104000900517      *
104100940712    1C     V0CDIN        IFNE      0
104200940712     C                   MOVE      V0CDIN        G02DAT
104300900517     C                   MOVEL     *BLANK        G02ERR
104400940711     C                   CALL      'XSRDA8'
104500900517     C                   PARM                    WLBDAT
104600940712    2C     G02ERR        IFEQ      '1'
104700900517     C                   SETON                                        76  90
104800940711     C                   GOTO      ENDCT0
104900940712    2C                   END
105000940711     C*
105100940711     C                   Z-ADD     G02INV        DATINW
105200940712     C                   Z-ADD     G02DAT        V0CDIN
105300940711     C*
105400940712   X1C                   ELSE
105500940711     C*
105600940711     C                   Z-ADD     0             DATINW
105700940712    1C                   END
105800900517      *
105900900517      * CONTROLLO DATA FINALE
106000900517      *
106100940712    1C     V0CDFI        IFEQ      0
106200940712     C                   Z-ADD     31129999      V0CDFI
106300940712     C                   Z-ADD     99991231      DATFIW
106400940712   X1C                   ELSE
106500900529      *
106600940712     C                   MOVE      V0CDFI        G02DAT
106700900517     C                   MOVEL     *BLANK        G02ERR
106800940712     C                   CALL      'XSRDA8'
106900900517     C                   PARM                    WLBDAT
107000940712    2C     G02ERR        IFEQ      '1'
107100900517     C                   SETON                                        77  90
107200940711     C                   GOTO      ENDCT0
107300940712    2C                   END
107400940711     C*
107500940711     C                   Z-ADD     G02INV        DATFIW
107600940712     C                   Z-ADD     G02DAT        V0CDFI
107700900517      *
107800900517      * DATA INIZIALE MAGGIORE DATA FINALE
107900940712    2C     DATINW        IFGT      DATFIW
108000900517     C                   SETON                                        75  90
108100940711     C                   GOTO      ENDCT0
108200940712    2C                   END
108300940712    1C                   END
108400940711     C*
108500940711      * NUMERO FOGLIO
108600940711     C*
108700940712    1C     V0CNFV        IFGT      0
108800940711     C* FOGLIO VIAGGIO
108900940712     C     KFVV          CHAIN     FNFVV000                           38
109000940711     C*
109100940711     C* VEDO SE E' FOGLIO DI FILIALE GESTIBILE
109200940712     C* FOGLIO INESISTENTE
109300941027    2C     *IN38         IFEQ      *ON
109400940712     C                   SETON                                          90
109500940712     C                   GOTO      ENDCT0
109600940712    2C                   ENDIF
109700940712    1C                   ENDIF
109800050512     c* Per visualizzare gli importi rchiedere tipi fogli = 2
109900050512     c                   if        v0impo='S' and v0ctfv<>'2'
110000050512     C                   MOVEL     MSG(3)        V0CMSG
110100050512     C                   SETON                                        469028
110200050512     C                   GOTO      ENDCT0
110300050512     c                   endif
110400940711     C*
110500940711     C     ENDCT0        ENDSR
110600900515      *-----------------------------------------------------***********
110700900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
110800900515      *-----------------------------------------------------***********
110900900515     C     RIEMPI        BEGSR
111000900515      *
111100940712     C                   SETOFF                                       525306
111200161125     C                   SETOFF                                       0751
111300161125     c
111400161125     c* Se sono in prova per IPG ricarico i tipi foglio in vbase alla filiale utente
111500161125     c                   if        not *in50
111600161125     c                   if        %lookup(%editc(fvvfgs:'X'):skFilOk)>0
111700161125     c                   seton                                        51
111800161125     c                   endif
111900161125     c                   exsr      car_inv
112000161125     c                   endif
112100161125     c
112200940712     C* FILIALE GESTIONE
112300940712     C     FVVFGS        CHAIN     AZORG                              30
112400940713     C  N30              MOVEL     ORGDES        V1DFGS
112500940712     C* DATA FOGLIO
112600910319     C                   MOVE      FVVDFV        G02INV
112700910319     C                   MOVE      *ZEROS        G02DAT
112800940712     C                   MOVEL     '3'           G02ERR
112900940712     C                   CALL      'XSRDA8'
113000910319     C                   PARM                    WLBDAT
113100940712     C                   Z-ADD     G02DAT        V1CDFV
113200910319      *
113300940712     C                   MOVE      FVVDSF        V1CDSF
113400050512     C*                  MOVE      FVVSPN        V1CSPN
113500050512     C                   MOVEl     FVVSPN        dfvvspn
113600050512     c                   movel     §fvvspn       v1cspn
113700050512     C* FOGLIO INVENTARIO
113800940712     C     FVVNPG        IFEQ      3
113900940712     C                   SETON                                        02
114000161006     c***                if        fvvfpp<>*blanks
114100161006     C***                MOVEL     FVVfpp        V1CSPG
114200161006     c***                else
114300160929     C                   MOVEL     FVVSPG        V1CSPG                          sottotipo
114400161006     c***                endif
114500940712     C                   END
114600911003     C*
114700910319      * VEDO SE ANNULLATO
114800910319     C     FVVATB        IFEQ      '*'
114900940712     C                   SETON                                        5206
115000940712     C                   MOVEL     CANNUL        V1DTFV
115100910319     C                   END
115200910319      *
115300910319      * VEDO SE CHIUSO
115400910319     C     FVVFCF        IFNE      *BLANKS
115500940712     C     FVVFCF        IFEQ      'S'
115600910319     C                   SETON                                        53
115700940712     C                   ELSE
115800940712     C                   SETON                                        06
115900940712     C                   MOVEL     CBLOCC        V1DTFV
116000940712     C                   ENDIF
116100940712     C                   ENDIF
116200941027     C*
116300941027     C* 07 ON ESISTONO SPUNTE SUL FOGLIO
116400000114     C                   Z-ADD     V0CNFV        W0CNFV
116500070112     C     KBRV          SETLL     FNBRV000                               07     ON=TROVATO
116600900515     C                   ENDSR
116700910319      *-----------------------------------------------------***********
116800910319      * VISUALIZZO FOGLIO VIAGGIO                             VISNFV
116900910319      *-----------------------------------------------------***********
117000910319     C     VISNFV        BEGSR
117100910319      *
117200910319     C* RIEMPO FORMATO
117300910319     C                   EXSR      RIEMPI
117400910319      *
117500950112     C     FMT01         TAG
117600940712     C                   EXFMT     LR01D01
117700950112     C*
117800950112     C* CMD21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
117900950112     C     *INKV         IFEQ      *ON
118000950112     C                   CLEAR                   PARA04
118100950112     C                   Z-ADD     V0CNFV        P04NFV
118200950112     C                   Z-ADD     99            P04NRS
118300950112     C                   Z-ADD     99            P04NPS
118400950112     C                   MOVEL     V0CNPG        P04NPG
118500950112     C                   MOVEL     '1'           P04FLG
118600950112     C                   MOVEL     'FNLR01R'     P04PGM
118700950112     C                   Z-ADD     V0CFGS        P04FGS
118800950112     C                   MOVEL     PARA04        KPJBU
118900950112     C                   CALL      'FNLR04R'
119000950112     C                   PARM                    KPJBA
119100950112     C                   GOTO      FMT01
119200950112     C                   ENDIF
119300910319      *
119400910319     C     ENDVIS        ENDSR
119500020423      *--------------------------------------------
119600020423     c     totalizzo     begsr
119700020423      *--------------------------------------------
119800020423     c                   clear                   w3imca           13 3
119900020423     c                   clear                   w3imas           13 3
120000020910     c     Karb78        setll     fnarb78l
120100020423     c                   do        *hival
120200020910     c     Karb78        reade     fnarb78l                               70
120300020423     c   70              leave
120400020423      * totalizzo gli assegnati
120500020423     c                   if        arbica = *blank or arbica = 'R'
120600020423     c     kimpo         chain     fiar601l                           71
120700020423     c  n71              add       ar6ift        w3imas
120800020423     c                   endif
120900020423      * totalizzo i contrassegni
121000020423     c                   if        arbicc = *blank or arbicc = 'R'
121100051114     c     kimpo         chain     fiar901l                           71
121200020423     c                   if        not *in71
121300020423     c                   if        ar9vca = 'EUR'
121400020423     c                   add       ar9cas        w3imca
121500020423     c                   else
121600020423      *se la divisa non è EUR traduco con yeurco l'importo
121700020423     c                   CLEAR                   YEURDS
121800020423     c                   MOVEL     ar9vca        YECDVI
121900020423     c                   Z-ADD     ar9cas        YECIMI
122000020423     c                   MOVEL     'EUR'         YECDVO
122100020423     c                   MOVEL     'H'           YECTAR
122200020423     c                   CALL      'YEURCO'
122300020423     c                   PARM                    YEURDS
122400020423     c                   add       YECIMO        W3imca
122500020423     c                   endif
122600020423     c                   endif
122700020423     c                   endif
122800020423     c                   enddo
122900020423     c                   endsr
123000020423      *--------------------------------------------
123100940711      * INZSR INIZIALE                                         INZSR
123200940711      *-----------------------------------------------------***********
123300940711     C     *INZSR        BEGSR
123400940711     C***
123500940711      * CHIAVI                                                        *
123600940711     C***
123700940711     C     KFVV          KLIST
123800940711     C                   KFLD                    WNPG
123900940711     C                   KFLD                    V0CNFV
124000941027     C                   KFLD                    V0CFGS
124100020910      * Fnarb
124200020910     c     Karb78        Klist
124300020910     c                   Kfld                    Wifp
124400020910     c                   Kfld                    Kfnvv
124500000114     C*
124600000114     C     KBRV          KLIST
124700000114     C                   KFLD                    WNPG
124800000114     C                   KFLD                    W0CNFV
124900000114     C                   KFLD                    V0CFGS
125000940711      *
125100010510     C     KORM          KLIST
125200010510     C                   KFLD                    V0CFGS
125300010510     C                   KFLD                    W0CNFV
125400010510      *
125500940711     C     KDAT          KLIST
125600940711     C                   KFLD                    WNPG
125700940711     C                   KFLD                    DATINW
125800020905
125900020905     c     Kdat1         Klist
126000020905     c                   Kfld                    V0cfgs
126100020905     c                   Kfld                    Wnpg
126200020905     c                   Kfld                    Datinw
126300020905
126400020905     c     Kdat2         Klist
126500020905     c                   Kfld                    V0cfgs
126600020905     c                   Kfld                    Wnpg
126700940711      *
126800940711     C     KTAB2         KLIST
126900940711     C                   KFLD                    CODUT
127000940711     C                   KFLD                    COD               2
127100940711     C                   KFLD                    KEY               8
127200940711     C     KTAB          KLIST
127300940711     C                   KFLD                    CODUT
127400940711     C                   KFLD                    COD               2
127500940711     C     KFVV3         KLIST
127600940711     C                   KFLD                    V0CFGS
127700940711     C                   KFLD                    WNPG
127800020423     C     Kimpo         Klist
127900020423     C                   Kfld                    arbaas
128000020423     C                   Kfld                    arblnp
128100020423     C                   Kfld                    arbnrs
128200020423     C                   Kfld                    arbnsp
128300940711     C* LIKE DEFN
128400940711     C     *LIKE         DEFINE    FVVDFV        DATINW
128500940711     C     *LIKE         DEFINE    FVVDFV        DATFIW
128600940711     C     *LIKE         DEFINE    FVVNPG        WNPG
128700000114     C     *LIKE         DEFINE    BRVNFV        W0CNFV
128800070329     C***
128900070329     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
129000070329     C                   TIME                    W0140            14 0
129100070329     C                   MOVE      W0140         UDATE8            8 0
129200070329     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
129300070329     C*
129400070329     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
129500070329     C                   MOVEL     *BLANK        G02ERR
129600070329     C                   CALL      'XSRDA8'
129700070329     C                   PARM                    WLBDAT
129800070329     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
129900070329      *
130000070329      *  Se richiamata da Azione/Menù
130100070329     c                   CLEAR                   se_EDP            1
130200070329     c                   if        %subst(Knmus:1:3) = 'EDP'
130300070329     c                   MOVE      'S'           se_EDP
130400070329     c                   end
130500940712     C*
130600940712     C                   ENDSR
130700940712     C*
130800941027**  PGM RICHIAMANTI
130900940711FNLR02R
131000940711FNLR25R
131100940711**  TES
131200920805 ** INTERROGAZIONE  FOGLI  VARI **
131300920709 ** ABBINAMENTO  FOGLI  VIAGGIO **
131400941027**
131500941027Filiale in gestione errata o mancante
131600050512Punto Operativo apertura Foglio obbligatorio per categoria 4
131700050512Importi visualizzabili soltanto richiedendo Tipo fogli=2 non chiusi
