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
007800900515      *
007900940711     D***
008000940711     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008100940711     D***
008200900515     D WLBDAT          DS
008300940711     D  G02DAT                 1      8  0
008400940711     D  G02INV                 9     16  0
008500940711     D  G02ERR                17     17
008600940711     D  G02TGI                18     22  0
008700950112     D PARA04          DS
008800950112     D  P04NFV                 1      5  0
008900950112     D  P04LNP                 6      8  0
009000950112     D  P04LNA                 9     11  0
009100950112     D  P04NRS                12     13  0
009200950112     D  P04NCD                14     20  0
009300950112     D  P04NCA                21     27  0
009400950112     D  P04NPS                28     29  0
009500950112     D  P04NPG                30     30  0
009600950112     D  P04FLG                31     31
009700950112     D  P04PGM                32     38
009800950112     D  P04FGS                39     41  0
009900101202     d* bolla
010000101202     d  p04bolp               42     44  0
010100101202     d  p04bors               45     46  0
010200101202     d  p04bosp               47     53  0
010300101202     d  p04boan               54     57  0
010400101202     D  P04ERR                58     58
010500101202     d
010600940712     D***
010700940712     D* COSTANTI
010800940712     D***
010900940712     D CANNUL          C                   CONST('  Annullato  ')
011000940712     D CBLOCC          C                   CONST('  Bloccato   ')
011100910319      *---------------------------------------------------------------*
011200910319      * INDICATORI USATI
011300910319      *---------------------------------------------------------------*
011400940712      * 01    = PGM RICHIAMATO
011500940712      * 02    = CATEGORIA INVENTARIO 3
011600941027      * 03/05 = USATI IN GESTIONE FOGLI VARI
011700940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
011800941027      * 07    = EMETTO LA SCRITTA : ESISTONO SPUNTE SUL FOGLIO
011900050512      * 08    = p.o. gestione con picking messaggerie
012000050512      * 09    = riemetto video per cambio p.o. gestione anche senza errori
012100010509      * 14    = RICERCA DI FOGLI CATEGORIA '4'
012200031111      * 19    = Richiamato per visualizzazione f.v. non immessi in FIMS03D
012300031111      *         (non viene richiesta alcuna opzione).
012400940712      * 20/29 = SU SFL STD
012500050512      * 38/46 = ERRORE
012600940712      * 52    = RECORD ANNULLATO
012700940712      * 53    = FOGLIO CHIUSO
012800010515      * 56/58 = USATO COME ERRORE NEL FILE VIDEO
012900010515      * 60/61 = USATO COME ERRORE NEL FILE VIDEO
013000910319      * 75/77 = ERRORE SUI CAMPI
013100940712      * 88    = DI COMODO
013200940712      * 90    = ERRORE GENERICO
013300010515      * 95    = USATO COME ERRORE NEL FILE VIDEO
013400010515      * 96    = RECORD DA CARICARE IN SUBFILE
013500940712      * 98    = INTERROGAZIONE
013600940712      * 99    = PROTEZIONE CAMPI
013700910319      *--------------------------------------------------------------*
013800000000     C     *ENTRY        PLIST
013900000000     C                   PARM                    KPJBA
014000031111     c                   parm                    $FV
014100031111      *
014200940802     C                   MOVEL     KPJBU         DSLR01
014300900516      *
014400000000     C                   Z-ADD     1             CODUT
014500920806     C                   CALL      'X§PARUT'
014600920806     C                   PARM                    UT§DSE
014700940711     C                   MOVEL     RAGUT         V0CDSA
014800920806     C                   MOVEL     REC80         CNCR80
014900900515      *
015000900515      *---------------------------------------------------------------*
015100900515      * FLUSSO PRINCIPALE                                             *
015200900515      *---------------------------------------------------------------*
015300920806     C*
015400940711     C* CARICO TABELLE
015500940711     C                   EXSR      CARTAB
015600920806     C* PER L'INTERROGAZIONE
015700941027     C* LA FILIALE APERTURA FOGLIO E' SEMPRE ABILITATA
015800941027     C                   SETON                                        990498
015900940712     C*
016000940712     C* LA PULIZIA LA ESEGUO UNA VOLTA SOLA ALL'INIZIO
016100940712     C                   EXSR      INZD00
016200940712     C*
016300040310     C* 01 ON - PROGRAMMA RICHIAMATO
016400950615     C     DLRRIC        IFNE      ' '
016500940711     C* PRENDO LA TESTATA DAL NOME DEL PROGRAMMA
016600940712     C                   Z-ADD     1             X                 2 0
016700940802     C     DLRPGM        LOOKUP    PGM(X)                                 30
016800940711     C   30              MOVEL     TES(X)        V0CTES
016900941027     C* ACCENDO GLI INDICATORI DI PGM RICHIAMATO
017000941027     C                   SETON                                        01
017100031111      * Condiziono l'emissione del campo "1=Selezione" nei sfl
017200031111     c     SDSprm        comp      1                                  19
017300031112      * Ripulisco eventuale flag di ritorno per FIMS03R
017400031112      * (se FNLR02R richiamato per sapere se ci sono distinte aperte
017500031112      *  non inserite nel piano a video di FIMS03R)
017600031112     c                   if        DLRtfv = '3'
017700031112     c                   clear                   DLRfs1
017800031112     c                   endif
017900940711     C* IMPOSTO I CAMPI
018000940802     C                   MOVEL     DLRPGM        V0CPGM
018100940802     C                   MOVEL     DLRGDA        V0CDIN
018200940802     C                   MOVEL     DLRGAL        V0CDFI
018300940802     C                   MOVEL     DLRTFV        V0CTFV
018400940802     C                   MOVEL     DLRNPG        V0CNPG
018500940802     C                   MOVEL     DLRFGS        V0CFGS
018600050512     c                   exsr      chainfgs
018700940802     C                   MOVEL     DLRNFV        V0CNFV
018800950615     C*
018900950615     C* VEDO SE VISUALIZZARE LA PRIMA VIDEATA (SCELTA PARAMETRI) O NO
019000950615     C     DLRRIC        IFNE      '§'                                          '§' SI 1° VIDEO
019100020612     c                   move      'N'           v0impo
019200020612     c                   eval      v0ctpb = 'N'
019300031113if  2c                   if        SDSprm > 1
019400031113     c                   eval      V0ctpb = 'T'
019500031113e   2c                   endif
019600950615     C                   GOTO      FOR01
019700950615     C                   ENDIF
019800911010     C                   END
019900911004     C*
020000900515      *          =============
020100900509     C     INIZIO        TAG
020200900515      *          =============
020300950615     C                   CLEAR                   V0CNFV
020400050512     C                   CLEAR                   V0Ctpm
020500020423     c                   move      'N'           v0impo
020600020612     c                   eval      v0ctpb = 'N'
020700911010     C*
020800900515     C     FMT00         TAG
020900911004     C                   SETOFF                                       02
021000950615     C                   EXFMT     LR01D00
021100941027     C*
021200941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
021300050512     C                   SETOFF                                       432809
021400050512     C                   SETOFF                                       46
021500941027     C*
021600910319     C* CMD3  - FINE LAVORO
021700910319     C   KC              GOTO      FINE                                         F3=FINE
021800940711     C*
021900950615     C     FOR01         TAG
022000940711     C* CONTROLLO CAMPI
022100940711     C                   EXSR      CTRD00
022200941027     C*
022300050512     c   09
022400050512     Cor 90              GOTO      FMT00
022500070329     C*
022600070329      * Se categoria foglio 4 devo chiamare il nuovo programma
022700070329     C*  FIDG31R
022800070329     c                   If        v0cNPG = '4'
022900070329      *
023000070329     C*  Controlla Con il P.O. la data abilitazione
023100070329     C     v0cFGS        CHAIN     AZORG01l
023200070329    1C                   IF        %found(azorg01l)
023300070329     C                   MOVEL     ORGde6        og146
023400070329     c                   if             §ogdda <> *blank and §ogdda <> *all'0'
023500070329      * se c'è una data in organigramma e siamo partiti in produzione
023600070329     c                   move      §ogdda        data_org          8 0
023700070406     c                   if        data_org <> 20391231 and dateu8 >= data_org
023800070329     C*
023900070329     C                   Z-ADD     DATinW        FDGGDA
024000070329     C                   Z-ADD     DATfiW        FDGGAL
024100070329     C                   move      V0CNPG        FDGNPG
024200070403     C                   move      'I'           FDGRIC
024300070329     C                   Z-ADD     V0CNFV        FDGNFV
024400070329     C                   Z-ADD     V0CFGS        FDGFGS
024500070329     C                   move      V0CTFV        FDGTFV
024600160309     C                   move      V0CTpm        FDGtdis
024700070329     C                   move      'T'           FDGTPV
024800070329     c                   eval      kpjbus = kpjbu
024900070404     c                   clear                   $FV6
025000070404     c                   do        600           yy                3 0
025100070404if  2c                   if        SDSprm > 1
025200070404     c                   z-add     $FV(yy)       $FV6(yy)
025300070404     c                   else
025400070404     c                   z-add     999999        $FV6(yy)
025500070404     c                   end
025600070404     c                   enddo
025700070329     C                   MOVEL(p)  FIDG31DS      KPJBU
025800070329     c                   call      'FIDG31R'
025900070329     c                   parm                    kpjba
026000070404     c                   parm                    $FV6
026100070329     c                   movel     kpjbu         fidg31ds
026200070419     C                   eval      dlrERR  =  FDGerr
026300070419     C                   eval      dlrFS1  =  FDGfs1
026400070419     C                   eval      V0CNFV  =  FDGNFV
026500070329     C                   eval      DLRNFV  =  FDGNFV
026600070329     c                   eval      kpjbu = kpjbus
026700070329     C*
026800070329     c                   if        DLRRIC = *blank
026900070329     C                   GOTO      FMT00
027000070329     c                   else
027100070329     C                   GOTO      Fine
027200070329     c                   end
027300070329     C*
027400070406     c                   endIf
027500070329     c                   end
027600070329     c                   end
027700070329    1C                   ENDIF
027800911010      **
027900940802      ** NON IMMESSO FOGLIO
028000940712     C     V0CNFV        IFEQ      0
028100010509     C     V0CNPG        COMP      '4'                                    14
028200900517     C                   EXSR      RICERC
028300940711     C*
028400911010      * NON CI SONO FV DA VISUALIZZARE
028500940712     C     *IN48         IFEQ      *ON
028600940712     C* PGM RICHIAMATO
028700940712     C     *IN01         IFEQ      *ON
028800940802     C                   MOVEL     '9'           DLRERR
028900940712     C                   GOTO      FINE
029000940712     C                   ELSE
029100940712     C* PGM NON RICHIAMATO
029200940712     C                   GOTO      FMT00
029300940712     C                   ENDIF
029400940712     C                   ENDIF
029500900522      *
029600010510     C     *IN14         IFEQ      *OFF
029700940712     C                   WRITE     LR01RC2
029800911015     C     FORCT         TAG
029900940712     C                   EXFMT     LR01CT2
030000010510     C                   ENDIF
030100010510     C     *IN14         IFEQ      *ON
030200010510     C                   WRITE     LR01RC3
030300010510     C     FORCT1        TAG
030400010510     C                   EXFMT     LR01CT3
030500010510     C                   ENDIF
030600910319      * CMD12 - RITORNO
030700940712     C   KL
030800940712     CANN01              GOTO      INIZIO
030900940712     C   KL
031000940712     CAN 01              GOTO      FINE
031100900522      *          =============
031200900522     C     LEGGI         TAG
031300900522      *          =============
031400010510     C  N14              READC     LR01SF2                                29
031500010510     C   14              READC     LR01SF3                                29
031600911015     C*
031700911015     C* FINE LETTURA SE NON SCELTE EFFETTUATE RIMANGO NEL SUBFILE
031800940712     C   29SCE           IFEQ      1
031900940712     C  N01              GOTO      INIZIO
032000940712     C   01              GOTO      FINE
032100911015     C                   ELSE
032200010510     C  N14              GOTO      FORCT
032300010510     C   14              GOTO      FORCT1
032400911015     C                   END
032500910204     C*
032600010510     C  N14V2CSCE        IFNE      '1'
032700910204     C                   GOTO      LEGGI
032800910204     C                   END
032900010510     C   14V3CSCE        IFNE      '1'
033000010510     C                   GOTO      LEGGI
033100010510     C                   END
033200900522      *
033300911015     C                   Z-ADD     1             SCE               1 0
033400910423      *
033500941027      * SE RICHIAMATO VADO A FINE PGM
033600940712     C     *IN01         IFEQ      *ON
033700010510     C  N14              Z-ADD     V2CNFV        DLRNFV
033800010510     C   14              Z-ADD     V3CNFV        DLRNFV
033900950615     C                   MOVEL     V0CNPG        DLRNPG
034000940712     C                   GOTO      FINE
034100940712     C                   ELSE
034200940712     C*
034300010510     C  N14              Z-ADD     V2CNFV        V0CNFV
034400010510     C  N14              Z-ADD     V2CFGS        V0CFGS
034500010510     C   14              Z-ADD     V3CNFV        V0CNFV
034600050512     C** 14              Z-ADD     V3CFGS        V0CFGS
034700940712     C     KFVV          CHAIN     FNFVV000                           30
034800910423      *
034900910319      * VISUALIZZO FOGLIO VIAGGIO
035000940712     C  N30              EXSR      VISNFV
035100910319      * CONTROLLO SE ALTRA SELEZIONE
035200910319     C                   GOTO      LEGGI
035300940712     C                   ENDIF
035400911010      **
035500911010      ** IMMESSO FOGLIO VIAGGIO
035600910319     C                   ELSE
035700910319      * VISUALIZZO
035800910319     C                   EXSR      VISNFV
035900910319      *
036000910319     C                   GOTO      INIZIO
036100900517     C                   END
036200900515      *
036300000000     C     FINE          TAG
036400940802     C                   MOVEL     DSLR01        KPJBU
036500000000     C                   SETON                                        LR
036600920806      *-----------------------------------------------------***********
036700920806      * CARICO TABELLE                                         CARTAB
036800920806      *-----------------------------------------------------***********
036900920806     C     CARTAB        BEGSR
037000161017     c***
037100161017     c* verifico se sono utente EDP o 001
037200161017     c***
037300161017     c                   if        %subst(knmus:1:3)='EDP' or simpou=001
037400161017     c                   seton                                        89
037500161017     c                   endif
037600161017     c
037700940711     C***
037800940711     C* CARICO TIPI INVENTARIO
037900940711     C***
038000940711     C                   MOVEL     *BLANKS       VIDSP1
038100940711     C                   MOVEL     *BLANKS       DESSP1
038200940711     C                   MOVEL     *BLANKS       VIDSP2
038300940711     C                   MOVEL     *BLANKS       DESSP2
038400940711     C                   MOVEL     *BLANKS       VIDSP3
038500940711     C                   MOVEL     *BLANKS       DESSP3
038600940711     C                   MOVEL     *BLANKS       VIDSP4
038700940711     C                   MOVEL     *BLANKS       DESSP4
038800940711     C                   MOVEL     *BLANKS       VIDSP5
038900940711     C                   MOVEL     *BLANKS       DESSP5
039000940711     C                   MOVEL     *BLANKS       VIDSP6
039100940711     C                   MOVEL     *BLANKS       DESSP6
039200940711     C*
039300940711     C                   MOVEL     '7B'          COD
039400940711     C                   Z-ADD     1             I                 2 0
039500940711     C     KTAB          SETLL     TABEL
039600940711     C     KTAB          READE     TABEL                                  30
039700940711     C*
039800940711     C     *IN30         DOWEQ     '0'
039900160929     C                   MOVEL     TBLUNI        DS7B
040000940711     C                   MOVEL     TBLKEY        TIN(I)
040100160929     C                   MOVEL     §7bdes        DIN(I)
040200160929     C                   MOVE      §7Buti        uIN(I)
040300160929     C                   MOVE      §7Bdesb       dbIN(I)
040400940711     C*
040500940711     C                   ADD       1             I
040600940711     C     KTAB          READE     TABEL                                  30
040700940711     C                   END
040800160929
040900160929     C* EMETTO A VIDEO I PRIMI 6
041000160929     c                   z-add     1             I
041100160929     c                   dow       tin(i)<>*blanks
041200160929     C     TIN(I)        IFNE      *BLANKS
041300160929     c     uin(i)        andne     'N'
041400161017     c                   if        *in89 or (not *in89 and uin(I)<>'1' )
041500160929     c                   select
041600160929     c                   when      vidsp1=*blanks
041700160929     C                   MOVEL     TIN(I)        VIDSP1
041800160929     C                   MOVE      ' ='          VIDSP1
041900160929     C                   MOVEL     DIN(I)        DESSP1
042000160929     c                   when      vidsp2=*blanks
042100160929     C                   MOVEL     TIN(I)        VIDSP2
042200160929     C                   MOVE      ' ='          VIDSP2
042300160929     C                   MOVEL     DIN(I)        DESSP2
042400160929     c                   when      vidsp3=*blanks
042500160929     C                   MOVEL     TIN(I)        VIDSP3
042600160929     C                   MOVE      ' ='          VIDSP3
042700160929     C                   MOVEL     DIN(I)        DESSP3
042800160929     c                   when      vidsp4=*blanks
042900160929     C                   MOVEL     TIN(I)        VIDSP4
043000160929     C                   MOVE      ' ='          VIDSP4
043100160929     C                   MOVEL     DIN(I)        DESSP4
043200160929     c                   when      vidsp5=*blanks
043300160929     C                   MOVEL     TIN(I)        VIDSP5
043400160929     C                   MOVE      ' ='          VIDSP5
043500160929     C                   MOVEL     DIN(I)        DESSP5
043600160929     c                   when      vidsp6=*blanks
043700160929     C                   MOVEL     TIN(I)        VIDSP6
043800160929     C                   MOVE      ' ='          VIDSP6
043900160929     C                   MOVEL     DIN(I)        DESSP6
044000160929     c                   leave
044100160929     c                   endsl
044200160929     c                   endif
044300161017     c                   endif
044400160929     c
044500160929     c                   add       1             I
044600160929
044700160929     C                   ENDdo
044800940711     C***
044900160929     C* VEDO SE SONO SIMFEL O 2 liv
045000940711     C***
045100020513     c     simtpp        ifeq      '2'
045200020513     c     simtpp        oreq      *blanks
045300020513     c                   movel     simpou        v0cfgs
045400940711     C                   SETON                                        05
045500940711     C                   ELSE
045600940711     C                   MOVEL     SIMFEL        V0CFGS
045700940711     C                   END
045800050512     c                   exsr      chainfgs
045900050512     c
046000940711     C***
046100940711     C* CARICO CATEGORIE FOGLI
046200940711     C***
046300940711     C                   MOVEL     '7N'          COD
046400940711     C                   Z-ADD     0             I
046500940711     C     KTAB          SETLL     TABEL
046600940711     C     KTAB          READE     TABEL                                  31
046700940711     C*
046800940711     C     *IN31         DOWEQ     '0'
046900940711     C                   MOVEL     TBLUNI        DS7N
047000940711     C* SOLO SE ABILITATA PER FOGLI VARI
047100940711     C     §7NFFV        IFEQ      'S'
047200940711     C     §7NUTI        ANDNE     '9'
047300940711     C                   ADD       1             I
047400940711     C                   MOVEL     ' ='          DEC(I)
047500940711     C                   MOVEL     ALF1          DEC(I)
047600940711     C                   MOVE      §7NDE2        DEC(I)
047700940711     C     I             IFEQ      6
047800940711     C                   SETON                                        31
047900940711     C                   END
048000940711     C                   END
048100940711     C*
048200940711     C  N31KTAB          READE     TABEL                                  31
048300940711     C                   END
048400940711     C*
048500940711     C* IMPOSTO LE DESCRIZIONI A VIDEO
048600940711     C     DEC(1)        IFNE      *BLANKS
048700940711     C                   MOVEL     DEC(1)        DESCA1
048800940711     C     DEC(2)        IFNE      *BLANKS
048900940711     C                   MOVEL     DEC(2)        DESCA2
049000940711     C     DEC(3)        IFNE      *BLANKS
049100940711     C                   MOVEL     DEC(3)        DESCA3
049200940711     C     DEC(4)        IFNE      *BLANKS
049300940711     C                   MOVEL     DEC(4)        DESCA4
049400940711     C     DEC(5)        IFNE      *BLANKS
049500940711     C                   MOVEL     DEC(5)        DESCA5
049600940711     C     DEC(6)        IFNE      *BLANKS
049700940711     C                   MOVEL     DEC(6)        DESCA6
049800940711     C                   END
049900940711     C                   END
050000940711     C                   END
050100940711     C                   END
050200940711     C                   END
050300940711     C                   END
050400940711     C***
050500940711     C* TESTATA
050600940711     C***
050700940712     C                   MOVE      TES(1)        V0CTES
050800941027     C***
050900941027     C* CARICO TABELLA FILIALI GESTITE £1
051000941027     C***
051100941027     C                   CLEAR                   DSUL06
051200941027     C                   MOVE      '£1'          D06COD
051300941027     C                   MOVEL     SIMFEL        D06KEY
051400941027     C                   MOVEL     'L'           D06TLA
051500941027     C                   MOVEL     DSUL06        KPJBU
051600941027     C*
051700941027     C                   CALL      'TRUL06R'
051800941027     C                   PARM                    KPJBA
051900941027     C                   MOVEL     KPJBU         DSUL06
052000941027     C                   MOVEA     LIN           L1
052100920806     C                   ENDSR
052200050512      *-----------------------------------------------------***********
052300050512     c* Impostazione iniziale p.o. gestione
052400050512      *-----------------------------------------------------***********
052500050512     c     CHAINFGS      BEGSR
052600050512     C     V0CFGS        CHAIN     AZORG                              31
052700050512     C  N31              MOVEL     ORGDES        V1DFGS
052800050512     C  N31              MOVEL     ORGDES        V0dFGS
052900050512     c  n31              movel     orgdf0        og150
053000050512     c   31              clear                   og150
053100050512     c   31              clear                   v0dfgs
053200050512     c   31              clear                   v1dfgs
053300050512     c* p.o. con piking messaggerie
053400050512     c                   if        §ogpkm='S'
053500050512     c                   seton                                        08
053600050512     c                   endif
053700050512     c
053800050512     c* salvo campi di comodo
053900050512     c                   eval      fgspkm=§ogpkm
054000050512     c                   eval      savfgs=v0cfgs
054100050512     c                   eval      savin08=*in08
054200050512     c                   ENDSR
054300900515      *-----------------------------------------------------***********
054400940712      * PULIZIA CAMPI VIDEO                                 *  INZD00 *
054500900515      *-----------------------------------------------------***********
054600940712     C     INZD00        BEGSR
054700900515      *
054800950119     C                   MOVEL     'FNLR02R'     V0CPGM
054900940711     C                   MOVEL     ' '           V0CNPG
055000940711     C                   Z-ADD     0             V0CNFV
055100940711     C                   Z-ADD     0             V0CDIN
055200940711     C                   Z-ADD     31129999      V0CDFI
055300940711     C                   MOVEL     *BLANKS       V0CTFV
055400900515      *
055500900514     C                   ENDSR
055600900515      *-----------------------------------------------------***********
055700900517      * RICERCA E SCELTA FOGLIO DI VIAGGIO                  *  RICERC *
055800900515      *-----------------------------------------------------***********
055900900517     C     RICERC        BEGSR
056000941027     C*
056100010509     C  N14              Z-ADD     0             NR2               4 0
056200010509     C   14              Z-ADD     0             NR3               4 0
056300900517      *
056400900517      * RIEMPIO IL SFL
056500040310if  1c                   if        DLRtfv <> '3'
056600010509     C     *IN14         IFEQ      *OFF
056700900517     C                   Z-ADD     1             NR2
056800941229     C                   SETOFF                                       211310
056900940712     C                   WRITE     LR01CT2
057000940712     C                   SETON                                        202123
057100010509     C                   ENDIF
057200010509     C     *IN14         IFEQ      *ON
057300010509     C                   Z-ADD     1             NR3
057400010509     C                   SETOFF                                       211310
057500010509     C                   WRITE     LR01CT3
057600010509     C                   SETON                                        202123
057700010509     C                   ENDIF
057800040310e   1c                   endif
057900911205     C*
058000940711     C* 13 ON - RICHIESTI SOLO FOGLI APERTI
058100940712     C     V0CTFV        IFEQ      '2'
058200040112     c     V0Ctfv        oreq      '3'
058300911205     C                   SETON                                        13
058400911205     C                   END
058500941229     C* 10 ON - IMMESSO FILIALE GESTIONE
058600941229     C     V0CFGS        IFGT      0
058700941229     C                   SETON                                        10
058800941229     C                   END
058900900517      *
059000020905     c                   Select
059100020905     c                   When      *In10 and Not *In13
059200020905     c     Kdat1         setll     fnfvv008
059300020905     c                   When      *In10 and *In13
059400020905     C     KFVV3         SETLL     FNFVV003
059500020905     c                   When      Not *In10 and *In13
059600020905     C     *LOVAL        SETLL     FNFVV003
059700020905     c                   Other
059800020905     c     Kdat          setll     fnfvv002
059900020905     c                   EndSl
060000911205     C*
060100020905     c                   Select
060200020905     c                   When      *In10 and Not *In13
060300020905     c     Kdat2         Reade     Fnfvv008                               88
060400020905     c                   When      *In10 and *In13
060500020905     C     KFVV3         READE     FNFVV003                               88
060600020905     c                   When      Not *In10 and *In13
060700020905     C                   READ      FNFVV003                               88
060800020905     c                   Other
060900020905     c     Wnpg          Reade     Fnfvv002                               88
061000020905     c                   EndSl
061100910319      *
061200040310do  1C     *IN88         DOWEQ     *OFF
061300910319      *
061400040310if  2C     FVVDFV        IFLE      DATFIW
061500900529      *
061600010515     C                   SETOFF                                       96
061700911205      *
061800911205     C   13FVVDFV        IFGE      DATINW
061900010515     C                   SETON                                        96
062000911205     C                   END
062100900529      *
062200910319      * SELEZIONATI SOLO I CHIUSI
062300940712     C     V0CTFV        IFEQ      '1'
062400910319     C     FVVFCF        ANDNE     ' '
062500010515     C                   SETON                                        96
062600900529     C                   END
062700900529      *
062800940712     C     V0CTFV        IFEQ      ' '
062900010515     C                   SETON                                        96
063000900529     C                   END
063100941027     C*
063200940711     C* CONTROLLO CHE SIA FOGLIO DI FILIALE IN GESTIONE
063300040310if  3C   96V0CFGS        IFNE      0
063400010515     C   96FVVFGS        IFNE      V0CFGS
063500010515     C                   SETOFF                                       96
063600941027     C                   ENDIF
063700941027     C*
063800040310x   3C                   ELSE
063900941027     C*
064000941027     C* CARICO SOLO LE FILIALE NON DELL'AREA
064100941027     C     FVVFGS        LOOKUP    L1                                     30
064200010515     C   30              SETOFF                                       96
064300040310e   3C                   ENDIF
064400020611
064500020612      * richiesta la parzializzazione per tipo bolla
064600020612     c                   if        *in14 and v0ctpb <> 'N'
064700020611     c                             and *in96 = *on
064800020612     c                   exsr      rictpb
064900020611     c                   endif
065000031111
065100031111      * richiamato da FIMS03R, che passa i F.V. già inseriti
065200040310if  3c                   if        SDSprm > 1
065300031111     c                   eval      *in30  = *off
065400031111     c     FVVnfv        lookup    $FV                                    30
065500040310if  4c                   if        *in30  = *on
065600031111     c                   eval      *in96  = *off
065700040310e   4c                   endif
065800040310e   3c                   endif
065900031112
066000031112      * richiamato SOLO per sapere se esistono distinte aperte
066100031112      *   non inserito nel piano di cui sono stati passati i F.V.
066200040310if  3c                   if            SDSprm > 1
066300031112     c                             and DLRtfv = '3'
066400040310     c                             and *in96  = *on
066500031112     c                   eval      DLRfs1 = *on
066600031112     c                   goto      FINE
066700040310e   3c                   endif
066800050512     c* Parzializzazione per tipo zone consegna parcel/messaggerie
066900050512     c                   movel     fvvspn        dfvvspn
067000050512     c
067100050512     c                   if        *in08 and v0ctpm<>' '
067200050512     c                   if        (v0ctpm='M' and §fvvtpm<>'M') or
067300050512     c                             (v0ctpm='P' and §fvvtpm ='M')
067400050512     c                   eval      *in96  = *off
067500050512     c                   endif
067600050512     c                   endif
067700910319      *
067800010515      * 96 ON - CARICO RECORD
067900040310if  3C     *IN96         IFEQ      *ON
068000900529      *
068100010509     C  N14              CLEAR                   V2CSCE
068200010509     C  N14              CLEAR                   V2CANN
068300010509     C  N14              CLEAR                   V2CFCF
068400010509     C   14              CLEAR                   V3CSCE
068500010509     C   14              CLEAR                   V3CANN
068600010509     C   14              CLEAR                   V3CFCF
068700010510     C   14              CLEAR                   V3CTPD
068800050512     C   14              CLEAR                   V3CTPM
068900940712     C* GIRO DATA FOGLIO
069000940712     C                   MOVE      FVVDFV        G02INV
069100940712     C                   MOVEL     '3'           G02ERR
069200940712     C                   CALL      'XSRDA8'
069300940712     C                   PARM                    WLBDAT
069400940712     C*
069500010509     C  N14              Z-ADD     G02DAT        V2CDFV
069600010510     C   14              Z-ADD     G02DAT        V3CDFV
069700900517      *
069800010509     C  N14              Z-ADD     FVVNFV        V2CNFV
069900010509     C  N14              MOVEL     FVVDSF        V2CDSF
070000010509     C   14              Z-ADD     FVVNFV        V3CNFV
070100010509     C   14              MOVEL     FVVDSF        V3CDSF
070200911003      * TIPO INVENTARIO
070300040310if  4C     *IN14         IFEQ      *OFF
070400940712     C                   MOVEL     *BLANKS       V2DINV
070500160929     C                   Z-ADD     1             I
070600940712     C     FVVSPG        IFNE      ' '
070700161006     c***                if        fvvfpp<>*blanks
070800161006     c***                eval      I=%lookup(fvvfpp:tin)
070900161006     C***                MOVEL     dbiN(I)       V2DINV
071000161006     c***                else
071100160929     c                   eval      I=%lookup(fvvspg:tin)
071200160929     C                   MOVEL     dbin(I)       V2DINV
071300161006     c***                endif
071400940712     C                   ENDIF
071500040310e   4C                   ENDIF
071600940712     C* FILIALE GESTIONE
071700010509     C  N14              MOVEL     FVVFGS        V2CFGS
071800940712     C     FVVFGS        CHAIN     AZORG                              30
071900010509     C  N14
072000010509     CANN30              MOVEL     ORGDES        V2DFGS
072100010509     C  N14
072200010509     CAN 30              MOVEL     *BLANKS       V2DFGS
072300010509      *
072400020611      * se richiesto tipo foglio "4"
072500040310if  4C                   IF        *in14 = *ON
072600050512     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
072700050512     c                   if        *in08
072800050512     c                   if        §fvvtpm='M'
072900050512     c                   movel     'Mes'         v3ctpm
073000050512     c                   endif
073100050512     c                   if        §fvvtpm<>'M'
073200050512     c                   movel     'Prc'         v3ctpm
073300050512     c                   endif
073400050512     c                   endif
073500050512     c
073600020611      * Flag tipo distinta (solo consegna, orm o entrambi)
073700010510     C                   CLEAR                   WBOL
073800010510     C                   CLEAR                   WORM
073900050512     c                   Z-Add     v0cfgs        Wifp
074000020910     C                   Z-ADD     V3CNFV        kfnvv
074100010510     C                   Z-ADD     V3CNFV        W0CNFV
074200020910     c     Karb78        Setll     Fnarb78l
074300040310if  5c                   if        %equal(fnarb78l)
074400010510     c                   movel     'S'           wbol              1
074500040310x   5c                   else
074600070112     C     KBRV          SETLL     FNBRV01L
074700070112     c                   if        %equal(fNbrv01l)
074800010510     c                   movel     'S'           wbol              1
074900010510     c                   endif
075000040310e   5c                   endif
075100010510     C     KORM          SETLL     FNORM06L
075200040310do  5c                   do        *hival
075300010511     C     KORM          reade     FNORM06L
075400010511     c                   if        %eof(fnorm06l)
075500010511     c                   leave
075600010511     c                   endif
075700010511     c                   if        ormatb = *blanks and ormfao < 999
075800010510     C                   move      'S'           worm              1
075900010510     c                   endif
076000040310e   5c                   enddo
076100010510     C                   if        wbol = 'S' and worm <> 'S'
076200020611     C                   movel     'CON'         v3ctpd
076300010510     C                   endif
076400010510     C                   if        wbol <> 'S' and worm = 'S'
076500020611     C                   movel     'ORM'         v3ctpd
076600010510     C                   endif
076700010510     C                   if        wbol = 'S' and worm = 'S'
076800020611     C                   movel     'C/O'         v3ctpd
076900010510     C                   endif
077000020612      * Calcolo importi se non già fatto prima
077100040310if  5c                   if        v0ctfv = '2' and v0impo = 'S'
077200020612     c                   if        v0ctpb = 'N'
077300020423     c                   exsr      totalizzo
077400020423     c                   z-add     w3imas        v3imas
077500020423     c                   z-add     w3imca        v3imca
077600020612     c                   endif
077700040310x   5c                   else
077800020423     c                   z-add     0             v3imas
077900020423     c                   z-add     0             v3imca
078000040310e   5c                   endif
078100020611
078200020612      * Tipo bolla
078300020612     c                   clear                   v3ctpb
078400040310if  5c                   if        v0ctpb <> 'N'
078500020611     c                   select
078600020611     c                   when      wbar = *on  and wppt = *off
078700020612     c                   eval      v3ctpb = 'BAR'
078800020611     c                   when      wbar = *off and wppt = *on
078900020612     c                   eval      v3ctpb = 'PPT'
079000020611     c                   when      wbar = *on  and wppt = *on
079100020612     c                   eval      v3ctpb = 'B/P'
079200020611     c                   endsl
079300040310e   5c                   endif
079400020611
079500040310e   4C                   ENDIF
079600911008      *
079700911008      * ANNULLATO
079800910319     C     FVVATB        IFEQ      '*'
079900010509     C  N14              MOVEL     'ANN'         V2CANN
080000010509     C   14              MOVEL     'ANN'         V3CANN
080100900517     C                   END
080200961016      * DATI IN RICEZIONE
080300040310if  4C     FVVFSC        IFEQ      'B'
080400010509     C  N14              MOVE      'DR'          V2CFCF
080500010509     C   14              MOVE      'DR'          V3CFCF
080600040310x   4C                   ELSE
080700040310if  5C     FVVFCF        IFNE      ' '
080800940712      * CHIUSO
080900940712     C     FVVFCF        IFEQ      'S'
081000010509     C  N14              MOVEL     'C '          V2CFCF
081100010509     C   14              MOVEL     'C '          V3CFCF
081200940712     C                   ELSE
081300940712     C* BLOCCATO
081400010509     C  N14              MOVEL     'BL'          V2CFCF
081500010509     C   14              MOVEL     'BL'          V3CFCF
081600010509     C                   ENDIF
081700040310e   5C                   ENDIF
081800040310e   4C                   ENDIF
081900900517      *
082000040310if  4c                   if        DLRtfv <> '3'
082100010509     C  N14              WRITE     LR01SF2
082200010509     C  N14              ADD       1             NR2
082300010509     C   14              WRITE     LR01SF3
082400010509     C   14              ADD       1             NR3
082500040310e   4c                   endif
082600900517      *
082700040310e   3C                   ENDif
082800910319      *
082900020905     c                   Select
083000020905     c                   When      *In10 and Not *In13
083100020905     c     Kdat2         Reade     Fnfvv008                               88
083200020905     c                   When      *In10 and *In13
083300020905     C     KFVV3         READE     FNFVV003                               88
083400020905     c                   When      Not *In10 and *In13
083500020905     C                   READ      FNFVV003                               88
083600020905     c                   Other
083700020905     c     Wnpg          Reade     Fnfvv002                               88
083800020905     c                   EndSl
083900900517      *
084000040310x   2C                   ELSE
084100911205     C  N13              SETON                                        88
084200941229     C   10
084300941229     CAN 13KFVV3         READE     FNFVV003                               88
084400941229     C  N10
084500941229     CAN 13              READ      FNFVV003                               88
084600040310e   2C                   ENDif
084700910319      *
084800040310e   1C                   ENDdo
084900900517      *
085000900517      * NON TROVATI FOGLI IN INTERVALLO RICHIESTO
085100040630     C**n14NR2           IFEQ      1
085200040630     C  n14NR2           IFLE      1
085300920806     C                   SETON                                        48
085400900517     C                   END
085500040630     C***14NR3           IFEQ      1
085600040630     C   14NR3           IFLE      1
085700010517     C                   SETON                                        48
085800010517     C                   END
085900900517      *
086000920806     C                   ENDSR
086100020611      *-----------------------------------------------------
086200020612      * RICERCA PER TIPO BOLLA
086300020611      *-----------------------------------------------------
086400020612     c     rictpb        begsr
086500020611
086600020611     c                   eval      wbar = *off
086700020611     c                   eval      wppt = *off
086800020612     c                   clear                   w3imca           13 3
086900020612     c                   clear                   w3imas           13 3
087000020611
087100020910     c                   Z-Add     fvvfgs        wifp
087200020611     c                   z-add     fvvnfv        kfnvv
087300020910     c     karb78        setll     fnarb78l
087400020910 b1  c                   if        not %equal(fnarb78l)
087500020612     c                   if        v0ctpb <> 'T'
087600020611     c                   eval      *in96 = *off
087700020612     c                   endif
087800020612 x1  c                   else
087900020612 b2  c                   do        *hival
088000020910     c     karb78        reade     fnarb78l
088100020910     c                   if        %eof(fnarb78l)
088200020611     c                   leave
088300020611     c                   endif
088400020611     c                   if        arbtsp <> 'P'
088500020611     c                   eval      wbar = *on
088600020611     c                   else
088700020611     c                   eval      wppt = *on
088800020611     c                   endif
088900020612
089000020612      * se richiesto ormai che ci sono totalizzo gli assegnati
089100020612 b3  c                   if        v0ctfv = '2' and v0impo = 'S'
089200020612     c                   if        arbica = *blank or arbica = 'R'
089300020612     c     kimpo         chain     fiar601l                           71
089400020612     c  n71              add       ar6ift        w3imas
089500020612     c                   endif
089600020612      * totalizzo i contrassegni
089700020612 b4  c                   if        arbicc = *blank or arbicc = 'R'
089800051114     c     kimpo         chain     fiar901l                           71
089900020612 b5  c                   if        not *in71
090000020612 b6  c                   if        ar9vca = 'EUR'
090100020612     c                   add       ar9cas        w3imca
090200020612 x6  c                   else
090300020612      *se la divisa non è EUR traduco con yeurco l'importo
090400020612     c                   CLEAR                   YEURDS
090500020612     c                   MOVEL     ar9vca        YECDVI
090600020612     c                   Z-ADD     ar9cas        YECIMI
090700020612     c                   MOVEL     'EUR'         YECDVO
090800020612     c                   MOVEL     'H'           YECTAR
090900020612     c                   CALL      'YEURCO'
091000020612     c                   PARM                    YEURDS
091100020612     c                   add       YECIMO        W3imca
091200020612 e6  c                   endif
091300020612 e5  c                   endif
091400020612 e4  c                   endif
091500020612
091600020612 x3  c                   else
091700020612     c                   z-add     0             v3imas
091800020612     c                   z-add     0             v3imca
091900020612 e3  c                   endif
092000020612
092100020612 e2  c                   enddo
092200020611
092300020611     c                   select
092400020612     c                   when      v0ctpb = 'B' and wppt = *on
092500020611     c                   eval      *in96 = *off
092600020612     c                   when      v0ctpb = 'P' and wbar = *on
092700020611     c                   eval      *in96 = *off
092800020612     c                   when      v0ctpb = 'M' and (wbar = *off or wppt = *off)
092900020611     c                   eval      *in96 = *off
093000020611     c                   endsl
093100020612 e1  c                   endif
093200020611
093300020611     c                   endsr
093400900517      *-----------------------------------------------------***********
093500940711      * CONTROLLI FORMATO 0                                 *  CTRD00 *
093600900517      *-----------------------------------------------------***********
093700940711     C     CTRD00        BEGSR
093800050512     C                   SETOFF                                       9008
093900941027     C*
094000941027     C* SE IMMESSA DEVE ESSERE FILIALE VALIDA
094100050512     c                   clear                   fgspkm
094200050512     c                   clear                   v0dfgs
094300941027     C     V0CFGS        IFNE      0
094400941027     C     V0CFGS        CHAIN     AZORG                              30
094500941027     C     *IN30         IFEQ      *ON
094600941027     C                   MOVEL     MSG(1)        V0CMSG
094700941027     C                   SETON                                        439028
094800941027     C                   GOTO      ENDCT0
094900050512     c                   else
095000050512     c                   movel     orgdf0        og150
095100050512     c                   movel     §ogpkm        fgspkm
095200050512     c                   movel     orgdes        v0dfgs
095300050512     c
095400050512     c* P.o. con picking, messaggerie: visualizzo parzializzazione
095500050512     c                   if        fgspkm='S'
095600050512     c                   seton                                        08
095700050512    1C                   ENDif
095800050512    1C                   END
095900941027    1C                   END
096000050512     c
096100050512     c* Filiale apertura obbligatoria se categoria 4
096200050512     c                   if        v0cnpg='4' and v0cfgs=0
096300050512     C                   MOVEL     MSG(2)        V0CMSG
096400050512     C                   SETON                                        439028
096500050512     C                   GOTO      ENDCT0
096600050512     c                   endif
096700050512     c
096800050512     c* riemetto video 1 senza errori se per cambio p.o. gestione, cambia
096900050512     c*  l'indicatore del picking messaggerie S/N
097000050512     c                   if        v0cfgs<>savfgs
097100050512     c                   if        (savin08='0' AND *IN08)   OR
097200050512     c                             (savin08='1' and not *in08)
097300050512     c                   seton                                        09
097400050512     c                   endif
097500050512     c                   eval      savfgs=v0cfgs
097600050512     c                   eval      savin08=*in08
097700050512     c                   endif
097800910319      *
097900910319      * CATEGORIA FOGLIO VIAGGIO OBBLIGATORIA
098000940712    1C     V0CNPG        IFEQ      ' '
098100910319     C                   SETON                                        4290
098200940711     C                   GOTO      ENDCT0
098300940712    1C                   END
098400920806     C* RICERCA IN TABELLA
098500940712    1C     V0CNPG        IFEQ      '?'
098600920806     C                   MOVEL     CODUT         §KUT
098700920806     C                   MOVEL     '7N'          §COD
098800940712     C                   MOVE      ' '           V0CNPG
098900920806     C                   MOVEL     *BLANKS       §KEY
099000920806     C                   CALL      'X§TABER'
099100920806     C                   PARM                    §KUT              1 0
099200920806     C                   PARM                    §COD              2
099300920806     C                   PARM                    §KEY              8
099400920806     C                   PARM                    §DES             30
099500940712     C                   MOVEL     §KEY          V0CNPG
099600920806     C                   SETON                                        90
099700940711     C                   GOTO      ENDCT0
099800940712    1C                   END
099900920806     C* CONTROLLO
100000920806     C                   MOVEL     '7N'          COD
100100920806     C                   MOVEL     *BLANKS       KEY
100200940712     C                   MOVEL     V0CNPG        KEY
100300920806     C* INESISTENTE
100400920806     C     KTAB2         CHAIN     TABEL                              42
100500920806     C* ANNULLATA
100600940712    1C  N42TBLFLG        IFNE      ' '
100700920806     C                   SETON                                        42
100800920806     C                   ELSE
100900920806     C                   MOVEL     TBLUNI        DS7N
101000920806     C* NON E' CATEGORIA DI FOGLI VARI
101100940712    2C     §7NFFV        IFNE      'S'
101200920806     C                   SETON                                        42
101300940712    2C                   END
101400940712    1C                   END
101500920806     C*
101600920806     C   42              SETON                                        90
101700940711     C   42              GOTO      ENDCT0
101800940712     C                   MOVEL     §7NDES        V1DNPG
101900940713     C                   MOVEL     §7NDES        V0DNPG
102000940712     C                   MOVEL     V0CNPG        WNPG
102100900517      *
102200900517      * CONTROLLO DATA INIZIALE
102300900517      *
102400940712    1C     V0CDIN        IFNE      0
102500940712     C                   MOVE      V0CDIN        G02DAT
102600900517     C                   MOVEL     *BLANK        G02ERR
102700940711     C                   CALL      'XSRDA8'
102800900517     C                   PARM                    WLBDAT
102900940712    2C     G02ERR        IFEQ      '1'
103000900517     C                   SETON                                        76  90
103100940711     C                   GOTO      ENDCT0
103200940712    2C                   END
103300940711     C*
103400940711     C                   Z-ADD     G02INV        DATINW
103500940712     C                   Z-ADD     G02DAT        V0CDIN
103600940711     C*
103700940712   X1C                   ELSE
103800940711     C*
103900940711     C                   Z-ADD     0             DATINW
104000940712    1C                   END
104100900517      *
104200900517      * CONTROLLO DATA FINALE
104300900517      *
104400940712    1C     V0CDFI        IFEQ      0
104500940712     C                   Z-ADD     31129999      V0CDFI
104600940712     C                   Z-ADD     99991231      DATFIW
104700940712   X1C                   ELSE
104800900529      *
104900940712     C                   MOVE      V0CDFI        G02DAT
105000900517     C                   MOVEL     *BLANK        G02ERR
105100940712     C                   CALL      'XSRDA8'
105200900517     C                   PARM                    WLBDAT
105300940712    2C     G02ERR        IFEQ      '1'
105400900517     C                   SETON                                        77  90
105500940711     C                   GOTO      ENDCT0
105600940712    2C                   END
105700940711     C*
105800940711     C                   Z-ADD     G02INV        DATFIW
105900940712     C                   Z-ADD     G02DAT        V0CDFI
106000900517      *
106100900517      * DATA INIZIALE MAGGIORE DATA FINALE
106200940712    2C     DATINW        IFGT      DATFIW
106300900517     C                   SETON                                        75  90
106400940711     C                   GOTO      ENDCT0
106500940712    2C                   END
106600940712    1C                   END
106700940711     C*
106800940711      * NUMERO FOGLIO
106900940711     C*
107000940712    1C     V0CNFV        IFGT      0
107100940711     C* FOGLIO VIAGGIO
107200940712     C     KFVV          CHAIN     FNFVV000                           38
107300940711     C*
107400940711     C* VEDO SE E' FOGLIO DI FILIALE GESTIBILE
107500940712     C* FOGLIO INESISTENTE
107600941027    2C     *IN38         IFEQ      *ON
107700940712     C                   SETON                                          90
107800940712     C                   GOTO      ENDCT0
107900940712    2C                   ENDIF
108000940712    1C                   ENDIF
108100050512     c* Per visualizzare gli importi rchiedere tipi fogli = 2
108200050512     c                   if        v0impo='S' and v0ctfv<>'2'
108300050512     C                   MOVEL     MSG(3)        V0CMSG
108400050512     C                   SETON                                        469028
108500050512     C                   GOTO      ENDCT0
108600050512     c                   endif
108700940711     C*
108800940711     C     ENDCT0        ENDSR
108900900515      *-----------------------------------------------------***********
109000900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
109100900515      *-----------------------------------------------------***********
109200900515     C     RIEMPI        BEGSR
109300900515      *
109400940712     C                   SETOFF                                       525306
109500050512     C                   SETOFF                                       07
109600940712     C* FILIALE GESTIONE
109700940712     C     FVVFGS        CHAIN     AZORG                              30
109800940713     C  N30              MOVEL     ORGDES        V1DFGS
109900940712     C* DATA FOGLIO
110000910319     C                   MOVE      FVVDFV        G02INV
110100910319     C                   MOVE      *ZEROS        G02DAT
110200940712     C                   MOVEL     '3'           G02ERR
110300940712     C                   CALL      'XSRDA8'
110400910319     C                   PARM                    WLBDAT
110500940712     C                   Z-ADD     G02DAT        V1CDFV
110600910319      *
110700940712     C                   MOVE      FVVDSF        V1CDSF
110800050512     C*                  MOVE      FVVSPN        V1CSPN
110900050512     C                   MOVEl     FVVSPN        dfvvspn
111000050512     c                   movel     §fvvspn       v1cspn
111100050512     C* FOGLIO INVENTARIO
111200940712     C     FVVNPG        IFEQ      3
111300940712     C                   SETON                                        02
111400161006     c***                if        fvvfpp<>*blanks
111500161006     C***                MOVEL     FVVfpp        V1CSPG
111600161006     c***                else
111700160929     C                   MOVEL     FVVSPG        V1CSPG                          sottotipo
111800161006     c***                endif
111900940712     C                   END
112000911003     C*
112100910319      * VEDO SE ANNULLATO
112200910319     C     FVVATB        IFEQ      '*'
112300940712     C                   SETON                                        5206
112400940712     C                   MOVEL     CANNUL        V1DTFV
112500910319     C                   END
112600910319      *
112700910319      * VEDO SE CHIUSO
112800910319     C     FVVFCF        IFNE      *BLANKS
112900940712     C     FVVFCF        IFEQ      'S'
113000910319     C                   SETON                                        53
113100940712     C                   ELSE
113200940712     C                   SETON                                        06
113300940712     C                   MOVEL     CBLOCC        V1DTFV
113400940712     C                   ENDIF
113500940712     C                   ENDIF
113600941027     C*
113700941027     C* 07 ON ESISTONO SPUNTE SUL FOGLIO
113800000114     C                   Z-ADD     V0CNFV        W0CNFV
113900070112     C     KBRV          SETLL     FNBRV000                               07     ON=TROVATO
114000900515     C                   ENDSR
114100910319      *-----------------------------------------------------***********
114200910319      * VISUALIZZO FOGLIO VIAGGIO                             VISNFV
114300910319      *-----------------------------------------------------***********
114400910319     C     VISNFV        BEGSR
114500910319      *
114600910319     C* RIEMPO FORMATO
114700910319     C                   EXSR      RIEMPI
114800910319      *
114900950112     C     FMT01         TAG
115000940712     C                   EXFMT     LR01D01
115100950112     C*
115200950112     C* CMD21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
115300950112     C     *INKV         IFEQ      *ON
115400950112     C                   CLEAR                   PARA04
115500950112     C                   Z-ADD     V0CNFV        P04NFV
115600950112     C                   Z-ADD     99            P04NRS
115700950112     C                   Z-ADD     99            P04NPS
115800950112     C                   MOVEL     V0CNPG        P04NPG
115900950112     C                   MOVEL     '1'           P04FLG
116000950112     C                   MOVEL     'FNLR01R'     P04PGM
116100950112     C                   Z-ADD     V0CFGS        P04FGS
116200950112     C                   MOVEL     PARA04        KPJBU
116300950112     C                   CALL      'FNLR04R'
116400950112     C                   PARM                    KPJBA
116500950112     C                   GOTO      FMT01
116600950112     C                   ENDIF
116700910319      *
116800910319     C     ENDVIS        ENDSR
116900020423      *--------------------------------------------
117000020423     c     totalizzo     begsr
117100020423      *--------------------------------------------
117200020423     c                   clear                   w3imca           13 3
117300020423     c                   clear                   w3imas           13 3
117400020910     c     Karb78        setll     fnarb78l
117500020423     c                   do        *hival
117600020910     c     Karb78        reade     fnarb78l                               70
117700020423     c   70              leave
117800020423      * totalizzo gli assegnati
117900020423     c                   if        arbica = *blank or arbica = 'R'
118000020423     c     kimpo         chain     fiar601l                           71
118100020423     c  n71              add       ar6ift        w3imas
118200020423     c                   endif
118300020423      * totalizzo i contrassegni
118400020423     c                   if        arbicc = *blank or arbicc = 'R'
118500051114     c     kimpo         chain     fiar901l                           71
118600020423     c                   if        not *in71
118700020423     c                   if        ar9vca = 'EUR'
118800020423     c                   add       ar9cas        w3imca
118900020423     c                   else
119000020423      *se la divisa non è EUR traduco con yeurco l'importo
119100020423     c                   CLEAR                   YEURDS
119200020423     c                   MOVEL     ar9vca        YECDVI
119300020423     c                   Z-ADD     ar9cas        YECIMI
119400020423     c                   MOVEL     'EUR'         YECDVO
119500020423     c                   MOVEL     'H'           YECTAR
119600020423     c                   CALL      'YEURCO'
119700020423     c                   PARM                    YEURDS
119800020423     c                   add       YECIMO        W3imca
119900020423     c                   endif
120000020423     c                   endif
120100020423     c                   endif
120200020423     c                   enddo
120300020423     c                   endsr
120400020423      *--------------------------------------------
120500940711      * INZSR INIZIALE                                         INZSR
120600940711      *-----------------------------------------------------***********
120700940711     C     *INZSR        BEGSR
120800940711     C***
120900940711      * CHIAVI                                                        *
121000940711     C***
121100940711     C     KFVV          KLIST
121200940711     C                   KFLD                    WNPG
121300940711     C                   KFLD                    V0CNFV
121400941027     C                   KFLD                    V0CFGS
121500020910      * Fnarb
121600020910     c     Karb78        Klist
121700020910     c                   Kfld                    Wifp
121800020910     c                   Kfld                    Kfnvv
121900000114     C*
122000000114     C     KBRV          KLIST
122100000114     C                   KFLD                    WNPG
122200000114     C                   KFLD                    W0CNFV
122300000114     C                   KFLD                    V0CFGS
122400940711      *
122500010510     C     KORM          KLIST
122600010510     C                   KFLD                    V0CFGS
122700010510     C                   KFLD                    W0CNFV
122800010510      *
122900940711     C     KDAT          KLIST
123000940711     C                   KFLD                    WNPG
123100940711     C                   KFLD                    DATINW
123200020905
123300020905     c     Kdat1         Klist
123400020905     c                   Kfld                    V0cfgs
123500020905     c                   Kfld                    Wnpg
123600020905     c                   Kfld                    Datinw
123700020905
123800020905     c     Kdat2         Klist
123900020905     c                   Kfld                    V0cfgs
124000020905     c                   Kfld                    Wnpg
124100940711      *
124200940711     C     KTAB2         KLIST
124300940711     C                   KFLD                    CODUT
124400940711     C                   KFLD                    COD               2
124500940711     C                   KFLD                    KEY               8
124600940711     C     KTAB          KLIST
124700940711     C                   KFLD                    CODUT
124800940711     C                   KFLD                    COD               2
124900940711     C     KFVV3         KLIST
125000940711     C                   KFLD                    V0CFGS
125100940711     C                   KFLD                    WNPG
125200020423     C     Kimpo         Klist
125300020423     C                   Kfld                    arbaas
125400020423     C                   Kfld                    arblnp
125500020423     C                   Kfld                    arbnrs
125600020423     C                   Kfld                    arbnsp
125700940711     C* LIKE DEFN
125800940711     C     *LIKE         DEFINE    FVVDFV        DATINW
125900940711     C     *LIKE         DEFINE    FVVDFV        DATFIW
126000940711     C     *LIKE         DEFINE    FVVNPG        WNPG
126100000114     C     *LIKE         DEFINE    BRVNFV        W0CNFV
126200070329     C***
126300070329     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
126400070329     C                   TIME                    W0140            14 0
126500070329     C                   MOVE      W0140         UDATE8            8 0
126600070329     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
126700070329     C*
126800070329     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
126900070329     C                   MOVEL     *BLANK        G02ERR
127000070329     C                   CALL      'XSRDA8'
127100070329     C                   PARM                    WLBDAT
127200070329     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
127300070329      *
127400070329      *  Se richiamata da Azione/Menù
127500070329     c                   CLEAR                   se_EDP            1
127600070329     c                   if        %subst(Knmus:1:3) = 'EDP'
127700070329     c                   MOVE      'S'           se_EDP
127800070329     c                   end
127900940712     C*
128000940712     C                   ENDSR
128100940712     C*
128200941027**  PGM RICHIAMANTI
128300940711FNLR02R
128400940711FNLR25R
128500940711**  TES
128600920805 ** INTERROGAZIONE  FOGLI  VARI **
128700920709 ** ABBINAMENTO  FOGLI  VIAGGIO **
128800941027**
128900941027Filiale in gestione errata o mancante
129000050512Punto Operativo apertura Foglio obbligatorio per categoria 4
129100050512Importi visualizzabili soltanto richiedendo Tipo fogli=2 non chiusi
