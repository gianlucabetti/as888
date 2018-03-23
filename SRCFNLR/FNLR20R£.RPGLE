000100041112     H DECEDIT('0,') DATEDIT(*yMd.)
000200020802      * FNLR20R *-----------------------------------------------------*
000300060714      *     TRASMISSIONE  FNFV200T               IN SEDE
000400041112      *     TRASMISSIONE  FNFV400T               IN SEDE
000500060714      *     scrittura diretta di fnfv200f per la filiale
000600020802      *---------------------------------------------------------------*
000700960130     FFNFVA01L  UF   E           K DISK
000800960131     F                                     RENAME(FNFVA000:FNFVA001)
000900020918     FFNFVA06L  IF   E           K DISK
001000060714     FFNFV201l  uF a E           K DISK
001100951122     FFNFV200T  O  A E             DISK    USROPN
001200951122     F                                     RENAME(FNFV2000:FNFV20TT)
001300021030     FFNFV403L  UF   E           K DISK
001400951122     FFNFV400T  O  A E             DISK    USROPN
001500951122     F                                     RENAME(FNFV4000:FNFV40TT)
001600951128     FTABEL00F  IF   E           K DISK
001700020802      *
001800020802      * S c h i e r e
001900020802      *
002000951128     D* SCHIERE RELATIVE ALLA TABELLA "TV"
002100951128     D TTV             S              1    DIM(50)                              TIPO TRAINO
002200951128     D UTC             S              1    DIM(50)                              UTILIZZO CONTROLLI S
002300960130     D DEF             S              1    DIM(50)                              DEFLUENZA
002400951122     D* COMANDI QCMDEXC
002500020802     D C56             S              1    DIM(42) CTDATA PERRCD(42)
002600020802     D C17             S              1    DIM(48) CTDATA PERRCD(48)
002700960201     D MSG             S             70    DIM(4) CTDATA PERRCD(1)              MSG PER SEDE
002800020802      *
002900020802      * S t r u t t u r e   D a t i   definite   E s t e r n a m e n t e
003000020802      *
003100020802     D KPJBA         E DS
003200020802     D DSCMZ         E DS
003300020802     D DSTV          E DS
003400020802     D UT§DSE0F      E DS
003500020802     D CNCR80        E DS
003600020802     D TIBS55DS      E DS
003700020918     d Trul06Ds      e ds
003800020918     d  Lin                    1     90  0 Dim(30)
003900020802      *
004000020802      * S t r u t t u r e   D a t i   definite   I n t e r n a m e n t e
004100020802      *
004200960131     D* PARAMETRI PASSATI DAI PGM CHIAMANTI
004300960131     D PARAM           DS
004400960131     D* PARNUM E' USATO PER RISOTTOMETTERE L'AZIONE DI TRASMISSIONE
004500960131     D  PARNUM                 1      1  0
004600960131     D*
004700951122     D                 DS
004800960131     D  EMME                   1      1
004900020802     D  WFELp                  2      4  0
005000020802     D  WFELa                  5      7  0
005100960131     D  WMBR                   1     10
005200960131     D                 DS
005300111103     D  MSGDET                 1   5000
005400020802     D  MSfelp                27     29
005500020802     D  MSMBR                 30     32
005600020802      *
005700020802      * V a r i a b i l i
005800020802      *
005900020802     d CMZLCL          s              1    inz
006000020802     d W236            s            236    inz
006100111103     d MSGtes          s             44    inz
006200020802     d NRR             s             10  0 inz
006300020802     d COMMAN          s             80    inz
006400020802     d LUNG            s             15  5 inz(80)
006500020802     d WFIL            s              3  0 inz
006600020802     d Y               s              3  0 inz
006700020802     d Var4            s              4    inz
006800020802     d Var5            s              5    inz
006900020802     d WAllM           s              1    inz
007000020802     d WAllR           s              1    inz
007100020802     d WMSed           s              3    inz
007200020802     d WMFil           s              3    inz
007300020802     d wSimFelA        s              3    inz
007400020802     d Fisico          s             10    inz
007500020802     d Logico          s             10    inz
007600020802     d LogMbr          s             10    inz
007700020802     d UlFlg           s              1    inz
007800020802     d WTime           s             14  0                                      ORA & DATA
007900020802     d UDate8          s              8  0                                      GG/MM/AAAA
008000020802     d MmGg            s              4  0
008100020802     d WLNP            s                   like(FV4LNP) inz
008200020802     d WNPG            s                   like(FVANPG) inz
008300020918     d Wlai            s                   Like(FvaLai)
008400021030     d WEpa            s                   Like(Fv4Epa) Inz('A')
008500020918     d XX              s              2  0
008600041112     d wapri           s              1    inz(' ')
008700060714     d wapri2          s              1    inz(' ')
008800020802
008900951122     C*****************************************************************
009000951122     C* RIEPILOGO INDICATORI
009100951122     C*****************************************************************
009200960131     C* 07    - RECORD DI FNFV4 DA TRASMETTERE
009300960201     C* 08    - NON RIUSCITE ALLOCAZIONI MEMBRI PER LA SEDE
009400951128     C* 30/31 - DI COMODO
009500951128     C* 35    - USATO PER LOKUP
009600960131     C* 40    - INDICATORE PER RITENTARE ALLOCAZIONE
009700960131     C* 50/53 - ERRORI IN ALLOCAZIONE
009800951122     C*****************************************************************
009900020802
010000000000     C     *ENTRY        PLIST
010100000000     C                   PARM                    KPJBA
010200960131     C                   MOVEL     KPJBU         PARAM
010300950109     C*
010400000000     C                   Z-ADD     1             CODUT
010500911001     C                   CALL      'X§PARUT'
010600020802     C                   PARM                    UT§DSE0F
010700911001     C                   MOVEL     REC80         CNCR80
010800020802     C                   move      SIMFEL        WSimFelA
010900020918
011000020918      * Carico tutti i p.o. del terminal
011100020918     c                   Clear                   Trul06Ds
011200020918     c                   Eval      D06Cod = '£1'
011300020918     c                   Movel     Simfel        D06Key
011400020918     c                   Movel     Trul06Ds      Kpjbu
011500020918     c                   Call      'TRUL06R'
011600020918     c                   Parm                    Kpjba
011700020918     c                   Movel     Kpjbu         Trul06Ds
011800951114     C*
011900900618     C*---------------------------------------------------------------*
012000951122     C* IMPOSTO ALCUNI VALORI FISSI X CREARE FILE TRASMISSIONE
012100951122     C                   EXSR      IMPO
012200911001     C*
012300960130     C* ALLOCAZIONE MEMBRO PER LA SEDE
012400960130     C                   EXSR      LCKSED
012500951122     C*
012600951122     C* S C R I V O   M E M B R I
012700951122     C                   EXSR      WTRFV
012800951122     C*
012900960131     C* I N V I O
013000041112     c* FV4 solo a sede
013100020802     C                   move      SIMfel        WFELp
013200041112     C                   MOVE      046           WFELa
013300960131     C                   MOVE      WMBR          CMZMBD
013400020802     C                   move      WMBR          CMZmba
013500951122     C* VEDO SE CONTIENE RECORD FNFV400T
013600951122     C                   MOVEL     'FNFV400T'    CMZFLD                          FNFV400T
013700951122     C                   EXSR      NUMREC
013800951122     C*
013900041112    1C     NRR           IFGT      0
014000041112     C                   MOVE      046           CMZDST
014100960131     C*
014200951122     C                   MOVE      'S'           CMZURG                          S=URGENTE
014300951122     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
014400951122     C                   MOVE      *BLANK        CMZAZN                          AZIONE
014500951122     C*
014600960131     C* TRASMISSIONE   F N F V 4 0 0 T
014700960131     C                   MOVEL     'FNFV400T'    CMZFLD                          DA: >>>
014800960131     C                   MOVEL     'FNFV400R'    CMZFLA                          A: >>>
014900960131     C                   MOVE      'TR41'        CMZAZN                          AZIONE
015000971217     C                   MOVE      NRR           CMZNRR                          N.REC.
015100960131     C*
015200041112     C                   MOVE      046           WFIL
015300960131     C                   EXSR      CALCMZ
015400041112    1C                   ENDIF
015500960131     C*
015600971217     C*
015700060714     c* FV2 solo a sede
015800060714     C                   move      SIMfel        WFELp
015900060714     C                   MOVE      046           WFELa
016000060714     C                   MOVE      WMBR          CMZMBD
016100060714     C                   move      WMBR          CMZmba
016200060714     C* VEDO SE CONTIENE RECORD FNFV400T
016300060714     C                   MOVEL     'FNFV200T'    CMZFLD                          FNFV400T
016400060714     C                   EXSR      NUMREC
016500060714     C*
016600060714    1C     NRR           IFGT      0
016700060714     C   30              MOVE      046           CMZDST
016800971217     C*
016900971217     C                   MOVE      'S'           CMZURG                          S=URGENTE
017000971217     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
017100971217     C                   MOVE      *BLANK        CMZAZN                          AZIONE
017200971217     C* TRASMISSIONE   F N F V 2 0 0 T
017300971217     C                   MOVEL     'FNFV200T'    CMZFLD                          DA: >>>
017400971217     C                   MOVEL     'FNFV200R'    CMZFLA                          A: >>>
017500971217     C                   MOVE      'TR42'        CMZAZN                          AZIONE SEDE
017600971217     C                   MOVE      NRR           CMZNRR                          N.REC.
017700971217     C*
017800060714     C                   MOVE      046           WFIL
017900971217     C                   EXSR      CALCMZ
018000971217    2C                   ENDIF
018100951122     C*
018200960131     C* SE NON HO TRASMESSO TUTTO A CAUSA DI ALLOCAZIONI NON RIUSCITE
018300960131     C*   RISOTTOMETTO IL PGM
018400960201    1C     WALLM         IFNE      ' '
018500960201     C     WALLR         ORNE      ' '
018600960131    2C     PARNUM        IFEQ      3
018700960131     C*
018800960131     C* SE DOPO 3 VOLTE NON SONO RIUSCITA AD ALLOCARE INVIO MSG IN SEDE
018900960201     C* TROVATO MEMBRO ALLOCATO PER INVIO IN SEDE
019000960201    3C   08WALLM         IFNE      ' '                                          MEMBRO ALLOCATO
019100960201     C                   MOVEL     MSG(1)        MSGTES
019200960201     C                   MOVEL(P)  MSG(2)        MSGDET
019300020802     C                   movel     SIMfel        MSfelp
019400960201     C                   MOVEL     WMSED         MSMBR
019500111103     C                   eval      wrkEml= 'ced@bartolini.it'
019600111103     C                   call(e)   'TIS701C'
019700111103     C                   parm                    wrkEml          253
019800111103     C                   PARM                    MSGTES
019900111103     C                   PARM                    MSGDET
020000960201    3C                   ENDIF
020100111103     c
020200960201     C* TROVATO RECORD ALLOCATO
020300960201    3C     WALLR         IFNE      ' '                                          RECORD ALLOCATO
020400960201     C                   MOVEL     MSG(1)        MSGTES
020500960201     C                   MOVEL(P)  MSG(4)        MSGDET
020600111103     C                   eval      wrkEml= 'ced@bartolini.it'
020700111103     C                   call(e)   'TIS701C'
020800111103     C                   parm                    wrkEml
020900111103     C                   PARM                    MSGTES
021000111103     C                   PARM                    MSGDET
021100960201    3C                   ENDIF
021200960131     C*
021300960131   X2C                   ELSE
021400960131     C* RISOTTOMETTO L'AZIONE
021500960131     C                   ADD       1             PARNUM
021600960131     C                   MOVEL     PARAM         KPJBU
021700971217     C                   MOVEL     'KSDIFFE'     KQIEB
021800960131     C                   MOVEL     'LR20'        KCOAZ
021900960131     C                   CALL      'BCH10'
022000960131     C                   PARM                    KPJBA
022100960131    2C                   ENDIF
022200960131    1C                   ENDIF
022300960131     C*
022400020802     C                   CLEAR                   TIBS55DS
022500971125     C                   MOVEL     'C'           I50TLA
022600020802     C                   CALL      'TIBS55R'
022700020802     C                   PARM                    TIBS55DS
022800971125     C*
022900020802     C                   MOVEL     *ON           *INLR
023000951122     C*
023100951122     C*--- IMPOSTO CAMPI FISSI ---------------------------------------*
023200951122     C     IMPO          BEGSR
023300960201     C                   CLEAR                   WALLM
023400960201     C                   CLEAR                   WALLR
023500960131     C*
023600951122     C                   MOVEL     'M'           EMME
023700951122     C                   MOVE      '0000'        CMZORI                          ORA
023800951122     C                   MOVE      '0000000'     CMZDTI                          DATA
023900951122     C                   MOVE      'A'           CMZADD                          IN ADD
024000951122     C                   MOVE      'S'           CMZCLR                          CLEAR
024100951122     C                   MOVE      'S'           CMZFTR                          DA TRANS.
024200951122     C                   MOVE      *BLANK        CMZERR                          1= ERR.
024300951122     C                   ENDSR
024400951114     C*
024500960130     C*--- ALLOCAZIONE MEMBRI PER LA SEDE ----------------------------*
024600960130     C     LCKSED        BEGSR
024700960201     C                   SETOFF                                       505108
024800020802     C                   MOVE      '))'          VAR5
024900971121     C                   MOVEL     046           VAR5
025000960130     C*
025100960130     C* ALLOCAZIONE   F N F V 2 0 0 T
025200960130     C                   MOVEA     'FNFV200T'    C56(13)                         FNFV200T
025300020802     C                   movea     WSimFela      C56(35)
025400020802     C                   MOVEA     VAR5          C56(38)
025500020802     C                   MOVEL     *BLANKS       COMMAN
025600960130     C                   MOVEA     C56           COMMAN
025700960130     C                   CALL      'QCMDEXC'                            50
025800960130     C                   PARM                    COMMAN
025900020802     C                   PARM                    LUNG
026000960130     C*
026100960130     C* SE NON RIUSCITA L'ALLOCAZIONE AGGIUNGO IL MEMBRO E RITENTO
026200960131    1C     *IN50         IFEQ      *ON
026300960130     C                   MOVEL     'FNFV200T'    FISICO
026400020802     C                   move      SIMfel        WFELp
026500020802     C                   MOVE      046           WFELa
026600960130     C                   EXSR      CRTMBR
026700960130     C* GIA' ALLOCATO RITENTO PIU' TARDI
026800960131    2C     *IN40         IFEQ      *ON
026900020802     C                   MOVEL     'M'           WALLM
027000020802     C                   MOVEL     046           WMSED
027100960201     C                   SETON                                        5008
027200960131   X2C                   ELSE
027300960131     C                   SETOFF                                       50
027400960131    2C                   ENDIF
027500960131    1C                   ENDIF
027600960130     C*
027700960130     C* ALLOCAZIONE   F N F V 4 0 0 T
027800960130     C                   MOVEA     'FNFV400T'    C56(13)                         FNFV400T
027900020802     C                   movea     WSimFelA      C56(35)
028000020802     C                   MOVEA     VAR5          C56(38)
028100020802     C                   MOVEL     *BLANKS       COMMAN
028200960130     C                   MOVEA     C56           COMMAN
028300960130     C                   CALL      'QCMDEXC'                            51
028400960130     C                   PARM                    COMMAN
028500960130     C                   PARM                    LUNG
028600960130     C*
028700960130     C* SE NON RIUSCITA L'ALLOCAZIONE AGGIUNGO IL MEMBRO E RITENTO
028800960131    1C     *IN51         IFEQ      *ON
028900960130     C                   MOVEL     'FNFV400T'    FISICO
029000020802     C                   move      SIMfel        WFELp
029100020802     C                   MOVE      046           WFELa
029200960130     C                   EXSR      CRTMBR
029300960130     C* GIA' ALLOCATO RITENTO PIU' TARDI
029400960131    2C     *IN40         IFEQ      *ON
029500960201     C                   MOVEL     'M'           WALLM
029600971121     C                   MOVEL     046           WMSED
029700960201     C                   SETON                                        5108
029800960131   X2C                   ELSE
029900960131     C                   SETOFF                                       51
030000960131    2C                   ENDIF
030100960131    1C                   ENDIF
030200960130     C                   ENDSR
030300960130     C*
030400960130     C*--- ADDPFM SUI MEMBRI PER LA SEDE -----------------------------*
030500960130     C     CRTMBR        BEGSR
030600960130     C*
030700960130     C                   CALL      'TRUL50C'
030800020802     C                   PARM                    FISICO
030900020802     C                   PARM                    WMBR
031000960131     C                   PARM                    KNSIF
031100020802     C                   PARM                    LOGICO
031200020802     C                   PARM                    LOGMBR
031300020802     C                   PARM                    ULFLG
031400960130     C* RITENTO L'ALLOCAZIONE
031500960131     C                   CALL      'QCMDEXC'                            40
031600960130     C                   PARM                    COMMAN
031700960130     C                   PARM                    LUNG
031800960130     C*
031900960130     C                   ENDSR
032000960130     C*
032100960130     C*--- SCRITTURA MEMBRI ------------------------------------------*
032200960130     C     WTRFV         BEGSR
032300960130     C******
032400960130     C**  F N F V 2 0 0 T
032500960130     C******
032600060714     c                   if        not *in50
032700060714     c
032800020918     C* USO LA VISTA LOGICA FNFVA06L IN MODO DA LEGGERE SOLO I FOGLI
032900960130     C*   VIAGGIO ABBINATI AD UN FOGLIO ARRIVI
033000020918      * e solo i record non trasmessi
033100960130     C                   Z-ADD     2             WNPG
033200020918      * Devo leggere per tutti i p.o. del terminal
033300020918Do  0c                   Do        30            xx
033400020918     c                   If        Lin(xx) = *zeros
033500020918     c                   Leave
033600020918     c                   EndIf
033700020918     c                   Movel     Lin(xx)       Wlai
033800020918     C     Kfva6         SETLL     FNFVA06L
033900021014     C     Wlai          READe     FNFVA06L                               31
034000960130    1C     *IN31         DOWEQ     *OFF
034100960130     C*
034200960131     C* TRASMETTO IL RECORD SOLO SE: - IL FLAG TRASMISSIONE E' *BLANK
034300960131     C*                              - IL F.VIAGGIO NON E' ILLUMINATO
034400020918    2C**!!!FVAFTR        IFEQ      ' '
034500021014     C     FVAATR        Ifeq      ' '
034600960131     C*
034700960131     C* 39 ON - RECORD ALLOCATO: LO SALTO E NON RIEMPIO IL FLAG TRASMISSIONE
034800960131     C     KFVA          CHAIN     FNFVA01L                           3239
034900960131    3C     *IN39         IFEQ      *ON
035000020802     C                   MOVEL     'R'           WALLR
035100960131   X3C                   ELSE
035200020918     C* TRASMETTO IL RECORD SOLO SE IL F.VIAGGIO NON E' ILLUMINATO
035300020918   3aC     FVAATR        IFEQ      ' '
035400960131     C*
035500960130     C* CONTROLLO SE IL FOGLIO VIAGGIO E' ABBINATO AD UN TRAINO
035600960131    4C     FVATRN        IFGT      0
035700960130     C*
035800960130     C                   Z-ADD     1             Y
035900960130     C     FVATTR        LOOKUP    TTV(Y)                                 35
036000960130     C*
036100960130     C* CONTROLLO CHE IL TIPO TRAINO SIA DA TRASMETTERE:
036200960130     C*  - IL TIPO TRAINO E' *BLANK
036300960205     C     FVATTR        IFEQ      ' '
036400960205     C*  - IL TIPO TRAINO E' PIENO MA NON C'E' IL RECORD IN TABELLA
036500960205     C     *IN35         OREQ      *OFF
036600960205     C*  - IL TIPO TRAINO E' PIENO ED E' DA USARE NEI CONTROLLI DI SEDE
036700960205     C     UTC(Y)        OREQ      'S'
036800060714     c                   if        wapri2=' '
036900960131     C                   EXSR      TRASE2
037000060714     c                   eval      wapri2='S'
037100060714     c                   endif
037200060714     C*
037300060714     C* SCRIVO FILE DI TRASMISSIONE FNFV200T
037400060714     C                   EXSR      WTRFV2r
037500060714     C                   WRITE     FNFV20TT
037600060714     c
037700960131     C                   EXSR      TRAFI2
037800960130     C*
037900960205   X5C                   ELSE
038000960130     C* ALTRIMENTI SE SI TRATTA DI DEFLUENZA LA INVIO SOLO IN FILIALE
038100960131     C                   EXSR      TRAFI2
038200960205    5C                   ENDIF
038300960130     C*
038400960131   X4C                   ELSE
038500960130     C* SI TRATTA DI FOGLIO VIAGGIO NON ABBINATO A TRAINO --> TRASMETTO
038600060714     c                   if        wapri2=' '
038700960131     C                   EXSR      TRASE2
038800060714     c                   eval      wapri2='S'
038900060714     c                   endif
039000060714     C*
039100060714     C* SCRIVO FILE DI TRASMISSIONE FNFV200T
039200060714     C                   EXSR      WTRFV2r
039300060714     C                   WRITE     FNFV20TT
039400060714     c
039500960131     C                   EXSR      TRAFI2
039600960131    4C                   ENDIF
039700960130     C*
039800960131     C* FLAGGO IL RECORD SE E' STATO INVIATO SIA IN SEDE CHE IN FILIALE
039900960130     C                   MOVEL     'T'           FVAFTR
040000041112     C                   Z-ADD     *date         FVADTR
040100960131     C                   UPDATE    FNFVA001
040200020918     C* SE IL F.VIAGGIO E' ILLUMINATO imposto fvaftr ='N'
040300020918  x3ac                   else
040400020918     c                   Eval      FvaFtr = 'N'
040500020918     c                   Update    Fnfva001
040600020918   3ac                   EndIf
040700960131    3C                   ENDIF
040800021014    2C                   ENDIF
040900960130     C*
041000021014     C     Wlai          READe     FNFVA06L                               31
041100960130    1C                   ENDDO
041200020918
041300020918    0c                   EndDo
041400060714     c
041500060714     c* Se  aperto il file lo chiudo
041600060714     c                   if        wapri2='S'
041700060714     c                   close     fnfv200t
041800060714     c                   endif
041900060714     c                   endif
042000960130     C*
042100960130     C******
042200960130     C**  F N F V 4 0 0 T
042300960130     C******
042400021030      * Devo leggere per tutti i p.o. del terminal
042500021030Do  0c                   Do        30            xx
042600021030     c                   If        Lin(xx) = *zeros
042700021030     c                   Leave
042800021030     c                   EndIf
042900021030     c                   Movel     Lin(xx)       Wlai
043000021030     C                   SETOFF                                       3139
043100021031     c     kfv4          SETLL     FNFV403L                               31
043200021031If 0ac                   If        *In31
043300021030     c     Kfv4          READE     FNFV403L                             3931
043400960131     C* 39 ON - RECORD ALLOCATO
043500960201     C   39              MOVEL     'R'           WALLR
043600960131     C*
043700960131    1C     *IN31         DOWEQ     *OFF
043800960131     C     *IN39         ANDEQ     *OFF
043900960131     C                   SETOFF                                       07
044000960131     C                   MOVEL     FV4LNP        WLNP
044100960131     C*
044200041112     c                   if        fv4ft1=' '
044300041112     C                   MOVEL     'N'           FV4FT1
044400041112     C                   Z-ADD     *date         FV4DT1
044500041112    4C                   ENDIF
044600960130     C*
044700041112     C* TRASMISSIONE solo A SEDE
044800960130    3C     FV4FT2        IFEQ      ' '
044900960131     C                   SETON                                        07
045000041112     c                   if        not *in51 and wapri=' '
045100960130     C                   EXSR      TRASE4
045200041112     c                   eval      wapri='S'
045300041112     c                   endif
045400960131     C* FLAGGO IL RECORD SOLO SE E' AVVENUTA LA TRASMISSIONE IN SEDE
045500041112    4C     *IN51         IFEQ      *OFF
045600041112     c                   exsr      wtrfv4
045700960130     C                   MOVEL     'T'           FV4FT2
045800041112     C                   Z-ADD     *date         FV4DT2
045900960131    4C                   ENDIF
046000960130    3C                   ENDIF
046100960130     C*
046200960130     C                   UPDATE    FNFV4000
046300021030    2C**!!!              ENDIF
046400960130     C*
046500021030     C     Kfv4          READE     FNFV403L                             3931
046600960131     C*
046700960131     C* SE TROVO UN RECORD ALLOCATO LO SALTO
046800960131    2C     *IN39         DOWEQ     *ON
046900960131     C     *IN31         ANDEQ     *OFF
047000021031     c     wlnp          andlt     999
047100960201     C                   MOVEL     'R'           WALLR
047200960131     C                   ADD       1             WLNP
047300021031     C     Kfv403        SETll     FNFV403L                               31
047400021031     c                   If        Not *In31
047500021031     c                   iter
047600021031     c                   EndIf
047700021030     C     kfv403        READE     FNFV403L                             3931
047800960131    2C                   ENDDO
047900960131     C*
048000960130    1C                   ENDDO
048100021030
048200021031   0ac                   EndIf
048300021030    0c                   EndDo
048400041112     c* Se  aperto il file lo chiudo
048500041112     c                   if        wapri='S'
048600041112     c                   close     fnfv400t
048700041112     c                   endif
048800960130     C*
048900960130     C                   ENDSR
049000960130     C*
049100960130     C*--- TRASMISSIONE A SEDE ---------------------------------------*
049200960131     C     TRASE2        BEGSR
049300960131     C* ESEGUO OVRDBF SOLO SE E' AVVENUTA L'ALLOCAZIONE DEL FILE
049400020802     C                   MOVE      ')'           VAR4
049500971121     C                   MOVEL     046           VAR4
049600960130     C*
049700960130     C* OVRDBF FNFV200T
049800960130     C                   MOVEA     'FNFV200T'    C17(13)
049900020802     C                   movea     WSimFelA      C17(42)
050000020802     C                   MOVEA     VAR4          C17(45)
050100960130     C                   MOVEL     *BLANKS       COMMAN
050200960130     C                   MOVEA     C17           COMMAN
050300960130     C                   CALL      'QCMDEXC'
050400960130     C                   PARM                    COMMAN
050500960130     C                   PARM                    LUNG
050600960130     C*
050700960130     C* APERTURA FILE
050800960130     C                   OPEN      FNFV200T
050900960130     C                   ENDSR
051000960130     C*
051100960130     C*--- TRASMISSIONE A SEDE ---------------------------------------*
051200960130     C     TRASE4        BEGSR
051300960131     C* ESEGUO OVRDBF SOLO SE E' AVVENUTA L'ALLOCAZIONE DEL FILE
051400960131    1C     *IN51         IFEQ      *OFF
051500020802     C                   MOVE      ')'           VAR4
051600971121     C                   MOVEL     046           VAR4
051700020802     ***C                   Z-ADD     46            LUNG
051800960130     C*
051900960130     C* OVRDBF FNFV400T
052000960130     C                   MOVEA     'FNFV400T'    C17(13)
052100020802     C                   movea     WSimFelA      C17(42)
052200020802     C                   MOVEA     VAR4          C17(45)
052300960130     C                   MOVEL     *BLANKS       COMMAN
052400960130     C                   MOVEA     C17           COMMAN
052500960130     C                   CALL      'QCMDEXC'
052600960130     C                   PARM                    COMMAN
052700960130     C                   PARM                    LUNG
052800960130     C*
052900960130     C* APERTURA FILE
053000960130     C                   OPEN      FNFV400T
053100960131    1C                   ENDIF
053200960130     C                   ENDSR
053300960130     C*
053400960130     C*--- TRASMISSIONE A FILIALE ------------------------------------*
053500960131     C     TRAFI2        BEGSR
053600060714     C* riempo i campi per ora arrivo reale
053700061019     C                   MOVEL     'A'           FV2EPA
053800061019     C                   MOVEL     'R'           FV2TDH
053900061019     c                   clear                   fv2abn
054000061019     c     kfv2          chain     fnfv201l
054100061019     c
054200060714     C                   EXSR      WTRFV2r
054300061019     c
054400060714     c                   eval      fv2ftr='N'
054500060714     c                   eval      fv2dtr=*date
054600061019     c                   if        %found(fnfv201l)
054700061019     C                   update    FNFV2000                             99
054800061019     c                   else
054900061019     C                   WRITE     FNFV2000                             99
055000061019     c                   endif
055100060714     c
055200960130     C                   ENDSR
055300960130     C*
055400960130     C*--- SCRITTURA FNFV200T ----------------------------------------*
055500060714     C     WTRFV2r       BEGSR
055600960130     C*
055700960130     C                   CLEAR                   FV2ATB
055800960130     C                   MOVEL     FVALNP        FV2LNP
055900960130     C                   MOVEL     FVANFV        FV2NFV
056000960130     C                   MOVEL     FVALAI        FV2LAI
056100960130     C                   MOVEL     'A'           FV2EPA
056200960130     C                   MOVEL     'R'           FV2TDH
056300960130     C                   MOVEL     FVAAED        FV2DPA
056400960130     C                   MOVEL     FVAAEH        FV2HPA
056500960130     C                   MOVEL     FVARTA        FV2RTC
056600960130     C                   ENDSR
056700960130     C*
056800960130     C*--- SCRITTURA FNFV400T ----------------------------------------*
056900960130     C     WTRFV4        BEGSR
057000041112     C                   MOVEL     wdata         FV4DTV
057100960130     C                   MOVEL     WTIME         FV4ORV
057200960130     C                   WRITE     FNFV40TT
057300960130     C                   ENDSR
057400960131     C*
057500960131     C*--- PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE -----------------*
057600960131     C     NUMREC        BEGSR
057700960131     C*
057800960131     C                   Z-ADD     0             NRR
057900960131     C                   CALL      'TRUL51C'
058000960131     C                   PARM                    KNSIF
058100020802     C                   PARM                    CMZFLD
058200020802     C                   PARM                    WMBR
058300020802     C                   PARM                    NRR
058400960131     C*
058500960131     C                   ENDSR
058600960131     C*
058700960131     C*--- CALL PGM CHE CREA FILE DI TRASMISSIONE --------------------*
058800960131     C     CALCMZ        BEGSR
058900960131     C*
059000020802     C                   CLEAR                   TIBS55DS
059100971121     C                   MOVEL     KNSIF         I50PSI
059200971121     C                   MOVEL     SIMFEL        I50PPO
059300971217     C                   MOVEL     WFIL          I50APO
059400020802     C                   CALL      'TIBS55R'
059500020802     C                   PARM                    TIBS55DS
059600020802     C*
059700971121     C                   MOVEL     O50PLA        CMZLBD
059800971121     C                   MOVEL     O50ALA        CMZLBA
059900971121     C                   MOVEL     O50ASI        CMZSYF
060000971121     C* IMPOSTO SE I 2 P.O. SONO LOCALI
060100020802     C                   MOVEL     O50LCL        CMZLCL
060200020802     C                   CALL      'TIBS53C'
060300020802     C                   PARM                    CMZLCL
060400960131     C                   PARM                    CMZDST
060500960131     C                   PARM                    CMZFLD
060600960131     C                   PARM                    CMZMBD
060700960131     C                   PARM                    CMZLBD
060800960131     C                   PARM                    CMZFLA
060900960131     C                   PARM                    CMZMBA
061000960131     C                   PARM                    CMZLBA
061100960131     C                   PARM                    CMZORI
061200960131     C                   PARM                    CMZDTI
061300960131     C                   PARM                    CMZURG
061400960131     C                   PARM                    CMZADD
061500960131     C                   PARM                    CMZCLR
061600960131     C                   PARM                    CMZFTR
061700960131     C                   PARM                    CMZERR
061800960131     C                   PARM                    CMZAZN
061900960131     C                   PARM                    CMZSYF
062000960131     C                   PARM                    CMZNRR
062100960131     C                   PARM                    CMZPRR
062200020802     C                   PARM                    W236
062300960131     C                   ENDSR
062400960131     C*
062500960131     C*--- SR INIZIALE -----------------------------------------------*
062600960131     C     *INZSR        BEGSR
062700960131     C*
062800960131     C****  KLIST  ****
062900960131     C* ACCESSO FNFVA01L
063000060714     C     KFV2          KLIST
063100960131     C                   KFLD                    FVALNP
063200960131     C                   KFLD                    FVANFV
063300060714     C                   KFLD                    fv2epa
063400060714     C                   KFLD                    FVALAI
063500060714     C                   KFLD                    fv2tdh
063600060714     C     KFVA          KLIST
063700060714     C                   KFLD                    FVALNP
063800060714     C                   KFLD                    FVANFV
063900060714     C                   KFLD                    FVALAI
064000020918      * FNFVA06L
064100020918     c     Kfva6         Klist
064200020918     c                   Kfld                    Wlai
064300020918     c                   Kfld                    Wnpg
064400021030      * FNFV403L
064500021030     c     Kfv4          Klist
064600021030     c                   Kfld                    Wepa
064700021030     c                   Kfld                    Wlai
064800021030
064900021030     c     Kfv403        Klist
065000021030     c                   Kfld                    Wepa
065100021030     c                   Kfld                    Wlai
065200021030     c                   Kfld                    Wlnp
065300021030
065400960131     C* ACCESSO TABEL00F
065500960131     C     KTAB          KLIST
065600960131     C                   KFLD                    CODUT
065700020802     C                   KFLD                    TBLcod
065800960131     C*
065900960131     C* DATA DEL GIORNO
066000020802     C                   TIME                    WTIME                          ORA & DATA
066100041112     C                   MOVE      *date         wdata             8 0          GG/MM/AAAA
066200960131     C*
066300960131     C****  CARICO TABELLA TIPI TRAINO  ****
066400020802     C                   Z-ADD     1             Y
066500020802     C                   Z-ADD     1             CODUT
066600020802     C                   MOVEL     'TV'          TBLcod
066700960131     C     KTAB          SETLL     TABEL
066800960131     C     KTAB          READE     TABEL                                  30
066900960131    1C     *IN30         DOWEQ     *OFF
067000960131    2C     TBLFLG        IFEQ      ' '
067100960131     C                   MOVEL     TBLUNI        DSTV
067200960131     C*
067300960131     C                   MOVEL     TBLKEY        TTV(Y)
067400960131     C                   MOVEL     §TVUTC        UTC(Y)
067500960131     C                   MOVEL     §TVDEF        DEF(Y)
067600960131     C                   ADD       1             Y
067700960131    2C                   ENDIF
067800960131     C*
067900960131     C     KTAB          READE     TABEL                                  30
068000960131    1C                   ENDDO
068100960131     C*
068200960131     C                   ENDSR
068300960130     C*---------------------------------------------------------------*
068400960201**         C56
068500020802ALCOBJ OBJ((XXXXXXXX *FILE *EXCL MXXXxxx))
068600951122**         C17
068700020802OVRDBF FILE(XXXXXXXX)               MBR(M000000)
068800960131** MSG
068900111103ERRORE TRASMISS.F.VIAGGIO PARTENZA DA ARRIVO
069000020802A SEDE   : TROVATO MEMBRO XXXxxx ALLOCATO.
069100020802A FILIALE: TROVATO MEMBRO XXXxxx ALLOCATO.
069200960201A SEDE E FILIALE: TROVATO RECORD ALLOCATO.
