000100011227      /TITLE Anagrafico raggruppamenti stime
000101011227      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000102011227      *PARMS CLOSQLCSR(*ENDMOD)
000103011227      *PARMS DYNUSRPRF(*OWNER)
000200000607      *
000300000606     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000400000607      *
000500011227     FYcoa820d  CF   E             WORKSTN
000600000612     F                                     SFILE(S01:RRNS01)
000601011227     FYnstr01l  iF   E           k disk    prefix($)
000602011228      *
000603020103     D Psds           SDS
000604020103     D  PgmName          *PROC
000605011228     D Kpjba         E DS
000606011228     D XSoc001DS     E DS                  inz
000607020108     D xchkautds     e ds                  inz
003200000616      *
003202011227     D ynstr           DS
003203011227     d sel0001                       15  0
003204011227     d strscg                         6
003205011227     d strdescr                      50
003206011227     d strlivrise                     1
003207011227     d strdtinval                      d
003211011227     d strdtfival                      d
003212011228      *
003213011227     d data            ds            10
003214011228     d aa                      3      4
003215011228     d mm                      6      7
003216011228     d gg                      9     10
003217011228      *
003218011228     D com6            S              6
003219011228     D WrkSqlCmd       S           1024
003220011227     D WrkEofS01       S              1
003221011227     D WrkCarS01       S              1
003701011228     D $VIDEO          S             10
003702011228     D VDESS           S                   LIKE(VDES)
003703011228     D rrns01          S              5i 0
003705011228     D SavOpzione      S                   like(s01opz)
003707011227     D TipXsc          S              6
003708011227     D SocXsc          S              3
003709011227     D CdsXsc          S              9  0
003710011227     D ModXsc          S              3
003711011227     D RtnXsc          S              1
003713011227     D dtaini          S               d
003714011227     D dtafin          S               d
003715011228      **********************************************************************
003716011228      * Constants
003718011228      *     MaxKey - Maximum number of key fields allowed by this program
003720011228      **********************************************************************
003721011228     D MaxKey          C                    1
003722011228     D Descrizione     C                   '1'
003723011228     D Ascendente      C                   1
003724011228     D Carattere       C                   6
003725011228     D Put             C                   1
003726011228     D EndPut          C                   2
003727011228     D Get             C                   3
003728011228      **********************************************************************
003729011228      * Standalone fields
003732011228      *     SizeList   - Size of list
003733011228      *     ReturnSize - Size of list returned by sort API
003734011228      **********************************************************************
003735011228     D NotUsed         S             16A
003736011228     D ReturnSize      S              9B 0
003737011228     D SizeList        S              9B 0
003738011228     D RrnLast         S              5i 0
003739011228     D WrkSort         S              1
003745011228      **********************************************************************
003746011228      * Data Structures
003747011228      *     SflRcd     - The entire subfile record to pass to the sort
003748011228      *     QLGSCB     - The sort request block for the QLGSORT API
003749011228      *     QLGSCB00   - The sort request block for the QLGSRTIO API
003750011228      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
003751011228      *     QUSEC      - Error structure for the QLGSORT API
003752011228      **********************************************************************
003753011228     D SflRcd          DS
003755011228     D  S01opz
003756011228     D  S01rag
003757011228     D  S01des
003758011228     D  S01ris
003759011228     D  S01dti
003760011228     D  S01dtf
003761011228     d  hnrr
003762011228     D  Selected                      1A
003763011228
003764011228      /COPY QSYSINC/QRPGLESRC,QLGSORT
003765011228     D QLGKL                         16    DIM(MaxKey)
003766011228     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
003767011228     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
003768011228     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
003769011228     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
003770011228
003771011228      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
003772011228      /COPY QSYSINC/QRPGLESRC,QUSEC
006200000608      *---------------- Gestione formato video R01 -------------------------
006300000608     C                   EXSR      InzR01
006302011227     C                   DOU       $Video <> 'R01'
006700000608      *----------- Visualizzo dati di output in caso di errori.-------------
006800000608     C                   IF        *IN99
006900000608     C                   EVAL      *IN99 = *OFF
007000000608     C                   WRITE     R01
007100000608     C                   EVAL      *IN99 = *ON
007200000608     C                   ENDIF
007300000608      *
007400000608     C                   EXFMT     R01
007500000608      *
009802011227     C                   SELECT
009803011227     C                   WHEN      *INKC
009804011227     C                             OR
009805011227     C                             *INKL
009806011227     C                   EVAL      $Video = *BLANKS
009807011227
009808011228     C                   WHEN      *INKD
009809011228     C                   EXSR      Search
009810011227     C                   WHEN      *INKE
009811011227     C                   EXSR      InzR01
009812011227     C                   WHEN      *INKB
009813011227     C                   EXSR      Chkr01
009814011227     C                   OTHER
009815011227     C                   EXSR      ChkR01
009816011227     C  N99              EXSR      WrkC01
009817011227     C                   ENDSL
009821011227      *
009822011227     C                   ENDDO
009823011227      *
009824011227     C                   EXSR      Uscita
009825011227     c**********************************************************************
009826011227     c* uscita
009827011227     c**********************************************************************
009828011227     C     Uscita        BEGSR
009829011227      *
009830011227     C                   EVAL      *INLR = *ON
009831011227     C                   RETURN
009832011227      *
009833011227     C                   ENDSR
009834011227     c**********************************************************************
009835011227     c* gestione sfl
009836011227     c**********************************************************************
010200000627     C     WrkC01        BEGSR
010201011227     c* compone comando sql
010202011227     c                   exsr      srsql
010800000627      *
010801011227     C                   EVAL      WrkCarS01 = *ON
010802011227     C                   EVAL      $Video = 'C01'
010803011227      *
010804011227     C                   DOU       $Video <> 'C01'
010900000627     C                   EXSR      CarS01
010901011228     c*Descrizione della ricerca
010902011228     c                   if        wrksort = ' '
010903011228     c                   eval      destps = 'Descrizione'
010904011228     c                   else
010905011228     c                   eval      destps = 'Codice'
010906011228     c                   end
011000000627      *
011300011227     C                   WRITE     r03
011400000607     C                   EXFMT     C01
011401011227     C                   SELECT
011403011228     c* fine
011404011227     C                   WHEN      *INKC=*ON
011405011227     C                   EVAL      $VIDEO=*BLANKS
011406011228     c* guida
011407011227     C                   WHEN      *INKL=*ON
011408011227     C                   EVAL      $VIDEO='R01'
011409011228     c* ripristino
011410011227     C                   WHEN      *INKE=*ON
011411011227     C                   EVAL      WrkCarS01 = *ON
011412011228     c*Cambio ordinamento
011413011228     C                   WHEN      *INKK
011414011228     C                   EXSR      Sort
011415011228     c*ripeti opzione (NON ABILITATO)
011416011228     C                   WHEN      *INKM
011417011228     C                   EXSR      RipetiOpz
011418011227
011419011227     C                   OTHER
011420011227
011421011227     C                   EXSR      GestioneSFL
011422011227     C                   ENDSL
011423011228
011424011227     C                   ENDDO
011425011227     C                   ENDSR
020401011227     c**********************************************************************
031600011227     c* Controlli video R01.
031601011227     c**********************************************************************
031700000608     C     ChkR01        BEGSR
031800000609      *
031900000608     C                   EVAL      *IN99 = *OFF
032700011227      * raggruppamento
032800011227     C                   IF        codragv <> '*'
032900011227     C     kyn           chain     ynstr01l                           01
032901011227     c   01              seton                                        99
033000000627     C                   ENDIF
033001011227     c* livello riservatezza
033100011227     C                   IF        livrisv <> '*' and (livrisv < '0' or
033101011227     c                             livrisv > '9')
033102011227     c                   seton                                        9902
033300000627     C                   ENDIF
033301011227     c* data inizio
033400011227     C                   IF        dtadalv <> 0
033500011227     C     *dmy          test(d)                 dtadalv                03
033501020103     c                   if        *in03
033502020103     c                   seton                                        99
033503020103     c                   else
033504011227     c     *dmy          move      dtadalv       dtaini
033505020103     c                   end
033506011228     c                   else
033507011228     c                   move      '0001-01-01'  dtaini
033600000627     C                   ENDIF
033601011227     c* data fine
033602020103     C                   IF        dtaalv <> 999999
033603011227     C     *dmy          test(d)                 dtaalv                 04
033604020103     c                   if        *in04
033605020103     c                   seton                                        99
033606020103     c                   else
033607011227     c     *dmy          move      dtaalv        dtafin
033608020103     c                   end
033609020103     c                   else
033610020103     c                   move      '9999-12-31'  dtafin
033611020103     c                   end
033612011228     c     dtafin        comp      dtaini                               99
033613011227     c   99              seton                                        0304
051000000608      *
051100000608     C                   ENDSR
051101011227     c**********************************************************************
051102011227     c* inizializza r01
051103011227     c**********************************************************************
051300000608     C     InzR01        BEGSR
051400000608
051401011228     C                   EVAL      $Video = 'R01'
051600011227     C                   EVAL      codragv = '*'
051700011227     C                   EVAL      deslunv = '*'
051800011227     C                   EVAL      livrisv = '*'
051900011227     C                   EVAL      dtadalv = 0
052000020103     C                   EVAL      dtaalv = 999999
052200000627     C                   EXSR      Chkr01
052300000609
052400000608     C                   ENDSR
052401011228     c**********************************************************************
052402011228     c* ricerca
052403011228     c**********************************************************************
052404011228     C     Search        BEGSR
052405011228     C                   ENDSR
052519011228     c**********************************************************************
052520011228     c* carica sfl
052521011228     c**********************************************************************
052522011228     C     CarS01        BEGSR
052523011228     C                   IF        WrkCarS01 = *ON
052524011228     C                   EVAL      RRNS01 = 0
052525011228     C                   EVAL      *IN90 = *ON
052526020219     C                   EVAL      *IN91 = *OFF
052527011228     C                   WRITE     C01
052528011228     C                   EVAL      *IN90 = *OFF
052530011228     c* Lettura ynstr01l
052531011228     C/EXEC SQL
052532011228     C+ PREPARE S1 FROM :WrkSqlCmd
052533011228     C/END-EXEC
052534011228
052535011228     C/EXEC SQL
052536011228     C+ DECLARE A1 CURSOR FOR S1
052537011228     C/END-EXEC
052538011228
052539011228     C/EXEC SQL
052540011228     C+ OPEN A1
052541011228     C/END-EXEC
052542011228
052543011228     C                   DOU       SqlCod <> 0
052544011228     C/EXEC SQL
052545011228     C+ FETCH NEXT FROM A1 INTO :ynstr
052546011228     C/END-EXEC
052547011228     c*
052548011228     C                   SELECT
052549011228     C                   WHEN      SqlCod = 0
052550011228     c                   movel     *blanks       s01opz
052551011228     c                   movel     sel0001       hnrr
052552011228     c                   movel     strscg        s01rag
052553011228     c                   movel     strdescr      s01des
052554011228     c                   movel     strlivrise    s01ris
052555011228     c                   z-add     0             s01dti
052556011228     c                   if        strdtinval <> *loval
052557011228     c                   move      strdtinval    data
052558011228     c                   eval      com6 = gg + mm + aa
052561011228     c                   move      com6          s01dti
052562011228     c                   end
052563020103     c                   z-add     999999        s01dtf
052564011228     c                   if        strdtfival <> *hival
052565011228     c                   move      strdtfival    data
052566011228     c                   eval      com6 = gg + mm + aa
052569011228     c                   move      com6          s01dtf
052570011228     c                   end
052571011228     C                   EVAL      RRNS01 = RRNS01 + 1
052572011228     C                   WRITE     S01
052573011228     C                   WHEN      SqlCod = 100
052574011228     C                   WHEN      SqlCod <> 0
052575011228      * Forzo la stampa del JOBLOG.
052576011228     C                   CALL      'X66CHGJOB'
052577011228     C                   ENDSL
052578011228     C                   ENDDO
052580011228
052581011228     C/EXEC SQL
052582011228     C+ CLOSE A1
052583011228     C/END-EXEC
052584020219     C                   IF        RRNS01 > 0
052586020219     C                   EVAL      *IN91 = *ON
052587020219     C                   MOVE      1             OK                1 0
052588020219     C                   ELSE
052589020219     C                   MOVE      0             OK                1 0
052590020219     C                   ENDIF
052591020219     c* carico api X CODICE
052592011228     C                   EVAL      RrnLast = Rrns01
052593011228     C                   EVAL      WrkSort = ' '
052594011228     c                   exsr      sort
052595011228     C                   ENDIF
052596020219     C                   EVAL      WrkCarS01 = *OFF
052597020219     C                   EVAL      RRNS01  = 1
052598020219     C                   EVAL      c01csrrrn = 1
052599011228     C                   ENDSR
052600011227     c**********************************************************************
052601011227     c* gestione sfl
052602011227     c**********************************************************************
072800000613     C     GestioneSFL   BEGSR
072900000627      *
073000000627     C                   EVAL      WrkEofS01 = *OFF
073100000627      * F10 Immissione.
073200000627     C                   IF        *INKJ
073201011227     c                   z-add     0             numrec
073202020103     c                   move      '1 '          mod
073203011227     c                   call      'YCOA821R'
073204011227     c                   parm                    numrec           15 0
073205011227     c                   parm                    mod               2
073206011227     c                   parm                    cmd               1
073600000627      * Se il programma chiamato si è chiuso con ...
073700000627     C                   SELECT
073800000627      * F3
073900011227     C                   WHEN      cmd = 'C'
074000000627     C                   EXSR      Uscita
074001011227      * F12
074002011227     C                   WHEN      cmd = 'L'
074003011227     C                   EVAL      WrkEofS01 = *ON
074400000703      * F6
074500011227     C                   WHEN      cmd = 'F'
074501011228     C                   EVAL      WrkCarS01 = *ON
074700000627     C                   ENDSL
074800000627      *
074900011228     C                   Else
075000000627      * Elaborazione opzioni.
075001020219     c                   IF        OK = 1
075002011228     c                   do        *hival        X                 4 0
075003011228     c     X             CHAIN     s01                                50
075004011228     C   50              LEAVE
075005011228     c*
076100011227     C                   IF        S01opz <> *blanks
076101011227     c                   z-add     hnrr          numrec
076102011227     c                   move      s01opz        mod
076103011227     c                   call      'YCOA821R'
076104011227     c                   parm                    numrec
076105011227     c                   parm                    mod
076106011227     c                   parm                    cmd
077700000627      * Se il programma chiamato si è chiuso con ...
077800000627     C                   SELECT
077900000627      * F3
078000011227     C                   WHEN      cmd = 'C'
078100000627     C                   EXSR      Uscita
078200000627      * F6
078300011227     C                   WHEN      cmd = 'F'
078301011227     C                   CLEAR                   S01opz
078303011227     C                   EVAL      WrkCarS01 = *ON
078304020128     C                   UPDATE    S01
078305011228     C                   LEAVE
078307011227      * F12
078308011227     C                   WHEN      cmd = 'L'
078309011227     C                   CLEAR                   S01opz
078310011227     C                   EVAL      WrkEofS01 = *ON
078311020128     C                   UPDATE    S01
078312011228     C                   LEAVE
079600000627     C                   ENDSL
079601011228     C                   ENDIF
079602011228     C* CONTROLLO SE EFFETTUATA RICERCA
079603011228     C                   IF        VDES <> ' ' AND VDES <> VDESS
079604011228     c                   setoff                                       91
079605011228     C                   if        (wrksort = ' ' and VDES <= S01DES) or
079606011228     C                             (wrksort = '1' and VDES <= S01rag)
079607011228     c                   seton                                        91
079608011228     C                   Z-ADD     X             rcdnbr
079609011228     C                   MOVEL     VDES          VDESS
079610011228     C                   LEAVE
079611011228     C                   END
079612011228     C                   END
079800000627     C                   UPDATE    S01
080300000619     C                   ENDDO
080303020219     C   91              clear                   VDES
080304020220     C  N91              WRITE     R04
080305020220     C                   MOVEL     VDES          VDESS
080311020219     c                   end
080312011228     c                   end
080400000627      *
080500000613     C                   ENDSR
080601011227     c**********************************************************************
080700000703      * Ripetizione opzione in tutte le righe del subfile.
080701011227     c**********************************************************************
080800000703     C     RipetiOpz     BEGSR
080900000703      *
081000000703     C                   IF        C01CsrRrn > *ZERO
081200000703     C                   EVAL      RrnS01 = C01CsrRrn
081300000703     C     RrnS01        CHAIN     S01
081400011228     C                   IF        %FOUND and s01opz <> *blanks
081800011227     C                   EVAL      SavOpzione = S01opz
082000000703     C                   UPDATE    S01
082200000703     C                   EVAL      WrkEofS01 = *OFF
082300000703     C                   DOU       WrkEofS01 = *ON
082400000703     C                   ADD       1             RrnS01
082500000703     C     RrnS01        CHAIN     S01
082600000703     C                   IF        %FOUND
082700011227     C                   EVAL      S01opz = SavOpzione
082800000703     C                   UPDATE    S01
082900000703     C                   ELSE
083000000703     C                   EVAL      WrkEofS01 = *ON
083100000703     C                   ENDIF
083200000703     C                   ENDDO
083600000703     C                   ENDIF
083800000703     C                   ENDIF
083900000703      *
084000000703     C                   ENDSR
084001011228      **********************************************************************
084002011228      * Subroutine - Sort
084003011228      *   This subroutine sorts the subfile records.
084004011228      **********************************************************************
084005011228     C     Sort          BEGSR
084006011228     C                   CLEAR                   vdes
084007011228     C                   CLEAR                   vdess
084008011228     C                   EVAL      rcdnbr  = 1
084010011228
084011011228      * Initialize the key fields to sort on. There is one extra field not in
084012011228      * the subfile -- Selected -- that is added to the record. This field
084013011228      * is used to place selected records in front of nonselected records.
084014011228
084015011228     C                   CLEAR                   QLGSCB
084016011228     C                   CLEAR                   QLGSCB00
084017011228
084018011228     ** Gestione della scelta ordinamento.
084019011228     C                   SELECT
084020011228     ** Ordinamento per descrizione.
084021011228     C                   WHEN      WrkSort = Descrizione
084022011228     ** 1 campo chiave.
084023011228     C                   EVAL      QLGNBRK    = 1
084024011228     ** La descrizione è
084025011228     ** a posizone 9, 49 byte, char, ascending.
084026011228     C                   EVAL      QLGSP      = 9
084027011228     C                   EVAL      QLGSS      = %SIZE(S01Des)
084028011228     C                   EVAL      QLGDT      = Carattere
084029011228     C                   EVAL      QLGSO      = Ascendente
084030011228     C                   EVAL      QLGKL(1)   = QLGSKL
084031011228     ** Al prossimo F11 ordino per CODICE
084032011228     C                   EVAL      WrkSort = ' '
084033011228     ** Ordinamento per Codice
084034011228     C                   WHEN      WrkSort = ' '
084035011228     C                   EVAL      QLGNBRK    = 1
084036011228     ** Il codice è
084037011228     ** a posizione 3, 6 byte, char, ascending
084038011228     C                   EVAL      QLGSP      = 3
084039011228     C                   EVAL      QLGSS      = %SIZE(S01rag)
084040011228     C                   EVAL      QLGDT      = Carattere
084041011228     C                   EVAL      QLGSO      = Ascendente
084042011228     C                   EVAL      QLGKL(1)   = QLGSKL
084043011228     ** Al prossimo F11 ordino per descrizione.
084044011228     C                   EVAL      WrkSort = Descrizione
084045011228     C                   ENDSL
084046011228      * Load other sort parameters.
084047011228     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
084048011228     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
084049011228     C                   EVAL      QLGRT     = 8
084050011228     C                   EVAL      QLGOKL    = 80
084051011228     C                   EVAL      QLGLKE    = 16
084052011228     C                   EVAL      QLGLSS    = 290
084053011228      * Initialize Sort I/O API fields.
084054011228     C                   EVAL      QLGRL00  = QLGRL
084055011228     C                   EVAL      QLGRC00  = 1
084056011228     C                   CLEAR                   QUSEI
084057011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
084058011228      * First step - Initialize the sort routine.
084059011228     C                   CALL      'QLGSORT'
084060011228     C                   PARM                    QLGSCB
084061011228     C                   PARM                    NotUsed
084062011228     C                   PARM                    NotUsed
084063011228     C                   PARM                    SizeList
084064011228     C                   PARM                    ReturnSize
084065011228     C                   PARM                    QUSEC
084066011228      * Next step - Write records to I/O routine.
084067011228     C                   EVAL      QLGRT00 = Put
084068011228     C                   DO        RrnLast       x
084069011228     C     x             CHAIN     S01
084070011228     ** Solo le righe con Selected = 'Y' sono riordinate,
084071011228     ** quindi per fare un ordinamento di tutte le righe
084072011228     ** metto 'Y' sempre.
084073011228     C                   EVAL      Selected = 'Y'
084074011228     C                   CLEAR                   QUSEI
084075011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
084076011228     C                   CALL      'QLGSRTIO'
084077011228     C                   PARM                    QLGSCB00
084078011228     C                   PARM                    SflRcd
084079011228     C                   PARM                    NotUsed
084080011228     C                   PARM                    SizeList
084081011228     C                   PARM                    NotUsed
084082011228     C                   PARM                    QUSEC
084083011228     C                   ENDDO
084084011228      * Next step - Signal end of input, clear subfile for reload.
084085011228     C                   EVAL      QLGRT00 = EndPut
084086011228     C                   CLEAR                   QUSEI
084087011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
084088011228
084089011228     C                   CALL      'QLGSRTIO'
084090011228     C                   PARM                    QLGSCB00
084091011228     C                   PARM                    SflRcd
084092011228     C                   PARM                    NotUsed
084093011228     C                   PARM                    SizeList
084094011228     C                   PARM                    NotUsed
084095011228     C                   PARM                    QUSEC
084096011228     c                   seton                                        90
084097011228     C                   WRITE     C01
084098011228     C                   setoff                                       90
084099011228      * Final step - Write the records back to the subfile.
084100011228     C                   EVAL      QLGRT00 = Get
084101011228     C                   DO        RrnLast       x
084102011228     C                   CLEAR                   QUSEI
084103011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
084104011228     C                   CALL      'QLGSRTIO'
084105011228     C                   PARM                    QLGSCB00
084106011228     C                   PARM                    NotUsed
084107011228     C                   PARM                    SflRcd
084108011228     C                   PARM                    QLGRL00
084109011228     C                   PARM                    NotUsed
084110011228     C                   PARM                    QUSEC
084111011228     c                   z-add     x             rrns01
084112011228     C                   WRITE     S01
084113011228     C                   ENDDO
084114011228     C                   ENDSR
084115011227     c**********************************************************************
085400000607     C     *INZSR        BEGSR
085401011227     c**********************************************************************
085500000607      *
085600000607     C     *ENTRY        PLIST
085700000607     C                   PARM                    Kpjba
085800000607      *
085900011227     C     Kyn           KLIST
086000011228     C                   KFLD                    xscsoc
086001011227     C                   KFLD                    codragv
089200000607      *---------------------------------------------------------------------
089300000607     C                   CLEAR                   XSoc001DS
089400000607     C                   CALLB(E)  'XSOC'
089500000607     C                   PARM      'SOC001'      TipXsc
089600000607     C                   PARM                    SocXsc
089700000607     C                   PARM                    CdsXsc
089800000607     C                   PARM                    ModXsc
089900000607     C                   PARM                    RtnXsc
090000000607     C                   PARM                    XSoc001DS
090100000607     C                   PARM                    Kpjba
090200011227     C                   IF        RtnXsc  <> '0'
090400000607     C                   EXSR      Uscita
090500020108     C                   ENDIF
090501020108     ** Inserire qui un eventuale controllo utente autorizzato.
090502020108     C                   CLEAR                   XChkAutDS
090503020108     C                   EVAL      XcaVfu = 'RAGGRUPPAMENTI'
090504020108     C                   EXSR      ChkAut
090505020108     C                   IF        XcaErr = '1'
090506020108     C                   exsr      uscita
090507020108     C                   ENDIF
092000011227     C                   ENDSR
092001020108     C******************************************************
092002020108     c*      Reperisco autorizzazioni per l'uso del pgm
092003020108     C     chkaut        BEGSR
092004020108     C******************************************************
092005020108     C*
092007020108     C                   EVAL      XcaSoc = XscSoc
092008020108     C                   EVAL      XcaGrp = XscGrp
092009020108     C                   EVAL      XcaFnc = 'YSTIME'
092010020108     C                   EVAL      XcaTck = 'CK'
092011020108     C                   EVAL      XcaCut = XscCut
092012020108     C                   EVAL      XcaNau = '1'
092013020108      *
092014020108     C                   CALLB     'XCHKAUT'
092015020108     C                   PARM                    XChkAutDS
092016020108     C*                                                    *
092017020108     C                   ENDSR
092100011227     c**********************************************************************
092200011227     c     srsql         begsr
092300011227     c**********************************************************************
092400011227     C                   EVAL      WrkSqlCmd
092500011227     C                             =
092600011227     C                             'SELECT '
092700011227     C                             +
092800011227     C                             'rrn(ynstr01l), strscg, strdescr, strlivrise-
092801011227     C                             , strdtinval, strdtfival FROM YNSTR01L '
092900011227     C                             +
093000011227     C                             ' WHERE STRSOCIETA <> '' '''
093100011227     c*scelta raggruppamento
094000011227     C                   IF        codragv <> '*'
094100011227     C                   EVAL      WrkSqlCmd
094200011227     C                             =
094300011227     C                             %TRIMR(WrkSqlCmd)
094400011227     C                             +
094500011227     C                             ' AND strscg = ' + '''' + codragv + ''''
094600011227     C                   endif
094700011227     c*scelta descrizione
094800011227     C                   IF        deslunv <> '*'
094900011227     C                   EVAL      WrkSqlCmd
095000011227     C                             =
095100011227     C                             %TRIMR(WrkSqlCmd)
095200011227     C                             +
095300011228     C                             ' AND strdescr like ' +
095301011228     C                             '''' + '%' + %trimr(deslunv) + '%' + ''''
095400011227     C                   endif
095500011227     c*scelta livello riservatezza
095600011227     C                   IF        livrisv <> '*'
095700011227     C                   EVAL      WrkSqlCmd
095800011227     C                             =
095900011227     C                             %TRIMR(WrkSqlCmd)
096000011227     C                             +
096001011227     C                             ' AND strlivrise = ' + '''' + livrisv + ''''
096400011227     C                   endif
096401011227     c*scelta data inizio validità
096402011227     C                   IF        dtadalv <> 0
096403011227     c     *dmy          move      dtadalv       dtaini
096404011227     c                   move      dtaini        dtainia          10
096405011227     C                   EVAL      WrkSqlCmd
096406011227     C                             =
096407011227     C                             %TRIMR(WrkSqlCmd)
096408011227     C                             +
096409011228     C                             ' AND strdtfival >= ' + '''' + dtainia + ''''
096410011227     C                   endif
096411011227     c*scelta data fine validità
096412020103     C                   IF        dtaalv <> 999999
096413011227     c     *dmy          move      dtaalv        dtafin
096414011227     c                   move      dtafin        dtafina          10
096415011227     C                   EVAL      WrkSqlCmd
096416011227     C                             =
096417011227     C                             %TRIMR(WrkSqlCmd)
096418011227     C                             +
096419011228     C                             ' AND strdtinval <= ' + '''' + dtafina + ''''
096420011227     C                   endif
100100011227     c*
100200011227     C                   ENDSR
