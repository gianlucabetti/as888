000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200890915     F*--------------------------------------------------------------*
000300890915     F*       - RICERCA GENERICA SU TABEL00F               -         *
000400000000     F*--------------------------------------------------------------*
000500890915     FTABEL00F  IF   E           K DISK
000600890915     FX§TABED   CF   E             WORKSTN
000700890915     F                                     SFILE(TABESF1:REC1)
000701050707      **********************************************************************
000702050707      * Constants
000703050707      *
000704050707      *     MaxKey - Maximum number of key fields allowed by this program
000705050707      *
000706050707      **********************************************************************
000707050707     D MaxKey          C                    1
000708050707     D Descrizione     C                   '1'
000709050707     D Ascendente      C                   1
000710050707     D Carattere       C                   6
000711050707     D Put             C                   1
000712050707     D EndPut          C                   2
000713050707     D Get             C                   3
000714050707      **********************************************************************
000715050707      * Standalone fields
000716050707      *
000717050707      *     Rec1       - Relative record number for subfile
000718050707      *     SizeList   - Size of list
000719050707      *     ReturnSize - Size of list returned by sort API
000720050707      **********************************************************************
000721050707     D NotUsed         S             16A
000722050707     D ReturnSize      S              9B 0
000723050707     D SizeList        S              9B 0
000724050707     D RrnLast         S              5i 0
000725050707     D WrkSort         S              1
000726050707     D VDESS           S                   LIKE(VDES)
000727050707     D rec1            S              5i 0
000728050707     D len             S              2  0
000729050707      **********************************************************************
000730050707      * Data Structures
000731050707      *     SflRcd     - The entire subfile record to pass to the sort
000732050707      *     QLGSCB     - The sort request block for the QLGSORT API
000733050707      *     QLGSCB00   - The sort request block for the QLGSRTIO API
000734050707      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
000735050707      *     QUSEC      - Error structure for the QLGSORT API
000736050707      **********************************************************************
000737050707     D SflRcd          DS
000738050707     D  Scelta
000739050707     D  codtab
000740050707     D  descri
000741050707     D  stattb
000744050707     D  Selected                      1A
000745050707
000746050707      /COPY QSYSINC/QRPGLESRC,QLGSORT
000747050707     D QLGKL                         16    DIM(MaxKey)
000748050707     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
000749050707     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
000750050707     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
000751050707     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
000752050707
000753050707      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
000754050707      /COPY QSYSINC/QRPGLESRC,QUSEC
000800000000     C*----------------------------------------------------*
000900000000     C     *ENTRY        PLIST                                                  *
001000890915     C                   PARM                    §KUT                           *
001100890915     C                   PARM                    §COD                           *
001200890915     C                   PARM                    §KEY                           *
001300891102     C                   PARM                    §DES             25            *
001400900726     C                   MOVEL     §DES          §AN2              2            *
001500900726     C**
001600900726     C* SE NELLE PRIME 2 POSIZIONI DEL PARAMETRO §DES
001700900726     C* SI TROVANO I CARATTERI §§ SI VOGLIONO ANCHE
001800900726     C* I RECORDS ANNULLATI
001900000000     C*----------                                          *
002000890915     C     *LIKE         DEFINE    TBLKUT        §KUT
002100890915     C     *LIKE         DEFINE    TBLCOD        §COD
002200890915     C     *LIKE         DEFINE    TBLKEY        §KEY
002300890915     C     *LIKE         DEFINE    TBLCOD        §CODK
002400890915     C     *LIKE         DEFINE    TBLKEY        §KEYK
002500890915     C*
002600890915     C     KEYTAB        KLIST                                                  *
002700890915     C                   KFLD                    §KUT                           *
002800890915     C                   KFLD                    §COD                           *
002900980930     C**
003000980930     C     KEYTA2        KLIST                                                  *
003100980930     C                   KFLD                    §KUT                           *
003200980930     C                   KFLD                    §COD                           *
003300980930     C                   KFLD                    §KEY                           *
003400890915     C     KEYTAK        KLIST                                                  *
003500890915     C                   KFLD                    §KUT                           *
003600890915     C                   KFLD                    §CODK                          *
003700890915     C                   KFLD                    §KEYK                          *
003800890915     C*
003900890915     C* DECODIFICO IL CODICE TABELLA   PASSATO
004000890915     C                   MOVE      *BLANK        §CODK
004100890915     C                   MOVE      *ZEROS        §KEYK
004200890915     C                   MOVE      §COD          §KEYK
004300900724     C                   MOVE      §COD          COTIPT
004400890915     C                   MOVE      *BLANK        DETIPT
004500890915     C     KEYTAK        CHAIN     TABEL                              90
004600890915     C  N90              MOVEL     TBLUNI        DETIPT
004700890915     C   90              MOVEL     'ERRORE'      DETIPT
004900000000     C*---------- GESTIONE SUBFILE ------------------------*
005500050707     C                   Z-ADD     0             REC1                           *
005600000000     C                   MOVE      *BLANKS       SCELTA                         *
005700000000     C*---------- PULIZIA SLFCT :                          *
005800000000     C                   SETON                                            70    *
005900890915     C                   WRITE     TABESC1                                      *
006000000000     C                   SETOFF                                       7071      *
006100000000     C*---------- CARICAMENTO SFL :                        *
006101050707     C     KEYTA2        SETLL     TABEL00F                           90  91    *
006103050707     C     SOPRA         TAG                                                    *
006300890915     C     KEYTAB        READE     TABEL                                  91    *
006301050707     C* FINE LETTURA
006302050707     C   91REC1          IFEQ      0
006303050707     C                   SETON                                        90
006304050707     C                   GOTO      FINE
006305050707     C                   ELSE
006306050707     C                   GOTO      EMISF1                                       *
006307050707     C                   ENDIF
007100980930     C**
007200900726     C     TBLFLG        IFEQ      '*'                                           ANN.TA
007300900726     C     §AN2          ANDNE     '§§'
007400900726     C                   GOTO      SOPRA
007500900726     C                   END
007600890915     C                   MOVEL     TBLKEY        CODTAB                         *
007700890915     C                   MOVEL     TBLUNI        DESCRI                         *
007800900724     C                   MOVEL     TBLFLG        STATTB                         *
007900000000     C     REC1          ADD       1             REC1                 71        *
008000890915     C                   WRITE     TABESF1                                      *
008100000000     C                   GOTO      SOPRA                                        *
008200000000     C*---------- EMISSIONE SFL :                          *
008201050707     C     EMISF1        TAG                                                    *
008202050707     C                   EVAL      RrnLast = Rec1
008203050707     C                   EVAL      rcdnbr  = 1
008205050707     C                   EVAL      wrksort = '1'
008206050707     c                   do        *hival
008302050707     c*Descrizione della ricerca
008303050707     c                   if        wrksort = ' '
008304050707     c                   eval      destps = 'Descrizione'
008305050707     c                   else
008306050707     c                   eval      destps = 'Codice'
008307050707     c                   end
008308050707     c*
008401050707     C                   write     TABEd1                                       *
008402050707     C                   EXFMT     TABESC1                                      *
008403050707     C   KC              SETON                                            90    *
008404050707     C   KC              GOTO      FINE                                         *
008601050707     c*Cambio ordinamento
008603050707     C   kk              EXSR      Sort
008604050707     c                   exsr      ctr
009101050707     c*effettuata una scelta
009102050707     c                   if        scelta <> ' '
009200890915     C                   MOVE      CODTAB        §KEY                           *
009300890915     C                   MOVEL     DESCRI        §DES                           *
009301050707     c                   leave
009304050707     c                   end
009305050707     c                   enddo
009400000000     C*----------------------------------------------------*
009401050707     C     FINE          TAG                                                    *
009402050707     C   90              MOVE      *BLANKS       §KEY                           *
009403050707     C   90              MOVE      *BLANKS       §DES                           *
009404050707     C   90              MOVEL     'ERRORE'      §DES                           *
009900000000     C                   SETON                                            LR    *
009901050707     c**********************************************************************
009902050707     ** Controlli Sfl
009903050707     c**********************************************************************
009904050707     C     ctr           BEGSR
009905050707     C*
009906050707     c                   setoff                                       41
009907050707     c                   do        *hival        X                 4 0
009908050707     c     X             CHAIN     tabesf1                            01
009909050707     C   01              LEAVE
009910050707     c                   if        scelta <> ' '
009913050707     c                   leave
009914050707     c                   end
009915050707     C* CONTROLLO SE EFFETTUATA RICERCA
009916050707     C                   IF        VDES <> ' ' AND VDES <> VDESS
009917050707     c                   seton                                        41
009918050707     C                   if        (wrksort = ' ' and VDES <= descri) or
009919050707     C                             (wrksort = '1' and VDES <= codtab)
009920050707     c                   setoff                                       41
009921050707     C                   Z-ADD     X             rcdnbr
009922050707     C                   MOVEL     VDES          VDESS
009923050707     C                   LEAVE
009924050707     C                   END
009925050707     C                   END
009926050707     C                   ENDDO
009927050720     C* n41              clear                   VDES
009928050707     C*
009929050707     C                   ENDSR
010000050707      **********************************************************************
010100050707      * Subroutine - Sort
010200050707      *   This subroutine sorts the subfile records.
010300050707     C     Sort          BEGSR
010400050707     C                   CLEAR                   vdes
010500050707     C                   CLEAR                   vdess
010600050707     C                   EVAL      rcdnbr  = 1
010700050707
010800050707      * Initialize the key fields to sort on. There is one extra field not in
010900050707      * the subfile -- Selected -- that is added to the record. This field
011000050707      * is used to place selected records in front of nonselected records.
011100050707
011200050707     C                   CLEAR                   QLGSCB
011300050707     C                   CLEAR                   QLGSCB00
011400050707
011500050707     ** Gestione della scelta ordinamento.
011600050707     C                   SELECT
011700050707     ** Ordinamento per descrizione.
011800050707     C                   WHEN      WrkSort = Descrizione
011900050707     ** 1 campo chiave.
012000050707     C                   EVAL      QLGNBRK    = 1
012100050707     ** La descrizione è
012200050707     ** a posizone 10, 25 byte, char, ascending.
012300050707     C                   EVAL      QLGSP      = 10
012400050707     C                   EVAL      QLGSS      = %SIZE(descri)
012500050707     C                   EVAL      QLGDT      = Carattere
012600050707     C                   EVAL      QLGSO      = Ascendente
012700050707     C                   EVAL      QLGKL(1)   = QLGSKL
012800050707     ** Al prossimo F11 ordino per CODICE
012900050707     C                   EVAL      WrkSort = ' '
013000050707     ** Ordinamento per Codice
013100050707     C                   WHEN      WrkSort = ' '
013200050707     C                   EVAL      QLGNBRK    = 1
013300050707     ** Il codice è
013400050707     ** a posizione 2, 8 byte, char, ascending
013500050707     C                   EVAL      QLGSP      = 2
013600050707     C                   EVAL      QLGSS      = %SIZE(codtab)
013700050707     C                   EVAL      QLGDT      = Carattere
013800050707     C                   EVAL      QLGSO      = Ascendente
013900050707     C                   EVAL      QLGKL(1)   = QLGSKL
014000050707     ** Al prossimo F11 ordino per descrizione.
014100050707     C                   EVAL      WrkSort = Descrizione
014200050707     C                   ENDSL
014300050707      * Load other sort parameters.
014400050707     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
014500050707     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
014600050707     C                   EVAL      QLGRT     = 8
014700050707     C                   EVAL      QLGOKL    = 80
014800050707     C                   EVAL      QLGLKE    = 16
014900050707     C                   EVAL      QLGLSS    = 290
015000050707      * Initialize Sort I/O API fields.
015100050707     C                   EVAL      QLGRL00  = QLGRL
015200050707     C                   EVAL      QLGRC00  = 1
015300050707     C                   CLEAR                   QUSEI
015400050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
015500050707      * First step - Initialize the sort routine.
015600050707     C                   CALL      'QLGSORT'
015700050707     C                   PARM                    QLGSCB
015800050707     C                   PARM                    NotUsed
015900050707     C                   PARM                    NotUsed
016000050707     C                   PARM                    SizeList
016100050707     C                   PARM                    ReturnSize
016200050707     C                   PARM                    QUSEC
016300050707      * Next step - Write records to I/O routine.
016400050707     C                   EVAL      QLGRT00 = Put
016500050707     C                   DO        RrnLast       x
016600050707     C     x             CHAIN     tabesf1
016700050707     ** Solo le righe con Selected = 'Y' sono riordinate,
016800050707     ** quindi per fare un ordinamento di tutte le righe
016900050707     ** metto 'Y' sempre.
017000050707     C                   EVAL      Selected = 'Y'
017100050707     C                   CLEAR                   QUSEI
017200050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017300050707     C                   CALL      'QLGSRTIO'
017400050707     C                   PARM                    QLGSCB00
017500050707     C                   PARM                    SflRcd
017600050707     C                   PARM                    NotUsed
017700050707     C                   PARM                    SizeList
017800050707     C                   PARM                    NotUsed
017900050707     C                   PARM                    QUSEC
018000050707     C                   ENDDO
018100050707      * Next step - Signal end of input, clear subfile for reload.
018200050707     C                   EVAL      QLGRT00 = EndPut
018300050707     C                   CLEAR                   QUSEI
018400050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
018500050707
018600050707     C                   CALL      'QLGSRTIO'
018700050707     C                   PARM                    QLGSCB00
018800050707     C                   PARM                    SflRcd
018900050707     C                   PARM                    NotUsed
019000050707     C                   PARM                    SizeList
019100050707     C                   PARM                    NotUsed
019200050707     C                   PARM                    QUSEC
019300050707     c                   seton                                        70
019400050707     C                   WRITE     TABESC1
019500050707     C                   setoff                                       70
019600050707      * Final step - Write the records back to the subfile.
019700050707     C                   EVAL      QLGRT00 = Get
019800050707     C                   DO        RrnLast       x
019900050707     C                   CLEAR                   QUSEI
020000050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
020100050707     C                   CALL      'QLGSRTIO'
020200050707     C                   PARM                    QLGSCB00
020300050707     C                   PARM                    NotUsed
020400050707     C                   PARM                    SflRcd
020500050707     C                   PARM                    QLGRL00
020600050707     C                   PARM                    NotUsed
020700050707     C                   PARM                    QUSEC
020800050707     c                   z-add     x             rec1
020900050707     C                   WRITE     tabesf1
021000050707     C                   ENDDO
021100050707     C                   ENDSR
