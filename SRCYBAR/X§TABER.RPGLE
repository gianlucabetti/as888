000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200890915     F*--------------------------------------------------------------*
000300890915     F*       - RICERCA GENERICA SU TABEL00F               -         *
000400000000     F*--------------------------------------------------------------*
000500890915     FTABEL00F  IF   E           K DISK
000600890915     FX§TABED   CF   E             WORKSTN
000700890915     F                                     SFILE(TABESF1:REC1)
000800050707      **********************************************************************
000900050707      * Constants
001000050707      *
001100050707      *     MaxKey - Maximum number of key fields allowed by this program
001200050707      *
001300050707      **********************************************************************
001400050707     D MaxKey          C                    1
001500050707     D Descrizione     C                   '1'
001600050707     D Ascendente      C                   1
001700050707     D Carattere       C                   6
001800050707     D Put             C                   1
001900050707     D EndPut          C                   2
002000050707     D Get             C                   3
002100050707      **********************************************************************
002200050707      * Standalone fields
002300050707      *
002400050707      *     Rec1       - Relative record number for subfile
002500050707      *     SizeList   - Size of list
002600050707      *     ReturnSize - Size of list returned by sort API
002700050707      **********************************************************************
002800050707     D NotUsed         S             16A
002900050707     D ReturnSize      S              9B 0
003000050707     D SizeList        S              9B 0
003100050707     D RrnLast         S              5i 0
003200050707     D WrkSort         S              1
003300050707     D VDESS           S                   LIKE(VDES)
003400050707     D rec1            S              5i 0
003500050707     D len             S              2  0
003600050707      **********************************************************************
003700050707      * Data Structures
003800050707      *     SflRcd     - The entire subfile record to pass to the sort
003900050707      *     QLGSCB     - The sort request block for the QLGSORT API
004000050707      *     QLGSCB00   - The sort request block for the QLGSRTIO API
004100050707      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
004200050707      *     QUSEC      - Error structure for the QLGSORT API
004300050707      **********************************************************************
004400050707     D SflRcd          DS
004500050707     D  Scelta
004600050707     D  codtab
004700050707     D  descri
004800050707     D  stattb
004900050707     D  Selected                      1A
005000050707
005100050707      /COPY QSYSINC/QRPGLESRC,QLGSORT
005200050707     D QLGKL                         16    DIM(MaxKey)
005300050707     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
005400050707     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
005500050707     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
005600050707     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
005700050707
005800050707      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
005900050707      /COPY QSYSINC/QRPGLESRC,QUSEC
006000000000     C*----------------------------------------------------*
006100000000     C     *ENTRY        PLIST                                                  *
006200890915     C                   PARM                    §KUT                           *
006300890915     C                   PARM                    §COD                           *
006400890915     C                   PARM                    §KEY                           *
006500891102     C                   PARM                    §DES             25            *
006600900726     C                   MOVEL     §DES          §AN2              2            *
006700900726     C**
006800900726     C* SE NELLE PRIME 2 POSIZIONI DEL PARAMETRO §DES
006900900726     C* SI TROVANO I CARATTERI §§ SI VOGLIONO ANCHE
007000900726     C* I RECORDS ANNULLATI
007100000000     C*----------                                          *
007200890915     C     *LIKE         DEFINE    TBLKUT        §KUT
007300890915     C     *LIKE         DEFINE    TBLCOD        §COD
007400890915     C     *LIKE         DEFINE    TBLKEY        §KEY
007500890915     C     *LIKE         DEFINE    TBLCOD        §CODK
007600890915     C     *LIKE         DEFINE    TBLKEY        §KEYK
007700890915     C*
007800890915     C     KEYTAB        KLIST                                                  *
007900890915     C                   KFLD                    §KUT                           *
008000890915     C                   KFLD                    §COD                           *
008100980930     C**
008200980930     C     KEYTA2        KLIST                                                  *
008300980930     C                   KFLD                    §KUT                           *
008400980930     C                   KFLD                    §COD                           *
008500980930     C                   KFLD                    §KEY                           *
008600890915     C     KEYTAK        KLIST                                                  *
008700890915     C                   KFLD                    §KUT                           *
008800890915     C                   KFLD                    §CODK                          *
008900890915     C                   KFLD                    §KEYK                          *
009000890915     C*
009100890915     C* DECODIFICO IL CODICE TABELLA   PASSATO
009200890915     C                   MOVE      *BLANK        §CODK
009300890915     C                   MOVE      *ZEROS        §KEYK
009400890915     C                   MOVE      §COD          §KEYK
009500900724     C                   MOVE      §COD          COTIPT
009600890915     C                   MOVE      *BLANK        DETIPT
009700890915     C     KEYTAK        CHAIN     TABEL                              90
009800890915     C  N90              MOVEL     TBLUNI        DETIPT
009900890915     C   90              MOVEL     'ERRORE'      DETIPT
010000000000     C*---------- GESTIONE SUBFILE ------------------------*
010100110210     c                   clear                   VIDmsg
010200110210     c                   eval      *in28 = *off
010300050707     C                   Z-ADD     0             REC1                           *
010400000000     C                   MOVE      *BLANKS       SCELTA                         *
010500000000     C*---------- PULIZIA SLFCT :                          *
010600000000     C                   SETON                                            70    *
010700890915     C                   WRITE     TABESC1                                      *
010800000000     C                   SETOFF                                       7071      *
010900000000     C*---------- CARICAMENTO SFL :                        *
011000050707     C     KEYTA2        SETLL     TABEL00F                           90  91    *
011100050707     C     SOPRA         TAG                                                    *
011200890915     C     KEYTAB        READE     TABEL                                  91    *
011300050707     C* FINE LETTURA
011400050707     C   91REC1          IFEQ      0
011500050707     C                   SETON                                        90
011600050707     C                   GOTO      FINE
011700050707     C                   ELSE
011800050707     C                   GOTO      EMISF1                                       *
011900050707     C                   ENDIF
012000980930     C**
012100900726     C     TBLFLG        IFEQ      '*'                                           ANN.TA
012200900726     C     §AN2          ANDNE     '§§'
012300900726     C                   GOTO      SOPRA
012400900726     C                   END
012500110210      *
012600110210     c                   if        Rec1 >= 9999
012700110210     c                   eval      VIDmsg = 'Superato il n° massimo di +
012800110210     c                                       record visualizzabili'
012900110210     c                   goto      EmiSf1
013000110210     c                   endif
013100110210      *
013200890915     C                   MOVEL     TBLKEY        CODTAB                         *
013300890915     C                   MOVEL     TBLUNI        DESCRI                         *
013400900724     C                   MOVEL     TBLFLG        STATTB                         *
013500000000     C     REC1          ADD       1             REC1                 71        *
013600890915     C                   WRITE     TABESF1                                      *
013700000000     C                   GOTO      SOPRA                                        *
013800000000     C*---------- EMISSIONE SFL :                          *
013900050707     C     EMISF1        TAG                                                    *
014000050707     C                   EVAL      RrnLast = Rec1
014100050707     C                   EVAL      rcdnbr  = 1
014200050707     C                   EVAL      wrksort = '1'
014300110210     c*
014400110210     c                   if        Not *in71
014500110210     c                   eval      VIDmsg = '                            +
014600110210     c                                       NON TROVATE INFORMAZIONI'
014700110210     c                   endif
014800110210     c*
014900050707     c                   do        *hival
015000050707     c*Descrizione della ricerca
015100050707     c                   if        wrksort = ' '
015200050707     c                   eval      destps = 'Descrizione'
015300050707     c                   else
015400050707     c                   eval      destps = 'Codice'
015500050707     c                   end
015600110210     c                   eval      *in28 = (VIDmsg <> *blank)
015700110210     c*
015800050707     C                   write     TABEd1                                       *
015900050707     C                   EXFMT     TABESC1                                      *
016000050707     C   KC              SETON                                            90    *
016100050707     C   KC              GOTO      FINE                                         *
016200050707     c*Cambio ordinamento
016300050707     C   kk              EXSR      Sort
016400050707     c                   exsr      ctr
016500050707     c*effettuata una scelta
016600050707     c                   if        scelta <> ' '
016700890915     C                   MOVE      CODTAB        §KEY                           *
016800890915     C                   MOVEL     DESCRI        §DES                           *
016900050707     c                   leave
017000050707     c                   end
017100050707     c                   enddo
017200000000     C*----------------------------------------------------*
017300050707     C     FINE          TAG                                                    *
017400050707     C   90              MOVE      *BLANKS       §KEY                           *
017500050707     C   90              MOVE      *BLANKS       §DES                           *
017600050707     C   90              MOVEL     'ERRORE'      §DES                           *
017700000000     C                   SETON                                            LR    *
017800050707     c**********************************************************************
017900050707     ** Controlli Sfl
018000050707     c**********************************************************************
018100050707     C     ctr           BEGSR
018200050707     C*
018300050707     c                   setoff                                       41
018400050707     c                   do        *hival        X                 4 0
018500050707     c     X             CHAIN     tabesf1                            01
018600050707     C   01              LEAVE
018700050707     c                   if        scelta <> ' '
018800050707     c                   leave
018900050707     c                   end
019000050707     C* CONTROLLO SE EFFETTUATA RICERCA
019100050707     C                   IF        VDES <> ' ' AND VDES <> VDESS
019200050707     c                   seton                                        41
019300050707     C                   if        (wrksort = ' ' and VDES <= descri) or
019400050707     C                             (wrksort = '1' and VDES <= codtab)
019500050707     c                   setoff                                       41
019600050707     C                   Z-ADD     X             rcdnbr
019700050707     C                   MOVEL     VDES          VDESS
019800050707     C                   LEAVE
019900050707     C                   END
020000050707     C                   END
020100050707     C                   ENDDO
020200050720     C* n41              clear                   VDES
020300050707     C*
020400050707     C                   ENDSR
020500050707      **********************************************************************
020600050707      * Subroutine - Sort
020700050707      *   This subroutine sorts the subfile records.
020800050707     C     Sort          BEGSR
020900050707     C                   CLEAR                   vdes
021000050707     C                   CLEAR                   vdess
021100050707     C                   EVAL      rcdnbr  = 1
021200050707
021300050707      * Initialize the key fields to sort on. There is one extra field not in
021400050707      * the subfile -- Selected -- that is added to the record. This field
021500050707      * is used to place selected records in front of nonselected records.
021600050707
021700050707     C                   CLEAR                   QLGSCB
021800050707     C                   CLEAR                   QLGSCB00
021900050707
022000050707     ** Gestione della scelta ordinamento.
022100050707     C                   SELECT
022200050707     ** Ordinamento per descrizione.
022300050707     C                   WHEN      WrkSort = Descrizione
022400050707     ** 1 campo chiave.
022500050707     C                   EVAL      QLGNBRK    = 1
022600050707     ** La descrizione è
022700050707     ** a posizone 10, 25 byte, char, ascending.
022800050707     C                   EVAL      QLGSP      = 10
022900050707     C                   EVAL      QLGSS      = %SIZE(descri)
023000050707     C                   EVAL      QLGDT      = Carattere
023100050707     C                   EVAL      QLGSO      = Ascendente
023200050707     C                   EVAL      QLGKL(1)   = QLGSKL
023300050707     ** Al prossimo F11 ordino per CODICE
023400050707     C                   EVAL      WrkSort = ' '
023500050707     ** Ordinamento per Codice
023600050707     C                   WHEN      WrkSort = ' '
023700050707     C                   EVAL      QLGNBRK    = 1
023800050707     ** Il codice è
023900050707     ** a posizione 2, 8 byte, char, ascending
024000050707     C                   EVAL      QLGSP      = 2
024100050707     C                   EVAL      QLGSS      = %SIZE(codtab)
024200050707     C                   EVAL      QLGDT      = Carattere
024300050707     C                   EVAL      QLGSO      = Ascendente
024400050707     C                   EVAL      QLGKL(1)   = QLGSKL
024500050707     ** Al prossimo F11 ordino per descrizione.
024600050707     C                   EVAL      WrkSort = Descrizione
024700050707     C                   ENDSL
024800050707      * Load other sort parameters.
024900050707     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
025000050707     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
025100050707     C                   EVAL      QLGRT     = 8
025200050707     C                   EVAL      QLGOKL    = 80
025300050707     C                   EVAL      QLGLKE    = 16
025400050707     C                   EVAL      QLGLSS    = 290
025500050707      * Initialize Sort I/O API fields.
025600050707     C                   EVAL      QLGRL00  = QLGRL
025700050707     C                   EVAL      QLGRC00  = 1
025800050707     C                   CLEAR                   QUSEI
025900050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
026000050707      * First step - Initialize the sort routine.
026100050707     C                   CALL      'QLGSORT'
026200050707     C                   PARM                    QLGSCB
026300050707     C                   PARM                    NotUsed
026400050707     C                   PARM                    NotUsed
026500050707     C                   PARM                    SizeList
026600050707     C                   PARM                    ReturnSize
026700050707     C                   PARM                    QUSEC
026800050707      * Next step - Write records to I/O routine.
026900050707     C                   EVAL      QLGRT00 = Put
027000050707     C                   DO        RrnLast       x
027100050707     C     x             CHAIN     tabesf1
027200050707     ** Solo le righe con Selected = 'Y' sono riordinate,
027300050707     ** quindi per fare un ordinamento di tutte le righe
027400050707     ** metto 'Y' sempre.
027500050707     C                   EVAL      Selected = 'Y'
027600050707     C                   CLEAR                   QUSEI
027700050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027800050707     C                   CALL      'QLGSRTIO'
027900050707     C                   PARM                    QLGSCB00
028000050707     C                   PARM                    SflRcd
028100050707     C                   PARM                    NotUsed
028200050707     C                   PARM                    SizeList
028300050707     C                   PARM                    NotUsed
028400050707     C                   PARM                    QUSEC
028500050707     C                   ENDDO
028600050707      * Next step - Signal end of input, clear subfile for reload.
028700050707     C                   EVAL      QLGRT00 = EndPut
028800050707     C                   CLEAR                   QUSEI
028900050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029000050707
029100050707     C                   CALL      'QLGSRTIO'
029200050707     C                   PARM                    QLGSCB00
029300050707     C                   PARM                    SflRcd
029400050707     C                   PARM                    NotUsed
029500050707     C                   PARM                    SizeList
029600050707     C                   PARM                    NotUsed
029700050707     C                   PARM                    QUSEC
029800050707     c                   seton                                        70
029900050707     C                   WRITE     TABESC1
030000050707     C                   setoff                                       70
030100050707      * Final step - Write the records back to the subfile.
030200050707     C                   EVAL      QLGRT00 = Get
030300050707     C                   DO        RrnLast       x
030400050707     C                   CLEAR                   QUSEI
030500050707     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030600050707     C                   CALL      'QLGSRTIO'
030700050707     C                   PARM                    QLGSCB00
030800050707     C                   PARM                    NotUsed
030900050707     C                   PARM                    SflRcd
031000050707     C                   PARM                    QLGRL00
031100050707     C                   PARM                    NotUsed
031200050707     C                   PARM                    QUSEC
031300050707     c                   z-add     x             rec1
031400050707     C                   WRITE     tabesf1
031500050707     C                   ENDDO
031600050707     C                   ENDSR
