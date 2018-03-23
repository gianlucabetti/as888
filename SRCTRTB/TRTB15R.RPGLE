000100950718     H*------------------------------------------------------------------------*
000200011120     H* RICERCA E SCELTA TABELLA CON SELEZIONE MULTIPLA                        *
000300011120     H* PERMETTE LA RICERCA E LA SELEZIONE PER CHIAVE TABELLA O PER         O  *
000400011120     H* DESCRIZIONE                                                            *
000500011120     H*                                                                        *
000600011120     h* PARAMETRI:
000700011120     h* codice tabella  2  I
000800011120     h* ordinamento     1  I  (*blanks = codice  1=descizione)
000900051201      * tipo richiamo   1  I  (N=Nota C=Contatti)
001000011120     h* chiave tabella  8  O
001100011126     h* codice ritorno  1  O  (comando premuto)
001200950718     H*------------------------------------------------------------------------*
001300950718     H DECEDIT('0,') DATEDIT(*DMY.)
001400950718     F*------------------------------------------------------------------------*
001500950718     F* DATA BASE
001600950718     F*------------------------------------------------------------------------*
001700051201     FTRTB15D   CF   E             WORKSTN
001800011120     F                                     SFILE(SFL01:RRN1)
001900011120     FTABEL00F  IF   E           K DISK
002000051201
002100051201     d ds1t          e ds
002200011120      **********************************************************************
002300011120      * Constants
002400011120      *
002500011120      *     MaxKey - Maximum number of key fields allowed by this program
002600011120      *
002700011120      **********************************************************************
002800011120     D MaxKey          C                    1
002900011120     D Descrizione     C                   '1'
003000011120     D Ascendente      C                   1
003100011120     D Carattere       C                   6
003200011120     D Put             C                   1
003300011120     D EndPut          C                   2
003400011120     D Get             C                   3
003500011120      **********************************************************************
003600011120      * Standalone fields
003700011120      *
003800011120      *     RRN1       - Relative record number for subfile
003900011120      *     SizeList   - Size of list
004000011120      *     ReturnSize - Size of list returned by sort API
004100011120      **********************************************************************
004200011120     D NotUsed         S             16A
004300011120     D ReturnSize      S              9B 0
004400011120     D SizeList        S              9B 0
004500011120     D RrnLast         S              5i 0
004600011120     D WrkSort         S              1
004700011120     D VDESS           S                   LIKE(VDES)
004800011120     D rrn1            S              5i 0
004900011121     D com45           S             45    inz
005000011121     D blanks          S             45    inz
005100011121     D len             S              2  0
005200011120      **********************************************************************
005300011120      * Data Structures
005400011120      *     SflRcd     - The entire subfile record to pass to the sort
005500011120      *     QLGSCB     - The sort request block for the QLGSORT API
005600011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
005700011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
005800011120      *     QUSEC      - Error structure for the QLGSORT API
005900011120      **********************************************************************
006000011120     D SflRcd          DS
006100011120     D  S01OPZ
006200011120     D  S01Cod
006300011120     D  S01Des
006400011120     D  Selected                      1A
006500011120
006600011120      /COPY QSYSINC/QRPGLESRC,QLGSORT
006700011120     D QLGKL                         16    DIM(MaxKey)
006800011120     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
006900011120     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007000011120     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007100011120     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
007200011120
007300011120      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
007400011120      /COPY QSYSINC/QRPGLESRC,QUSEC
007500011120     c* ordina il sfl in base al parametro passato (ORDINA)
007600011120     C                   EXSR      Initialize
007700011121     c                   if        rrn1 = 0
007800011121     c                   seton                                        41
007900011121     c                   end
008000011120     c*
008100011120     C                   DO        *HIVAL
008200011121     c*Descrizione della ricerca
008300011120     c                   if        wrksort = ' '
008400011120     c                   eval      destps = 'Descrizione'
008500011120     c                   else
008600051206     c                   eval      destps = 'Codice'
008700011120     c                   end
008800011121     c*
008900011121     C                   WRITE     TOP
009000011121     C                   WRITE     BOT
009100011120     C                   EXFMT     CTL01
009200011126     c                   if        *inkl
009300011126     c                   move      'L'           cmd
009400011126     C                   LEAVE
009500011126     c                   end
009600011121     c                   if        rrn1 = 0
009700011121     c                   iter
009800011121     c                   end
009900011121     C                   SELECT
010000011121     c*Rivisualizza
010100011121     C                   WHEN      *INKE
010200011121     C                   EXSR      Initialize
010300011121     c*Cambio ordinamento
010400011121     C                   WHEN      *INKK
010500011121     C                   EXSR      Sort
010600011121     C                   ENDSL
010700011121     c*Controlli
010800011120     c                   exsr      ctr
010900011121     c*effettuata una scelta
011000020218     c                   if        keytab <> *blanks
011100011126     c                   clear                   cmd
011200020218     c                   if        keytab = '********'
011300020218     c                   clear                   keytab
011400020218     c                   end
011500011120     c                   leave
011600011120     c                   end
011700011120     C                   ENDDO
011800011120     C                   SETON                                        LR
011900011120      **********************************************************************
012000011120     C     Initialize    BEGSR
012100011121     c*
012200011120     C                   SETON                                        40
012300011120     C                   WRITE     CTL01
012400011120     C                   SETOFF                                       40
012500011120     C                   CLEAR                   RRN1
012600011121     c*Carico il subfile in ordine di codice
012700011120     C     KTAB          SETLL     Tabel00F
012800011120     C                   DO        *HIVAL
012900011120     C     KTAB          READE     Tabel00F
013000011120     C                   IF        %eof(TABEL00F)
013100011120     C                   LEAVE
013200011120     C                   END
013300020218     C*                  IF        TblKey <> *BLANK
013400051201     c                   eval      ds1t = tbluni
013500051201     c                   If        §1trich <> tiporich
013600051201     c                   Iter
013700051201     c                   EndIf
013800011120     C                   MOVEL(P)  TBLKEY        S01COD
013900051201     C                   MOVEL(P)  §1tdes        S01DES
014000011120     C                   ADD       1             RRN1
014100011120     C                   WRITE     SFL01
014200020218     C*                  ENDIF
014300011120     C                   ENDDO
014400011120     C                   EVAL      RrnLast = Rrn1
014500011120     C                   EVAL      rcdnbr  = 1
014600011120     c* se l'ordinamento richiesto in entrata è per descrizione sorto il sfl
014700011120     C                   EVAL      WrkSort = ordina
014800011219     c                   exsr      sort
014900011120     C                   ENDSR
015000011120
015100011120      **********************************************************************
015200011120      * Subroutine - Sort
015300011120      *   This subroutine sorts the subfile records.
015400011120     C     Sort          BEGSR
015500011121     C                   CLEAR                   vdes
015600011121     C                   CLEAR                   vdess
015700011121     C                   EVAL      rcdnbr  = 1
015800011120
015900011120      * Initialize the key fields to sort on. There is one extra field not in
016000011120      * the subfile -- Selected -- that is added to the record. This field
016100011120      * is used to place selected records in front of nonselected records.
016200011120
016300011120     C                   CLEAR                   QLGSCB
016400011120     C                   CLEAR                   QLGSCB00
016500011120
016600011120     ** Gestione della scelta ordinamento.
016700011120     C                   SELECT
016800011120     ** Ordinamento per descrizione.
016900011120     C                   WHEN      WrkSort = Descrizione
017000011120     ** 1 campo chiave.
017100011120     C                   EVAL      QLGNBRK    = 1
017200011120     ** La descrizione è
017300011121     ** a posizone 10, 25 byte, char, ascending.
017400011121     C                   EVAL      QLGSP      = 10
017500011120     C                   EVAL      QLGSS      = %SIZE(S01Des)
017600011120     C                   EVAL      QLGDT      = Carattere
017700011120     C                   EVAL      QLGSO      = Ascendente
017800011120     C                   EVAL      QLGKL(1)   = QLGSKL
017900011120     ** Al prossimo F11 ordino per CODICE
018000011120     C                   EVAL      WrkSort = ' '
018100011120     ** Ordinamento per Codice
018200011120     C                   WHEN      WrkSort = ' '
018300011120     C                   EVAL      QLGNBRK    = 1
018400011120     ** Il codice è
018500011120     ** a posizione 2, 7 byte, char, ascending
018600011120     C                   EVAL      QLGSP      = 2
018700011120     C                   EVAL      QLGSS      = %SIZE(S01cod)
018800011120     C                   EVAL      QLGDT      = Carattere
018900011120     C                   EVAL      QLGSO      = Ascendente
019000011120     C                   EVAL      QLGKL(1)   = QLGSKL
019100011120     ** Al prossimo F11 ordino per descrizione.
019200011120     C                   EVAL      WrkSort = Descrizione
019300011120     C                   ENDSL
019400011120      * Load other sort parameters.
019500011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
019600011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
019700011120     C                   EVAL      QLGRT     = 8
019800011120     C                   EVAL      QLGOKL    = 80
019900011120     C                   EVAL      QLGLKE    = 16
020000011120     C                   EVAL      QLGLSS    = 290
020100011120      * Initialize Sort I/O API fields.
020200011120     C                   EVAL      QLGRL00  = QLGRL
020300011120     C                   EVAL      QLGRC00  = 1
020400011120     C                   CLEAR                   QUSEI
020500011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
020600011120      * First step - Initialize the sort routine.
020700011120     C                   CALL      'QLGSORT'
020800011120     C                   PARM                    QLGSCB
020900011120     C                   PARM                    NotUsed
021000011120     C                   PARM                    NotUsed
021100011120     C                   PARM                    SizeList
021200011120     C                   PARM                    ReturnSize
021300011120     C                   PARM                    QUSEC
021400011120      * Next step - Write records to I/O routine.
021500011120     C                   EVAL      QLGRT00 = Put
021600011121     C                   DO        RrnLast       x
021700011121     C     x             CHAIN     SFL01
021800011120     ** Solo le righe con Selected = 'Y' sono riordinate,
021900011120     ** quindi per fare un ordinamento di tutte le righe
022000011120     ** metto 'Y' sempre.
022100011120     C                   EVAL      Selected = 'Y'
022200011120     C                   CLEAR                   QUSEI
022300011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
022400011120     C                   CALL      'QLGSRTIO'
022500011120     C                   PARM                    QLGSCB00
022600011120     C                   PARM                    SflRcd
022700011120     C                   PARM                    NotUsed
022800011120     C                   PARM                    SizeList
022900011120     C                   PARM                    NotUsed
023000011120     C                   PARM                    QUSEC
023100011120     C                   ENDDO
023200011120      * Next step - Signal end of input, clear subfile for reload.
023300011120     C                   EVAL      QLGRT00 = EndPut
023400011120     C                   CLEAR                   QUSEI
023500011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
023600011120
023700011120     C                   CALL      'QLGSRTIO'
023800011120     C                   PARM                    QLGSCB00
023900011120     C                   PARM                    SflRcd
024000011120     C                   PARM                    NotUsed
024100011120     C                   PARM                    SizeList
024200011120     C                   PARM                    NotUsed
024300011120     C                   PARM                    QUSEC
024400011120     c                   seton                                        40
024500011120     C                   WRITE     Ctl01
024600011120     C                   setoff                                       40
024700011120      * Final step - Write the records back to the subfile.
024800011120     C                   EVAL      QLGRT00 = Get
024900011121     C                   DO        RrnLast       x
025000011120     C                   CLEAR                   QUSEI
025100011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
025200011120     C                   CALL      'QLGSRTIO'
025300011120     C                   PARM                    QLGSCB00
025400011120     C                   PARM                    NotUsed
025500011120     C                   PARM                    SflRcd
025600011120     C                   PARM                    QLGRL00
025700011120     C                   PARM                    NotUsed
025800011120     C                   PARM                    QUSEC
025900011121     c                   z-add     x             rrn1
026000011120     C                   WRITE     Sfl01
026100011120     C                   ENDDO
026200011120     C                   ENDSR
026300011120     c**********************************************************************
026400011120     ** Controlli Sfl
026500011120     c**********************************************************************
026600011120     C     ctr           BEGSR
026700011120     C*
026800011121     c                   setoff                                       41
026900011120     c                   do        *hival        X                 4 0
027000011120     c     X             CHAIN     sfl01                              01
027100011120     C   01              LEAVE
027200020218     c                   if        s01opz <> *blanks
027300011120     c                   movel     s01cod        keytab
027400020218     c                   if        keytab = *blanks
027500020218     c                   movel     *all'*'       keytab
027600020218     c                   end
027700011120     c                   leave
027800011120     c                   end
027900011120     C* CONTROLLO SE EFFETTUATA RICERCA
028000011121     C                   IF        VDES <> ' ' AND VDES <> VDESS
028100011121     c                   seton                                        41
028200011121     C                   if        (wrksort = ' ' and VDES <= S01DES) or
028300011121     C                             (wrksort = '1' and VDES <= S01cod)
028400011121     c                   setoff                                       41
028500011120     C                   Z-ADD     X             rcdnbr
028600011121     C                   MOVEL     VDES          VDESS
028700011120     C                   LEAVE
028800011120     C                   END
028900011121     C                   END
029000011120     C                   ENDDO
029100011121     C  n41              clear                   VDES
029200011120     C*
029300011120     C                   ENDSR
029400011120     c**********************************************************************
029500011120     ** Operazioni iniziali.
029600011120     c**********************************************************************
029700011120     C     *INZSR        BEGSR
029800011120     C*
029900011120     C     *ENTRY        PLIST
030000011120     C                   PARM                    CODTAB            2
030100011120     C                   PARM                    ORDINA            1
030200051201     C                   PARM                    TipoRich          1
030300011120     C                   PARM                    keytab            8
030400011126     C                   PARM                    cmd               1
030500011126     c                   clear                   cmd
030600011126     c                   clear                   keytab
030700011121     C                   EVAL      TblKut = 1
030800011120     C*
030900011120     C     KTAB          KLIST
031000011120     C                   KFLD                    TblKut
031100011120     C                   KFLD                    CODTAB
031200051206     c                   If        tiporich = 'N'
031300051206     c                   Eval      com45 = 'Tipi Nota'
031400051206     c                   endIf
031500051206     c                   If        tiporich = 'C'
031600051206     c                   Eval      com45 = 'Tipi Contatto'
031700051206     c                   endIf
031800051206     c                   eval      len=(%len(com45)-%len(%trimr(com45)))/2
031900051206     c                   eval      vtipr = %subst(blanks:1:len) + com45
032000011121     c                   eval      %subst(vtipr:43:3)=' '+codtab
032100011121     c*
032200011120     C                   ENDSR
