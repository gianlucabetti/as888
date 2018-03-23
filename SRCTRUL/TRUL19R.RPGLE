000100950718     H*------------------------------------------------------------------------*
000200011120     H* RICERCA E SCELTA TABELLA CON SELEZIONE MULTIPLA                        *
000300011120     H* PERMETTE LA RICERCA E LA SELEZIONE PER CHIAVE TABELLA O PER         O  *
000400011120     H* DESCRIZIONE                                                            *
000500011120     H*                                                                        *
000600011120     h* PARAMETRI:
000700011120     h* codice tabella  2  I
000800011120     h* ordinamento     1  I  (*blanks = codice  1=descizione)
000900011120     h* chiave tabella  8  O
001000011126     h* codice ritorno  1  O  (comando premuto)
001100950718     H*------------------------------------------------------------------------*
001200950718     H DECEDIT('0,') DATEDIT(*DMY.)
001300950718     F*------------------------------------------------------------------------*
001400950718     F* DATA BASE
001500950718     F*------------------------------------------------------------------------*
001600011120     FTRUL19D   CF   E             WORKSTN
001700011120     F                                     SFILE(SFL01:RRN1)
001800011120     FTABEL00F  IF   E           K DISK
001900011120      **********************************************************************
002000011120      * Constants
002100011120      *
002200011120      *     MaxKey - Maximum number of key fields allowed by this program
002300011120      *
002400011120      **********************************************************************
002500011120     D MaxKey          C                    1
002600011120     D Descrizione     C                   '1'
002700180111     d Cod_Naz_ISO     c                   '2'
002800011120     D Ascendente      C                   1
002900011120     D Carattere       C                   6
003000011120     D Put             C                   1
003100011120     D EndPut          C                   2
003200011120     D Get             C                   3
003300011120      **********************************************************************
003400011120      * Standalone fields
003500011120      *
003600011120      *     RRN1       - Relative record number for subfile
003700011120      *     SizeList   - Size of list
003800011120      *     ReturnSize - Size of list returned by sort API
003900011120      **********************************************************************
004000011120     D NotUsed         S             16A
004100011120     D ReturnSize      S              9B 0
004200011120     D SizeList        S              9B 0
004300011120     D RrnLast         S              5i 0
004400011120     D WrkSort         S              1
004500011120     D VDESS           S                   LIKE(VDES)
004600011120     D rrn1            S              5i 0
004700180112     D*// com45           S             45    inz
004800180112     D*// blanks          S             45    inz
004900180112     D*// len             S              2  0
005000180112     d PosIni          s              2  0 inz
005100011120      **********************************************************************
005200011120      * Data Structures
005300011120      *     SflRcd     - The entire subfile record to pass to the sort
005400011120      *     QLGSCB     - The sort request block for the QLGSORT API
005500011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
005600011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
005700011120      *     QUSEC      - Error structure for the QLGSORT API
005800011120      **********************************************************************
005900011120     D SflRcd          DS
006000011120     D  S01OPZ
006100011120     D  S01Cod
006200011120     D  S01Des
006300180112     D  S01de2
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
007500180112
007600180112      * Tabelle "particolari" (gestite all'interno del *pgm)
007700180112     d ds15          e ds                  inz
007800180112
007900011120     c* ordina il sfl in base al parametro passato (ORDINA)
008000011120     C                   EXSR      Initialize
008100011121     c                   if        rrn1 = 0
008200011121     c                   seton                                        41
008300011121     c                   end
008400011120     c*
008500011120     C                   DO        *HIVAL
008600011121     c*Descrizione della ricerca
008700180112     c*//                if        wrksort = ' '
008800180112     c                   Select
008900180112     c                   When      wrkSort = *blank
009000180112     c                   eval      destps = 'Codice'
009100180112     c*//                else
009200180112     c                   When      wrkSort = '1'
009300180112     c                   eval      destps = 'Descrizione'
009400180112     c*//                end
009500180112     c                   When      wrkSort = '2'
009600180112     c                   eval      destps = 'Codice ISO'
009700180112     c                   EndSl
009800011121     c*
009900011121     C                   WRITE     TOP
010000011121     C                   WRITE     BOT
010100011120     C                   EXFMT     CTL01
010200011126     c                   if        *inkl
010300011126     c                   move      'L'           cmd
010400011126     C                   LEAVE
010500011126     c                   end
010600011121     c                   if        rrn1 = 0
010700011121     c                   iter
010800011121     c                   end
010900011121     C                   SELECT
011000011121     c*Rivisualizza
011100011121     C                   WHEN      *INKE
011200011121     C                   EXSR      Initialize
011300011121     c*Cambio ordinamento
011400011121     C                   WHEN      *INKK
011500180112     c                   exsr      Change_Ord
011600011121     C                   EXSR      Sort
011700011121     C                   ENDSL
011800011121     c*Controlli
011900011120     c                   exsr      ctr
012000011121     c*effettuata una scelta
012100020218     c                   if        keytab <> *blanks
012200011126     c                   clear                   cmd
012300020218     c                   if        keytab = '********'
012400020218     c                   clear                   keytab
012500020218     c                   end
012600011120     c                   leave
012700011120     c                   end
012800011120     C                   ENDDO
012900011120     C                   SETON                                        LR
013000011120      **********************************************************************
013100011120     C     Initialize    BEGSR
013200011121     c*
013300180112     c                   eval      *in51 = ( CodTab = '15' )
013400011120     C                   SETON                                        40
013500011120     C                   WRITE     CTL01
013600011120     C                   SETOFF                                       40
013700011120     C                   CLEAR                   RRN1
013800011121     c*Carico il subfile in ordine di codice
013900011120     C     KTAB          SETLL     Tabel00F
014000011120     C                   DO        *HIVAL
014100011120     C     KTAB          READE     Tabel00F
014200011120     C                   IF        %eof(TABEL00F)
014300011120     C                   LEAVE
014400011120     C                   END
014500020218     C*                  IF        TblKey <> *BLANK
014600011120     C                   MOVEL(P)  TBLKEY        S01COD
014700011120     C                   MOVEL(P)  TBLUNI        S01DES
014800180112     c                   clear                   S01de2
014900180112     c                   if        CodTab = '15'
015000180112     c                   movel     TBLuni        ds15
015100180112     c                   movel(P)  §15cod        S01de2
015200180112     c                   endif
015300011120     C                   ADD       1             RRN1
015400011120     C                   WRITE     SFL01
015500020218     C*                  ENDIF
015600011120     C                   ENDDO
015700180112     C*
015800180112     C                   eval      *in42   = *on
015900011120     C                   EVAL      RrnLast = Rrn1
016000011120     C                   EVAL      rcdnbr  = 1
016100011120     c* se l'ordinamento richiesto in entrata è per descrizione sorto il sfl
016200011120     C                   EVAL      WrkSort = ordina
016300011219     c                   exsr      sort
016400011120     C                   ENDSR
016500180112
016600180112      **********************************************************************
016700180112     c     Change_ORD    BEGSR
016800180112      *
016900180112     c                   Select
017000180112     c                   When      WrkSort = *blank
017100180112     c                   eval      WrkSort = '1'
017200180112     c                   When      WrkSort = '1'  and  CodTab = '15'
017300180112     c                   eval      WrkSort = '2'
017400180112     c                   Other
017500180112     c                   eval      WrkSort = *blank
017600180112     c                   EndSl
017700180112      *
017800180112     c                   ENDSR
017900011120
018000011120      **********************************************************************
018100011120      * Subroutine - Sort
018200011120      *   This subroutine sorts the subfile records.
018300011120     C     Sort          BEGSR
018400011121     C                   CLEAR                   vdes
018500011121     C                   CLEAR                   vdess
018600011121     C                   EVAL      rcdnbr  = 1
018700011120
018800011120      * Initialize the key fields to sort on. There is one extra field not in
018900011120      * the subfile -- Selected -- that is added to the record. This field
019000011120      * is used to place selected records in front of nonselected records.
019100011120
019200011120     C                   CLEAR                   QLGSCB
019300011120     C                   CLEAR                   QLGSCB00
019400011120
019500011120     ** Gestione della scelta ordinamento.
019600011120     C                   SELECT
019700180112     **
019800011120     ** Ordinamento per Codice
019900011120     C                   WHEN      WrkSort = ' '
020000011120     C                   EVAL      QLGNBRK    = 1
020100011120     ** Il codice è
020200180112     ** a posizione 2, 8 byte, char, ascending
020300180112     C                   EVAL      QLGSP      = %size(S01opz) + 1
020400011120     C                   EVAL      QLGSS      = %SIZE(S01cod)
020500011120     C                   EVAL      QLGDT      = Carattere
020600011120     C                   EVAL      QLGSO      = Ascendente
020700011120     C                   EVAL      QLGKL(1)   = QLGSKL
020800180112     ** Al prossimo F11 ordino per Descrizione.
020900180112     ** (NON più: ora è gestito nella subr. "Change_ORD")
021000180112     C*//                EVAL      WrkSort = Descrizione
021100180112     **
021200180112     ** Ordinamento per Descrizione.
021300180112     C                   WHEN      WrkSort = Descrizione
021400180112     ** 1 campo chiave.
021500180112     C                   EVAL      QLGNBRK    = 1
021600180112     ** La descrizione è
021700180112     ** a posizone 10, 25 byte, char, ascending.
021800180112     C                   EVAL      QLGSP      = %size(S01opz)
021900180112     C                                        + %size(S01cod) + 1
022000180112     C                   EVAL      QLGSS      = %SIZE(S01Des)
022100180112     C                   EVAL      QLGDT      = Carattere
022200180112     C                   EVAL      QLGSO      = Ascendente
022300180112     C                   EVAL      QLGKL(1)   = QLGSKL
022400180112     ** Al prossimo F11 ordino per CODICE
022500180112     ** (NON più: ora è gestito nella subr. "Change_ORD")
022600180112     C*//                EVAL      WrkSort = ' '
022700180112     **
022800180112     ** Ordinamento per Codice ISO
022900180112     C                   WHEN      WrkSort = Cod_Naz_ISO
023000180112     C                   EVAL      QLGNBRK    = 1
023100180112     ** Il codice è
023200180112     ** a posizione 35, 7 byte, char, ascending
023300180112     C                   EVAL      QLGSP      = %size(S01opz)
023400180112     C                                        + %size(S01cod)
023500180112     C                                        + %size(S01des) + 1
023600180112     C                   EVAL      QLGSS      = %size(S01de2)
023700180112     C                   EVAL      QLGDT      = Carattere
023800180112     C                   EVAL      QLGSO      = Ascendente
023900180112     C                   EVAL      QLGKL(1)   = QLGSKL
024000180112     ** Al prossimo F11 ordino per Codice.
024100180112     ** (NON più: ora è gestito nella subr. "Change_ORD")
024200180112     C*//                EVAL      WrkSort = *blanks
024300011120     C                   ENDSL
024400180112      *
024500011120      * Load other sort parameters.
024600011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
024700011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
024800011120     C                   EVAL      QLGRT     = 8
024900011120     C                   EVAL      QLGOKL    = 80
025000011120     C                   EVAL      QLGLKE    = 16
025100011120     C                   EVAL      QLGLSS    = 290
025200011120      * Initialize Sort I/O API fields.
025300011120     C                   EVAL      QLGRL00  = QLGRL
025400011120     C                   EVAL      QLGRC00  = 1
025500011120     C                   CLEAR                   QUSEI
025600011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
025700011120      * First step - Initialize the sort routine.
025800011120     C                   CALL      'QLGSORT'
025900011120     C                   PARM                    QLGSCB
026000011120     C                   PARM                    NotUsed
026100011120     C                   PARM                    NotUsed
026200011120     C                   PARM                    SizeList
026300011120     C                   PARM                    ReturnSize
026400011120     C                   PARM                    QUSEC
026500011120      * Next step - Write records to I/O routine.
026600011120     C                   EVAL      QLGRT00 = Put
026700011121     C                   DO        RrnLast       x
026800011121     C     x             CHAIN     SFL01
026900011120     ** Solo le righe con Selected = 'Y' sono riordinate,
027000011120     ** quindi per fare un ordinamento di tutte le righe
027100011120     ** metto 'Y' sempre.
027200011120     C                   EVAL      Selected = 'Y'
027300011120     C                   CLEAR                   QUSEI
027400011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027500011120     C                   CALL      'QLGSRTIO'
027600011120     C                   PARM                    QLGSCB00
027700011120     C                   PARM                    SflRcd
027800011120     C                   PARM                    NotUsed
027900011120     C                   PARM                    SizeList
028000011120     C                   PARM                    NotUsed
028100011120     C                   PARM                    QUSEC
028200011120     C                   ENDDO
028300011120      * Next step - Signal end of input, clear subfile for reload.
028400011120     C                   EVAL      QLGRT00 = EndPut
028500011120     C                   CLEAR                   QUSEI
028600011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028700011120
028800011120     C                   CALL      'QLGSRTIO'
028900011120     C                   PARM                    QLGSCB00
029000011120     C                   PARM                    SflRcd
029100011120     C                   PARM                    NotUsed
029200011120     C                   PARM                    SizeList
029300011120     C                   PARM                    NotUsed
029400011120     C                   PARM                    QUSEC
029500011120     c                   seton                                        40
029600011120     C                   WRITE     Ctl01
029700011120     C                   setoff                                       40
029800011120      * Final step - Write the records back to the subfile.
029900011120     C                   EVAL      QLGRT00 = Get
030000011121     C                   DO        RrnLast       x
030100011120     C                   CLEAR                   QUSEI
030200011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030300011120     C                   CALL      'QLGSRTIO'
030400011120     C                   PARM                    QLGSCB00
030500011120     C                   PARM                    NotUsed
030600011120     C                   PARM                    SflRcd
030700011120     C                   PARM                    QLGRL00
030800011120     C                   PARM                    NotUsed
030900011120     C                   PARM                    QUSEC
031000011121     c                   z-add     x             rrn1
031100011120     C                   WRITE     Sfl01
031200011120     C                   ENDDO
031300011120     C                   ENDSR
031400011120     c**********************************************************************
031500011120     ** Controlli Sfl
031600011120     c**********************************************************************
031700011120     C     ctr           BEGSR
031800011120     C*
031900011121     c                   setoff                                       41
032000011120     c                   do        *hival        X                 4 0
032100011120     c     X             CHAIN     sfl01                              01
032200011120     C   01              LEAVE
032300020218     c                   if        s01opz <> *blanks
032400011120     c                   movel     s01cod        keytab
032500020218     c                   if        keytab = *blanks
032600020218     c                   movel     *all'*'       keytab
032700020218     c                   end
032800011120     c                   leave
032900011120     c                   end
033000011120     C* CONTROLLO SE EFFETTUATA RICERCA
033100011121     C                   IF        VDES <> ' ' AND VDES <> VDESS
033200011121     c                   seton                                        41
033300180112     C                   if        (wrksort = ' ' and VDES <= S01cod) or
033400180112     C                             (wrksort = '1' and VDES <= S01des) or
033500180112     C                             (wrkSort = '2' and VDES <= S01de2)
033600011121     c                   setoff                                       41
033700011120     C                   Z-ADD     X             rcdnbr
033800011121     C                   MOVEL     VDES          VDESS
033900011120     C                   LEAVE
034000011120     C                   END
034100011121     C                   END
034200011120     C                   ENDDO
034300011121     C  n41              clear                   VDES
034400011120     C*
034500011120     C                   ENDSR
034600011120     c**********************************************************************
034700011120     ** Operazioni iniziali.
034800011120     c**********************************************************************
034900011120     C     *INZSR        BEGSR
035000011120     C*
035100011120     C     *ENTRY        PLIST
035200011120     C                   PARM                    CODTAB            2
035300011120     C                   PARM                    ORDINA            1
035400011120     C                   PARM                    keytab            8
035500011126     C                   PARM                    cmd               1
035600180112     C*
035700011126     c                   clear                   cmd
035800011126     c                   clear                   keytab
035900011121     C                   EVAL      TblKut = 1
036000011120     C*
036100011120     C     KTAB          KLIST
036200011120     C                   KFLD                    TblKut
036300011120     C                   KFLD                    CODTAB
036400011121     C*
036500011121     C     KTAB1         KLIST
036600011121     C                   KFLD                    TblKut
036700011121     C                   KFLD                    CODTABi           2
036800011121     C                   KFLD                    keyTABi           8
036900180112     C*
037000011121     C                   movel     *all'0'       keytabi
037100011121     C                   move      codtab        keytabi
037200011121     c     ktab1         chain     tabel00f
037300011121     c                   if        %found(tabel00f)
037400180112     c*//                movel     tbluni        com45
037500180112     c*//                eval      len=(%len(com45)-%len(%trimr(com45)))/2
037600180112     c*//                eval      vtipr = %subst(blanks:1:len) + com45
037700180112     c*//                eval      %subst(vtipr:43:3)=' '+codtab
037800180112     c*//                eval      vtipr = %subst(blanks:1:len) + com45
037900180112     c                   eval      PosIni = ( ( ( %len(VtipR) - 3 ) -
038000180112     c                                        %len( %trimr( %subst(
038100180112     c                                        TBLuni : 1 : %len(VtipR)-3
038200180112     c                                      ) ) ) ) / 2 ) + 1
038300180112     c                   clear                   VtipR
038400180112     c                   eval      %subst( VtipR : PosIni ) =
038500180112     c                             %trim( %subst( TBLuni : 1 :
038600180112     c                                            %len(VtipR) - 3 ) )
038700180112     c                   eval      %subst( VtipR : %len( VtipR ) - 2 ) =
038800180112     c                                           ' ' + CodTab
038900180112     c                   endif
039000011121     c*
039100011120     C                   ENDSR
