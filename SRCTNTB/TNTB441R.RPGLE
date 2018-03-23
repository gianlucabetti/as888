000100020121     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020121      *--------------------------------------------------------------------------------------------*
000300060201      * Interrogazione tabella Abilitazioni Clienti 'ABC'                                          *
000400060201      * Permette la ricerca per descrizione                                                        *
000500020121      *--------------------------------------------------------------------------------------------*
000600020121
000700020121      * PARAMETRI:
000800060201      * Abilitazione   15  I
000900060201      * Ordinamento     1  I  (0 = Abilitazione;
001000060209      *                        1 = Ordinamento visualizzazione
001100060201      * Chiave tabella 15  O
001200021114      * Codice ritorno  1  O  (comando premuto)
001300020121
001400020121      *--------------------------------------------------------------------------------------------*
001500020121
001600060201     ftntb441d  cf   e             workstn
001700060201     f                                     sfile(tb441s01:nrr)
001800110804     ftntbe01l  uf a e           k disk    usropn
001900011120      **********************************************************************
002000011120      * Constants
002100011120      *
002200011120      *     MaxKey - Maximum number of key fields allowed by this program
002300011120      *
002400011120      **********************************************************************
002500060209     D MaxKey          C                   1
002600011120     D Ascendente      C                   1
002700011120     D Carattere       C                   6
002800060209     D Numero          C                   2
002900011120     D Put             C                   1
003000011120     D EndPut          C                   2
003100011120     D Get             C                   3
003200011120      **********************************************************************
003300011120      * Standalone fields
003400011120      *
003500020121      *     Nrr        - Relative record number for subfile
003600011120      *     SizeList   - Size of list
003700011120      *     ReturnSize - Size of list returned by sort API
003800011120      **********************************************************************
003900011120     D NotUsed         S             16A
004000011120     D ReturnSize      S              9B 0
004100011120     D SizeList        S              9B 0
004200011120     D RrnLast         S              5i 0
004300051027     d Nrr             s              5i 0
004400060209     D WrkSort         S              1    inz('0')
004500011120      **********************************************************************
004600011120      * Data Structures
004700011120      *     SflRcd     - The entire subfile record to pass to the sort
004800011120      *     QLGSCB     - The sort request block for the QLGSORT API
004900011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
005000011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
005100011120      *     QUSEC      - Error structure for the QLGSORT API
005200011120      **********************************************************************
005300011120     D SflRcd          DS
005400020121     d  vs1sce
005500060201     d  vs1abc
005600020121     d  vs1des
005700060201     d  vs1aut
005800060209     d  vs1ord
005900011120     D  Selected                      1A
006000021114
006100051027     d KPJBA         e ds
006200021114     d param01         ds
006300060201     d  p01abc                       15
006400021114     d  p01ord                        1
006500060201     d  p01ke1                       15
006600021114     d  p01rit                        1
006700021114
006800051027      * - Parametri x Controllo profilo utenti
006900051027     d TIBS34DS      e ds
007000051027      * - Ds di riferimento al file esterno AZUTE00F
007100051027     d AZUTEds       e ds                  extname(AZUTE00F)
007200051027      * - Ds per dati organigramma
007300051027     d DDatiUte      e ds
007400051027
007500060201     d dabc          e ds                  inz
007600111027     d tibs02ds      e ds                  inz
007700020121
007800011120      /COPY QSYSINC/QRPGLESRC,QLGSORT
007900011120     D QLGKL                         16    DIM(MaxKey)
008000011120     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
008100011120     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
008200011120     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
008300011120     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008400011120
008500011120      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008600011120      /COPY QSYSINC/QRPGLESRC,QUSEC
008700051027
008800051027     d kcod            s                   like(tbecod)  inz
008900051027     d klin            s                   like(tbelin)  inz
009000051027     d ksif            s                   like(tbesif)  inz
009100051027     d kke1            s                   like(tbeke1)  inz
009200051027      *
009300060201     d S1abc           s                   like(V1abc)   inz
009400060209     d S1ord           s                   like(V1ord)   inz
009500051027
009600051027     d XX              s              4  0 inz
009700110804
009800110804       // - Esecuzione comando di sistema
009900110804      /copy gaitrasrc/srcProtoPR,QCMDEXC
010000110804
010100110804      // - Campi per richiamo QCMDEXC
010200110804     d Qcmd            s            128    inz
010300110804
010400110804      *--------------------------------------------------------------------------------------------*
010500020121
010600020121      * Ordina il sfl in base al parametro passato
010700020121     c                   exsr      Initialize
010800020121     c                   if        nrr = 0
010900011121     c                   seton                                        41
011000020121     c                   endif
011100020121
011200020121     c                   do        *hival
011300021114      *Descrizione della ricerca
011400051027     c                   select
011500051027     c                   when      WrkSort = '0'
011600051027     c                   eval      *in42 = *off
011700051027     c                   when      WrkSort = '1'
011800051027     c                   eval      *in42 = *on
011900051027     c                   endsl
012000020121
012100060201     c                   write     tb441t01
012200060201     c                   write     tb441z01
012300060201     c                   exfmt     tb441c01
012400021114
012500021114     c                   if        *inkc
012600021114     c                   move      'C'           p01rit
012700021114     c                   leave
012800021114     c                   endif
012900020121
013000011126     c                   if        *inkl
013100021114     c                   move      'L'           p01rit
013200020121     c                   leave
013300020121     c                   endif
013400020121
013500020121     c                   select
013600020121
013700020121      * Rivisualizza
013800020121     c                   when      *inke
013900020121     c                   exsr      Initialize
014000020121
014100051027      * F11 = Cambio ordinamento
014200020121     c                   when      *inkk
014300020121     c                   exsr      Sort
014400020121
014500020121     c                   endsl
014600020121
014700020121      * Controlli
014800020123     c                   exsr      Ctr01
014900020121
015000020121      * Effettuata una scelta
015100021114     c                   if        p01ke1 <> *Blanks
015200021114     c                   Clear                   p01rit
015300011120     c                   leave
015400020121     c                   endif
015500020121
015600020121     c                   enddo
015700020121
015800020121     c                   movel     param01       kpjbu
015900020121
016000020121     c                   eval      *inlr = *on
016100020121
016200020121      *------------------------------------------------------------------*
016300020121     c     Initialize    begsr
016400020121
016500020121     c                   eval      *in40 = *on
016600060201     c                   write     tb441c01
016700020121     c                   eval      *in40 = *off
016800020121     c                   clear                   nrr
016900020121
017000020121      * Carico il subfile in ordine di codice
017100051027     c     kcod          setll     tntbe000
017200020121     c                   do        *hival
017300051027     c     kcod          reade     tntbe000
017400020121
017500020121      * Fine file
017600020121     c                   if        %eof(tntbe01l)
017700020121     c                   leave
017800020121     c                   endif
017900020123
018000020123      * Escludo annullati
018100020123     c                   if        tbeatb <> *blanks
018200020123     c                   iter
018300020123     c                   endif
018400020121
018500060201     c                   movel(p)  tbeke1        vs1abc
018600060201     c                   movel(p)  tbeuni        Dabc
018700060201     c                   movel     d§abcdes      vs1des
018800111027
018900111027     c                   clear                   vs1aut
019000111027     c                   clear                   tibs02ds
019100111027     c                   eval      T02mod = 'C'
019200111027     c                   eval      T02cod = 'RAC'
019300111027     c                   movel     d§abcaut      t02ke1
019400111027     c                   call      'TIBS02R'
019500111027     c                   parm                    kpjba
019600111027     c                   parm                    tibs02ds
019700111027     c                   movel     t02uni        vs1aut
019800111027
019900060209     c                   movel     d§abcord      vs1ord
020000020121     c                   add       1             nrr
020100060201     c                   write     tb441s01
020200020121     c                   enddo
020300020121
020400020121     c                   eval      RrnLast = nrr
020500020121     c                   eval      rcdnbr  = 1
020600020121
020700020121     c                   endsr
020800011120
020900011120      **********************************************************************
021000011120      * Subroutine - Sort
021100011120      *   This subroutine sorts the subfile records.
021200011120     C     Sort          BEGSR
021300051027
021400051027     c                   select
021500051027     c                   when      WrkSort = '0'
021600060201     c                   clear                   V1abc
021700060201     c                   clear                   S1abc
021800051027     c                   when      WrkSort = '1'
021900060209     c                   clear                   V1ord
022000060209     c                   clear                   S1ord
022100051027     c                   endsl
022200011121     C                   EVAL      rcdnbr  = 1
022300060209      *
022400060209sel 1c                   select
022500060209      * Sono in ordine di codice: all'F11 passo all'ordinamento
022600060209w   1c                   when      WrkSort  = '0'
022700060209     c                   eval      WrkSort  = '1'
022800060209      * Sono in ordine di ordinamento: all'F11 passo al codice
022900060209w   1c                   when      WrkSort  = '1'
023000060209     c                   eval      WrkSort  = '0'
023100060209e   1c                   endsl
023200011120
023300011120      * Initialize the key fields to sort on. There is one extra field not in
023400011120      * the subfile -- Selected -- that is added to the record. This field
023500011120      * is used to place selected records in front of nonselected records.
023600011120
023700051027     C                   CLEAR                   QLGKL
023800051027     C                   CLEAR                   QLGSKL
023900011120     C                   CLEAR                   QLGSCB
024000011120     C                   CLEAR                   QLGSCB00
024100011120
024200011120     ** Gestione della scelta ordinamento.
024300011120     C                   SELECT
024400060209     ** Ordinamento per ordinamento
024500051027     c                   WHEN      WrkSort = '1'
024600060201     ** 1 campo chiave
024700060201     c                   EVAL      QLGNBRK    = 1
024800060209     ** Unico campo: Ordinamento
024900060405     **           a posizione 72, 3 byte, zoned, ascending.
025000060405     c                   eval      QLGSP      = 72
025100060210     c**!!!              eval      QLGSS      = %size(V1ord)
025200060210     c                   eval      QLGSS      = 3
025300060209     c                   eval      QLGDT      = Numero
025400051027     c                   eval      QLGSO      = Ascendente
025500051027     c                   eval      QLGKL(1)   = QLGSKL
025600021114
025700011120     ** Ordinamento per Codice
025800051027     C                   WHEN      WrkSort = '0'
025900060209     ** 1 campo chiave
026000011120     C                   EVAL      QLGNBRK    = 1
026100060209     ** Unico campo: Codice tabella
026200060210     **           a posizione 2, 15 byte, char, ascending
026300021114     C                   EVAL      QLGSP      = 2
026400060201     C                   EVAL      QLGSS      = %SIZE(vs1abc)
026500011120     C                   EVAL      QLGDT      = Carattere
026600011120     C                   EVAL      QLGSO      = Ascendente
026700011120     C                   EVAL      QLGKL(1)   = QLGSKL
026800051027
026900011120     C                   ENDSL
027000011120      * Load other sort parameters.
027100011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
027200011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
027300011120     C                   EVAL      QLGRT     = 8
027400011120     C                   EVAL      QLGOKL    = 80
027500011120     C                   EVAL      QLGLKE    = 16
027600011120     C                   EVAL      QLGLSS    = 290
027700011120      * Initialize Sort I/O API fields.
027800011120     C                   EVAL      QLGRL00  = QLGRL
027900011120     C                   EVAL      QLGRC00  = 1
028000011120     C                   CLEAR                   QUSEI
028100011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028200011120      * First step - Initialize the sort routine.
028300011120     C                   CALL      'QLGSORT'
028400011120     C                   PARM                    QLGSCB
028500011120     C                   PARM                    NotUsed
028600011120     C                   PARM                    NotUsed
028700011120     C                   PARM                    SizeList
028800011120     C                   PARM                    ReturnSize
028900011120     C                   PARM                    QUSEC
029000011120      * Next step - Write records to I/O routine.
029100011120     C                   EVAL      QLGRT00 = Put
029200051027     C                   DO        RrnLast       xx
029300060201     C     xx            CHAIN     tb441s01
029400011120     ** Solo le righe con Selected = 'Y' sono riordinate,
029500011120     ** quindi per fare un ordinamento di tutte le righe
029600011120     ** metto 'Y' sempre.
029700011120     C                   EVAL      Selected = 'Y'
029800011120     C                   CLEAR                   QUSEI
029900011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030000011120     C                   CALL      'QLGSRTIO'
030100011120     C                   PARM                    QLGSCB00
030200011120     C                   PARM                    SflRcd
030300011120     C                   PARM                    NotUsed
030400011120     C                   PARM                    SizeList
030500011120     C                   PARM                    NotUsed
030600011120     C                   PARM                    QUSEC
030700011120     C                   ENDDO
030800011120      * Next step - Signal end of input, clear subfile for reload.
030900011120     C                   EVAL      QLGRT00 = EndPut
031000011120     C                   CLEAR                   QUSEI
031100011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
031200011120
031300011120     C                   CALL      'QLGSRTIO'
031400011120     C                   PARM                    QLGSCB00
031500011120     C                   PARM                    SflRcd
031600011120     C                   PARM                    NotUsed
031700011120     C                   PARM                    SizeList
031800011120     C                   PARM                    NotUsed
031900011120     C                   PARM                    QUSEC
032000020121     C                   seton                                        40
032100060201     C                   WRITE     tb441c01
032200011120     C                   setoff                                       40
032300011120      * Final step - Write the records back to the subfile.
032400011120     C                   EVAL      QLGRT00 = Get
032500051027     C                   DO        RrnLast       xx
032600011120     C                   CLEAR                   QUSEI
032700011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
032800011120     C                   CALL      'QLGSRTIO'
032900011120     C                   PARM                    QLGSCB00
033000011120     C                   PARM                    NotUsed
033100011120     C                   PARM                    SflRcd
033200011120     C                   PARM                    QLGRL00
033300011120     C                   PARM                    NotUsed
033400011120     C                   PARM                    QUSEC
033500051027     C                   z-add     xx            nrr
033600060201     C                   WRITE     tb441s01
033700011120     C                   ENDDO
033800011120     C                   ENDSR
033900020121
034000020121      *------------------------------------------------------------------*
034100051027     c     Ctr01         begsr
034200020121
034300020121     c                   eval      *in41 = *off
034400051027
034500051027do  1c                   do        *hival        xx
034600051027
034700060201     c     xx            chain     tb441s01                           01
034800020121     c   01              leave
034900020121
035000020121     c                   if        vs1sce <> ' '
035100060201     c                   movel(p)  vs1abc        p01ke1
035200011120     c                   leave
035300011120     c                   end
035400020121
035500051027      * Controllo se effettuata ricerca
035600051027if  2c                   if            (WrkSort  = '0'
035700060201     c                             and  V1abc   <> *blanks
035800060201     c                             and  V1abc   <> S1abc)
035900051027     c                             OR  (WrkSort  = '1'
036000060209     c                             and (V1ord   <> *Zeros
036100060209     c                             and  V1ord   <> S1ord))
036200020121     c                   eval      *in41 = *on
036300051027if  3c                   if            (WrkSort  = '0'
036400060201     c                             and  V1abc   <= VS1abc)
036500051027     c                             OR  (WrkSort  = '1'
036600060209     c                             and  V1ord   <= VS1ord)
036700020121     c                   eval      *in41 = *off
036800051027     c                   z-add     xx            RcdNbr
036900051027     c                   select
037000051027     c                   when      WrkSort = '0'
037100060201     c                   movel     V1abc         S1abc
037200051027     c                   when      WrkSort = '1'
037300060209     c                   movel     V1ord         S1ord
037400051027     c                   endsl
037500020121     c                   leave
037600051027e   3c                   endif
037700051027e   2c                   endif
037800051027
037900051027e   1c                   enddo
038000051027
038100051027     c                   if        *in41   = *off
038200051027     c                   select
038300051027     c                   when      WrkSort = '0'
038400060201     c                   clear                   V1abc
038500051027     c                   when      WrkSort = '1'
038600060209     c                   clear                   V1ord
038700051027     c                   endsl
038800051027     c                   endif
038900020121
039000020121     c                   endsr
039100020121
039200020121      *------------------------------------------------------------------*
039300020121      *                   ROUTINE INIZIALE
039400020121      *------------------------------------------------------------------*
039500020121
039600020121     c     *inzsr        begsr
039700020121
039800020121     c     *entry        plist
039900021114     c                   Parm                    kpjba
040000051027
040100051027      * Reperimento Dati del job (Utente/Operativi)
040200051027     c     *dtaara       define    §azute        azuteds
040300051027     c     *dtaara       define    §datiute      ddatiute
040400051027     c                   in(E)     *dtaara
040500051027     c                   IF        %ERROR or RSUT = *blanks
040600051027     c                   clear                   Tibs34Ds
040700051027     c                   call      'TIBS34R'
040800051027     c                   parm                    Tibs34Ds
040900051027     c                   in        *dtaara
041000051027     c                   ENDIF
041100020121
041200020121     c                   movel     kpjbu         param01
041300110804
041400110804      /free
041500110804       //?Apro i file tabelle di sede
041600110804         Qcmd = 'OVRDBF FILE(TNTBE01L) TOFILE(';
041700110804         IF  %subst(knsif : 7 : 1) = 'P';
041800110804           Qcmd = %trimr(Qcmd) + 'GAITRAGRPS/TNTBE01L)';
041900110804         ELSE;
042000110804           Qcmd = %trimr(Qcmd) + 'GAITRAGRU/TNTBE01L)';
042100110804         ENDIF;
042200110804         ExecuteCommand ( Qcmd : %size(Qcmd));
042300110804         open  TNTBE01L;
042400110804      /end-free
042500020121
042600020121     c     ktntbe        klist
042700020121     c                   kfld                    kcod
042800020121     c                   kfld                    kke1
042900020121
043000060201     c                   eval      kcod = 'ABC'
043100020121
043200020121     c                   endsr
