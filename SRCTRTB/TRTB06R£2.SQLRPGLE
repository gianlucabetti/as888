000100070828      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200091214     /*PRM closqlcsr(*endmod) dbgview(*source) dynusrprf(*owner)
000300091214     /*END
000400051109      *===============================================================*
000500051109      *?TRTB06R * Interrogazione legami tabelle VAS                  ?*
000600051109      *===============================================================*
000700051109      *
000800051109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900051109     h alwnull(*inputonly)
001000051109      *
001100051109      *?A R C H I V I?------------------------------------------------*
001200051109      *
001300051109     fTABEL00F  if   e           k disk
001400060202     fTNTBE01L  if   e           k disk
001500051109      *
001600051110     fTRTB06D   cf   e             workstn
001700051110     f                                     sfile(TB06S1 : S01nrr)
001800051110     f                                     infds(DSFMT)
001900051109      *
002000051109     fPRTF198   o    f  198        printer usropn
002100051109     f                                     oflind(*inOF)
002200051110      *
002300051110      *?P A R A M E T R I   P E R   O R D I N A M E N T O   S F L?----*
002400051110      *
002500051110      *****************************************************************
002600051110      * Constants
002700051110      *
002800051110      *   MaxKey - Maximum number of key fields allowed by this program
002900051110     d MaxKey          c                   const(2)
003000051122      * Sort order:
003100051122      *  1 = Sort in an ascending sequence
003200051122      *  2 = Sort in a descending sequence
003300051110     d Ascendente      c                   const(1)
003400051122     d Discendente     c                   const(2)
003500051122      * Key data type:
003600051122      *  0 = Signed binary
003700051122      *  2 = Signed zoned decimal
003800051122      *  3 = Signed packed decimal
003900051122      *  6 = Character with no national language sort sequence applied,
004000051122      *      if specified
004100051122      *  7 = Unsigned packed decimal
004200051122      *      All numbers will have the sign forced positive ('F'X).
004300051122      *  8 = Unsigned zoned decimal
004400051122      *      All numbers will have the sign forced positive ('F'X).
004500051122      *  9 = Unsigned binary
004600051122      * 14 = Date in form of DD/MM/YY
004700051122      * 15 = Date in form of DD.MM.YYYY
004800051122     d Numero          c                   const(2)
004900051110     d Carattere       c                   const(6)
005000051110     d Put             c                   const(1)
005100051110     d EndPut          c                   const(2)
005200051110     d Get             c                   const(3)
005300051110      *
005400051110      *****************************************************************
005500051110      * Data Structures
005600051110      *   SflRcd     - The entire subfile record to pass to the sort
005700051110      *   QLGSCB     - The sort request block for the QLGSORT API
005800051110      *   QLGSCB00   - The sort request block for the QLGSRTIO API
005900051110      *   QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
006000051110      *   QUSEC      - Error structure for the QLGSORT API
006100051110      *****************************************************************
006200051110     d SflRcd          ds                  inz
006300051122     d  VH1dke                             inz
006400120807     d  VH1abl                             inz
006500051122     d  VH1du1                             inz
006600051122     d  VH1du2                             inz
006700051122     d  VS1opz                             inz
006800051110     d  VS1cod                             inz
006900051110     d  VS1key                             inz
007000051122     d  VS1dke                             inz
007100051110     d  VS1cu1                             inz
007200051122     d  VS1du1                             inz
007300051110     d  VS1cu2                             inz
007400051122     d  VS1du2                             inz
007500051123     d  VS1dc1                             inz
007600051123     d  VS1dc2                             inz
007700051110     d  Selected                      1A
007800051110      /COPY QSYSINC/QRPGLESRC,QLGSORT
007900051110     d QLGKL                         16    dim(MaxKey)
008000051110     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
008100051110     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
008200051110     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
008300051110     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
008400051110      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008500051110      /COPY QSYSINC/QRPGLESRC,QUSEC
008600051110      *
008700051110      *****************************************************************
008800051110      * Standalone fields
008900051110      *
009000051122      *   Nrr        - Relative record number for subfile
009100051110      *   SizeList   - Size of list
009200051110      *   ReturnSize - Size of list returned by sort API
009300051110      *****************************************************************
009400051110     d NotUsed         s             16a   inz
009500051110     d ReturnSize      s              9b 0 inz
009600051110     d SizeList        s              9b 0 inz
009700051110     d RrnLast         s              5i 0 inz
009800051122     d Nrr             s              5i 0 inz
009900051122      * WrkSort    - Ordinamento impostato a video
010000051122     d WrkSort         s              1    inz(*off)
010100051109      *
010200051109      *?C O S T A N T I?----------------------------------------------*
010300051109      *
010400051110      * - Nr. di righe del sfl (SFLPAG)
010500051110     d C_SflPag        c                   const(16)
010600051110      * - Tabelle elaborate con questo pgm
010700051109     d C_tab1          c                   const('3C')
010800051109     d C_tab2          c                   const('3K')
010900051109     d C_tab3          c                   const('3R')
011000051109     d C_tab4          c                   const('4C')
011100060217     d C_tab5          c                   const('3Q')
011200060217     d C_tab6          c                   const('VAP')
011300060410     d C_tab7          c                   const('DSC')
011400120111     d C_tab8          c                   const('LAC')
011500130129     d C_tab9          c                   const('DKC')
011600130129     d C_tab10         c                   const('OSR')
011700051123     d C_tab1_a        c                   const('Cliente Unificante ')
011800051123     d C_tab2_a        c                   const('Unificante Consegna')
011900051123     d C_tab2_b        c                   const('Unificante Giacenza')
012000051123     d C_tab4_a        c                   const('Unificante Consegna')
012100051123     d C_tab4_b        c                   const('Unificante Fatturaz')
012200060217     d C_tab5_a        c                   const('Cliente Unificante ')
012300060217     d C_tab6_a        c                   const('Cliente di invio   ')
012400060410     d C_tab7_a        c                   const('Cliente Unificante ')
012500120111     d C_tab8_a        c                   const('Cliente Raggruppam.')
012600120111     d C_tab8_b        c                   const('Cliente Forzato    ')
012700120112     d C_tab9_a        c                   const('Cliente 1          ')
012800120112     d C_tab9_b        c                   const('Cliente 2          ')
012900060202      * - Costanti per l'SQL
013000060202     d C_Union         c                   const('UNION')
013100051109     d C_OrderBy       c                   const('ORDER BY +
013200060202     d                                            TABcod, +
013300060202     d                                            TABke1, +
013400060202     d                                            TABdat')
013500051109     d C_ForFetch      c                   const('FOR FETCH ONLY')
013600051109      *
013700051109      *?S C H I E R E?------------------------------------------------*
013800051109      *
013900051110     d $Msg            s             78    dim( 4) ctdata perrcd(1)
014000060202     d $Sql_TABEL      s             78    dim( 6) ctdata perrcd(1)
014100060410     d $Sql_TNTBE      s             78    dim( 8) ctdata perrcd(1)
014200051109     d $Cmd            s                   like(Qcmd)
014300060217     d                                     dim( 2) ctdata perrcd(1)
014400051110     d $Txt            s             66    dim( 9) ctdata perrcd(1)
014500051109      *
014600051109      *?S T R U T T U R E   D A T I?----------------------------------*
014700051109      *
014800051109      * - Parametri
014900051109     d KPJBA         e ds
015000100211     d TRTB06ds      e ds                  inz
015100051109      *
015200051109      * - Parametri x Controllo profilo utenti
015300051109     d TIBS34ds      e ds                  inz
015400051109      * - Ds di riferimento al file esterno AZUTE00F
015500051109     d AZUTEds       e ds                  extname(AZUTE00F)
015600051109      * - Ds per dati organigramma
015700051109     d DDatiUte      e ds
015800051109      *
015900051109     d DS            e ds                  inz
016000051109     d ds3C          e ds                  inz
016100051109     d ds3K          e ds                  inz
016200060217     d ds3Q          e ds                  inz
016300051109     d ds3R          e ds                  inz
016400051109     d ds4C          e ds                  inz
016500130129     d*dDKC*****     e ds                  inz
016600060410     d dDSC          e ds                  inz
016700120111     d dLAC          e ds                  inz
016800130129     d*dVAP*****     e ds                  inz
016900120112     d dOSR          e ds                  inz
017000051109      *
017100051109     d TIBS69ds      e ds                  inz
017200051109     d DS_cnaco      e ds                  inz  extname(CNACO00F)
017300051109     d DS_cnind      e ds                  inz  extname(CNIND00F)
017400051109     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
017500051109     d DS_fncls      e ds                  inz  extname(FNCLS00F)
017600120111      *
017700120111      * - Parametri x manutenzione tab. "LAC"
017800120111     d TNTB46ds      e ds                  inz
017900051109      *
018000051109     d Status         sds
018100130129     d   SDSpgm          *proc
018200130129     d   SDSusr              254    263
018300051110      *
018400051110     d DSFMT           ds           512
018500130129     d   £Tasto              369    369
018600130129     d   £SFLnrr             378    379B 0
018700051110      *
018800051117     d ds_TABEL        ds                  inz
018900130129     d   wSQLcod                           inz like(TBEcod)
019000130129     d   wSQLkey                           inz like(TBEke1)
019100130129     d   wSQLuni                           inz like(TBEuni)
019200120112      *
019300120112     d ds_Comodo       ds          6000    inz
019400120112     d   sk_Comodo                   60    inz  dim(100)
019500051109      *
019600051109      *?V A R I A B I L I?--------------------------------------------*
019700051109      *
019800051109      * - Flags booleani
019900120112     d $EoF            s               n   inz(*off)
020000120112     d $Err            s               n   inz(*off)
020100120112     d $Fine           s               n   inz(*off)
020200120112     d $InzD01         s               n   inz(*on)
020300120112     d $InzS01         s               n   inz(*on)
020400120112     d WinKG           s               n   inz(*off)
020500120112     d $Tabel          s               n   inz(*off)
020600120112     d $Tntbe1         s               n   inz(*off)
020700120112     d $Tntbe2         s               n   inz(*off)
020800120112     d $Tntbe3         s               n   inz(*off)
020900091214     d $Called         s               n   inz
021000051109      *
021100051109     d $Video          s              1    inz('1')
021200051109      * - Indici di schiera / Contatori
021300051109     d xx              s              3  0 inz
021400051109     d yy              s              3  0 inz
021500051109      * - Parametri per SQL
021600120112     d wSQL            s           5000    inz  varying
021700051215     d wAndOr          s              5    inz('AND (')
021800051109      * - Parametri per QCMDEXC
021900051109     d Qcmd            s             80    inz
022000051109     d Qlen            s             15  5 inz(80)
022100051109      * - Variabili riferite al data base o al display file
022200051122     d S01nrr          s                   like(C01rcd) inz
022300051110     d Wksc            s                   like(I69kac) inz
022400051109      * - Campi di comodo
022500051123     d WkscA           s              7    inz(*zeros)
022600051109     d Data_Iso        s               d   inz  datfmt(*iso)
022700051110     d Data_Eur        s               d   inz  datfmt(*eur)
022800051110     d wDate           s              8  0 inz
022900051110     d wTime           s              6  0 inz
023000051110     d wPag            s              4  0 inz
023100051110     d wRig            s              3  0 inz
023200051110      *
023300051110      *?K E Y - L I S T?----------------------------------------------*
023400051110      *
023500051110     c     K03TABEL      klist
023600051110     c                   kfld                    TBLkut
023700051110     c                   kfld                    TBLcod
023800051110     c                   kfld                    TBLkey
023900060202      *
024000060202     c     K05TBE01      klist
024100060202     c                   kfld                    TBEcod
024200060202     c                   kfld                    TBEke1
024300060202     c                   kfld                    TBEke2
024400060202     c                   kfld                    TBElin
024500060202     c                   kfld                    TBEsif
024600060202     c     K02TNTBE      klist
024700060202     c                   kfld                    TBEcod
024800060202     c                   kfld                    TBEke1
024900051109      *
025000051109      *===============================================================*
025100051109      *                                                               *
025200051109      *?I N D I C A T O R I?------------------------------------------*
025300051109      *                                                               *
025400051109      *  28     - Emette il messaggio di errore a video               *
025500051109      *  30     - Ripulisce il subfile S1                             *
025600051109      *  31     - NON emette il subfile S1                            *
025700051109      *  32     - Record modificato nel subfile S1  (sflnxtchg)       *
025800051109      *  33     - Fine dati nel subfile S1          (sflend)          *
025900051110      *  39     - subfile S1 senza records                            *
026000051110      *  40     - condiziona ordinamento per codice tabella/cliente   *
026100100216      *  41     - condiziona gestione opzioni                         *
026200120807      *  42     - evidenzia a video S1 cliente BLOCCATO               *
026300100216      *  45     - abilita ricerca cliente per ragione sociale         *
026400051109      *  50     - fmt D1: Codice cliente errato                       *
026500051109      *           fmt S1: Opzione errata                              *
026600051109      *  51     - fmt D1: Selezione 1ª tabella errata                 *
026700051109      * (52     - fmt D1: Selezione 2ª tabella errata)                *
026800051109      * (53     - fmt D1: Selezione 3ª tabella errata)                *
026900051109      * (54     - fmt D1: Selezione 4ª tabella errata)                *
027000130129      * (55     - fmt D1: Selezione 5ª tabella errata)                *
027100130129      * (56     - fmt D1: Selezione 6ª tabella errata)                *
027200130129      * (57     - fmt D1: Selezione 7ª tabella errata)                *
027300130129      * (58     - fmt D1: Selezione 8ª tabella errata)                *
027400130129      * (59     - fmt D1: Selezione 9ª tabella errata)                *
027500130129      * (60     - fmt D1: Selezione 10ª tabella errata)               *
027600051109      *                                                               *
027700051109      *===============================================================*
027800051109      *
027900051109     c     *Entry        plist
028000051109     c                   parm                    KPJBA
028100140115     C
028200140115     C/EXEC SQL
028300140115     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
028400140115     C/END-EXEC
028500140115     C
028600051109      *
028700051109      * Operazioni iniziali
028800051110     c                   exsr      OpeIni
028900051109      *
029000051109      * Gestione Video
029100051109do  1c                   DOW       $Fine   =  *off
029200051109      * - Filtro di lancio
029300051109cas 2c     $Video        caseq     '1'           GesD01
029400051109      * - SubFile
029500051109cas 2c     $Video        caseq     '2'           GesS01
029600051109e   2c                   endcs
029700051109e   1c                   ENDDO
029800051109      *
029900051110      * Fine programma
030000051110     c                   eval      *inLR   =  *on
030100051109      *
030200051109      *
030300051109      *
030400051109      *?S U B R O U T I N E S?----------------------------------------*
030500051109      *
030600051109      *---------------------------------------------------------------*
030700051109      *?Operazioni iniziali                                          ?*
030800051109      *---------------------------------------------------------------*
030900051110     c     OpeIni        BEGSR
031000051109      *
031100051109      * Reperisce dati job
031200051109     c     *dtaara       define    §azute        azuteds
031300051109     c     *dtaara       define    §datiute      ddatiute
031400051109      *
031500051109     c                   in(E)     *dtaara
031600051109     c                   IF        %ERROR or RSUT = *blanks
031700051109     c                   clear                   Tibs34Ds
031800051109     c                   call      'TIBS34R'
031900051109     c                   parm                    Tibs34Ds
032000051109     c                   in        *dtaara
032100051109     c                   ENDIF
032200051110      *
032300060217     c                   movel     SDSpgm        T1pgm
032400091214      *
032500091214      /free
032600091214         // -?Verifica se chiamato (con codice cliente come parametro);?
032700091214         //  ?se chiamato: salta la 1ª videata?
032800100211         if  %subst( KPJBU : 1 : 7 ) > *zero;
032900091214           $Called = *on;
033000091214           $InzD01 = *off;
033100091214           exsr  InzD01;
033200100211           //V1Cksc = %int( %subst( KPJBU : 1 : %len(V1Cksc) ) );
033300100211           TRTB06ds = kpjbu;
033400100211           V1Cksc = D06ksc;
033500100211           if  D063C  <> *blank   or
033600100211               D063K  <> *blank   or
033700100211               D063R  <> *blank   or
033800100211               D064C  <> *blank   or
033900100211               D063Q  <> *blank   or
034000100211               D06VAP <> *blank   or
034100120111               D06DSC <> *blank   or
034200120112               D06LAC <> *blank   or
034300130129               D06DKC <> *blank   or
034400120112               D06OSR <> *blank;
034500100211             V1Cse1 = D063C;
034600100211             V1Cse2 = D063K;
034700100211             V1Cse3 = D063R;
034800100211             V1Cse4 = D064C;
034900100211             V1Cse5 = D063Q;
035000100211             V1Cse6 = D06VAP;
035100100211             V1Cse7 = D06DSC;
035200120111             V1Cse8 = D06LAC;
035300130129             V1Cse9 = D06DKC;
035400130129             V1Cse10 = D06OSR;
035500100211           endif;
035600091214           exsr  CtrD01;
035700091214           if  *in90;
035800091214             write  TB06T1;
035900130129           //else;
036000130129           //  $Video = '2';
036100091214           endif;
036200130129           *in51 = *on;
036300091214         endif;
036400091214      /end-free
036500051109      *
036600051109     c                   ENDSR
036700051109      *
036800051109      *---------------------------------------------------------------*
036900051110      *?Gestione videata D01                                         ?*
037000051109      *---------------------------------------------------------------*
037100051109     c     GesD01        BEGSR
037200051109      *
037300051109      * Inizializzo la videata
037400051109if  1c                   if        $InzD01 =  *on
037500051109     c                   exsr      InzD01
037600051110     c                   eval      $InzD01 =  *off
037700051109e   1c                   endif
037800051109      *
037900051109      * Scrivo la testata e la riga tasti funzionali abilitati
038000051109if  1c                   if        NOT *in90
038100051109     c                   write     TB06T1
038200051109e   1c                   endif
038300051109      * Emetto la videata
038400051109     c                   exfmt     TB06D1
038500051109     c                   setoff                                       28  90
038600051109     c                   clear                   V1Dmsg
038700051109      *
038800051109sel 1c                   select
038900051109      * F3=Fine
039000051109w   1c                   when      *inKC
039100051109     c                   exsr      F03D01
039200051109     c                   leavesr
039300051123      * F7=Ricerca cliente
039400051123w   1c                   when          *inKG
039500051123     c                             and *in45  =  *off
039600051123     c                   eval      *in45   =  *on
039700051123     c                   eval      WinKG   =  *on
039800051123     c                   leavesr
039900051123      * Ricerca alfabetica se:
040000051123      *   · è stato premuto F7
040100051123w   1c                   when          *in45  =  *on
040200051123     c                             and WinKG  =  *on
040300051123      *   · il codice cliente è vuoto
040400051123     c                             and V1Cksc =  *zeros
040500051123      *   · la ragione sociale è piena
040600051123     c                             and V1Dksc <> *blanks
040700051123     c                   exsr      SearchCli
040800051124     c                   leavesr
040900051109      *
041000051109e   1c                   endsl
041100051109      *
041200051109      * Controllo dati immessi a video
041300051109     c                   exsr      CtrD01
041400051109     c                   if        *in90
041500051109     c                   leavesr
041600051109     c                   endif
041700051109      *
041800051109      * Passaggio al subfile
041900051109     c                   eval      $InzS01 =  *on
042000051109     c                   eval      $Video  =  '2'
042100051109      *
042200051109     c                   ENDSR
042300051109      *
042400051109      *---------------------------------------------------------------*
042500051110      *?Inizializzazione videata D01                                 ?*
042600051109      *---------------------------------------------------------------*
042700051109     c     InzD01        BEGSR
042800051109      *
042900051109     c                   clear                   TB06D1
043000051109      *
043100051109      * Imposto i dati di default
043200051123     c                   eval      *in45   =  *off
043300051109     c                   eval      V1Ctb1  =  C_tab1
043400051109     c                   eval      V1Ctb2  =  C_tab2
043500051109     c                   eval      V1Ctb3  =  C_tab3
043600051109     c                   eval      V1Ctb4  =  C_tab4
043700060202     c                   eval      V1Ctb5  =  C_tab5
043800060217     c                   eval      V1Ctb6  =  C_tab6
043900060410     c                   eval      V1Ctb7  =  C_tab7
044000120111     c                   eval      V1Ctb8  =  C_tab8
044100120112     c                   eval      V1Ctb9  =  C_tab9
044200130129     c                   eval      V1Ctb10 =  C_tab10
044300051109     c                   eval      V1Cse1  =  'S'
044400051109     c                   eval      V1Cse2  =  'S'
044500051109     c                   eval      V1Cse3  =  'S'
044600051109     c                   eval      V1Cse4  =  'S'
044700060202     c                   eval      V1Cse5  =  'S'
044800060217     c                   eval      V1Cse6  =  'S'
044900060410     c                   eval      V1Cse7  =  'S'
045000120111     c                   eval      V1Cse8  =  'S'
045100120112     c                   eval      V1Cse9  =  'S'
045200130129     c                   eval      V1Cse10 =  'S'
045300051109      *
045400051109      * Decodifica tabelle
045500051109     c                   eval      TBLkut  =  1
045600051109     c                   clear                   TBLcod
045700051109     c                   eval      TBLkey  =  *zeros
045800060202     c                   clear                   TBEcod
045900060202     c                   movel     *zeros        TBEke1
046000060202     c                   clear                   TBEke2
046100060202     c                   clear                   TBElin
046200060202     c                   movel     KNSIF         TBEsif
046300051109      * - 1ª tabella ('3C')
046400051109if  1c                   if        V1Ctb1  <> *blanks
046500060202     c***                move      V1Ctb1        TBLkey
046600060202     c                   eval      %subst(TBLkey:7:2) =
046700060202     c                             %subst(V1Ctb1:1:2)
046800051109     c     K03TABEL      chain     TABEL
046900130808     c                   if        %found(TABEL00F) and
047000130808     c                             TBLflg  =  *blanks
047100051109     c                   movel     TBLuni        DS
047200060202     c*** già impostato: move      TBLkey        V1Ctb1
047300051109     c                   movel     §des1         V1Dtb1
047400051109     c                   movel     §pgm          V1Hpg1
047500051109     c                   endif
047600051109e   1c                   endif
047700051109      * - 2ª tabella ('3K')
047800051123if  1c                   if        V1Ctb2  <> *blanks
047900060202     c***                move      V1Ctb2        TBLkey
048000060202     c                   eval      %subst(TBLkey:7:2) =
048100060202     c                             %subst(V1Ctb2:1:2)
048200051109     c     K03TABEL      chain     TABEL
048300130808     c                   if        %found(TABEL00F) and
048400130808     c                             TBLflg  =  *blanks
048500051109     c                   movel     TBLuni        DS
048600060202     c*** già impostato: move      TBLkey        V1Ctb2
048700051109     c                   movel     §des1         V1Dtb2
048800051109     c                   movel     §pgm          V1Hpg2
048900051109     c                   endif
049000051123e   1c                   endif
049100051109      * - 3ª tabella ('3R')
049200051123if  1c                   if        V1Ctb3  <> *blanks
049300060202     c***                move      V1Ctb3        TBLkey
049400060202     c                   eval      %subst(TBLkey:7:2) =
049500060202     c                             %subst(V1Ctb3:1:2)
049600051109     c     K03TABEL      chain     TABEL
049700130808     c                   if        %found(TABEL00F) and
049800130808     c                             TBLflg  =  *blanks
049900051109     c                   movel     TBLuni        DS
050000060202     c*** già impostato: move      TBLkey        V1Ctb3
050100051109     c                   movel     §des1         V1Dtb3
050200051109     c                   movel     §pgm          V1Hpg3
050300051109     c                   endif
050400051123e   1c                   endif
050500051109      * - 4ª tabella ('4C')
050600051123if  1c                   if        V1Ctb4  <> *blanks
050700060202     c***                move      V1Ctb4        TBLkey
050800060202     c                   eval      %subst(TBLkey:7:2) =
050900060202     c                             %subst(V1Ctb4:1:2)
051000051109     c     K03TABEL      chain     TABEL
051100130808     c                   if        %found(TABEL00F) and
051200130808     c                             TBLflg  =  *blanks
051300051109     c                   movel     TBLuni        DS
051400060202     c*** già impostato: move      TBLkey        V1Ctb4
051500051109     c                   movel     §des1         V1Dtb4
051600051109     c                   movel     §pgm          V1Hpg4
051700051109     c                   endif
051800051123e   1c                   endif
051900060217      * - 5ª tabella ('3Q')
052000060217if  1c                   if        V1Ctb5  <> *blanks
052100060217     c***                move      V1Ctb5        TBLkey
052200060217     c                   eval      %subst(TBLkey:7:2) =
052300060217     c                             %subst(V1Ctb5:1:2)
052400060217     c     K03TABEL      chain     TABEL
052500130808     c                   if        %found(TABEL00F) and
052600130808     c                             TBLflg  =  *blanks
052700060217     c                   movel     TBLuni        DS
052800060217     c*** già impostato: move      TBLkey        V1Ctb5
052900060217     c                   movel     §des1         V1Dtb5
053000060217     c                   movel     §pgm          V1Hpg5
053100060217     c                   endif
053200060217e   1c                   endif
053300060217      * - 6ª tabella ('VAP')
053400060217if  1c                   if        V1Ctb6  <> *blanks
053500060217     c                   move      V1Ctb6        TBEke1
053600060202     c                   movel     KNSIF         TBEsif
053700060202     c     K05TBE01      chain     TNTBE000
053800060202     c                   if        NOT %found(TNTBE01L)
053900060202     c                   clear                   TBEsif
054000060202     c     K05TBE01      chain     TNTBE000
054100060202     c                   endif
054200130808     c                   if        %found(TNTBE01L) and
054300130808     c                             TBLflg  =  *blanks
054400060217     c                   move      TBEke1        V1Ctb6
054500060217     c                   movel     TBEuni        V1Dtb6
054600060217     c                   move      TBEuni        V1Hpg6
054700060202     c                   endif
054800060202e   1c                   endif
054900060410      * - 7ª tabella ('DSC')
055000060410if  1c                   if        V1Ctb7  <> *blanks
055100060410     c                   move      V1Ctb7        TBEke1
055200060410     c                   movel     KNSIF         TBEsif
055300060410     c     K05TBE01      chain     TNTBE000
055400060410     c                   if        NOT %found(TNTBE01L)
055500060410     c                   clear                   TBEsif
055600060410     c     K05TBE01      chain     TNTBE000
055700060410     c                   endif
055800130808     c                   if        %found(TNTBE01L) and
055900130808     c                             TBLflg  =  *blanks
056000060410     c                   move      TBEke1        V1Ctb7
056100060410     c                   movel     TBEuni        V1Dtb7
056200060410     c                   move      TBEuni        V1Hpg7
056300060410     c                   endif
056400060410e   1c                   endif
056500120111      * - 8ª tabella ('LAC')
056600120111if  1c                   if        V1Ctb8  <> *blanks
056700120111     c                   move      V1Ctb8        TBEke1
056800120111     c                   movel     KNSIF         TBEsif
056900120111     c     K05TBE01      chain     TNTBE000
057000120111     c                   if        NOT %found(TNTBE01L)
057100120111     c                   clear                   TBEsif
057200120111     c     K05TBE01      chain     TNTBE000
057300120111     c                   endif
057400130808     c                   if        %found(TNTBE01L) and
057500130808     c                             TBLflg  =  *blanks
057600120111     c                   move      TBEke1        V1Ctb8
057700120111     c                   movel     TBEuni        V1Dtb8
057800120111     c                   move      TBEuni        V1Hpg8
057900120111     c                   endif
058000120111e   1c                   endif
058100130129      * - 9ª tabella ('DKC')
058200120112if  1c                   if        V1Ctb9  <> *blanks
058300120112     c                   move      V1Ctb9        TBEke1
058400120112     c                   movel     KNSIF         TBEsif
058500120112     c     K05TBE01      chain     TNTBE000
058600120112     c                   if        NOT %found(TNTBE01L)
058700120112     c                   clear                   TBEsif
058800120112     c     K05TBE01      chain     TNTBE000
058900120112     c                   endif
059000130808     c                   if        %found(TNTBE01L) and
059100130808     c                             TBLflg  =  *blanks
059200120112     c                   move      TBEke1        V1Ctb9
059300120112     c                   movel     TBEuni        V1Dtb9
059400120112     c                   move      TBEuni        V1Hpg9
059500120112     c                   endif
059600120112e   1c                   endif
059700130129      * - 10ª tabella ('OSR')
059800130129if  1c                   if        V1Ctb10 <> *blanks
059900130129     c                   move      V1Ctb10       TBEke1
060000130129     c                   movel     KNSIF         TBEsif
060100130129     c     K05TBE01      chain     TNTBE000
060200130129     c                   if        NOT %found(TNTBE01L)
060300130129     c                   clear                   TBEsif
060400130129     c     K05TBE01      chain     TNTBE000
060500130129     c                   endif
060600130808     c                   if        %found(TNTBE01L) and
060700130808     c                             TBLflg  =  *blanks
060800130129     c                   move      TBEke1        V1Ctb10
060900130129     c                   movel     TBEuni        V1Dtb10
061000130129     c                   move      TBEuni        V1Hpg10
061100130129     c                   endif
061200130129e   1c                   endif
061300051109      *
061400051109     c                   ENDSR
061500051109      *
061600051109      *---------------------------------------------------------------*
061700051110      *?Gestione tasto funzionale F03 da videata D01                 ?*
061800051109      *---------------------------------------------------------------*
061900051109     c     F03D01        BEGSR
062000051109      *
062100051109      * Chiudo il programma
062200051109     c                   eval      $Fine   =  *on
062300051109      *
062400051109     c                   ENDSR
062500051109      *
062600051109      *---------------------------------------------------------------*
062700051110      *?Controllo dati immessi in videata D01                        ?*
062800051109      *---------------------------------------------------------------*
062900051109     c     CtrD01        BEGSR
063000051109      *
063100051109     c                   movea     *zeros        *in(50)
063200051109      *
063300051109      * Controllo/Decodifico codice cliente
063400051109     c                   clear                   V1Dksc
063500120807     c                   clear                   V1Dabl
063600051109if  1c                   IF        V1Cksc  =  *zeros
063700051109     c                   seton                                        285090
063800051109     c                   eval      V1Dmsg  =  $Msg(1)
063900051109x   1c                   ELSE
064000051110     c                   move      V1Cksc        Wksc
064100051110     c                   exsr      Decod_Cli
064200051109if  2c                   if        O69err  =  *on
064300051109     c                   seton                                        285090
064400051109     c                   eval      V1Dmsg  =  $Msg(2)
064500051109x   2c                   else
064600051109     c                   movel     ACOrag        V1Dksc
064700130315     c                   if        ACOabl <> *blanks
064800120807     c                   eval      V1Dabl = 'BLOCCATO'
064900120807     c                   endif
065000051109e   2c                   endif
065100051109e   1c                   ENDIF
065200051109     c                   if        *in90
065300051109     c                   leavesr
065400051109     c                   endif
065500051109      *
065600051109      * Controllo selezioni
065700051109     c                   if            V1Cse1 = *blanks
065800051109     c                             and V1Cse2 = *blanks
065900051109     c                             and V1Cse3 = *blanks
066000051109     c                             and V1Cse4 = *blanks
066100060202     c                             and V1Cse5 = *blanks
066200060217     c                             and V1Cse6 = *blanks
066300060410     c                             and V1Cse7 = *blanks
066400120111     c                             and V1Cse8 = *blanks
066500120112     c                             and V1Cse9 = *blanks
066600130129     c                             and V1Cse10 = *blanks
066700051109     c                   seton                                        285190
066800051109     c                   eval      V1Dmsg  =  $Msg(3)
066900051109     c                   leavesr
067000051109     c                   endif
067100060202     c                   if            V1Cse1 = *blanks
067200060202     c                             and V1Cse2 = *blanks
067300060202     c                             and V1Cse3 = *blanks
067400060202     c                             and V1Cse4 = *blanks
067500060217     c                             and V1Cse5 = *blanks
067600060202     c                   eval      $Tabel  =  *off
067700060202     c                   else
067800060202     c                   eval      $Tabel  =  *on
067900060202     c                   endif
068000060217     c                   if            V1Cse6 = *blanks
068100060410     c                   eval      $Tntbe2 =  *off
068200060202     c                   else
068300060410     c                   eval      $Tntbe2 =  *on
068400060202     c                   endif
068500060410     c                   if            V1Cse7 = *blanks
068600120111     c                             and V1Cse8 = *blanks
068700130129     c                             and V1Cse9 = *blanks
068800060410     c                   eval      $Tntbe1 =  *off
068900060410     c                   else
069000060410     c                   eval      $Tntbe1 =  *on
069100060410     c                   endif
069200130129     c                   if            V1Cse10 = *blanks
069300120112     c                   eval      $Tntbe3 =  *off
069400120112     c                   else
069500120112     c                   eval      $Tntbe3 =  *on
069600120112     c                   endif
069700051109      *
069800051109     c                   ENDSR
069900051123      *
070000051123      *---------------------------------------------------------------*
070100051123      *?Ricerca cliente                                              ?*
070200051123      *---------------------------------------------------------------*
070300051123     c     SearchCli     BEGSR
070400051123      *
070500051123     c                   call      'XALFA3BR'
070600051123     c                   parm      RSUT          PAXdut           30
070700051123     c                   parm                    TBLkut
070800051123     c                   parm      V1Dksc        PAXdmt           48
070900051123     c                   parm      DUTkci        PAXccc            4 0
071000051123     c                   parm      9             PAXsta            1 0
071100051123     c                   parm                    PAXflr           90
071200051123     c                   parm      *blanks       PAXdit            3
071300051123     c                   parm      1             PAXnum            2 0
071400051123     c                   parm                    PAXksc           80
071500051123     c                   parm                    PAXksm          140
071600051123     c                   parm                    PAXkdm           60
071700051123      *
071800051123      *   · Imposto il codice selezionato a video
071900051123      *     (se selezionato e non premuto F12)
072000051123if  1c                   if        PAXsta >= *zeros
072100051123     c                   movel     PAXksm        V1Cksc
072200051123     c                   movel     PAXdmt        V1Dksc
072300051123     c                   eval      *in45   =  *off
072400051123     c                   reset                   WinKG
072500051123e   1c                   endif
072600051123      *
072700051123     c                   ENDSR
072800051109      *
072900051109      *---------------------------------------------------------------*
073000051110      *?Gestione videata C01 / S01                                   ?*
073100051109      *---------------------------------------------------------------*
073200051109     c     GesS01        BEGSR
073300051109      *
073400051109      * Inizializzo la videata
073500051109if  1c                   if        $InzS01 =  *on
073600051109     c                   exsr      InzS01
073700051110     c                   eval      $InzS01 = *off
073800051109e   1c                   endif
073900100216      *
074000100216      * Impostazione attributi di visualizzazione
074100130201     c                   eval      *in41 = ($Called = *on  and
074200130201     c                                      D06op0  = *blank)
074300051110      *
074400051110      * Posiziono il cursore
074500051110if  1c                   if        C01csr  >  *zeros
074600051110     c                   z-add     C01csr        C01rcd
074700051110e   1c                   endif
074800051109      *
074900051109      * Scrivo la testata e la riga tasti funzionali abilitati
075000051110     c                   eval      *in39   =  (C01rcd <= *zeros)
075100051109if  1c                   if        NOT *in90
075200051109     c                   write     TB06T1
075300051110     c                   write     TB06Z1
075400051109e   1c                   endif
075500051110      *
075600051110      * Visualizzo (eventualmente) il msg per la mancanza di dati
075700051110if  1c                   if        C01rcd  <= *zeros
075800051110     c                   write     TB06D2
075900051110e   1c                   endif
076000051109      * Emetto la videata
076100051110     c                   exfmt     TB06C1
076200051110     c                   z-add     £SFLnrr       C01rcd
076300051109     c                   setoff                                       28  90
076400051109     c                   clear                   V1Dmsg
076500051109      *
076600051110sel 1c                   SELECT
076700051109      * F3=Fine
076800051110w   1c                   WHEN      *inKC
076900051109     c                   exsr      F03D01
077000051110      * F8=Stampa elenco legami
077100051110w   1c                   WHEN      *inKH
077200051110     c                   exsr      F08S01
077300051122      * F11=Ordinamento x codice cliente / codice tabella
077400051122w   1c                   WHEN      *inKK
077500051122     c                   exsr      F11S01
077600051110      * F12=Ritorno
077700051110w   1c                   WHEN      *inKL
077800051110     c                   exsr      F12S01
077900051110      * NO rec.
078000051110w   1c                   WHEN      *in39
078100051110      * Gestione opzioni inserite
078200051110x   1c                   OTHER
078300051110     c                   exsr      OpzS01
078400051110      *
078500051110e   1c                   ENDSL
078600051109      *
078700051109     c                   ENDSR
078800051109      *
078900051109      *---------------------------------------------------------------*
079000051110      *?Inizializzazione videata C01 / S01                           ?*
079100051109      *---------------------------------------------------------------*
079200051110     c     InzS01        BEGSR
079300051110      *
079400051110      * Pulizia subfile
079500051110     c                   seton                                        3031
079600051110     c                   write     TB06C1
079700051110     c                   setoff                                         3133
079800051110      *
079900051110     c                   reset                   $EoF
080000120807     c                   reset                   WrkSort
080100051110     c                   clear                   C01rcd
080200051110     c                   clear                   C01csr
080300051110     c                   clear                   S01nrr
080400051110     c                   clear                   V1Dmsg
080500051110     c                   setoff                                       28  90
080600051110     c                   eval      *in32   =  *off
080700051122     c                   eval      *in40   =  (WrkSort = *on)
080800051110     c                   movea     *zeros        *in(50)
080900051110      *
081000051110      * Caricamento dei dati da presentare nel sfl (NON a pagine)
081100051110      *
081200051110      * - Preparazione stringa SQL
081300060202     c                   clear                   wSQL
081400060202      *   - da TABEL00F
081500060202     c                   if        $Tabel  =  *on
081600060202     c                   exsr      PrepSQL1
081700060202     c                   endif
081800060202      *   - da TNTBE00F
081900060410     c                   if            $Tntbe1 =  *on
082000060410     c                             or  $Tntbe2 =  *on
082100120112     c                             or  $Tntbe3 =  *on
082200060202     c                   exsr      PrepSQL2
082300060202     c                   endif
082400060202      *   - impostazioni finali
082500060202     c                   exsr      PrepSQL3
082600051110      *
082700051110      * - Apertura cursore
082800051110     c                   exsr      OpenCursor
082900051110      *
083000051110      * - Esecuzione SQL
083100051110     c                   dou       $Eof    =  *on
083200051110     c                   exsr      ReadCursor
083300051110     c                   enddo
083400051110      *
083500051110      * Chiusura cursore
083600051110     c                   exsr      CloseCursor
083700051110      *
083800051110      * Memorizzo NRR dell'ultimo record scritto nel sfl
083900051110     c                   eval      RRNlast =  S01nrr
084000051110      *
084100051110      * Avendo caricato TUTTI i dati (NON SOLO quelli della 1ª pagina)
084200051110      *  occorre impostare il posizionamento cursore sul 1° rec del sfl
084300051110if  1c                   if        C01csr  >  *zeros
084400051110     c                   eval      C01csr  =  1
084500051110e   1c                   endif
084600051109      *
084700051109     c                   ENDSR
084800051109      *
084900051109      *---------------------------------------------------------------*
085000060202      *?Preparazione stringa SQL per file TABEL00F                   ?*
085100051109      *---------------------------------------------------------------*
085200060202     c     PrepSQL1      BEGSR
085300051109      *
085400051109     c                   movel(p)  V1Cksc        TBLkey
085500051110     c                   reset                   wAndOr
085600051123      *
085700051123      * Reperisco eventuali codici clienti unificanti da tab. 3C
085800051123     c                   clear                   ds3C
085900060410if  1c                   if        V1Cse1  =  'S'
086000060410     c                   eval      TBLcod  = '3C'
086100051123     c     K03TABEL      chain     TABEL
086200051123     c                   if            %found(TABEL00F)
086300051123     c                             and TBLflg = *blanks
086400051123     c                   movel     TBLuni        ds3C
086500051123     c                   endif
086600060410e   1c                   endif
086700051123      * Reperisco eventuali codici clienti unificanti da tab. 3K
086800051123     c                   clear                   ds3K
086900060410if  1c                   if        V1Cse2  =  'S'
087000060410     c                   eval      TBLcod  = '3K'
087100051123     c     K03TABEL      chain     TABEL
087200051123     c                   if            %found(TABEL00F)
087300051123     c                             and TBLflg = *blanks
087400051123     c                   movel     TBLuni        ds3K
087500051123     c                   endif
087600060410e   1c                   endif
087700060217      * Reperisco eventuali codici clienti unificanti da tab. 3Q
087800060217     c                   clear                   ds3Q
087900060410if  1c                   if        V1Cse5  =  'S'
088000060410     c                   eval      TBLcod  = '3Q'
088100060217     c     K03TABEL      chain     TABEL
088200060217     c                   if            %found(TABEL00F)
088300060217     c                             and TBLflg = *blanks
088400060217     c                   movel     TBLuni        ds3Q
088500060217     c                   endif
088600060410e   1c                   endif
088700051123      * Reperisco eventuali codici clienti unificanti da tab. 3R
088800051123     c                   clear                   ds3R
088900060410if  1c                   if        V1Cse3  =  'S'
089000060410     c                   eval      TBLcod  = '3R'
089100051123     c     K03TABEL      chain     TABEL
089200051123     c                   if            %found(TABEL00F)
089300051123     c                             and TBLflg = *blanks
089400051123     c                   movel     TBLuni        ds3R
089500051123     c                   endif
089600060410e   1c                   endif
089700051123      * Reperisco eventuali codici clienti unificanti da tab. 4C
089800051123     c                   clear                   ds4C
089900060410if  1c                   if        V1Cse4  =  'S'
090000060410     c                   eval      TBLcod  = '4C'
090100051123     c     K03TABEL      chain     TABEL
090200051123     c                   if            %found(TABEL00F)
090300051123     c                             and TBLflg = *blanks
090400051123     c                   movel     TBLuni        ds4C
090500051123     c                   endif
090600060410e   1c                   endif
090700051109      *
090800051109      *? E S E M P I O : ?
090900051109      *
091000051109      * select TBLcod, TBLkey, TBLuni
091100051109      * from   TABEL00F
091200051109      * where  TBLflg   =  ' '
091300051215      *   AND ((TBLcod  =  '3C'
091400051109      *    and  (TBLkey =  '0430839 '
091500051109      *     or   substr(TBLuni, 79, 7) = '0430839'))
091600051109      *   OR   (TBLcod  =  '3K'
091700051109      *    and  (TBLkey =  '0430839 '
091800051109      *     or   substr(TBLuni, 27, 7) = '0430839'
091900051109      *     or   substr(TBLuni, 55, 7) = '0430839'))
092000051109      *   OR   (TBLcod  =  '3R'
092100051109      *    and  (TBLkey =  '0430839 '))
092200051109      *   OR   (TBLcod  =  '4C'
092300051109      *    and  (TBLkey =  '0430839 '
092400051109      *     or   substr(TBLuni, 29, 7) = '0430839'
092500051215      *     or   substr(TBLuni, 58, 7) = '0430839')))
092600051109      * order  by  TBLcod, TBLkey
092700051109      *
092800060202      *?Impostazioni iniziali per estrazione da TABEL?
092900051109     c                   eval      wSQL    =  %trim($Sql_TABEL(1))
093000051109     c                                     +  ' '
093100051109     c                                     +  %trim($Sql_TABEL(2))
093200051109     c                                     +  ' '
093300051109     c                                     +  %trim($Sql_TABEL(3))
093400051215     c*** ancora così:   eval      wAndOr  =  'AND ('
093500051109      *
093600051109      *?Impostazione estrazione dati da tabella "3C"?
093700051109      *
093800051110if  1c                   if        V1Cse1  =  'S'
093900120112     c                   eval      wSQL   +=  ' '
094000051109     c                                     +  %trim(wAndOr)
094100060202     c                                     +  ' '
094200051109     c                                     +  %trim($Sql_TABEL(4))
094300051109     c                                     +  C_tab1 +  ''''
094400051109     c                                     +  ' '
094500051109     c                                     +  %trim($Sql_TABEL(5))
094600051109     c                                     +  TBLkey +  ''''
094700140115     C                   IF        VidSoloCli = *blank
094800051123     c                   exsr      Add_Sel_3C
094900140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
095000140115     C                   ELSE
095100140115      * chiusura
095200140115     c                   eval      wSQL   +=  '))'
095300140115     C                   ENDIF
095400051215     c                   eval      wAndOr  =  'OR   '
095500051109e   1c                   endif
095600051109      *
095700051109      *?Impostazione estrazione dati da tabella "3K"?
095800051109      *
095900051110if  1c                   if        V1Cse2  =  'S'
096000120112     c                   eval      wSQL   +=  ' '
096100051109     c                                     +  %trim(wAndOr)
096200051109     c                                     +  ' '
096300051109     c                                     +  %trim($Sql_TABEL(4))
096400051109     c                                     +  C_tab2 +  ''''
096500051109     c                                     +  ' '
096600051109     c                                     +  %trim($Sql_TABEL(5))
096700051109     c                                     +  TBLkey +  ''''
096800140115     C                   IF        VidSoloCli = *blank
096900051123     c                   exsr      Add_Sel_3K
097000140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
097100140115     C                   ELSE
097200140115      * chiusura
097300140115     c                   eval      wSQL   +=  '))'
097400140115     C                   ENDIF
097500051215     c                   eval      wAndOr  =  'OR   '
097600051109e   1c                   endif
097700051109      *
097800051109      *?Impostazione estrazione dati da tabella "3R"?
097900051109      *
098000051110if  1c                   if        V1Cse3  =  'S'
098100120112     c                   eval      wSQL   +=  ' '
098200051109     c                                     +  %trim(wAndOr)
098300051109     c                                     +  ' '
098400051109     c                                     +  %trim($Sql_TABEL(4))
098500051109     c                                     +  C_tab3 +  ''''
098600051109     c                                     +  ' '
098700051109     c                                     +  %trim($Sql_TABEL(5))
098800051109     c                                     +  TBLkey +  ''''
098900140115     C                   IF        VidSoloCli = *blank
099000051123     c                   exsr      Add_Sel_3R
099100140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
099200140115     C                   ELSE
099300140115      * chiusura
099400140115     c                   eval      wSQL   +=  '))'
099500140115     C                   ENDIF
099600051215     c                   eval      wAndOr  =  'OR   '
099700051109e   1c                   endif
099800051109      *
099900051109      *?Impostazione estrazione dati da tabella "4C"?
100000051109      *
100100051110if  1c                   if        V1Cse4  =  'S'
100200120112     c                   eval      wSQL   +=  ' '
100300051109     c                                     +  %trim(wAndOr)
100400051109     c                                     +  ' '
100500051109     c                                     +  %trim($Sql_TABEL(4))
100600051109     c                                     +  C_tab4 +  ''''
100700051109     c                                     +  ' '
100800051109     c                                     +  %trim($Sql_TABEL(5))
100900051109     c                                     +  TBLkey +  ''''
101000140115     C                   IF        VidSoloCli = *blank
101100051123     c                   exsr      Add_Sel_4C
101200140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
101300140115     C                   ELSE
101400140115      * chiusura
101500140115     c                   eval      wSQL   +=  '))'
101600140115     C                   ENDIF
101700051215     c                   eval      wAndOr  =  'OR   '
101800051109e   1c                   endif
101900060217      *
102000060217      *?Impostazione estrazione dati da tabella "3Q"?
102100060217      *
102200060217if  1c                   if        V1Cse5  =  'S'
102300120112     c                   eval      wSQL   +=  ' '
102400060217     c                                     +  %trim(wAndOr)
102500060217     c                                     +  ' '
102600060217     c                                     +  %trim($Sql_TABEL(4))
102700060217     c                                     +  C_tab5 +  ''''
102800060217     c                                     +  ' '
102900060217     c                                     +  %trim($Sql_TABEL(5))
103000060217     c                                     +  TBLkey +  ''''
103100140115     C                   IF        VidSoloCli = *blank
103200060217     c                   exsr      Add_Sel_3Q
103300140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
103400140115     C                   ELSE
103500140115      * chiusura
103600140115     c                   eval      wSQL   +=  '))'
103700140115     C                   ENDIF
103800060217     c                   eval      wAndOr  =  'OR   '
103900060217e   1c                   endif
104000051109      *
104100060202      *?chiusura?
104200051109      *
104300120112     c                   eval      wSQL   +=  ')'
104400051109      *
104500051109     c                   ENDSR
104600060202      *
104700060202      *---------------------------------------------------------------*
104800060202      *?Preparazione stringa SQL per file TNTBE00F                   ?*
104900060202      *---------------------------------------------------------------*
105000060202     c     PrepSQL2      BEGSR
105100060202      *
105200060202     c                   reset                   wAndOr
105300060202      *
105400060202      * Reperisco codice cliente di invio da tab. VAP
105500060410     c*****              clear                   dVAP
105600060410if  1c                   if        V1Cse6  =  'S'
105700060410     c                   eval      TBEcod  =  C_tab6
105800060202     c                   move      V1Cksc        WkscA
105900060410     c                   eval      TBEke1  =  '0' + WkscA
106000060202     c                   clear                   TBEke2
106100060202     c     K02TNTBE      chain     TNTBE000
106200060202     c                   if        NOT %found(TNTBE01L)
106300060202     c                             or  TBEatb <> *blanks
106400060202     c                   clear                   TBEke2
106500060202     c                   endif
106600060410e   1c                   endif
106700060410      * Reperisco eventuali codici clienti unificanti da tab. DSC
106800060410     c                   clear                   dDSC
106900060410if  1c                   if        V1Cse7  =  'S'
107000060410     c                   eval      TBEcod  =  C_tab7
107100060410     c                   movel(p)  V1Cksc        TBEke1
107200060410     c                   clear                   TBEke2
107300060410     c                   clear                   TBElin
107400060410     c                   clear                   TBEsif
107500060410     c     K05TBE01      chain     TNTBE000
107600060410     c                   if            %found(TNTBE01L)
107700060410     c                             and TBEatb = *blanks
107800060410     c                   movel     TBEuni        dDSC
107900060410     c                   endif
108000060410e   1c                   endif
108100120111      * Reperisco eventuali codici clienti da tab. LAC
108200120111     c                   clear                   dLAC
108300120111if  1c                   if        V1Cse8  =  'S'
108400120111     c                   eval      TBEcod  =  C_tab8
108500120111     c                   movel(p)  V1Cksc        TBEke1
108600120111     c                   clear                   TBEke2
108700120111     c                   clear                   TBElin
108800120111     c                   clear                   TBEsif
108900120111     c     K05TBE01      chain     TNTBE000
109000120111     c                   if            %found(TNTBE01L)
109100120111     c                             and TBEatb = *blanks
109200120111     c                   movel     TBEuni        dLAC
109300120111     c                   endif
109400120111e   1c                   endif
109500130129      * La tab. DKC non ha altri codici...
109600060202      *
109700060202      *?UNION?
109800060202if  1c                   if        $Tabel  =  *on
109900120112     c                   eval      wSQL   +=  ' '
110000060202     c                                     +  C_Union
110100060202e   1c                   endif
110200060202      *
110300060410      *?Impostazioni iniziali per estrazione da TNTBE (ke1+ke2)?
110400060410if  1c                   if        $Tntbe2 =  *on
110500120112     c                   eval      wSQL   +=  ' '
110600060202     c                                     +  %trim($Sql_TNTBE(1))
110700060202     c                                     +  ' '
110800060410     c                                     +  %trim($Sql_TNTBE(3))
110900060202     c                                     +  ' '
111000060410     c                                     +  %trim($Sql_TNTBE(4))
111100060202     c                   eval      wAndOr  =  'AND ('
111200060410e   1c                   endif
111300060202      *
111400060202      *?Impostazione estrazione dati da tabella "VAP"?
111500060202      *
111600060217if  1c                   if        V1Cse6  =  'S'
111700120112     c                   eval      wSQL   +=  ' '
111800060202     c                                     +  %trim(wAndOr)
111900060202     c                                     +  ' '
112000060410     c                                     +  %trim($Sql_TNTBE(5))
112100060217     c                                     +  C_tab6 +  ''''
112200060202     c                                     +  ' '
112300060410     c                                     +  %trim($Sql_TNTBE(6))
112400060410     c                                     +  '0' + WkscA
112500060410     c                                     +  ''''
112600140115     C                   IF        VidSoloCli = *blank
112700060202     c                   exsr      Add_Sel_VAP
112800140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
112900140115     C                   ELSE
113000140115      * chiusura
113100140115     c                   eval      wSQL   +=  '))'
113200140115     C                   ENDIF
113300060202     c                   eval      wAndOr  =  'OR   '
113400060202e   1c                   endif
113500100803      *
113600060410if  1c                   if            $Tntbe1 =  *on
113700060410     c                             and $Tntbe2 =  *on
113800100803      *
113900100803      *?chiusura 1ª selezione da TNTBE00F (ke1+ke2)?
114000100803      *
114100120112     c                   eval      wSQL   +=  ')'
114200100803      *
114300100803      *?UNION?
114400120112     c                   eval      wSQL   +=  ' '
114500060410     c                                     +  C_Union
114600100803      *
114700060410e   1c                   endif
114800060410      *
114900060410      *?Impostazioni iniziali per estrazione da TNTBE (ke1+uni)?
115000060410if  1c                   if        $Tntbe1 =  *on
115100120112     c                   eval      wSQL   +=  ' '
115200060410     c                                     +  %trim($Sql_TNTBE(2))
115300060410     c                                     +  ' '
115400060410     c                                     +  %trim($Sql_TNTBE(3))
115500060410     c                                     +  ' '
115600060410     c                                     +  %trim($Sql_TNTBE(4))
115700060410     c                   eval      wAndOr  =  'AND ('
115800060410e   1c                   endif
115900060410      *
116000060410      *?Impostazione estrazione dati da tabella "DSC"?
116100060410      *
116200060410if  1c                   if        V1Cse7  =  'S'
116300120112     c                   eval      wSQL   +=  ' '
116400060410     c                                     +  %trim(wAndOr)
116500060410     c                                     +  ' '
116600060410     c                                     +  %trim($Sql_TNTBE(5))
116700060410     c                                     +  C_tab7 +  ''''
116800060410     c                                     +  ' '
116900060410     c                                     +  %trim($Sql_TNTBE(6))
117000120112     c                                     +  %trimr(TBEke1) +  ''''
117100140115     C                   IF        VidSoloCli = *blank
117200060410     c                   exsr      Add_Sel_DSC
117300140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
117400140115     C                   ELSE
117500140115      * chiusura
117600140115     c                   eval      wSQL   +=  '))'
117700140115     C                   ENDIF
117800060410     c                   eval      wAndOr  =  'OR   '
117900060410e   1c                   endif
118000120111      *
118100120111      *?Impostazione estrazione dati da tabella "LAC"?
118200120111      *
118300120111if  1c                   if        V1Cse8  =  'S'
118400120112     c                   eval      wSQL   +=  ' '
118500120111     c                                     +  %trim(wAndOr)
118600120111     c                                     +  ' '
118700120111     c                                     +  %trim($Sql_TNTBE(5))
118800120111     c                                     +  C_tab8 +  ''''
118900120111     c                                     +  ' '
119000120111     c                                     +  %trim($Sql_TNTBE(6))
119100120112     c                                     +  %trimr(TBEke1) +  ''''
119200140115     C                   IF        VidSoloCli = *blank
119300140115     c                   exsr      Add_Sel_LAC
119400140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
119500140115     C                   ELSE
119600140115      * chiusura
119700140115     c                   eval      wSQL   +=  '))'
119800140115     C                   ENDIF
119900120111     c                   eval      wAndOr  =  'OR   '
120000120111e   1c                   endif
120100130129      *
120200130129      *?Impostazione estrazione dati da tabella "DKC"?
120300130129      *
120400130129if  1c                   if        V1Cse9  =  'S'
120500130129     c                   eval      wSQL   +=  ' '
120600130129     c                                     +  %trim(wAndOr)
120700130129     c                                     +  ' '
120800130129     c                                     +  %trim($Sql_TNTBE(5))
120900130129     c                                     +  C_tab9 +  ''''
121000130129     c                                     +  ' '
121100130129     c                                     +  %trim($Sql_TNTBE(6))
121200130201     c                                     +  %editc( V1Cksc : 'X' )
121300130201     c                                     +  ''''
121400130129     c*//                exsr      Add_Sel_DKC
121500130129     c                   eval      wSQL   +=  '))'
121600130129     c                   eval      wAndOr  =  'OR   '
121700130129e   1c                   endif
121800120112      *
121900120112if  1c                   if           ($Tntbe1 =  *on
122000120112     c                             or  $Tntbe2 =  *on)
122100120112     c                             and $tntbe3 =  *on
122200120112      *
122300120112      *?chiusura 2ª selezione da TNTBE00F (ke1+uni)?
122400120112      *
122500120112     c                   eval      wSQL   +=  ')'
122600120112      *
122700120112      *?UNION?
122800120112     c                   eval      wSQL   +=  ' '
122900120112     c                                     +  C_Union
123000120112      *
123100120112e   1c                   endif
123200120112      *
123300120112      *?Impostazioni iniziali per estrazione da TNTBE (uni)?
123400120112if  1c                   if        $Tntbe3 =  *on
123500120112     c                   eval      wSQL   +=  ' '
123600120112     c                                     +  %trim($Sql_TNTBE(2))
123700120112     c                                     +  ' '
123800120112     c                                     +  %trim($Sql_TNTBE(3))
123900120112     c                                     +  ' '
124000120112     c                                     +  %trim($Sql_TNTBE(4))
124100120112     c                   eval      wAndOr  =  'AND ('
124200120112e   1c                   endif
124300120112      *
124400120112      *?Impostazione estrazione dati da tabella "OSR"?
124500120112      *
124600130129if  1c                   if        V1Cse10 =  'S'
124700120112     c                   eval      wSQL   +=  ' '
124800120112     c                                     +  %trim(wAndOr)
124900120112     c                                     +  ' '
125000120112     c                                     +  %trim($Sql_TNTBE(5))
125100130129     c                                     +  C_tab10 +  ''''
125200140115     C***                IF        VidSoloCli = *blank
125300120112     c                   exsr      Add_Sel_OSR
125400140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
125500140115     C***                ELSE
125600140115      * chiusura
125700140115     c***                eval      wSQL   +=  '))'
125800140115     C***                ENDIF
125900120112     c                   eval      wAndOr  =  'OR   '
126000120112e   1c                   endif
126100120112      *
126200120112      *?chiusura 3ª selezione da TNTBE00F (uni)?
126300120112      *
126400120112     c                   eval      wSQL   +=  ')'
126500060202      *
126600060202     c                   ENDSR
126700060202      *
126800060202      *---------------------------------------------------------------*
126900060202      *?Impostazioni finali nella stringa SQL                        ?*
127000060202      *---------------------------------------------------------------*
127100060202     c     PrepSQL3      BEGSR
127200060202      *
127300060202      *
127400120112     c                   eval      wSQL   +=  ' '
127500060202      *?order by?
127600120112     c                                     +  C_OrderBy
127700060202      *?for fatch only?
127800060202     c                                     +  ' '
127900060202     c                                     +  C_ForFetch
128000120112      *
128100120112     c                   movel     wSQL          ds_Comodo
128200060202      *
128300060202     c                   ENDSR
128400051109      *
128500051109      *---------------------------------------------------------------*
128600051109      *?Aggiunta selezione tab. "3C"                                 ?*
128700051109      *---------------------------------------------------------------*
128800051123     c     Add_Sel_3C    BEGSR
128900051109      *
129000120112     c                   eval      wSQL   +=  ' '
129100051123      * aggiunta selezione per §3CCKS
129200051109     c                                     +  %trim($Sql_TABEL(6))
129300051109     c                                     +  ' 79, 7) = '''
129400051109     c                                     +  %trim(TBLkey)
129500051109     c                                     +  ''''
129600051123      * aggiunta selezione per stesso unificante (se diverso)
129700051123     c                   move      §3Ccks        WkscA
129800051123     c                   if            WkscA   > *zeros
129900051123     c                             and WkscA  <> TBLkey
130000120112     c                   eval      wSQL   +=  ' '
130100051123     c                                     +  %trim($Sql_TABEL(6))
130200051123     c                                     +  ' 79, 7) = '''
130300051123     c                                     +  WkscA
130400051123     c                                     +  ''''
130500051123     c                   endif
130600051109      * chiusura
130700120112     c                   eval      wSQL   +=  '))'
130800051109      *
130900051109     c                   ENDSR
131000051109      *
131100051109      *---------------------------------------------------------------*
131200051109      *?Aggiunta selezione tab. "3K"                                 ?*
131300051109      *---------------------------------------------------------------*
131400051123     c     Add_Sel_3K    BEGSR
131500051109      *
131600120112     c                   eval      wSQL   +=  ' '
131700051109      * aggiunta selezione per §3KCKS
131800051109     c                                     +  %trim($Sql_TABEL(6))
131900051109     c                                     +  ' 27, 7) = '''
132000051109     c                                     +  %trim(TBLkey)
132100051109     c                                     +  ''''
132200051109      * aggiunta selezione per §3KGKS
132300051109     c                                     +  ' '
132400051109     c                                     +  %trim($Sql_TABEL(6))
132500051109     c                                     +  ' 55, 7) = '''
132600051109     c                                     +  %trim(TBLkey)
132700051109     c                                     +  ''''
132800051123      * aggiunta selezione per stesso unificante-consegna (se diverso)
132900051123     c                   move      §3Kcks        WkscA
133000051123     c                   if            WkscA   > *zeros
133100051123     c                             and WkscA  <> TBLkey
133200120112     c                   eval      wSQL   +=  ' '
133300051123     c                                     +  %trim($Sql_TABEL(6))
133400051123     c                                     +  ' 27, 7) = '''
133500051123     c                                     +  WkscA
133600051123     c                                     +  ''''
133700051123     c                   endif
133800051123      * aggiunta selezione per stesso unificante-giacenza (se diverso)
133900051123     c                   move      §3Kgks        WkscA
134000051123     c                   if            WkscA   > *zeros
134100051123     c                             and WkscA  <> TBLkey
134200120112     c                   eval      wSQL   +=  ' '
134300051123     c                                     +  %trim($Sql_TABEL(6))
134400051123     c                                     +  ' 55, 7) = '''
134500051123     c                                     +  WkscA
134600051123     c                                     +  ''''
134700051123     c                   endif
134800051109      * chiusura
134900120112     c                   eval      wSQL   +=  '))'
135000051109      *
135100051109     c                   ENDSR
135200060217      *
135300060217      *---------------------------------------------------------------*
135400060217      *?Aggiunta selezione tab. "3Q"                                 ?*
135500060217      *---------------------------------------------------------------*
135600060217     c     Add_Sel_3Q    BEGSR
135700060217      *
135800120112     c                   eval      wSQL   +=  ' '
135900060217      * aggiunta selezione per §3QCKS
136000060217     c                                     +  %trim($Sql_TABEL(6))
136100060217     c                                     +  ' 13, 7) = '''
136200060217     c                                     +  %trim(TBLkey)
136300060217     c                                     +  ''''
136400060217      * chiusura
136500120112     c                   eval      wSQL   +=  '))'
136600060217      *
136700060217     c                   ENDSR
136800051109      *
136900051109      *---------------------------------------------------------------*
137000051109      *?Aggiunta selezione tab. "3R"                                 ?*
137100051109      *---------------------------------------------------------------*
137200051123     c     Add_Sel_3R    BEGSR
137300051109      *
137400051123      * chiusura
137500120112     c                   eval      wSQL   +=  '))'
137600051109      *
137700051109     c                   ENDSR
137800051109      *
137900051109      *---------------------------------------------------------------*
138000051109      *?Aggiunta selezione tab. "4C"                                 ?*
138100051109      *---------------------------------------------------------------*
138200051123     c     Add_Sel_4C    BEGSR
138300051109      *
138400120112     c                   eval      wSQL   +=  ' '
138500051109      * aggiunta selezione per §4CCKS
138600051109     c                                     +  %trim($Sql_TABEL(6))
138700051109     c                                     +  ' 29, 7) = '''
138800051109     c                                     +  %trim(TBLkey)
138900051109     c                                     +  ''''
139000051109      * aggiunta selezione per §4CFKS
139100051109     c                                     +  ' '
139200051109     c                                     +  %trim($Sql_TABEL(6))
139300051109     c                                     +  ' 58, 7) = '''
139400051109     c                                     +  %trim(TBLkey)
139500051109     c                                     +  ''''
139600051123      * aggiunta selezione per stesso unificante-consegna (se diverso)
139700051123     c                   move      §4Ccks        WkscA
139800051123     c                   if            WkscA   > *zeros
139900051123     c                             and WkscA  <> TBLkey
140000120112     c                   eval      wSQL   +=  ' '
140100051123     c                                     +  %trim($Sql_TABEL(6))
140200051123     c                                     +  ' 29, 7) = '''
140300051123     c                                     +  WkscA
140400051123     c                                     +  ''''
140500051123     c                   endif
140600051123      * aggiunta selezione per stesso unificante-fatturaz (se diverso)
140700051123     c                   move      §4Cfks        WkscA
140800051123     c                   if            WkscA   > *zeros
140900051123     c                             and WkscA  <> TBLkey
141000120112     c                   eval      wSQL   +=  ' '
141100051123     c                                     +  %trim($Sql_TABEL(6))
141200051123     c                                     +  ' 58, 7) = '''
141300051123     c                                     +  WkscA
141400051123     c                                     +  ''''
141500051123     c                   endif
141600051109      * chiusura
141700120112     c                   eval      wSQL   +=  '))'
141800051109      *
141900051109     c                   ENDSR
142000060202      *
142100060202      *---------------------------------------------------------------*
142200060202      *?Aggiunta selezione tab. "VAP"                                ?*
142300060202      *---------------------------------------------------------------*
142400060202     c     Add_Sel_VAP   BEGSR
142500060202      *
142600120112     c                   eval      wSQL   +=  ' '
142700060202      * aggiunta selezione per TBEke2
142800060410     c                                     +  %trim($Sql_TNTBE(7))
142900060410     c                                     +  '0' + WkscA
143000060410     c                                     +  ''''
143100060202      * chiusura
143200120112     c                   eval      wSQL   +=  '))'
143300060202      *
143400060202     c                   ENDSR
143500060410      *
143600060410      *---------------------------------------------------------------*
143700060410      *?Aggiunta selezione tab. "DSC"                                ?*
143800060410      *---------------------------------------------------------------*
143900060410     c     Add_Sel_DSC   BEGSR
144000060410      *
144100120112     c                   eval      wSQL   +=  ' '
144200120111      * aggiunta selezione per §DSCKSU
144300060410     c                                     +  %trim($Sql_TNTBE(8))
144400060410     c                                     +  ' 3, 7) = '''
144500060410     c                                     +  %trim(TBEke1)
144600060410     c                                     +  ''''
144700060410      * aggiunta selezione per stesso unificante (se diverso)
144800060410     c                   move      §DSCksu       WkscA
144900060410     c                   if            WkscA   > *zeros
145000060410     c                             and WkscA  <> TBEke1
145100120112     c                   eval      wSQL   +=  ' '
145200060410     c                                     +  %trim($Sql_TNTBE(8))
145300060410     c                                     +  ' 3, 7) = '''
145400060410     c                                     +  WkscA
145500060410     c                                     +  ''''
145600060410     c                   endif
145700060410      * chiusura
145800120112     c                   eval      wSQL   +=  '))'
145900060410      *
146000060410     c                   ENDSR
146100120111      *
146200120111      *---------------------------------------------------------------*
146300120111      *?Aggiunta selezione tab. "LAC"                                ?*
146400120111      *---------------------------------------------------------------*
146500120111     c     Add_Sel_LAC   BEGSR
146600120111      *
146700120112     c                   eval      wSQL   +=  ' '
146800120111      * aggiunta selezione per §LACKSU
146900120111     c                                     +  %trim($Sql_TNTBE(8))
147000120111     c                                     +  ' 156, 7) = '''
147100120111     c                                     +  %trim(TBEke1)
147200120111     c                                     +  ''''
147300120111      * aggiunta selezione per stesso unificante (se diverso)
147400120111     c                   move      §LACksu       WkscA
147500120111     c                   if            WkscA   > *zeros
147600120111     c                             and WkscA  <> TBEke1
147700120112     c                   eval      wSQL   +=  ' '
147800120111     c                                     +  %trim($Sql_TNTBE(8))
147900120111     c                                     +  ' 156, 7) = '''
148000120111     c                                     +  WkscA
148100120111     c                                     +  ''''
148200120111     c                   endif
148300120111      * aggiunta selezione per §LACKSC
148400120112     c                   eval      wSQL   +=  ' '
148500120111     c                                     +  %trim($Sql_TNTBE(8))
148600120111     c                                     +  ' 178, 7) = '''
148700120111     c                                     +  %trim(TBEke1)
148800120111     c                                     +  ''''
148900120111      * aggiunta selezione per cliente forzato (se diverso)
149000120111     c                   move      §LACksc       WkscA
149100120111     c                   if            WkscA   > *zeros
149200120111     c                             and WkscA  <> TBEke1
149300120112     c                   eval      wSQL   +=  ' '
149400120111     c                                     +  %trim($Sql_TNTBE(8))
149500120111     c                                     +  ' 178, 7) = '''
149600120111     c                                     +  WkscA
149700120111     c                                     +  ''''
149800120111     c                   endif
149900120111      *
150000120111      * chiusura
150100120112     c                   eval      wSQL   +=  '))'
150200120111      *
150300120111     c                   ENDSR
150400120112      /free
150500120112
150600120112       //--------------------------------------------------------------
150700120112       //?Aggiunta selezione tab. "OSR"                                ?
150800120112       //--------------------------------------------------------------
150900120112       BEGSR  Add_Sel_OSR;
151000120112
151100120112         // -?aggiunta selezione per D§OSRCLI/CL2/3-16?
151200120112         wSQL += ' and (substr(TBEuni,   1, 7) = ''' + %editc(V1Cksc:'X') +
151300120112                 ''' or substr(TBEuni,  42, 7) = ''' + %editc(V1Cksc:'X') +
151400120112                 ''' or substr(TBEuni,  74, 7) = ''' + %editc(V1Cksc:'X') +
151500120112                 ''' or substr(TBEuni, 106, 7) = ''' + %editc(V1Cksc:'X') +
151600120112                 ''' or substr(TBEuni, 138, 7) = ''' + %editc(V1Cksc:'X') +
151700120112                 ''' or substr(TBEuni, 171, 7) = ''' + %editc(V1Cksc:'X') +
151800120112                 ''' or substr(TBEuni, 178, 7) = ''' + %editc(V1Cksc:'X') +
151900120112                 ''' or substr(TBEuni, 185, 7) = ''' + %editc(V1Cksc:'X') +
152000120112                 ''' or substr(TBEuni, 192, 7) = ''' + %editc(V1Cksc:'X') +
152100120112                 ''' or substr(TBEuni, 199, 7) = ''' + %editc(V1Cksc:'X') +
152200120112                 ''' or substr(TBEuni, 206, 7) = ''' + %editc(V1Cksc:'X') +
152300120112                 ''' or substr(TBEuni, 213, 7) = ''' + %editc(V1Cksc:'X') +
152400120112                 ''' or substr(TBEuni, 220, 7) = ''' + %editc(V1Cksc:'X') +
152500120112                 ''' or substr(TBEuni, 227, 7) = ''' + %editc(V1Cksc:'X') +
152600120112                 ''' or substr(TBEuni, 234, 7) = ''' + %editc(V1Cksc:'X') +
152700120112                 ''' or substr(TBEuni, 241, 7) = ''' + %editc(V1Cksc:'X') +
152800120112                 '''';
152900120112         // -?aggiunta selezione per codice ordinante ORM?
153000120112         wSQL += ' or substr(TBEuni, 146, 7) = ''' + %editc(V1Cksc:'X') +
153100120112                 '''';
153200120112         // -?aggiunta selezione per codice destinatario ORM?
153300120112         wSQL += ' or substr(TBEuni, 153, 7) = ''' + %editc(V1Cksc:'X') +
153400120112                 '''';
153500120112
153600120112         // -?chiusura?
153700120112         wSQL += '))';
153800120112
153900120112       ENDSR;
154000120112
154100120112      /end-free
154200051109      *
154300051109      *---------------------------------------------------------------*
154400051109      *?Dichiarazione e Apertura cursore                             ?*
154500051109      *---------------------------------------------------------------*
154600051109     c     OpenCursor    BEGSR
154700051109      *
154800051109      * Dichiarazione cursore
154900051109      *
155000051109     c/exec sql
155100051109     c+     PREPARE S1 FROM :wSQL
155200051109     c/end-exec
155300051109     c*
155400051109     c/exec sql
155500051109     c+     DECLARE C1 CURSOR FOR S1
155600051109     c/end-exec
155700051109      *
155800051109      * Apertura del cursore
155900051109      *
156000051109     c/exec sql
156100051109     c+     OPEN C1
156200051109     c/end-exec
156300051109      *
156400051109     c                   ENDSR
156500051109      *
156600051109      *---------------------------------------------------------------*
156700051109      *?Lettura cursore                                              ?*
156800051109      *---------------------------------------------------------------*
156900051109     c     ReadCursor    BEGSR
157000051109      *
157100051110     c                   clear                   ds_TABEL
157200051109     c/exec sql
157300051110     c+     FETCH NEXT FROM C1 INTO :ds_TABEL
157400051109     c/end-exec
157500051109      *
157600051109sel 1c                   select
157700051109w   1c                   when      SQLcod  =  100
157800051109     c                   eval      $EoF    =  *on
157900051109w   1c                   when      SQLcod  <  *zeros
158000051109     c                   eval      $Err    =  *on
158100051109     c                   eval      $Eof    =  *on
158200051109x   1c                   other
158300051110     c                   exsr      RieS01
158400051109e   1c                   endsl
158500051110      *
158600051110      * Visualizzazione del SFL (se ci sono dati)
158700051110     c                   eval      *in30   =  (S01nrr = *zeros)
158800051110      *
158900051110      * Attivazione (eventuale) del SFLEND
159000051110     c                   eval      *in33   =  $EoF
159100051110      *
159200051110      * Posizionamento cursore al 1° rcd della pagina
159300051110if  1c                   if        S01nrr  >  *zeros
159400051110     c     S01nrr        div       C_SflPag      wPag
159500051110     c                   mvr                     wRig
159600051110     c                   eval      C01rcd  =  wPag * C_SflPag
159700051110     c                   if        wRig    >  *zeros
159800051110     c                   eval      C01rcd  =  C01rcd + 1
159900051110     c                   else
160000051110     c                   eval      C01rcd  =  C01rcd - C_SflPag + 1
160100051110     c                   endif
160200051110x   1c                   else
160300051110     c                   clear                   C01rcd
160400051110e   1c                   endif
160500051110     c                   eval      C01csr  =  C01rcd
160600051109      *
160700051109     c                   ENDSR
160800051109      *
160900051109      *---------------------------------------------------------------*
161000051110      *?Caricamento dati nel record del subfile S01                  ?*
161100051109      *---------------------------------------------------------------*
161200051110     c     RieS01        BEGSR
161300051110      *
161400051110     c                   clear                   TB06S1
161500060202      *
161600060202     c                   eval      VS1cod  = %trim(wSQLcod)
161700051110      *
161800051110      * Impostazione DS corretta (in base al tipo tabella)
161900051110     c                   SELECT
162000051110      * - tab. 3C
162100060202     c                   WHEN      %trim(wSQLcod) = C_tab1
162200060202     c                   movel     wSQLkey       VS1key
162300051117     c                   movel     wSQLuni       ds3C
162400051110     c                   movel     §3Crag        VS1dke
162500051110     c                   movel     §3Ccks        VS1cu1
162600051110     c***                movel                   VS1du1
162700051110     c***                movel                   VS1cu2
162800051110     c***                movel                   VS1du2
162900060202     c                   if        §3Ccks  > *zeros
163000051123     c                   movel     C_tab1_a      VS1dc1
163100051123     c                   endif
163200051110      * - tab. 3K
163300060202     c                   WHEN      %trim(wSQLcod) = C_tab2
163400060202     c                   movel     wSQLkey       VS1key
163500051117     c                   movel     wSQLuni       ds3K
163600051110     c***                movel                   VS1dke
163700051110     c                   movel     §3Kcks        VS1cu1
163800051110     c***                movel                   VS1du1
163900051110     c                   movel     §3Kgks        VS1cu2
164000051110     c***                movel                   VS1du2
164100060202     c                   if        §3Kcks  > *zeros
164200051123     c                   movel     C_tab2_a      VS1dc1
164300051123     c                   endif
164400060202     c                   if        §3Kgks  > *zeros
164500051123     c                   movel     C_tab2_b      VS1dc2
164600051123     c                   endif
164700051110      * - tab. 3R
164800060202     c                   WHEN      %trim(wSQLcod) = C_tab3
164900060202     c                   movel     wSQLkey       VS1key
165000051117     c                   movel     wSQLuni       ds3R
165100051110     c***                movel                   VS1dke
165200051110     c***                movel                   VS1cu1
165300051110     c***                movel                   VS1du1
165400051110     c***                movel                   VS1cu2
165500051110     c***                movel                   VS1du2
165600051110      * - tab. 4C
165700060202     c                   WHEN      %trim(wSQLcod) = C_tab4
165800060202     c                   movel     wSQLkey       VS1key
165900051117     c                   movel     wSQLuni       ds4C
166000051110     c***                movel                   VS1dke
166100051122     c                   if        §4Ccks <> *zeros
166200051110     c                   movel     §4Ccks        VS1cu1
166300051122     c                   endif
166400051110     c***                movel                   VS1du1
166500051122     c                   if        §4Cfks <> *zeros
166600051110     c                   movel     §4Cfks        VS1cu2
166700051122     c                   endif
166800051110     c***                movel                   VS1du2
166900060202     c                   if        §4Ccks  > *zeros
167000051123     c                   movel     C_tab4_a      VS1dc1
167100051123     c                   endif
167200060202     c                   if        §4Cfks  > *zeros
167300051123     c                   movel     C_tab4_b      VS1dc2
167400051123     c                   endif
167500060217      * - tab. 3Q
167600060217     c                   WHEN      %trim(wSQLcod) = C_tab5
167700060217     c                   movel     wSQLkey       VS1key
167800060217     c                   movel     wSQLuni       ds3Q
167900060217     c***                movel                   VS1dke
168000060217     c                   movel     §3Qcks        VS1cu1
168100060217     c***                movel                   VS1du1
168200060217     c***                movel                   VS1cu2
168300060217     c***                movel                   VS1du2
168400060217     c                   if        §3Qcks  > *zeros
168500060217     c                   movel     C_tab5_a      VS1dc1
168600060217     c                   endif
168700060202      * - tab. VAP
168800060217     c                   WHEN      %trim(wSQLcod) = C_tab6
168900060202     c                   eval      VS1key  = %subst(wSQLkey:2:7)
169000060410     c*** inesistente:   movel     wSQLuni       dVAP
169100060202     c***                movel                   VS1dke
169200060202     c                   eval      VS1cu1  = %subst(wSQLuni:2:7)
169300060202     c***                movel                   VS1du1
169400060202     c***                movel                   VS1cu2
169500060202     c***                movel                   VS1du2
169600060202     c*** sempre:        if        VS1cu1  > *zeros
169700060217     c                   movel     C_tab6_a      VS1dc1
169800060202     c*** sempre:        endif
169900060410      * - tab. DSC
170000060410     c                   WHEN      %trim(wSQLcod) = C_tab7
170100060410     c                   movel     wSQLkey       VS1key
170200060410     c                   movel     wSQLuni       dDSC
170300060410     c***                movel                   VS1dke
170400060410     c                   if        §DSCksu > *zeros
170500060410     c                   movel     §DSCksu       VS1cu1
170600060410     c                   endif
170700060410     c***                movel                   VS1du1
170800060410     c***                movel                   VS1cu2
170900060410     c***                movel                   VS1du2
171000060410     c*** sempre:        if        VS1cu1  > *zeros
171100060410     c                   movel     C_tab7_a      VS1dc1
171200060410     c*** sempre:        endif
171300120111      * - tab. LAC
171400120111     c                   WHEN      %trim(wSQLcod) = C_tab8
171500120111     c                   movel     wSQLkey       VS1key
171600120111     c                   movel     wSQLuni       dLAC
171700120111     c***                movel     §LACrag       VS1dke
171800120111     c                   if        §LACksu > *zeros
171900120111     c                   movel     §LACksu       VS1cu1
172000120111     c                   endif
172100120111     c***                movel                   VS1du1
172200120111     c                   if        §LACksc > *zeros
172300120111     c                   movel     §LACksc       VS1cu2
172400120111     c                   endif
172500120111     c***                movel                   VS1du2
172600120111     c*** sempre:        if        VS1cu1  > *zeros
172700120111     c                   movel     C_tab8_a      VS1dc1
172800120111     c*** sempre:        endif
172900120111     c*** sempre:        if        VS1cu2  > *zeros
173000120111     c                   movel     C_tab8_b      VS1dc2
173100120111     c*** sempre:        endif
173200130129      * - tab. DKC
173300130129     c                   WHEN      %trim(wSQLcod) = C_tab9
173400130129     c                   movel     wSQLkey       VS1key
173500130129     c*// No altri cod.  movel     wSQLuni       dDKC
173600130129     c*//                movel                   VS1dke
173700130129     c*//                movel                   VS1cu1
173800130129     c*//                movel                   VS1du1
173900130129     c*//                movel                   VS1cu2
174000130129     c*//                movel                   VS1du2
174100130129     c*// mai:           if        VS1cu1  > *zeros
174200130129     c*//                movel     C_tab?_a      VS1dc1
174300130129     c*// mai:           endif
174400120112      * - tab. OSR
174500130129     c                   WHEN      %trim(wSQLcod) = C_tab10
174600120112     c                   movel     wSQLkey       VS1key
174700120112     c                   movel     wSQLuni       dOSR
174800120112     c                   eval      VS1dke = '-Fil.emiss.ORM++
174900120112     c                                        Serie'
175000120112     c                   if            d§OSRcl3  > *zeros
175100120112     c                             or  d§OSRcl4  > *zeros
175200120112     c                             or  d§OSRcl5  > *zeros
175300120112     c                             or  d§OSRcl6  > *zeros
175400120112     c                             or  d§OSRcl7  > *zeros
175500120112     c                             or  d§OSRcl8  > *zeros
175600120112     c                             or  d§OSRcl9  > *zeros
175700120112     c                             or  d§OSRcl10 > *zeros
175800120112     c                             or  d§OSRcl11 > *zeros
175900120112     c                             or  d§OSRcl12 > *zeros
176000120112     c                             or  d§OSRcl13 > *zeros
176100120112     c                             or  d§OSRcl14 > *zeros
176200120112     c                             or  d§OSRcl15 > *zeros
176300120112     c                             or  d§OSRcl16 > *zeros
176400120112     c                             or  d§OSRcor  > *zeros
176500120112     c                             or  d§OSRcrc  > *zeros
176600120112     c                   eval      VS1dke = '   (altri clienti)'
176700120112     c                   endif
176800120112     c                   if        d§OSRcli > *zeros
176900120112     c                   movel     d§OSRcli      VS1cu1
177000120112     c                   endif
177100120112     c***                movel     d§OSRrag      VS1du1
177200120112     c                   if        d§OSRcl2 > *zeros
177300120112     c                   movel     d§OSRcl2      VS1cu2
177400120112     c                   endif
177500120112     c***                movel     d§OSRra2      VS1du2
177600120112     c*** sempre:        if        VS1cu1  > *zeros
177700120112     c                   movel     C_tab8_a      VS1dc1
177800120112     c*** sempre:        endif
177900120112     c*** sempre:        if        VS1cu2  > *zeros
178000120112     c                   movel     C_tab8_b      VS1dc2
178100120112     c*** sempre:        endif
178200051110     c                   ENDSL
178300051110      *
178400051110      * Decodifica cliente - chiave
178500060202if  1c                   if        VS1key  > *zeros
178600130129     c                             and  %trim(wSQLcod) <> C_tab10
178700051110     c                   move      VS1key        Wksc
178800051110     c                   exsr      Decod_Cli
178900051110if  2c                   if        O69err  =  *on
179000051110     c                   movel     *all'? '      VH1dke
179100051110x   2c                   else
179200051110     c                   movel     ACOrag        VH1dke
179300120807     c                   movel     ACOabl        VH1abl
179400051110e   2c                   endif
179500060202if  2c                   if        VS1dke  = *blanks
179600051110     c                   movel     VH1dke        VS1dke
179700051110e   2c                   endif
179800051110e   1c                   endif
179900051110      * Decodifica cliente - unificante 1
180000060202if  1c                   if        VS1cu1  > *zeros
180100051110     c                   move      VS1cu1        Wksc
180200051110     c                   exsr      Decod_Cli
180300051110if  2c                   if        O69err  =  *on
180400051110     c                   movel     *all'? '      VH1du1
180500051110x   2c                   else
180600051110     c                   movel     ACOrag        VH1du1
180700051110e   2c                   endif
180800060202if  2c                   if        VS1du1  = *blanks
180900051110     c                   movel     VH1du1        VS1du1
181000051110e   2c                   endif
181100051110e   1c                   endif
181200051110      * Decodifica cliente - unificante 2
181300051110if  1c                   if        VS1cu2 >  *zeros
181400051110     c                   move      VS1cu2        Wksc
181500051110     c                   exsr      Decod_Cli
181600051110if  2c                   if        O69err  =  *on
181700051110     c                   movel     *all'? '      VH1du2
181800051110x   2c                   else
181900051110     c                   movel     ACOrag        VH1du2
182000051110e   2c                   endif
182100060202if  2c                   if        VS1du2  = *blanks
182200051110     c                   movel     VH1du2        VS1du2
182300051110e   2c                   endif
182400051110e   1c                   endif
182500100216      *
182600100216      * Impostazione attributi di visualizzazione
182700130201     c                   eval      *in41 = ($Called = *on  and
182800130201     c                                      D06op0  = *blank)
182900130315     c                   eval      *in42 = (VH1abl  <> *blanks)
183000051110      *
183100051110      * Scrittura record subfile
183200051110     c                   add       1             S01nrr
183300051110     c                   write     TB06S1
183400051110      *
183500051110     c                   ENDSR
183600051109      *
183700051109      *---------------------------------------------------------------*
183800051109      *?Chiusura cursore                                             ?*
183900051109      *---------------------------------------------------------------*
184000051109     c     CloseCursor   BEGSR
184100051109      *
184200051109     c/exec sql
184300051109     c+     CLOSE C1
184400051109     c/end-exec
184500051109      *
184600051109     c                   ENDSR
184700051110      *
184800051110      *---------------------------------------------------------------*
184900051110      *?Gestione tasto funzionale F08 da videata C01 / S01           ?*
185000120111      *?F8-Stampa elenco                                             ?*
185100051110      *---------------------------------------------------------------*
185200051110     c     F08S01        BEGSR
185300051110      *
185400051110      * Override al prtf
185500051110     c                   call      'QCMDEXC'
185600051110     c                   parm      $Cmd(1)       Qcmd
185700051110     c                   parm                    Qlen
185800051110      *
185900051110      * Apertura prtf
186000051110     c                   open      PRTF198
186100051110      *
186200051110      * Stampa parametri - stampa della testata iniziale
186300051110     c                   move      *date         Data_Eur
186400051110     c     *eur          movel     Data_Eur      wDate
186500051110     c                   time                    wTime
186600051110      * Stampa parametri - stampa dei parametri di lancio
186700051110     c                   except    PRTini
186800051110     c                   except    PRTpar
186900051110      *
187000051110      * Stampa lista     - stampa della testata ininiziale
187100051110     c                   except    PRTini
187200051110     c                   except    PRTtxt
187300051110     c                   eval      *inOF   =  *off
187400070828      *
187500070828if  0c                   IF        C01rcd  >  *zeros
187600070828      *
187700051110      * Stampa lista     - stampa dettagio
187800051110      * - ciclo di lettura sfl
187900051110if  1c                   DO        *hival        S01nrr
188000051110     c     S01nrr        chain     TB06S1
188100051110if  2c                   if        NOT %found(TRTB06D)
188200051110     c                   leave
188300051110e   2c                   endif
188400051110      * - stampa riga di dettaglio
188500051110     c                   if        *inOF    = *on
188600051110     c                   except    PRTini
188700051110     c                   except    PRTtxt
188800051110     c                   eval      *inOF    = *off
188900051110     c                   endif
189000051110     c                   except    PRTdet
189100051110      * - riattivazione sflnxtchg (se immessa opzione)
189200051110if  2c                   if        VS1opz  <> *blanks
189300051110     c                   eval      *in32   =  *on
189400130201     c                   eval      *in41 = ($Called = *on  and
189500130201     c                                      D06op0  = *blank)
189600130315     c                   eval      *in42 = (VH1abl  <> *blanks)
189700051110     c                   UPDATE    TB06S1
189800051110e   2c                   endif
189900051110e   1c                   ENDDO
190000070828      *
190100070828e   0c                   ENDIF
190200051110      *
190300051110      * Stampa "Fine Lista"
190400051110     c                   time                    wTime
190500051110     c                   except    PRTend
190600051110      *
190700051110      * Chiusura prtf
190800051110     c                   close     PRTF198
190900051110      *
191000051110      * Cancellazione override
191100051110     c                   call      'QCMDEXC'
191200051110     c                   parm      $Cmd(2)       Qcmd
191300051110     c                   parm                    Qlen
191400051110      *
191500051110     c                   ENDSR
191600051110      *
191700051110      *---------------------------------------------------------------*
191800051110      *?Gestione tasto funzionale F11 da videata C01 / S01           ?*
191900051110      *?F11-Cambia ordinamento sfl                                   ?*
192000051110      *---------------------------------------------------------------*
192100051110     c     F11S01        BEGSR
192200051110      *
192300051110     c                   select
192400051110     c                   when      WrkSort =  *off
192500051110     c                   eval      WrkSort =  *on
192600051110     c                   when      WrkSort =  *on
192700051110     c                   eval      WrkSort =  *off
192800051110     c                   endsl
192900051123     c                   eval      *in40   =  (WrkSort = *on)
193000051123if  1c                   if        C01rcd  >  *zeros
193100051110     c                   exsr      Sort
193200051123e   1c                   endif
193300051110      *
193400051110     c                   ENDSR
193500051110      *
193600051110      *---------------------------------------------------------------*
193700051110      *?Gestione tasto funzionale F12 da videata C01 / S01           ?*
193800051110      *?F12-Ritorno                                                  ?*
193900051110      *---------------------------------------------------------------*
194000051110     c     F12S01        BEGSR
194100051110      *
194200130129     c*//                if        Not $Called
194300051110      * Torno alla videata precedente
194400051110     c                   eval      $Video  =  '1'
194500130129     c                   eval      *in51   =  ($Called = *on)
194600130129     c*//                else
194700091214      * Esco dal programma
194800130129     c*//                eval      $Fine   =  *on
194900130129     c*//                endif
195000051110      *
195100051110     c                   ENDSR
195200051110      *
195300051110      *---------------------------------------------------------------*
195400051110      *?Gestione opzioni da subfile S01                              ?*
195500051110      *---------------------------------------------------------------*
195600051110     c     OpzS01        BEGSR
195700051110      *
195800051110     c                   readc     TB06S1
195900051110      *
196000051110do  1c                   DOW       NOT %eof(TRTB06D)
196100051110      *
196200051110     c                   eval      *in32   =  *off
196300051110     c                   movea     *zeros        *in(50)
196400051110     c                   z-add     S01nrr        C01rcd
196500051110      *
196600051110sel 2c                   SELECT
196700051110      * - Nessuna opzione
196800051110w   2c                   WHEN      VS1opz  =  *blanks
196900051110      * - Gestione delle singola tabella
197000051110w   2c                   WHEN          VS1opz  =  '2'
197100051117     c***                          or  VS1opz  =  '5'
197200051110     c                   exsr      sr_OpzS1
197300051110      * - ? = Opzione NON valida
197400051110x   2c                   OTHER
197500051110     c                   seton                                        502890
197600051110     c                   eval      V1Dmsg  =  $Msg(4)
197700051110e   2c                   ENDSL
197800051110      *
197900051110      * Aggiornamento sfl
198000051110sel 2c                   select
198100051110w   2c                   when      *in28
198200051110     c                   eval      *in32   =  *on
198300051110     c                   z-add     C01rcd        C01csr
198400051110w   2c                   when      *in90
198500051110     c                   z-add     C01rcd        C01csr
198600051110     c                   clear                   VS1opz
198700051110x   2c                   other
198800051110     c                   clear                   VS1opz
198900051110e   2c                   endsl
199000130201     c                   eval      *in41 = ($Called = *on  and
199100130201     c                                      D06op0  = *blank)
199200130315     c                   eval      *in42 = (VH1abl  <> *blanks)
199300051110     c                   UPDATE    TB06S1
199400051110if  2c                   if        *in28  OR  *in90
199500051110     c                   leave
199600051110e   2c                   endif
199700051110      *
199800051110     c                   readc     TB06S1
199900051110      *
200000051110e   1c                   ENDDO
200100051110      *
200200051110     c                   ENDSR
200300051110      *
200400051110      *---------------------------------------------------------------*
200500051110      *?Gestione singola opzione dal subfile S01                     ?*
200600051110      *---------------------------------------------------------------*
200700051110     c     sr_OpzS1      BEGSR
200800051110      *
200900051110sel 1c                   select
201000051216      * - 1ª tabella ('3C')
201100051216w   1c                   when           VS1cod =  V1Ctb1
201200051110     c                             and (VS1opz =  '2'
201300051117     c                              or  VS1opz =  '5')
201400120807     c*//                movel(p)  VS1key        KPJBU
201500120807     c                   eval      KPJBU = VS1key + SDSpgm
201600051216     c                   call      V1Hpg1
201700051110     c                   parm                    KPJBA
201800051216      * - 2ª tabella ('3K')
201900051216w   1c                   when           VS1cod =  V1Ctb2
202000051110     c                             and (VS1opz =  '2'
202100051117     c                              or  VS1opz =  '5')
202200051110     c                   movel(p)  VS1key        KPJBU
202300051216     c                   call      V1Hpg2
202400051110     c                   parm                    KPJBA
202500051216      * - 3ª tabella ('3R')
202600051216w   1c                   when           VS1cod =  V1Ctb3
202700051110     c                             and (VS1opz =  '2'
202800051117     c                              or  VS1opz =  '5')
202900051110     c                   movel(p)  VS1key        KPJBU
203000051216     c                   call      V1Hpg3
203100051110     c                   parm                    KPJBA
203200051216      * - 4ª tabella ('4C')
203300051216w   1c                   when           VS1cod =  V1Ctb4
203400051110     c                             and (VS1opz =  '2'
203500051117     c                              or  VS1opz =  '5')
203600051110     c                   movel(p)  VS1key        KPJBU
203700051216     c                   call      V1Hpg4
203800051110     c                   parm                    KPJBA
203900060217      * - 5ª tabella ('3Q')
204000060217w   1c                   when           VS1cod =  V1Ctb5
204100060217     c                             and (VS1opz =  '2'
204200060217     c                              or  VS1opz =  '5')
204300071016     c                   movel(p)  VS1key        KPJBU
204400071016     c                   call      V1Hpg5
204500071016     c                   parm                    KPJBA
204600060217      * - 6ª tabella ('VAP')
204700060217w   1c                   when           VS1cod =  V1Ctb6
204800060202     c                             and (VS1opz =  '2'
204900060202     c                              or  VS1opz =  '5')
205000060202     c                   clear                   KPJBU
205100060202     c                   eval      KPJBU = '0' + VS1key
205200060217     c                   call      V1Hpg6
205300060202     c                   parm                    KPJBA
205400120111      * - 7ª tabella ('DSC')
205500060410w   1c                   when           VS1cod =  V1Ctb7
205600060410     c                             and (VS1opz =  '2'
205700060410     c                              or  VS1opz =  '5')
205800060410     c                   clear                   KPJBU
205900060410     c                   movel(p)  VS1key        KPJBU
206000060410     c                   call      V1Hpg7
206100060410     c                   parm                    KPJBA
206200120111      * - 8ª tabella ('LAC')
206300120111w   1c                   when           VS1cod =  V1Ctb8
206400120111     c                             and (VS1opz =  '2'
206500120111     c                              or  VS1opz =  '5')
206600120111     c                   clear                   TNTB46ds
206700120111     c                   eval      B46ke1 = VS1key
206800120111     c                   movel(p)  TNTB46ds      KPJBU
206900120111     c                   call      V1Hpg8
207000120111     c                   parm                    KPJBA
207100130129      * - 9ª tabella ('DKC')
207200120112w   1c                   when           VS1cod =  V1Ctb9
207300120112     c                             and (VS1opz =  '2'
207400120112     c                              or  VS1opz =  '5')
207500120112     c                   clear                   KPJBU
207600120112     c                   movel(p)  VS1key        KPJBU
207700120112     c                   call      V1Hpg9
207800120112     c                   parm                    KPJBA
207900130129      * - 10ª tabella ('OSR')
208000130129w   1c                   when           VS1cod =  V1Ctb10
208100130129     c                             and (VS1opz =  '2'
208200130129     c                              or  VS1opz =  '5')
208300130129     c                   clear                   KPJBU
208400130129     c                   movel(p)  VS1key        KPJBU
208500130129     c                   call      V1Hpg10
208600130129     c                   parm                    KPJBA
208700051110      *
208800051110e   1c                   endsl
208900051110      *
209000051110     c                   ENDSR
209100051110      *
209200051110      *---------------------------------------------------------------*
209300051110      *?Decodifica cliente                                           ?*
209400051110      *---------------------------------------------------------------*
209500051110     c     Decod_Cli     BEGSR
209600051110      *
209700051110     c                   clear                   TIBS69ds
209800051110     c                   clear                   ds_CNACO
209900051110     c                   clear                   ds_CNIND
210000051110     c                   clear                   ds_CNCLP
210100051110     c                   clear                   ds_FNCLS
210200051110      *
210300051110     c                   z-add     Wksc          I69kac
210400051110      *
210500051110     c                   call      'TIBS69R'
210600051110     c                   parm                    TIBS69ds
210700051110     c                   parm                    ds_CNACO
210800051110     c                   parm                    ds_CNIND
210900051110     c                   parm                    ds_CNCLP
211000051110     c                   parm                    ds_FNCLS
211100051110      *
211200051110     c                   ENDSR
211300051110      *
211400051110      *---------------------------------------------------------------*
211500051110      *?Ordinamento subfile                                          ?*
211600051110      *?  Subroutine - Sort                                          ?*
211700051110      *?  This subroutine sorts the subfile records.                 ?*
211800051110      *---------------------------------------------------------------*
211900051110     c     Sort          BEGSR
212000051110      *
212100051122     c                   eval      C01rcd  =  1
212200051122     c                   clear                   C01csr
212300051122     c                   clear                   S01nrr
212400051122     c                   clear                   V1Dmsg
212500051122     c                   setoff                                       28  90
212600051122     c                   eval      *in32   =  *off
212700051122     c                   movea     *zeros        *in(50)
212800051110      *
212900051110      * Initialize the key fields to sort on. There is one extra field not in
213000051110      * the subfile -- Selected -- that is added to the record. This field
213100051110      * is used to place selected records in front of nonselected records.
213200051110      *
213300051110     c                   clear                   QLGKL
213400051110     c                   clear                   QLGSKL
213500051110     c                   clear                   QLGSCB
213600051110     c                   clear                   QLGSCB00
213700051110      *
213800051110      * Gestione della scelta ordinamento.
213900051110sel 1c                   SELECT
214000051110      * Ordinamento per Chiave_Tabella/Tipo_Tabella
214100051122w   1c                   WHEN      WrkSort  = *on
214200051110      *   2 campi chiave
214300051110     c                   eval      QLGNBRK  = 2
214400051122      *   1° campo: chiave tabella (VS1key - codice cliente)
214500120807      *             a posizone  111,  7 byte, char, ascending.
214600120807     c                   eval      QLGSP    = 111
214700051110     c                   eval      QLGSS    = %size(VS1key)
214800051110     c                   eval      QLGDT    = Carattere
214900051110     c                   eval      QLGSO    = Ascendente
215000051110     c                   eval      QLGKL(1) = QLGSKL
215100051122      *   2° campo: tipo tabella (VS1cod - 3C/3K/3R/4C)
215200120807      *             a posizone  108,  3 byte, char, ascending.
215300120807     c                   eval      QLGSP    = 108
215400051110     c                   eval      QLGSS    = %size(VS1cod)
215500051110     c                   eval      QLGDT    = Carattere
215600051110     c                   eval      QLGSO    = Ascendente
215700051110     c                   eval      QLGKL(2) = QLGSKL
215800051110      *
215900051110      * Ordinamento per Tipo_Tabella/Chiave_Tabella
216000051110w   1c                   WHEN      WrkSort  = *off
216100051110      *   2 campi chiave
216200051110     c                   eval      QLGNBRK  = 2
216300051122      *   1° campo: tipo tabella (VS1cod - 3C/3K/3R/4C)
216400120807      *             a posizone  108,  3 byte, char, ascending.
216500120807     c                   eval      QLGSP    = 108
216600051110     c                   eval      QLGSS    = %size(VS1cod)
216700051110     c                   eval      QLGDT    = Carattere
216800051110     c                   eval      QLGSO    = Ascendente
216900051110     c                   eval      QLGKL(1) = QLGSKL
217000051122      *   2° campo: chiave tabella (VS1key - codice cliente)
217100120807      *             a posizone  111,  7 byte, char, ascending.
217200120807     c                   eval      QLGSP    = 111
217300051110     c                   eval      QLGSS    = %size(VS1key)
217400051110     c                   eval      QLGDT    = Carattere
217500051110     c                   eval      QLGSO    = Ascendente
217600051110     c                   eval      QLGKL(2) = QLGSKL
217700051110      *
217800051110e   1c                   ENDSL
217900051110      *
218000051110      * Load other sort parameters.
218100051110     c                   eval      QLGLB    = 80 + 16 * MaxKey
218200051110     c                   eval      QLGRL    = %size(SflRcd) - 1
218300051110     c                   eval      QLGRT    = 8
218400051110     c                   eval      QLGOKL   = 80
218500051110     c                   eval      QLGLKE   = 16
218600051110     c                   eval      QLGLSS   = 290
218700051110      * Initialize Sort I/O API fields.
218800051110     c                   eval      QLGRL00  = QLGRL
218900051110     c                   eval      QLGRC00  = 1
219000051110     c                   clear                   QUSEI
219100051110     c                   eval      QUSBPRV  = %size(QUSEC)
219200051110      * First step - Initialize the sort routine.
219300051110     c                   call      'QLGSORT'
219400051110     c                   parm                    QLGSCB
219500051110     c                   parm                    NotUsed
219600051110     c                   parm                    NotUsed
219700051110     c                   parm                    SizeList
219800051110     c                   parm                    ReturnSize
219900051110     c                   parm                    QUSEC
220000051110      *
220100051110      * Next step - Write records to I/O routine.
220200051110     c                   eval      QLGRT00  = Put
220300051122do  1c                   DO        RrnLast       Nrr
220400051122     c     Nrr           chain     TB06S1
220500051110      * Solo le righe con Selected = 'Y' sono riordinate,
220600051110      * quindi per fare un ordinamento di tutte le righe
220700051110      * metto 'Y' sempre.
220800051110     c                   eval      Selected = 'Y'
220900051110     c                   clear                   QUSEI
221000051110     c                   eval      QUSBPRV  = %size(QUSEC)
221100051110     c                   call      'QLGSRTIO'
221200051110     c                   parm                    QLGSCB00
221300051110     c                   parm                    SflRcd
221400051110     c                   parm                    NotUsed
221500051110     c                   parm                    SizeList
221600051110     c                   parm                    NotUsed
221700051110     c                   parm                    QUSEC
221800051110e   1c                   ENDDO
221900051110      * Next step - Signal end of input, clear subfile for reload.
222000051110     c                   eval      QLGRT00  = EndPut
222100051110     c                   clear                   QUSEI
222200051110     c                   eval      QUSBPRV  = %size(QUSEC)
222300051110      *
222400051110     c                   call      'QLGSRTIO'
222500051110     c                   parm                    QLGSCB00
222600051110     c                   parm                    SflRcd
222700051110     c                   parm                    NotUsed
222800051110     c                   parm                    SizeList
222900051110     c                   parm                    NotUsed
223000051110     c                   parm                    QUSEC
223100051110      *
223200051110      * Pulizia subfile
223300051110     c                   seton                                        3031
223400051110     c                   write     TB06C1
223500051110     c                   setoff                                         3133
223600051110      *
223700051110      * Final step - Write the records back to the subfile.
223800051122     c                   eval      QLGRT00  = Get
223900051122do  1c                   do        RrnLast       Nrr
224000051110     c                   clear                   QUSEI
224100051122     c                   eval      QUSBPRV  = %size(QUSEC)
224200051110     c                   call      'QLGSRTIO'
224300051110     c                   parm                    QLGSCB00
224400051110     c                   parm                    NotUsed
224500051110     c                   parm                    SflRcd
224600051110     c                   parm                    QLGRL00
224700051110     c                   parm                    NotUsed
224800051110     c                   parm                    QUSEC
224900051122      * - Ripristino indicatori utilizzati nel sfl rec
225000051122     c                   eval      *in32   =  (VS1opz <> *blanks)
225100130201     c                   eval      *in41   =  ($Called = *on  and
225200130201     c                                         D06op0  = *blank)
225300130315     c                   eval      *in42   =  (VH1abl  <> *blanks)
225400051122     c                   z-add     Nrr           S01nrr
225500051110     c                   WRITE     TB06S1
225600051110e   1c                   enddo
225700051122      *
225800051122      * Visualizzazione del SFL (se ci sono dati)
225900051122     c                   eval      *in30   =  (S01nrr = *zeros)
226000051122      *
226100051122      * Attivazione (eventuale) del SFLEND
226200051122     c                   eval      *in33   =  $EoF
226300051110      *
226400051110     c                   ENDSR
226500051110
226600051110      *---------------------------------------------------------------*
226700051110
226800051110     oPRTF198   e            PRTini            2
226900051110     o                       RSUT
227000051110     o                                        +  20 'ELENCO LEGAMI TAB-
227100051110     o                                              ELLE   V A S'
227200051110     o                       SDSpgm           +  20
227300051110     o                       Wdate         Y  +   5
227400051110     o                                        +   5 'Pag.'
227500051110     o                       Page          Z  +   0
227600051110     o          e            PRTini      0
227700051110     o                                        +  40 'ELENCO LEGAMI TAB-
227800051110     o                                              ELLE   V A S'
227900051110     o          e            PRTini      1
228000051110     o                       KNSIF
228100051110     o                       SDSusr           +   1
228200051110     o                                        +  20 '------------------
228300051110     o                                              ------------'
228400051110     o                       Wtime            +  35 '  :  :  '
228500051110     o          e            PRTini      0
228600051110     o                                        +  40 '------------------
228700051110     o                                              ------------'
228800051110      *
228900051110     o          e            PRTpar      5
229000051110     o                                              'PARAMETRI DI LANC-
229100051110     o                                              IO:'
229200051110     o                                        +   5 'Codice cliente ..-
229300051110     o                                              ...:'
229400051110     o                       V1Cksc           +   2
229500051110     o                       V1Dksc           +   2
229600140115     o          e            PRTpar      1
229700140115     o                                           46 'Solo cliente rich-
229800140115     o                                              . .:'
229900140115     o                       VidSoloCli       +   2
230000051110     o          e            PRTpar      0  0
230100051110     o                                              '_________________-
230200051110     o                                              ___'
230300051110      *
230400051110     o          e            PRTpar      5
230500051110     o                                        +  25 'Tabelle da esamin-
230600051110     o                                              are:'
230700051110     o                       V1Ctb1           +   2
230800051110     o                       V1Dtb1           +   2
230900051110     o                       V1Cse1           +   2
231000051110     o                                        +   2 '(S=Si)'
231100051110     o          e            PRTpar      1
231200051110     o                       V1Ctb2           +  48
231300051110     o                       V1Dtb2           +   2
231400051110     o                       V1Cse2           +   2
231500051110     o                                        +   2 '(S=Si)'
231600051110     o          e            PRTpar      1
231700051110     o                       V1Ctb3           +  48
231800051110     o                       V1Dtb3           +   2
231900051110     o                       V1Cse3           +   2
232000051110     o                                        +   2 '(S=Si)'
232100051110     o          e            PRTpar      1
232200051110     o                       V1Ctb4           +  48
232300051110     o                       V1Dtb4           +   2
232400051110     o                       V1Cse4           +   2
232500051110     o                                        +   2 '(S=Si)'
232600060202     o          e            PRTpar      1
232700060202     o                       V1Ctb5           +  48
232800060202     o                       V1Dtb5           +   2
232900060202     o                       V1Cse5           +   2
233000060202     o                                        +   2 '(S=Si)'
233100060217     o          e            PRTpar      1
233200060217     o                       V1Ctb6           +  48
233300060217     o                       V1Dtb6           +   2
233400060217     o                       V1Cse6           +   2
233500060217     o                                        +   2 '(S=Si)'
233600060410     o          e            PRTpar      1
233700060410     o                       V1Ctb7           +  48
233800060410     o                       V1Dtb7           +   2
233900060410     o                       V1Cse7           +   2
234000060410     o                                        +   2 '(S=Si)'
234100120111     o          e            PRTpar      1
234200120111     o                       V1Ctb8           +  48
234300120111     o                       V1Dtb8           +   2
234400120111     o                       V1Cse8           +   2
234500120111     o                                        +   2 '(S=Si)'
234600120228     o          e            PRTpar      1
234700120228     o                       V1Ctb9           +  48
234800120228     o                       V1Dtb9           +   2
234900120228     o                       V1Cse9           +   2
235000120228     o                                        +   2 '(S=Si)'
235100130129     o          e            PRTpar      1
235200130129     o                       V1Ctb10          +  48
235300130129     o                       V1Dtb10          +   2
235400130129     o                       V1Cse10          +   2
235500130129     o                                        +   2 '(S=Si)'
235600051110      *
235700051110     o          e            PRTtxt      3
235800051110     o                       $Txt(1)
235900051110     o                       $Txt(4)          +   0
236000051110     o                       $Txt(7)          +   0
236100051110     o          e            PRTtxt      1
236200051110     o                       $Txt(2)
236300051110     o                       $Txt(5)          +   0
236400051110     o                       $Txt(8)          +   0
236500051110      *
236600051110     o          e            PRTdet      1
236700060202     o                       VS1cod
236800051110     o                       VS1key           +   4
236900051110     o                       VH1dke           +   1
237000051110     o                       VS1cu1           +   3
237100051110     o                       VH1du1           +   1
237200051110     o                       VS1cu2           +   3
237300051110     o                       VH1du2           +   1
237400051110      *
237500051110     o          e            PRTend      3
237600051110     o                                        +  40 '***  FINE  LISTA -
237700051110     o                                               ***'
237800051110     o          e            PRTend      0
237900051110     o                                        +  40 '***  FINE  LISTA -
238000051110     o                                               ***'
238100051109
238200051109      *---------------------------------------------------------------*
238300051109
238400051109**   $Msg -------------------------------------------------------------------*
238500051110Immettere un codice cliente                                                    1
238600051110Codice cliente errato                                                          2
238700051110Selezionare almeno una tabella da esaminare                                    3
238800051110Opzione non valida                                                             4
238900060202**   $Sql_TABEL -------------------------------------------------------------*
239000100803SELECT ' ' concat TBLcod as TABcod, TBLkey as TABke1, TBLuni as TABdat         1
239100100803  FROM TABEL00F                                                                2
239200100803 WHERE TBLflg   =  ' '                                                         3
239300051110       (TBLcod  =  '                                                           4
239400051110   and  (TBLkey =  '                                                           5
239500051110    or   substr(TBLuni,                                                        6
239600060202**   $Sql_TNTBE -------------------------------------------------------------*
239700060202SELECT TBEcod as TABcod, TBEke1 as TABke1, TBEke2 as TABdat                    1
239800060410SELECT TBEcod as TABcod, TBEke1 as TABke1, TBEuni as TABdat                    2
239900100803  FROM TNTBE00F                                                                3
240000100803 WHERE TBEatb   =  ' '                                                         4
240100060410       (TBEcod  =  '                                                           5
240200060410   and  (TBEke1 =  '                                                           6
240300060410    or   TBEke2 =  '                                                           7
240400060410    or   substr(TBEuni,                                                        8
240500060202**   $Cmd -------------------------------------------------------------------*
240600051109OVRPRTF FILE(PRTF198) USRDTA('Legami_VAS')
240700051109DLTOVR  FILE(PRTF198)
240800051110**   $Txt -------------------------------------------------------*
240900051110Tab.   Cliente                                       Cliente Unifi
241000051110----   -------------------------------------------   -------------
241100051110 12    1234567 *...+....1....+....2....+....3....+   1234567 *...+
241200051110cante (1)                        Cliente Unificante (2)
241300051110------------------------------   ---------------------------------
241400051110....1....+....2....+....3....+   1234567 *...+....1....+....2....+
241500051110
241600051110----------
241700051110....3....+
