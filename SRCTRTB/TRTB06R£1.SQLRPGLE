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
028100051109      *
028200051109      * Operazioni iniziali
028300051110     c                   exsr      OpeIni
028400051109      *
028500051109      * Gestione Video
028600051109do  1c                   DOW       $Fine   =  *off
028700051109      * - Filtro di lancio
028800051109cas 2c     $Video        caseq     '1'           GesD01
028900051109      * - SubFile
029000051109cas 2c     $Video        caseq     '2'           GesS01
029100051109e   2c                   endcs
029200051109e   1c                   ENDDO
029300051109      *
029400051110      * Fine programma
029500051110     c                   eval      *inLR   =  *on
029600051109      *
029700051109      *
029800051109      *
029900051109      *?S U B R O U T I N E S?----------------------------------------*
030000051109      *
030100051109      *---------------------------------------------------------------*
030200051109      *?Operazioni iniziali                                          ?*
030300051109      *---------------------------------------------------------------*
030400051110     c     OpeIni        BEGSR
030500051109      *
030600051109      * Reperisce dati job
030700051109     c     *dtaara       define    §azute        azuteds
030800051109     c     *dtaara       define    §datiute      ddatiute
030900051109      *
031000051109     c                   in(E)     *dtaara
031100051109     c                   IF        %ERROR or RSUT = *blanks
031200051109     c                   clear                   Tibs34Ds
031300051109     c                   call      'TIBS34R'
031400051109     c                   parm                    Tibs34Ds
031500051109     c                   in        *dtaara
031600051109     c                   ENDIF
031700051110      *
031800060217     c                   movel     SDSpgm        T1pgm
031900091214      *
032000091214      /free
032100091214         // -?Verifica se chiamato (con codice cliente come parametro);?
032200091214         //  ?se chiamato: salta la 1ª videata?
032300100211         if  %subst( KPJBU : 1 : 7 ) > *zero;
032400091214           $Called = *on;
032500091214           $InzD01 = *off;
032600091214           exsr  InzD01;
032700100211           //V1Cksc = %int( %subst( KPJBU : 1 : %len(V1Cksc) ) );
032800100211           TRTB06ds = kpjbu;
032900100211           V1Cksc = D06ksc;
033000100211           if  D063C  <> *blank   or
033100100211               D063K  <> *blank   or
033200100211               D063R  <> *blank   or
033300100211               D064C  <> *blank   or
033400100211               D063Q  <> *blank   or
033500100211               D06VAP <> *blank   or
033600120111               D06DSC <> *blank   or
033700120112               D06LAC <> *blank   or
033800130129               D06DKC <> *blank   or
033900120112               D06OSR <> *blank;
034000100211             V1Cse1 = D063C;
034100100211             V1Cse2 = D063K;
034200100211             V1Cse3 = D063R;
034300100211             V1Cse4 = D064C;
034400100211             V1Cse5 = D063Q;
034500100211             V1Cse6 = D06VAP;
034600100211             V1Cse7 = D06DSC;
034700120111             V1Cse8 = D06LAC;
034800130129             V1Cse9 = D06DKC;
034900130129             V1Cse10 = D06OSR;
035000100211           endif;
035100091214           exsr  CtrD01;
035200091214           if  *in90;
035300091214             write  TB06T1;
035400130129           //else;
035500130129           //  $Video = '2';
035600091214           endif;
035700130129           *in51 = *on;
035800091214         endif;
035900091214      /end-free
036000051109      *
036100051109     c                   ENDSR
036200051109      *
036300051109      *---------------------------------------------------------------*
036400051110      *?Gestione videata D01                                         ?*
036500051109      *---------------------------------------------------------------*
036600051109     c     GesD01        BEGSR
036700051109      *
036800051109      * Inizializzo la videata
036900051109if  1c                   if        $InzD01 =  *on
037000051109     c                   exsr      InzD01
037100051110     c                   eval      $InzD01 =  *off
037200051109e   1c                   endif
037300051109      *
037400051109      * Scrivo la testata e la riga tasti funzionali abilitati
037500051109if  1c                   if        NOT *in90
037600051109     c                   write     TB06T1
037700051109e   1c                   endif
037800051109      * Emetto la videata
037900051109     c                   exfmt     TB06D1
038000051109     c                   setoff                                       28  90
038100051109     c                   clear                   V1Dmsg
038200051109      *
038300051109sel 1c                   select
038400051109      * F3=Fine
038500051109w   1c                   when      *inKC
038600051109     c                   exsr      F03D01
038700051109     c                   leavesr
038800051123      * F7=Ricerca cliente
038900051123w   1c                   when          *inKG
039000051123     c                             and *in45  =  *off
039100051123     c                   eval      *in45   =  *on
039200051123     c                   eval      WinKG   =  *on
039300051123     c                   leavesr
039400051123      * Ricerca alfabetica se:
039500051123      *   · è stato premuto F7
039600051123w   1c                   when          *in45  =  *on
039700051123     c                             and WinKG  =  *on
039800051123      *   · il codice cliente è vuoto
039900051123     c                             and V1Cksc =  *zeros
040000051123      *   · la ragione sociale è piena
040100051123     c                             and V1Dksc <> *blanks
040200051123     c                   exsr      SearchCli
040300051124     c                   leavesr
040400051109      *
040500051109e   1c                   endsl
040600051109      *
040700051109      * Controllo dati immessi a video
040800051109     c                   exsr      CtrD01
040900051109     c                   if        *in90
041000051109     c                   leavesr
041100051109     c                   endif
041200051109      *
041300051109      * Passaggio al subfile
041400051109     c                   eval      $InzS01 =  *on
041500051109     c                   eval      $Video  =  '2'
041600051109      *
041700051109     c                   ENDSR
041800051109      *
041900051109      *---------------------------------------------------------------*
042000051110      *?Inizializzazione videata D01                                 ?*
042100051109      *---------------------------------------------------------------*
042200051109     c     InzD01        BEGSR
042300051109      *
042400051109     c                   clear                   TB06D1
042500051109      *
042600051109      * Imposto i dati di default
042700051123     c                   eval      *in45   =  *off
042800051109     c                   eval      V1Ctb1  =  C_tab1
042900051109     c                   eval      V1Ctb2  =  C_tab2
043000051109     c                   eval      V1Ctb3  =  C_tab3
043100051109     c                   eval      V1Ctb4  =  C_tab4
043200060202     c                   eval      V1Ctb5  =  C_tab5
043300060217     c                   eval      V1Ctb6  =  C_tab6
043400060410     c                   eval      V1Ctb7  =  C_tab7
043500120111     c                   eval      V1Ctb8  =  C_tab8
043600120112     c                   eval      V1Ctb9  =  C_tab9
043700130129     c                   eval      V1Ctb10 =  C_tab10
043800051109     c                   eval      V1Cse1  =  'S'
043900051109     c                   eval      V1Cse2  =  'S'
044000051109     c                   eval      V1Cse3  =  'S'
044100051109     c                   eval      V1Cse4  =  'S'
044200060202     c                   eval      V1Cse5  =  'S'
044300060217     c                   eval      V1Cse6  =  'S'
044400060410     c                   eval      V1Cse7  =  'S'
044500120111     c                   eval      V1Cse8  =  'S'
044600120112     c                   eval      V1Cse9  =  'S'
044700130129     c                   eval      V1Cse10 =  'S'
044800051109      *
044900051109      * Decodifica tabelle
045000051109     c                   eval      TBLkut  =  1
045100051109     c                   clear                   TBLcod
045200051109     c                   eval      TBLkey  =  *zeros
045300060202     c                   clear                   TBEcod
045400060202     c                   movel     *zeros        TBEke1
045500060202     c                   clear                   TBEke2
045600060202     c                   clear                   TBElin
045700060202     c                   movel     KNSIF         TBEsif
045800051109      * - 1ª tabella ('3C')
045900051109if  1c                   if        V1Ctb1  <> *blanks
046000060202     c***                move      V1Ctb1        TBLkey
046100060202     c                   eval      %subst(TBLkey:7:2) =
046200060202     c                             %subst(V1Ctb1:1:2)
046300051109     c     K03TABEL      chain     TABEL
046400130808     c                   if        %found(TABEL00F) and
046500130808     c                             TBLflg  =  *blanks
046600051109     c                   movel     TBLuni        DS
046700060202     c*** già impostato: move      TBLkey        V1Ctb1
046800051109     c                   movel     §des1         V1Dtb1
046900051109     c                   movel     §pgm          V1Hpg1
047000051109     c                   endif
047100051109e   1c                   endif
047200051109      * - 2ª tabella ('3K')
047300051123if  1c                   if        V1Ctb2  <> *blanks
047400060202     c***                move      V1Ctb2        TBLkey
047500060202     c                   eval      %subst(TBLkey:7:2) =
047600060202     c                             %subst(V1Ctb2:1:2)
047700051109     c     K03TABEL      chain     TABEL
047800130808     c                   if        %found(TABEL00F) and
047900130808     c                             TBLflg  =  *blanks
048000051109     c                   movel     TBLuni        DS
048100060202     c*** già impostato: move      TBLkey        V1Ctb2
048200051109     c                   movel     §des1         V1Dtb2
048300051109     c                   movel     §pgm          V1Hpg2
048400051109     c                   endif
048500051123e   1c                   endif
048600051109      * - 3ª tabella ('3R')
048700051123if  1c                   if        V1Ctb3  <> *blanks
048800060202     c***                move      V1Ctb3        TBLkey
048900060202     c                   eval      %subst(TBLkey:7:2) =
049000060202     c                             %subst(V1Ctb3:1:2)
049100051109     c     K03TABEL      chain     TABEL
049200130808     c                   if        %found(TABEL00F) and
049300130808     c                             TBLflg  =  *blanks
049400051109     c                   movel     TBLuni        DS
049500060202     c*** già impostato: move      TBLkey        V1Ctb3
049600051109     c                   movel     §des1         V1Dtb3
049700051109     c                   movel     §pgm          V1Hpg3
049800051109     c                   endif
049900051123e   1c                   endif
050000051109      * - 4ª tabella ('4C')
050100051123if  1c                   if        V1Ctb4  <> *blanks
050200060202     c***                move      V1Ctb4        TBLkey
050300060202     c                   eval      %subst(TBLkey:7:2) =
050400060202     c                             %subst(V1Ctb4:1:2)
050500051109     c     K03TABEL      chain     TABEL
050600130808     c                   if        %found(TABEL00F) and
050700130808     c                             TBLflg  =  *blanks
050800051109     c                   movel     TBLuni        DS
050900060202     c*** già impostato: move      TBLkey        V1Ctb4
051000051109     c                   movel     §des1         V1Dtb4
051100051109     c                   movel     §pgm          V1Hpg4
051200051109     c                   endif
051300051123e   1c                   endif
051400060217      * - 5ª tabella ('3Q')
051500060217if  1c                   if        V1Ctb5  <> *blanks
051600060217     c***                move      V1Ctb5        TBLkey
051700060217     c                   eval      %subst(TBLkey:7:2) =
051800060217     c                             %subst(V1Ctb5:1:2)
051900060217     c     K03TABEL      chain     TABEL
052000130808     c                   if        %found(TABEL00F) and
052100130808     c                             TBLflg  =  *blanks
052200060217     c                   movel     TBLuni        DS
052300060217     c*** già impostato: move      TBLkey        V1Ctb5
052400060217     c                   movel     §des1         V1Dtb5
052500060217     c                   movel     §pgm          V1Hpg5
052600060217     c                   endif
052700060217e   1c                   endif
052800060217      * - 6ª tabella ('VAP')
052900060217if  1c                   if        V1Ctb6  <> *blanks
053000060217     c                   move      V1Ctb6        TBEke1
053100060202     c                   movel     KNSIF         TBEsif
053200060202     c     K05TBE01      chain     TNTBE000
053300060202     c                   if        NOT %found(TNTBE01L)
053400060202     c                   clear                   TBEsif
053500060202     c     K05TBE01      chain     TNTBE000
053600060202     c                   endif
053700130808     c                   if        %found(TNTBE01L) and
053800130808     c                             TBLflg  =  *blanks
053900060217     c                   move      TBEke1        V1Ctb6
054000060217     c                   movel     TBEuni        V1Dtb6
054100060217     c                   move      TBEuni        V1Hpg6
054200060202     c                   endif
054300060202e   1c                   endif
054400060410      * - 7ª tabella ('DSC')
054500060410if  1c                   if        V1Ctb7  <> *blanks
054600060410     c                   move      V1Ctb7        TBEke1
054700060410     c                   movel     KNSIF         TBEsif
054800060410     c     K05TBE01      chain     TNTBE000
054900060410     c                   if        NOT %found(TNTBE01L)
055000060410     c                   clear                   TBEsif
055100060410     c     K05TBE01      chain     TNTBE000
055200060410     c                   endif
055300130808     c                   if        %found(TNTBE01L) and
055400130808     c                             TBLflg  =  *blanks
055500060410     c                   move      TBEke1        V1Ctb7
055600060410     c                   movel     TBEuni        V1Dtb7
055700060410     c                   move      TBEuni        V1Hpg7
055800060410     c                   endif
055900060410e   1c                   endif
056000120111      * - 8ª tabella ('LAC')
056100120111if  1c                   if        V1Ctb8  <> *blanks
056200120111     c                   move      V1Ctb8        TBEke1
056300120111     c                   movel     KNSIF         TBEsif
056400120111     c     K05TBE01      chain     TNTBE000
056500120111     c                   if        NOT %found(TNTBE01L)
056600120111     c                   clear                   TBEsif
056700120111     c     K05TBE01      chain     TNTBE000
056800120111     c                   endif
056900130808     c                   if        %found(TNTBE01L) and
057000130808     c                             TBLflg  =  *blanks
057100120111     c                   move      TBEke1        V1Ctb8
057200120111     c                   movel     TBEuni        V1Dtb8
057300120111     c                   move      TBEuni        V1Hpg8
057400120111     c                   endif
057500120111e   1c                   endif
057600130129      * - 9ª tabella ('DKC')
057700120112if  1c                   if        V1Ctb9  <> *blanks
057800120112     c                   move      V1Ctb9        TBEke1
057900120112     c                   movel     KNSIF         TBEsif
058000120112     c     K05TBE01      chain     TNTBE000
058100120112     c                   if        NOT %found(TNTBE01L)
058200120112     c                   clear                   TBEsif
058300120112     c     K05TBE01      chain     TNTBE000
058400120112     c                   endif
058500130808     c                   if        %found(TNTBE01L) and
058600130808     c                             TBLflg  =  *blanks
058700120112     c                   move      TBEke1        V1Ctb9
058800120112     c                   movel     TBEuni        V1Dtb9
058900120112     c                   move      TBEuni        V1Hpg9
059000120112     c                   endif
059100120112e   1c                   endif
059200130129      * - 10ª tabella ('OSR')
059300130129if  1c                   if        V1Ctb10 <> *blanks
059400130129     c                   move      V1Ctb10       TBEke1
059500130129     c                   movel     KNSIF         TBEsif
059600130129     c     K05TBE01      chain     TNTBE000
059700130129     c                   if        NOT %found(TNTBE01L)
059800130129     c                   clear                   TBEsif
059900130129     c     K05TBE01      chain     TNTBE000
060000130129     c                   endif
060100130808     c                   if        %found(TNTBE01L) and
060200130808     c                             TBLflg  =  *blanks
060300130129     c                   move      TBEke1        V1Ctb10
060400130129     c                   movel     TBEuni        V1Dtb10
060500130129     c                   move      TBEuni        V1Hpg10
060600130129     c                   endif
060700130129e   1c                   endif
060800051109      *
060900051109     c                   ENDSR
061000051109      *
061100051109      *---------------------------------------------------------------*
061200051110      *?Gestione tasto funzionale F03 da videata D01                 ?*
061300051109      *---------------------------------------------------------------*
061400051109     c     F03D01        BEGSR
061500051109      *
061600051109      * Chiudo il programma
061700051109     c                   eval      $Fine   =  *on
061800051109      *
061900051109     c                   ENDSR
062000051109      *
062100051109      *---------------------------------------------------------------*
062200051110      *?Controllo dati immessi in videata D01                        ?*
062300051109      *---------------------------------------------------------------*
062400051109     c     CtrD01        BEGSR
062500051109      *
062600051109     c                   movea     *zeros        *in(50)
062700051109      *
062800051109      * Controllo/Decodifico codice cliente
062900051109     c                   clear                   V1Dksc
063000120807     c                   clear                   V1Dabl
063100051109if  1c                   IF        V1Cksc  =  *zeros
063200051109     c                   seton                                        285090
063300051109     c                   eval      V1Dmsg  =  $Msg(1)
063400051109x   1c                   ELSE
063500051110     c                   move      V1Cksc        Wksc
063600051110     c                   exsr      Decod_Cli
063700051109if  2c                   if        O69err  =  *on
063800051109     c                   seton                                        285090
063900051109     c                   eval      V1Dmsg  =  $Msg(2)
064000051109x   2c                   else
064100051109     c                   movel     ACOrag        V1Dksc
064200130315     c                   if        ACOabl <> *blanks
064300120807     c                   eval      V1Dabl = 'BLOCCATO'
064400120807     c                   endif
064500051109e   2c                   endif
064600051109e   1c                   ENDIF
064700051109     c                   if        *in90
064800051109     c                   leavesr
064900051109     c                   endif
065000051109      *
065100051109      * Controllo selezioni
065200051109     c                   if            V1Cse1 = *blanks
065300051109     c                             and V1Cse2 = *blanks
065400051109     c                             and V1Cse3 = *blanks
065500051109     c                             and V1Cse4 = *blanks
065600060202     c                             and V1Cse5 = *blanks
065700060217     c                             and V1Cse6 = *blanks
065800060410     c                             and V1Cse7 = *blanks
065900120111     c                             and V1Cse8 = *blanks
066000120112     c                             and V1Cse9 = *blanks
066100130129     c                             and V1Cse10 = *blanks
066200051109     c                   seton                                        285190
066300051109     c                   eval      V1Dmsg  =  $Msg(3)
066400051109     c                   leavesr
066500051109     c                   endif
066600060202     c                   if            V1Cse1 = *blanks
066700060202     c                             and V1Cse2 = *blanks
066800060202     c                             and V1Cse3 = *blanks
066900060202     c                             and V1Cse4 = *blanks
067000060217     c                             and V1Cse5 = *blanks
067100060202     c                   eval      $Tabel  =  *off
067200060202     c                   else
067300060202     c                   eval      $Tabel  =  *on
067400060202     c                   endif
067500060217     c                   if            V1Cse6 = *blanks
067600060410     c                   eval      $Tntbe2 =  *off
067700060202     c                   else
067800060410     c                   eval      $Tntbe2 =  *on
067900060202     c                   endif
068000060410     c                   if            V1Cse7 = *blanks
068100120111     c                             and V1Cse8 = *blanks
068200130129     c                             and V1Cse9 = *blanks
068300060410     c                   eval      $Tntbe1 =  *off
068400060410     c                   else
068500060410     c                   eval      $Tntbe1 =  *on
068600060410     c                   endif
068700130129     c                   if            V1Cse10 = *blanks
068800120112     c                   eval      $Tntbe3 =  *off
068900120112     c                   else
069000120112     c                   eval      $Tntbe3 =  *on
069100120112     c                   endif
069200051109      *
069300051109     c                   ENDSR
069400051123      *
069500051123      *---------------------------------------------------------------*
069600051123      *?Ricerca cliente                                              ?*
069700051123      *---------------------------------------------------------------*
069800051123     c     SearchCli     BEGSR
069900051123      *
070000051123     c                   call      'XALFA3BR'
070100051123     c                   parm      RSUT          PAXdut           30
070200051123     c                   parm                    TBLkut
070300051123     c                   parm      V1Dksc        PAXdmt           48
070400051123     c                   parm      DUTkci        PAXccc            4 0
070500051123     c                   parm      9             PAXsta            1 0
070600051123     c                   parm                    PAXflr           90
070700051123     c                   parm      *blanks       PAXdit            3
070800051123     c                   parm      1             PAXnum            2 0
070900051123     c                   parm                    PAXksc           80
071000051123     c                   parm                    PAXksm          140
071100051123     c                   parm                    PAXkdm           60
071200051123      *
071300051123      *   · Imposto il codice selezionato a video
071400051123      *     (se selezionato e non premuto F12)
071500051123if  1c                   if        PAXsta >= *zeros
071600051123     c                   movel     PAXksm        V1Cksc
071700051123     c                   movel     PAXdmt        V1Dksc
071800051123     c                   eval      *in45   =  *off
071900051123     c                   reset                   WinKG
072000051123e   1c                   endif
072100051123      *
072200051123     c                   ENDSR
072300051109      *
072400051109      *---------------------------------------------------------------*
072500051110      *?Gestione videata C01 / S01                                   ?*
072600051109      *---------------------------------------------------------------*
072700051109     c     GesS01        BEGSR
072800051109      *
072900051109      * Inizializzo la videata
073000051109if  1c                   if        $InzS01 =  *on
073100051109     c                   exsr      InzS01
073200051110     c                   eval      $InzS01 = *off
073300051109e   1c                   endif
073400100216      *
073500100216      * Impostazione attributi di visualizzazione
073600130201     c                   eval      *in41 = ($Called = *on  and
073700130201     c                                      D06op0  = *blank)
073800051110      *
073900051110      * Posiziono il cursore
074000051110if  1c                   if        C01csr  >  *zeros
074100051110     c                   z-add     C01csr        C01rcd
074200051110e   1c                   endif
074300051109      *
074400051109      * Scrivo la testata e la riga tasti funzionali abilitati
074500051110     c                   eval      *in39   =  (C01rcd <= *zeros)
074600051109if  1c                   if        NOT *in90
074700051109     c                   write     TB06T1
074800051110     c                   write     TB06Z1
074900051109e   1c                   endif
075000051110      *
075100051110      * Visualizzo (eventualmente) il msg per la mancanza di dati
075200051110if  1c                   if        C01rcd  <= *zeros
075300051110     c                   write     TB06D2
075400051110e   1c                   endif
075500051109      * Emetto la videata
075600051110     c                   exfmt     TB06C1
075700051110     c                   z-add     £SFLnrr       C01rcd
075800051109     c                   setoff                                       28  90
075900051109     c                   clear                   V1Dmsg
076000051109      *
076100051110sel 1c                   SELECT
076200051109      * F3=Fine
076300051110w   1c                   WHEN      *inKC
076400051109     c                   exsr      F03D01
076500051110      * F8=Stampa elenco legami
076600051110w   1c                   WHEN      *inKH
076700051110     c                   exsr      F08S01
076800051122      * F11=Ordinamento x codice cliente / codice tabella
076900051122w   1c                   WHEN      *inKK
077000051122     c                   exsr      F11S01
077100051110      * F12=Ritorno
077200051110w   1c                   WHEN      *inKL
077300051110     c                   exsr      F12S01
077400051110      * NO rec.
077500051110w   1c                   WHEN      *in39
077600051110      * Gestione opzioni inserite
077700051110x   1c                   OTHER
077800051110     c                   exsr      OpzS01
077900051110      *
078000051110e   1c                   ENDSL
078100051109      *
078200051109     c                   ENDSR
078300051109      *
078400051109      *---------------------------------------------------------------*
078500051110      *?Inizializzazione videata C01 / S01                           ?*
078600051109      *---------------------------------------------------------------*
078700051110     c     InzS01        BEGSR
078800051110      *
078900051110      * Pulizia subfile
079000051110     c                   seton                                        3031
079100051110     c                   write     TB06C1
079200051110     c                   setoff                                         3133
079300051110      *
079400051110     c                   reset                   $EoF
079500120807     c                   reset                   WrkSort
079600051110     c                   clear                   C01rcd
079700051110     c                   clear                   C01csr
079800051110     c                   clear                   S01nrr
079900051110     c                   clear                   V1Dmsg
080000051110     c                   setoff                                       28  90
080100051110     c                   eval      *in32   =  *off
080200051122     c                   eval      *in40   =  (WrkSort = *on)
080300051110     c                   movea     *zeros        *in(50)
080400051110      *
080500051110      * Caricamento dei dati da presentare nel sfl (NON a pagine)
080600051110      *
080700051110      * - Preparazione stringa SQL
080800060202     c                   clear                   wSQL
080900060202      *   - da TABEL00F
081000060202     c                   if        $Tabel  =  *on
081100060202     c                   exsr      PrepSQL1
081200060202     c                   endif
081300060202      *   - da TNTBE00F
081400060410     c                   if            $Tntbe1 =  *on
081500060410     c                             or  $Tntbe2 =  *on
081600120112     c                             or  $Tntbe3 =  *on
081700060202     c                   exsr      PrepSQL2
081800060202     c                   endif
081900060202      *   - impostazioni finali
082000060202     c                   exsr      PrepSQL3
082100051110      *
082200051110      * - Apertura cursore
082300051110     c                   exsr      OpenCursor
082400051110      *
082500051110      * - Esecuzione SQL
082600051110     c                   dou       $Eof    =  *on
082700051110     c                   exsr      ReadCursor
082800051110     c                   enddo
082900051110      *
083000051110      * Chiusura cursore
083100051110     c                   exsr      CloseCursor
083200051110      *
083300051110      * Memorizzo NRR dell'ultimo record scritto nel sfl
083400051110     c                   eval      RRNlast =  S01nrr
083500051110      *
083600051110      * Avendo caricato TUTTI i dati (NON SOLO quelli della 1ª pagina)
083700051110      *  occorre impostare il posizionamento cursore sul 1° rec del sfl
083800051110if  1c                   if        C01csr  >  *zeros
083900051110     c                   eval      C01csr  =  1
084000051110e   1c                   endif
084100051109      *
084200051109     c                   ENDSR
084300051109      *
084400051109      *---------------------------------------------------------------*
084500060202      *?Preparazione stringa SQL per file TABEL00F                   ?*
084600051109      *---------------------------------------------------------------*
084700060202     c     PrepSQL1      BEGSR
084800051109      *
084900051109     c                   movel(p)  V1Cksc        TBLkey
085000051110     c                   reset                   wAndOr
085100051123      *
085200051123      * Reperisco eventuali codici clienti unificanti da tab. 3C
085300051123     c                   clear                   ds3C
085400060410if  1c                   if        V1Cse1  =  'S'
085500060410     c                   eval      TBLcod  = '3C'
085600051123     c     K03TABEL      chain     TABEL
085700051123     c                   if            %found(TABEL00F)
085800051123     c                             and TBLflg = *blanks
085900051123     c                   movel     TBLuni        ds3C
086000051123     c                   endif
086100060410e   1c                   endif
086200051123      * Reperisco eventuali codici clienti unificanti da tab. 3K
086300051123     c                   clear                   ds3K
086400060410if  1c                   if        V1Cse2  =  'S'
086500060410     c                   eval      TBLcod  = '3K'
086600051123     c     K03TABEL      chain     TABEL
086700051123     c                   if            %found(TABEL00F)
086800051123     c                             and TBLflg = *blanks
086900051123     c                   movel     TBLuni        ds3K
087000051123     c                   endif
087100060410e   1c                   endif
087200060217      * Reperisco eventuali codici clienti unificanti da tab. 3Q
087300060217     c                   clear                   ds3Q
087400060410if  1c                   if        V1Cse5  =  'S'
087500060410     c                   eval      TBLcod  = '3Q'
087600060217     c     K03TABEL      chain     TABEL
087700060217     c                   if            %found(TABEL00F)
087800060217     c                             and TBLflg = *blanks
087900060217     c                   movel     TBLuni        ds3Q
088000060217     c                   endif
088100060410e   1c                   endif
088200051123      * Reperisco eventuali codici clienti unificanti da tab. 3R
088300051123     c                   clear                   ds3R
088400060410if  1c                   if        V1Cse3  =  'S'
088500060410     c                   eval      TBLcod  = '3R'
088600051123     c     K03TABEL      chain     TABEL
088700051123     c                   if            %found(TABEL00F)
088800051123     c                             and TBLflg = *blanks
088900051123     c                   movel     TBLuni        ds3R
089000051123     c                   endif
089100060410e   1c                   endif
089200051123      * Reperisco eventuali codici clienti unificanti da tab. 4C
089300051123     c                   clear                   ds4C
089400060410if  1c                   if        V1Cse4  =  'S'
089500060410     c                   eval      TBLcod  = '4C'
089600051123     c     K03TABEL      chain     TABEL
089700051123     c                   if            %found(TABEL00F)
089800051123     c                             and TBLflg = *blanks
089900051123     c                   movel     TBLuni        ds4C
090000051123     c                   endif
090100060410e   1c                   endif
090200051109      *
090300051109      *? E S E M P I O : ?
090400051109      *
090500051109      * select TBLcod, TBLkey, TBLuni
090600051109      * from   TABEL00F
090700051109      * where  TBLflg   =  ' '
090800051215      *   AND ((TBLcod  =  '3C'
090900051109      *    and  (TBLkey =  '0430839 '
091000051109      *     or   substr(TBLuni, 79, 7) = '0430839'))
091100051109      *   OR   (TBLcod  =  '3K'
091200051109      *    and  (TBLkey =  '0430839 '
091300051109      *     or   substr(TBLuni, 27, 7) = '0430839'
091400051109      *     or   substr(TBLuni, 55, 7) = '0430839'))
091500051109      *   OR   (TBLcod  =  '3R'
091600051109      *    and  (TBLkey =  '0430839 '))
091700051109      *   OR   (TBLcod  =  '4C'
091800051109      *    and  (TBLkey =  '0430839 '
091900051109      *     or   substr(TBLuni, 29, 7) = '0430839'
092000051215      *     or   substr(TBLuni, 58, 7) = '0430839')))
092100051109      * order  by  TBLcod, TBLkey
092200051109      *
092300060202      *?Impostazioni iniziali per estrazione da TABEL?
092400051109     c                   eval      wSQL    =  %trim($Sql_TABEL(1))
092500051109     c                                     +  ' '
092600051109     c                                     +  %trim($Sql_TABEL(2))
092700051109     c                                     +  ' '
092800051109     c                                     +  %trim($Sql_TABEL(3))
092900051215     c*** ancora così:   eval      wAndOr  =  'AND ('
093000051109      *
093100051109      *?Impostazione estrazione dati da tabella "3C"?
093200051109      *
093300051110if  1c                   if        V1Cse1  =  'S'
093400120112     c                   eval      wSQL   +=  ' '
093500051109     c                                     +  %trim(wAndOr)
093600060202     c                                     +  ' '
093700051109     c                                     +  %trim($Sql_TABEL(4))
093800051109     c                                     +  C_tab1 +  ''''
093900051109     c                                     +  ' '
094000051109     c                                     +  %trim($Sql_TABEL(5))
094100051109     c                                     +  TBLkey +  ''''
094200051123     c                   exsr      Add_Sel_3C
094300051215     c                   eval      wAndOr  =  'OR   '
094400051109e   1c                   endif
094500051109      *
094600051109      *?Impostazione estrazione dati da tabella "3K"?
094700051109      *
094800051110if  1c                   if        V1Cse2  =  'S'
094900120112     c                   eval      wSQL   +=  ' '
095000051109     c                                     +  %trim(wAndOr)
095100051109     c                                     +  ' '
095200051109     c                                     +  %trim($Sql_TABEL(4))
095300051109     c                                     +  C_tab2 +  ''''
095400051109     c                                     +  ' '
095500051109     c                                     +  %trim($Sql_TABEL(5))
095600051109     c                                     +  TBLkey +  ''''
095700051123     c                   exsr      Add_Sel_3K
095800051215     c                   eval      wAndOr  =  'OR   '
095900051109e   1c                   endif
096000051109      *
096100051109      *?Impostazione estrazione dati da tabella "3R"?
096200051109      *
096300051110if  1c                   if        V1Cse3  =  'S'
096400120112     c                   eval      wSQL   +=  ' '
096500051109     c                                     +  %trim(wAndOr)
096600051109     c                                     +  ' '
096700051109     c                                     +  %trim($Sql_TABEL(4))
096800051109     c                                     +  C_tab3 +  ''''
096900051109     c                                     +  ' '
097000051109     c                                     +  %trim($Sql_TABEL(5))
097100051109     c                                     +  TBLkey +  ''''
097200051123     c                   exsr      Add_Sel_3R
097300051215     c                   eval      wAndOr  =  'OR   '
097400051109e   1c                   endif
097500051109      *
097600051109      *?Impostazione estrazione dati da tabella "4C"?
097700051109      *
097800051110if  1c                   if        V1Cse4  =  'S'
097900120112     c                   eval      wSQL   +=  ' '
098000051109     c                                     +  %trim(wAndOr)
098100051109     c                                     +  ' '
098200051109     c                                     +  %trim($Sql_TABEL(4))
098300051109     c                                     +  C_tab4 +  ''''
098400051109     c                                     +  ' '
098500051109     c                                     +  %trim($Sql_TABEL(5))
098600051109     c                                     +  TBLkey +  ''''
098700051123     c                   exsr      Add_Sel_4C
098800051215     c                   eval      wAndOr  =  'OR   '
098900051109e   1c                   endif
099000060217      *
099100060217      *?Impostazione estrazione dati da tabella "3Q"?
099200060217      *
099300060217if  1c                   if        V1Cse5  =  'S'
099400120112     c                   eval      wSQL   +=  ' '
099500060217     c                                     +  %trim(wAndOr)
099600060217     c                                     +  ' '
099700060217     c                                     +  %trim($Sql_TABEL(4))
099800060217     c                                     +  C_tab5 +  ''''
099900060217     c                                     +  ' '
100000060217     c                                     +  %trim($Sql_TABEL(5))
100100060217     c                                     +  TBLkey +  ''''
100200060217     c                   exsr      Add_Sel_3Q
100300060217     c                   eval      wAndOr  =  'OR   '
100400060217e   1c                   endif
100500051109      *
100600060202      *?chiusura?
100700051109      *
100800120112     c                   eval      wSQL   +=  ')'
100900051109      *
101000051109     c                   ENDSR
101100060202      *
101200060202      *---------------------------------------------------------------*
101300060202      *?Preparazione stringa SQL per file TNTBE00F                   ?*
101400060202      *---------------------------------------------------------------*
101500060202     c     PrepSQL2      BEGSR
101600060202      *
101700060202     c                   reset                   wAndOr
101800060202      *
101900060202      * Reperisco codice cliente di invio da tab. VAP
102000060410     c*****              clear                   dVAP
102100060410if  1c                   if        V1Cse6  =  'S'
102200060410     c                   eval      TBEcod  =  C_tab6
102300060202     c                   move      V1Cksc        WkscA
102400060410     c                   eval      TBEke1  =  '0' + WkscA
102500060202     c                   clear                   TBEke2
102600060202     c     K02TNTBE      chain     TNTBE000
102700060202     c                   if        NOT %found(TNTBE01L)
102800060202     c                             or  TBEatb <> *blanks
102900060202     c                   clear                   TBEke2
103000060202     c                   endif
103100060410e   1c                   endif
103200060410      * Reperisco eventuali codici clienti unificanti da tab. DSC
103300060410     c                   clear                   dDSC
103400060410if  1c                   if        V1Cse7  =  'S'
103500060410     c                   eval      TBEcod  =  C_tab7
103600060410     c                   movel(p)  V1Cksc        TBEke1
103700060410     c                   clear                   TBEke2
103800060410     c                   clear                   TBElin
103900060410     c                   clear                   TBEsif
104000060410     c     K05TBE01      chain     TNTBE000
104100060410     c                   if            %found(TNTBE01L)
104200060410     c                             and TBEatb = *blanks
104300060410     c                   movel     TBEuni        dDSC
104400060410     c                   endif
104500060410e   1c                   endif
104600120111      * Reperisco eventuali codici clienti da tab. LAC
104700120111     c                   clear                   dLAC
104800120111if  1c                   if        V1Cse8  =  'S'
104900120111     c                   eval      TBEcod  =  C_tab8
105000120111     c                   movel(p)  V1Cksc        TBEke1
105100120111     c                   clear                   TBEke2
105200120111     c                   clear                   TBElin
105300120111     c                   clear                   TBEsif
105400120111     c     K05TBE01      chain     TNTBE000
105500120111     c                   if            %found(TNTBE01L)
105600120111     c                             and TBEatb = *blanks
105700120111     c                   movel     TBEuni        dLAC
105800120111     c                   endif
105900120111e   1c                   endif
106000130129      * La tab. DKC non ha altri codici...
106100060202      *
106200060202      *?UNION?
106300060202if  1c                   if        $Tabel  =  *on
106400120112     c                   eval      wSQL   +=  ' '
106500060202     c                                     +  C_Union
106600060202e   1c                   endif
106700060202      *
106800060410      *?Impostazioni iniziali per estrazione da TNTBE (ke1+ke2)?
106900060410if  1c                   if        $Tntbe2 =  *on
107000120112     c                   eval      wSQL   +=  ' '
107100060202     c                                     +  %trim($Sql_TNTBE(1))
107200060202     c                                     +  ' '
107300060410     c                                     +  %trim($Sql_TNTBE(3))
107400060202     c                                     +  ' '
107500060410     c                                     +  %trim($Sql_TNTBE(4))
107600060202     c                   eval      wAndOr  =  'AND ('
107700060410e   1c                   endif
107800060202      *
107900060202      *?Impostazione estrazione dati da tabella "VAP"?
108000060202      *
108100060217if  1c                   if        V1Cse6  =  'S'
108200120112     c                   eval      wSQL   +=  ' '
108300060202     c                                     +  %trim(wAndOr)
108400060202     c                                     +  ' '
108500060410     c                                     +  %trim($Sql_TNTBE(5))
108600060217     c                                     +  C_tab6 +  ''''
108700060202     c                                     +  ' '
108800060410     c                                     +  %trim($Sql_TNTBE(6))
108900060410     c                                     +  '0' + WkscA
109000060410     c                                     +  ''''
109100060202     c                   exsr      Add_Sel_VAP
109200060202     c                   eval      wAndOr  =  'OR   '
109300060202e   1c                   endif
109400100803      *
109500060410if  1c                   if            $Tntbe1 =  *on
109600060410     c                             and $Tntbe2 =  *on
109700100803      *
109800100803      *?chiusura 1ª selezione da TNTBE00F (ke1+ke2)?
109900100803      *
110000120112     c                   eval      wSQL   +=  ')'
110100100803      *
110200100803      *?UNION?
110300120112     c                   eval      wSQL   +=  ' '
110400060410     c                                     +  C_Union
110500100803      *
110600060410e   1c                   endif
110700060410      *
110800060410      *?Impostazioni iniziali per estrazione da TNTBE (ke1+uni)?
110900060410if  1c                   if        $Tntbe1 =  *on
111000120112     c                   eval      wSQL   +=  ' '
111100060410     c                                     +  %trim($Sql_TNTBE(2))
111200060410     c                                     +  ' '
111300060410     c                                     +  %trim($Sql_TNTBE(3))
111400060410     c                                     +  ' '
111500060410     c                                     +  %trim($Sql_TNTBE(4))
111600060410     c                   eval      wAndOr  =  'AND ('
111700060410e   1c                   endif
111800060410      *
111900060410      *?Impostazione estrazione dati da tabella "DSC"?
112000060410      *
112100060410if  1c                   if        V1Cse7  =  'S'
112200120112     c                   eval      wSQL   +=  ' '
112300060410     c                                     +  %trim(wAndOr)
112400060410     c                                     +  ' '
112500060410     c                                     +  %trim($Sql_TNTBE(5))
112600060410     c                                     +  C_tab7 +  ''''
112700060410     c                                     +  ' '
112800060410     c                                     +  %trim($Sql_TNTBE(6))
112900120112     c                                     +  %trimr(TBEke1) +  ''''
113000060410     c                   exsr      Add_Sel_DSC
113100060410     c                   eval      wAndOr  =  'OR   '
113200060410e   1c                   endif
113300120111      *
113400120111      *?Impostazione estrazione dati da tabella "LAC"?
113500120111      *
113600120111if  1c                   if        V1Cse8  =  'S'
113700120112     c                   eval      wSQL   +=  ' '
113800120111     c                                     +  %trim(wAndOr)
113900120111     c                                     +  ' '
114000120111     c                                     +  %trim($Sql_TNTBE(5))
114100120111     c                                     +  C_tab8 +  ''''
114200120111     c                                     +  ' '
114300120111     c                                     +  %trim($Sql_TNTBE(6))
114400120112     c                                     +  %trimr(TBEke1) +  ''''
114500120111     c                   exsr      Add_Sel_LAC
114600120111     c                   eval      wAndOr  =  'OR   '
114700120111e   1c                   endif
114800130129      *
114900130129      *?Impostazione estrazione dati da tabella "DKC"?
115000130129      *
115100130129if  1c                   if        V1Cse9  =  'S'
115200130129     c                   eval      wSQL   +=  ' '
115300130129     c                                     +  %trim(wAndOr)
115400130129     c                                     +  ' '
115500130129     c                                     +  %trim($Sql_TNTBE(5))
115600130129     c                                     +  C_tab9 +  ''''
115700130129     c                                     +  ' '
115800130129     c                                     +  %trim($Sql_TNTBE(6))
115900130201     c                                     +  %editc( V1Cksc : 'X' )
116000130201     c                                     +  ''''
116100130129     c*//                exsr      Add_Sel_DKC
116200130129     c                   eval      wSQL   +=  '))'
116300130129     c                   eval      wAndOr  =  'OR   '
116400130129e   1c                   endif
116500120112      *
116600120112if  1c                   if           ($Tntbe1 =  *on
116700120112     c                             or  $Tntbe2 =  *on)
116800120112     c                             and $tntbe3 =  *on
116900120112      *
117000120112      *?chiusura 2ª selezione da TNTBE00F (ke1+uni)?
117100120112      *
117200120112     c                   eval      wSQL   +=  ')'
117300120112      *
117400120112      *?UNION?
117500120112     c                   eval      wSQL   +=  ' '
117600120112     c                                     +  C_Union
117700120112      *
117800120112e   1c                   endif
117900120112      *
118000120112      *?Impostazioni iniziali per estrazione da TNTBE (uni)?
118100120112if  1c                   if        $Tntbe3 =  *on
118200120112     c                   eval      wSQL   +=  ' '
118300120112     c                                     +  %trim($Sql_TNTBE(2))
118400120112     c                                     +  ' '
118500120112     c                                     +  %trim($Sql_TNTBE(3))
118600120112     c                                     +  ' '
118700120112     c                                     +  %trim($Sql_TNTBE(4))
118800120112     c                   eval      wAndOr  =  'AND ('
118900120112e   1c                   endif
119000120112      *
119100120112      *?Impostazione estrazione dati da tabella "OSR"?
119200120112      *
119300130129if  1c                   if        V1Cse10 =  'S'
119400120112     c                   eval      wSQL   +=  ' '
119500120112     c                                     +  %trim(wAndOr)
119600120112     c                                     +  ' '
119700120112     c                                     +  %trim($Sql_TNTBE(5))
119800130129     c                                     +  C_tab10 +  ''''
119900120112     c                   exsr      Add_Sel_OSR
120000120112     c                   eval      wAndOr  =  'OR   '
120100120112e   1c                   endif
120200120112      *
120300120112      *?chiusura 3ª selezione da TNTBE00F (uni)?
120400120112      *
120500120112     c                   eval      wSQL   +=  ')'
120600060202      *
120700060202     c                   ENDSR
120800060202      *
120900060202      *---------------------------------------------------------------*
121000060202      *?Impostazioni finali nella stringa SQL                        ?*
121100060202      *---------------------------------------------------------------*
121200060202     c     PrepSQL3      BEGSR
121300060202      *
121400060202      *
121500120112     c                   eval      wSQL   +=  ' '
121600060202      *?order by?
121700120112     c                                     +  C_OrderBy
121800060202      *?for fatch only?
121900060202     c                                     +  ' '
122000060202     c                                     +  C_ForFetch
122100120112      *
122200120112     c                   movel     wSQL          ds_Comodo
122300060202      *
122400060202     c                   ENDSR
122500051109      *
122600051109      *---------------------------------------------------------------*
122700051109      *?Aggiunta selezione tab. "3C"                                 ?*
122800051109      *---------------------------------------------------------------*
122900051123     c     Add_Sel_3C    BEGSR
123000051109      *
123100120112     c                   eval      wSQL   +=  ' '
123200051123      * aggiunta selezione per §3CCKS
123300051109     c                                     +  %trim($Sql_TABEL(6))
123400051109     c                                     +  ' 79, 7) = '''
123500051109     c                                     +  %trim(TBLkey)
123600051109     c                                     +  ''''
123700051123      * aggiunta selezione per stesso unificante (se diverso)
123800051123     c                   move      §3Ccks        WkscA
123900051123     c                   if            WkscA   > *zeros
124000051123     c                             and WkscA  <> TBLkey
124100120112     c                   eval      wSQL   +=  ' '
124200051123     c                                     +  %trim($Sql_TABEL(6))
124300051123     c                                     +  ' 79, 7) = '''
124400051123     c                                     +  WkscA
124500051123     c                                     +  ''''
124600051123     c                   endif
124700051109      * chiusura
124800120112     c                   eval      wSQL   +=  '))'
124900051109      *
125000051109     c                   ENDSR
125100051109      *
125200051109      *---------------------------------------------------------------*
125300051109      *?Aggiunta selezione tab. "3K"                                 ?*
125400051109      *---------------------------------------------------------------*
125500051123     c     Add_Sel_3K    BEGSR
125600051109      *
125700120112     c                   eval      wSQL   +=  ' '
125800051109      * aggiunta selezione per §3KCKS
125900051109     c                                     +  %trim($Sql_TABEL(6))
126000051109     c                                     +  ' 27, 7) = '''
126100051109     c                                     +  %trim(TBLkey)
126200051109     c                                     +  ''''
126300051109      * aggiunta selezione per §3KGKS
126400051109     c                                     +  ' '
126500051109     c                                     +  %trim($Sql_TABEL(6))
126600051109     c                                     +  ' 55, 7) = '''
126700051109     c                                     +  %trim(TBLkey)
126800051109     c                                     +  ''''
126900051123      * aggiunta selezione per stesso unificante-consegna (se diverso)
127000051123     c                   move      §3Kcks        WkscA
127100051123     c                   if            WkscA   > *zeros
127200051123     c                             and WkscA  <> TBLkey
127300120112     c                   eval      wSQL   +=  ' '
127400051123     c                                     +  %trim($Sql_TABEL(6))
127500051123     c                                     +  ' 27, 7) = '''
127600051123     c                                     +  WkscA
127700051123     c                                     +  ''''
127800051123     c                   endif
127900051123      * aggiunta selezione per stesso unificante-giacenza (se diverso)
128000051123     c                   move      §3Kgks        WkscA
128100051123     c                   if            WkscA   > *zeros
128200051123     c                             and WkscA  <> TBLkey
128300120112     c                   eval      wSQL   +=  ' '
128400051123     c                                     +  %trim($Sql_TABEL(6))
128500051123     c                                     +  ' 55, 7) = '''
128600051123     c                                     +  WkscA
128700051123     c                                     +  ''''
128800051123     c                   endif
128900051109      * chiusura
129000120112     c                   eval      wSQL   +=  '))'
129100051109      *
129200051109     c                   ENDSR
129300060217      *
129400060217      *---------------------------------------------------------------*
129500060217      *?Aggiunta selezione tab. "3Q"                                 ?*
129600060217      *---------------------------------------------------------------*
129700060217     c     Add_Sel_3Q    BEGSR
129800060217      *
129900120112     c                   eval      wSQL   +=  ' '
130000060217      * aggiunta selezione per §3QCKS
130100060217     c                                     +  %trim($Sql_TABEL(6))
130200060217     c                                     +  ' 13, 7) = '''
130300060217     c                                     +  %trim(TBLkey)
130400060217     c                                     +  ''''
130500060217      * chiusura
130600120112     c                   eval      wSQL   +=  '))'
130700060217      *
130800060217     c                   ENDSR
130900051109      *
131000051109      *---------------------------------------------------------------*
131100051109      *?Aggiunta selezione tab. "3R"                                 ?*
131200051109      *---------------------------------------------------------------*
131300051123     c     Add_Sel_3R    BEGSR
131400051109      *
131500051123      * chiusura
131600120112     c                   eval      wSQL   +=  '))'
131700051109      *
131800051109     c                   ENDSR
131900051109      *
132000051109      *---------------------------------------------------------------*
132100051109      *?Aggiunta selezione tab. "4C"                                 ?*
132200051109      *---------------------------------------------------------------*
132300051123     c     Add_Sel_4C    BEGSR
132400051109      *
132500120112     c                   eval      wSQL   +=  ' '
132600051109      * aggiunta selezione per §4CCKS
132700051109     c                                     +  %trim($Sql_TABEL(6))
132800051109     c                                     +  ' 29, 7) = '''
132900051109     c                                     +  %trim(TBLkey)
133000051109     c                                     +  ''''
133100051109      * aggiunta selezione per §4CFKS
133200051109     c                                     +  ' '
133300051109     c                                     +  %trim($Sql_TABEL(6))
133400051109     c                                     +  ' 58, 7) = '''
133500051109     c                                     +  %trim(TBLkey)
133600051109     c                                     +  ''''
133700051123      * aggiunta selezione per stesso unificante-consegna (se diverso)
133800051123     c                   move      §4Ccks        WkscA
133900051123     c                   if            WkscA   > *zeros
134000051123     c                             and WkscA  <> TBLkey
134100120112     c                   eval      wSQL   +=  ' '
134200051123     c                                     +  %trim($Sql_TABEL(6))
134300051123     c                                     +  ' 29, 7) = '''
134400051123     c                                     +  WkscA
134500051123     c                                     +  ''''
134600051123     c                   endif
134700051123      * aggiunta selezione per stesso unificante-fatturaz (se diverso)
134800051123     c                   move      §4Cfks        WkscA
134900051123     c                   if            WkscA   > *zeros
135000051123     c                             and WkscA  <> TBLkey
135100120112     c                   eval      wSQL   +=  ' '
135200051123     c                                     +  %trim($Sql_TABEL(6))
135300051123     c                                     +  ' 58, 7) = '''
135400051123     c                                     +  WkscA
135500051123     c                                     +  ''''
135600051123     c                   endif
135700051109      * chiusura
135800120112     c                   eval      wSQL   +=  '))'
135900051109      *
136000051109     c                   ENDSR
136100060202      *
136200060202      *---------------------------------------------------------------*
136300060202      *?Aggiunta selezione tab. "VAP"                                ?*
136400060202      *---------------------------------------------------------------*
136500060202     c     Add_Sel_VAP   BEGSR
136600060202      *
136700120112     c                   eval      wSQL   +=  ' '
136800060202      * aggiunta selezione per TBEke2
136900060410     c                                     +  %trim($Sql_TNTBE(7))
137000060410     c                                     +  '0' + WkscA
137100060410     c                                     +  ''''
137200060202      * chiusura
137300120112     c                   eval      wSQL   +=  '))'
137400060202      *
137500060202     c                   ENDSR
137600060410      *
137700060410      *---------------------------------------------------------------*
137800060410      *?Aggiunta selezione tab. "DSC"                                ?*
137900060410      *---------------------------------------------------------------*
138000060410     c     Add_Sel_DSC   BEGSR
138100060410      *
138200120112     c                   eval      wSQL   +=  ' '
138300120111      * aggiunta selezione per §DSCKSU
138400060410     c                                     +  %trim($Sql_TNTBE(8))
138500060410     c                                     +  ' 3, 7) = '''
138600060410     c                                     +  %trim(TBEke1)
138700060410     c                                     +  ''''
138800060410      * aggiunta selezione per stesso unificante (se diverso)
138900060410     c                   move      §DSCksu       WkscA
139000060410     c                   if            WkscA   > *zeros
139100060410     c                             and WkscA  <> TBEke1
139200120112     c                   eval      wSQL   +=  ' '
139300060410     c                                     +  %trim($Sql_TNTBE(8))
139400060410     c                                     +  ' 3, 7) = '''
139500060410     c                                     +  WkscA
139600060410     c                                     +  ''''
139700060410     c                   endif
139800060410      * chiusura
139900120112     c                   eval      wSQL   +=  '))'
140000060410      *
140100060410     c                   ENDSR
140200120111      *
140300120111      *---------------------------------------------------------------*
140400120111      *?Aggiunta selezione tab. "LAC"                                ?*
140500120111      *---------------------------------------------------------------*
140600120111     c     Add_Sel_LAC   BEGSR
140700120111      *
140800120112     c                   eval      wSQL   +=  ' '
140900120111      * aggiunta selezione per §LACKSU
141000120111     c                                     +  %trim($Sql_TNTBE(8))
141100120111     c                                     +  ' 156, 7) = '''
141200120111     c                                     +  %trim(TBEke1)
141300120111     c                                     +  ''''
141400120111      * aggiunta selezione per stesso unificante (se diverso)
141500120111     c                   move      §LACksu       WkscA
141600120111     c                   if            WkscA   > *zeros
141700120111     c                             and WkscA  <> TBEke1
141800120112     c                   eval      wSQL   +=  ' '
141900120111     c                                     +  %trim($Sql_TNTBE(8))
142000120111     c                                     +  ' 156, 7) = '''
142100120111     c                                     +  WkscA
142200120111     c                                     +  ''''
142300120111     c                   endif
142400120111      * aggiunta selezione per §LACKSC
142500120112     c                   eval      wSQL   +=  ' '
142600120111     c                                     +  %trim($Sql_TNTBE(8))
142700120111     c                                     +  ' 178, 7) = '''
142800120111     c                                     +  %trim(TBEke1)
142900120111     c                                     +  ''''
143000120111      * aggiunta selezione per cliente forzato (se diverso)
143100120111     c                   move      §LACksc       WkscA
143200120111     c                   if            WkscA   > *zeros
143300120111     c                             and WkscA  <> TBEke1
143400120112     c                   eval      wSQL   +=  ' '
143500120111     c                                     +  %trim($Sql_TNTBE(8))
143600120111     c                                     +  ' 178, 7) = '''
143700120111     c                                     +  WkscA
143800120111     c                                     +  ''''
143900120111     c                   endif
144000120111      *
144100120111      * chiusura
144200120112     c                   eval      wSQL   +=  '))'
144300120111      *
144400120111     c                   ENDSR
144500120112      /free
144600120112
144700120112       //--------------------------------------------------------------
144800120112       //?Aggiunta selezione tab. "OSR"                                ?
144900120112       //--------------------------------------------------------------
145000120112       BEGSR  Add_Sel_OSR;
145100120112
145200120112         // -?aggiunta selezione per D§OSRCLI/CL2/3-16?
145300120112         wSQL += ' and (substr(TBEuni,   1, 7) = ''' + %editc(V1Cksc:'X') +
145400120112                 ''' or substr(TBEuni,  42, 7) = ''' + %editc(V1Cksc:'X') +
145500120112                 ''' or substr(TBEuni,  74, 7) = ''' + %editc(V1Cksc:'X') +
145600120112                 ''' or substr(TBEuni, 106, 7) = ''' + %editc(V1Cksc:'X') +
145700120112                 ''' or substr(TBEuni, 138, 7) = ''' + %editc(V1Cksc:'X') +
145800120112                 ''' or substr(TBEuni, 171, 7) = ''' + %editc(V1Cksc:'X') +
145900120112                 ''' or substr(TBEuni, 178, 7) = ''' + %editc(V1Cksc:'X') +
146000120112                 ''' or substr(TBEuni, 185, 7) = ''' + %editc(V1Cksc:'X') +
146100120112                 ''' or substr(TBEuni, 192, 7) = ''' + %editc(V1Cksc:'X') +
146200120112                 ''' or substr(TBEuni, 199, 7) = ''' + %editc(V1Cksc:'X') +
146300120112                 ''' or substr(TBEuni, 206, 7) = ''' + %editc(V1Cksc:'X') +
146400120112                 ''' or substr(TBEuni, 213, 7) = ''' + %editc(V1Cksc:'X') +
146500120112                 ''' or substr(TBEuni, 220, 7) = ''' + %editc(V1Cksc:'X') +
146600120112                 ''' or substr(TBEuni, 227, 7) = ''' + %editc(V1Cksc:'X') +
146700120112                 ''' or substr(TBEuni, 234, 7) = ''' + %editc(V1Cksc:'X') +
146800120112                 ''' or substr(TBEuni, 241, 7) = ''' + %editc(V1Cksc:'X') +
146900120112                 '''';
147000120112         // -?aggiunta selezione per codice ordinante ORM?
147100120112         wSQL += ' or substr(TBEuni, 146, 7) = ''' + %editc(V1Cksc:'X') +
147200120112                 '''';
147300120112         // -?aggiunta selezione per codice destinatario ORM?
147400120112         wSQL += ' or substr(TBEuni, 153, 7) = ''' + %editc(V1Cksc:'X') +
147500120112                 '''';
147600120112
147700120112         // -?chiusura?
147800120112         wSQL += '))';
147900120112
148000120112       ENDSR;
148100120112
148200120112      /end-free
148300051109      *
148400051109      *---------------------------------------------------------------*
148500051109      *?Dichiarazione e Apertura cursore                             ?*
148600051109      *---------------------------------------------------------------*
148700051109     c     OpenCursor    BEGSR
148800051109      *
148900051109      * Dichiarazione cursore
149000051109      *
149100051109     c/exec sql
149200051109     c+     PREPARE S1 FROM :wSQL
149300051109     c/end-exec
149400051109     c*
149500051109     c/exec sql
149600051109     c+     DECLARE C1 CURSOR FOR S1
149700051109     c/end-exec
149800051109      *
149900051109      * Apertura del cursore
150000051109      *
150100051109     c/exec sql
150200051109     c+     OPEN C1
150300051109     c/end-exec
150400051109      *
150500051109     c                   ENDSR
150600051109      *
150700051109      *---------------------------------------------------------------*
150800051109      *?Lettura cursore                                              ?*
150900051109      *---------------------------------------------------------------*
151000051109     c     ReadCursor    BEGSR
151100051109      *
151200051110     c                   clear                   ds_TABEL
151300051109     c/exec sql
151400051110     c+     FETCH NEXT FROM C1 INTO :ds_TABEL
151500051109     c/end-exec
151600051109      *
151700051109sel 1c                   select
151800051109w   1c                   when      SQLcod  =  100
151900051109     c                   eval      $EoF    =  *on
152000051109w   1c                   when      SQLcod  <  *zeros
152100051109     c                   eval      $Err    =  *on
152200051109     c                   eval      $Eof    =  *on
152300051109x   1c                   other
152400051110     c                   exsr      RieS01
152500051109e   1c                   endsl
152600051110      *
152700051110      * Visualizzazione del SFL (se ci sono dati)
152800051110     c                   eval      *in30   =  (S01nrr = *zeros)
152900051110      *
153000051110      * Attivazione (eventuale) del SFLEND
153100051110     c                   eval      *in33   =  $EoF
153200051110      *
153300051110      * Posizionamento cursore al 1° rcd della pagina
153400051110if  1c                   if        S01nrr  >  *zeros
153500051110     c     S01nrr        div       C_SflPag      wPag
153600051110     c                   mvr                     wRig
153700051110     c                   eval      C01rcd  =  wPag * C_SflPag
153800051110     c                   if        wRig    >  *zeros
153900051110     c                   eval      C01rcd  =  C01rcd + 1
154000051110     c                   else
154100051110     c                   eval      C01rcd  =  C01rcd - C_SflPag + 1
154200051110     c                   endif
154300051110x   1c                   else
154400051110     c                   clear                   C01rcd
154500051110e   1c                   endif
154600051110     c                   eval      C01csr  =  C01rcd
154700051109      *
154800051109     c                   ENDSR
154900051109      *
155000051109      *---------------------------------------------------------------*
155100051110      *?Caricamento dati nel record del subfile S01                  ?*
155200051109      *---------------------------------------------------------------*
155300051110     c     RieS01        BEGSR
155400051110      *
155500051110     c                   clear                   TB06S1
155600060202      *
155700060202     c                   eval      VS1cod  = %trim(wSQLcod)
155800051110      *
155900051110      * Impostazione DS corretta (in base al tipo tabella)
156000051110     c                   SELECT
156100051110      * - tab. 3C
156200060202     c                   WHEN      %trim(wSQLcod) = C_tab1
156300060202     c                   movel     wSQLkey       VS1key
156400051117     c                   movel     wSQLuni       ds3C
156500051110     c                   movel     §3Crag        VS1dke
156600051110     c                   movel     §3Ccks        VS1cu1
156700051110     c***                movel                   VS1du1
156800051110     c***                movel                   VS1cu2
156900051110     c***                movel                   VS1du2
157000060202     c                   if        §3Ccks  > *zeros
157100051123     c                   movel     C_tab1_a      VS1dc1
157200051123     c                   endif
157300051110      * - tab. 3K
157400060202     c                   WHEN      %trim(wSQLcod) = C_tab2
157500060202     c                   movel     wSQLkey       VS1key
157600051117     c                   movel     wSQLuni       ds3K
157700051110     c***                movel                   VS1dke
157800051110     c                   movel     §3Kcks        VS1cu1
157900051110     c***                movel                   VS1du1
158000051110     c                   movel     §3Kgks        VS1cu2
158100051110     c***                movel                   VS1du2
158200060202     c                   if        §3Kcks  > *zeros
158300051123     c                   movel     C_tab2_a      VS1dc1
158400051123     c                   endif
158500060202     c                   if        §3Kgks  > *zeros
158600051123     c                   movel     C_tab2_b      VS1dc2
158700051123     c                   endif
158800051110      * - tab. 3R
158900060202     c                   WHEN      %trim(wSQLcod) = C_tab3
159000060202     c                   movel     wSQLkey       VS1key
159100051117     c                   movel     wSQLuni       ds3R
159200051110     c***                movel                   VS1dke
159300051110     c***                movel                   VS1cu1
159400051110     c***                movel                   VS1du1
159500051110     c***                movel                   VS1cu2
159600051110     c***                movel                   VS1du2
159700051110      * - tab. 4C
159800060202     c                   WHEN      %trim(wSQLcod) = C_tab4
159900060202     c                   movel     wSQLkey       VS1key
160000051117     c                   movel     wSQLuni       ds4C
160100051110     c***                movel                   VS1dke
160200051122     c                   if        §4Ccks <> *zeros
160300051110     c                   movel     §4Ccks        VS1cu1
160400051122     c                   endif
160500051110     c***                movel                   VS1du1
160600051122     c                   if        §4Cfks <> *zeros
160700051110     c                   movel     §4Cfks        VS1cu2
160800051122     c                   endif
160900051110     c***                movel                   VS1du2
161000060202     c                   if        §4Ccks  > *zeros
161100051123     c                   movel     C_tab4_a      VS1dc1
161200051123     c                   endif
161300060202     c                   if        §4Cfks  > *zeros
161400051123     c                   movel     C_tab4_b      VS1dc2
161500051123     c                   endif
161600060217      * - tab. 3Q
161700060217     c                   WHEN      %trim(wSQLcod) = C_tab5
161800060217     c                   movel     wSQLkey       VS1key
161900060217     c                   movel     wSQLuni       ds3Q
162000060217     c***                movel                   VS1dke
162100060217     c                   movel     §3Qcks        VS1cu1
162200060217     c***                movel                   VS1du1
162300060217     c***                movel                   VS1cu2
162400060217     c***                movel                   VS1du2
162500060217     c                   if        §3Qcks  > *zeros
162600060217     c                   movel     C_tab5_a      VS1dc1
162700060217     c                   endif
162800060202      * - tab. VAP
162900060217     c                   WHEN      %trim(wSQLcod) = C_tab6
163000060202     c                   eval      VS1key  = %subst(wSQLkey:2:7)
163100060410     c*** inesistente:   movel     wSQLuni       dVAP
163200060202     c***                movel                   VS1dke
163300060202     c                   eval      VS1cu1  = %subst(wSQLuni:2:7)
163400060202     c***                movel                   VS1du1
163500060202     c***                movel                   VS1cu2
163600060202     c***                movel                   VS1du2
163700060202     c*** sempre:        if        VS1cu1  > *zeros
163800060217     c                   movel     C_tab6_a      VS1dc1
163900060202     c*** sempre:        endif
164000060410      * - tab. DSC
164100060410     c                   WHEN      %trim(wSQLcod) = C_tab7
164200060410     c                   movel     wSQLkey       VS1key
164300060410     c                   movel     wSQLuni       dDSC
164400060410     c***                movel                   VS1dke
164500060410     c                   if        §DSCksu > *zeros
164600060410     c                   movel     §DSCksu       VS1cu1
164700060410     c                   endif
164800060410     c***                movel                   VS1du1
164900060410     c***                movel                   VS1cu2
165000060410     c***                movel                   VS1du2
165100060410     c*** sempre:        if        VS1cu1  > *zeros
165200060410     c                   movel     C_tab7_a      VS1dc1
165300060410     c*** sempre:        endif
165400120111      * - tab. LAC
165500120111     c                   WHEN      %trim(wSQLcod) = C_tab8
165600120111     c                   movel     wSQLkey       VS1key
165700120111     c                   movel     wSQLuni       dLAC
165800120111     c***                movel     §LACrag       VS1dke
165900120111     c                   if        §LACksu > *zeros
166000120111     c                   movel     §LACksu       VS1cu1
166100120111     c                   endif
166200120111     c***                movel                   VS1du1
166300120111     c                   if        §LACksc > *zeros
166400120111     c                   movel     §LACksc       VS1cu2
166500120111     c                   endif
166600120111     c***                movel                   VS1du2
166700120111     c*** sempre:        if        VS1cu1  > *zeros
166800120111     c                   movel     C_tab8_a      VS1dc1
166900120111     c*** sempre:        endif
167000120111     c*** sempre:        if        VS1cu2  > *zeros
167100120111     c                   movel     C_tab8_b      VS1dc2
167200120111     c*** sempre:        endif
167300130129      * - tab. DKC
167400130129     c                   WHEN      %trim(wSQLcod) = C_tab9
167500130129     c                   movel     wSQLkey       VS1key
167600130129     c*// No altri cod.  movel     wSQLuni       dDKC
167700130129     c*//                movel                   VS1dke
167800130129     c*//                movel                   VS1cu1
167900130129     c*//                movel                   VS1du1
168000130129     c*//                movel                   VS1cu2
168100130129     c*//                movel                   VS1du2
168200130129     c*// mai:           if        VS1cu1  > *zeros
168300130129     c*//                movel     C_tab?_a      VS1dc1
168400130129     c*// mai:           endif
168500120112      * - tab. OSR
168600130129     c                   WHEN      %trim(wSQLcod) = C_tab10
168700120112     c                   movel     wSQLkey       VS1key
168800120112     c                   movel     wSQLuni       dOSR
168900120112     c                   eval      VS1dke = '-Fil.emiss.ORM++
169000120112     c                                        Serie'
169100120112     c                   if            d§OSRcl3  > *zeros
169200120112     c                             or  d§OSRcl4  > *zeros
169300120112     c                             or  d§OSRcl5  > *zeros
169400120112     c                             or  d§OSRcl6  > *zeros
169500120112     c                             or  d§OSRcl7  > *zeros
169600120112     c                             or  d§OSRcl8  > *zeros
169700120112     c                             or  d§OSRcl9  > *zeros
169800120112     c                             or  d§OSRcl10 > *zeros
169900120112     c                             or  d§OSRcl11 > *zeros
170000120112     c                             or  d§OSRcl12 > *zeros
170100120112     c                             or  d§OSRcl13 > *zeros
170200120112     c                             or  d§OSRcl14 > *zeros
170300120112     c                             or  d§OSRcl15 > *zeros
170400120112     c                             or  d§OSRcl16 > *zeros
170500120112     c                             or  d§OSRcor  > *zeros
170600120112     c                             or  d§OSRcrc  > *zeros
170700120112     c                   eval      VS1dke = '   (altri clienti)'
170800120112     c                   endif
170900120112     c                   if        d§OSRcli > *zeros
171000120112     c                   movel     d§OSRcli      VS1cu1
171100120112     c                   endif
171200120112     c***                movel     d§OSRrag      VS1du1
171300120112     c                   if        d§OSRcl2 > *zeros
171400120112     c                   movel     d§OSRcl2      VS1cu2
171500120112     c                   endif
171600120112     c***                movel     d§OSRra2      VS1du2
171700120112     c*** sempre:        if        VS1cu1  > *zeros
171800120112     c                   movel     C_tab8_a      VS1dc1
171900120112     c*** sempre:        endif
172000120112     c*** sempre:        if        VS1cu2  > *zeros
172100120112     c                   movel     C_tab8_b      VS1dc2
172200120112     c*** sempre:        endif
172300051110     c                   ENDSL
172400051110      *
172500051110      * Decodifica cliente - chiave
172600060202if  1c                   if        VS1key  > *zeros
172700130129     c                             and  %trim(wSQLcod) <> C_tab10
172800051110     c                   move      VS1key        Wksc
172900051110     c                   exsr      Decod_Cli
173000051110if  2c                   if        O69err  =  *on
173100051110     c                   movel     *all'? '      VH1dke
173200051110x   2c                   else
173300051110     c                   movel     ACOrag        VH1dke
173400120807     c                   movel     ACOabl        VH1abl
173500051110e   2c                   endif
173600060202if  2c                   if        VS1dke  = *blanks
173700051110     c                   movel     VH1dke        VS1dke
173800051110e   2c                   endif
173900051110e   1c                   endif
174000051110      * Decodifica cliente - unificante 1
174100060202if  1c                   if        VS1cu1  > *zeros
174200051110     c                   move      VS1cu1        Wksc
174300051110     c                   exsr      Decod_Cli
174400051110if  2c                   if        O69err  =  *on
174500051110     c                   movel     *all'? '      VH1du1
174600051110x   2c                   else
174700051110     c                   movel     ACOrag        VH1du1
174800051110e   2c                   endif
174900060202if  2c                   if        VS1du1  = *blanks
175000051110     c                   movel     VH1du1        VS1du1
175100051110e   2c                   endif
175200051110e   1c                   endif
175300051110      * Decodifica cliente - unificante 2
175400051110if  1c                   if        VS1cu2 >  *zeros
175500051110     c                   move      VS1cu2        Wksc
175600051110     c                   exsr      Decod_Cli
175700051110if  2c                   if        O69err  =  *on
175800051110     c                   movel     *all'? '      VH1du2
175900051110x   2c                   else
176000051110     c                   movel     ACOrag        VH1du2
176100051110e   2c                   endif
176200060202if  2c                   if        VS1du2  = *blanks
176300051110     c                   movel     VH1du2        VS1du2
176400051110e   2c                   endif
176500051110e   1c                   endif
176600100216      *
176700100216      * Impostazione attributi di visualizzazione
176800130201     c                   eval      *in41 = ($Called = *on  and
176900130201     c                                      D06op0  = *blank)
177000130315     c                   eval      *in42 = (VH1abl  <> *blanks)
177100051110      *
177200051110      * Scrittura record subfile
177300051110     c                   add       1             S01nrr
177400051110     c                   write     TB06S1
177500051110      *
177600051110     c                   ENDSR
177700051109      *
177800051109      *---------------------------------------------------------------*
177900051109      *?Chiusura cursore                                             ?*
178000051109      *---------------------------------------------------------------*
178100051109     c     CloseCursor   BEGSR
178200051109      *
178300051109     c/exec sql
178400051109     c+     CLOSE C1
178500051109     c/end-exec
178600051109      *
178700051109     c                   ENDSR
178800051110      *
178900051110      *---------------------------------------------------------------*
179000051110      *?Gestione tasto funzionale F08 da videata C01 / S01           ?*
179100120111      *?F8-Stampa elenco                                             ?*
179200051110      *---------------------------------------------------------------*
179300051110     c     F08S01        BEGSR
179400051110      *
179500051110      * Override al prtf
179600051110     c                   call      'QCMDEXC'
179700051110     c                   parm      $Cmd(1)       Qcmd
179800051110     c                   parm                    Qlen
179900051110      *
180000051110      * Apertura prtf
180100051110     c                   open      PRTF198
180200051110      *
180300051110      * Stampa parametri - stampa della testata iniziale
180400051110     c                   move      *date         Data_Eur
180500051110     c     *eur          movel     Data_Eur      wDate
180600051110     c                   time                    wTime
180700051110      * Stampa parametri - stampa dei parametri di lancio
180800051110     c                   except    PRTini
180900051110     c                   except    PRTpar
181000051110      *
181100051110      * Stampa lista     - stampa della testata ininiziale
181200051110     c                   except    PRTini
181300051110     c                   except    PRTtxt
181400051110     c                   eval      *inOF   =  *off
181500070828      *
181600070828if  0c                   IF        C01rcd  >  *zeros
181700070828      *
181800051110      * Stampa lista     - stampa dettagio
181900051110      * - ciclo di lettura sfl
182000051110if  1c                   DO        *hival        S01nrr
182100051110     c     S01nrr        chain     TB06S1
182200051110if  2c                   if        NOT %found(TRTB06D)
182300051110     c                   leave
182400051110e   2c                   endif
182500051110      * - stampa riga di dettaglio
182600051110     c                   if        *inOF    = *on
182700051110     c                   except    PRTini
182800051110     c                   except    PRTtxt
182900051110     c                   eval      *inOF    = *off
183000051110     c                   endif
183100051110     c                   except    PRTdet
183200051110      * - riattivazione sflnxtchg (se immessa opzione)
183300051110if  2c                   if        VS1opz  <> *blanks
183400051110     c                   eval      *in32   =  *on
183500130201     c                   eval      *in41 = ($Called = *on  and
183600130201     c                                      D06op0  = *blank)
183700130315     c                   eval      *in42 = (VH1abl  <> *blanks)
183800051110     c                   UPDATE    TB06S1
183900051110e   2c                   endif
184000051110e   1c                   ENDDO
184100070828      *
184200070828e   0c                   ENDIF
184300051110      *
184400051110      * Stampa "Fine Lista"
184500051110     c                   time                    wTime
184600051110     c                   except    PRTend
184700051110      *
184800051110      * Chiusura prtf
184900051110     c                   close     PRTF198
185000051110      *
185100051110      * Cancellazione override
185200051110     c                   call      'QCMDEXC'
185300051110     c                   parm      $Cmd(2)       Qcmd
185400051110     c                   parm                    Qlen
185500051110      *
185600051110     c                   ENDSR
185700051110      *
185800051110      *---------------------------------------------------------------*
185900051110      *?Gestione tasto funzionale F11 da videata C01 / S01           ?*
186000051110      *?F11-Cambia ordinamento sfl                                   ?*
186100051110      *---------------------------------------------------------------*
186200051110     c     F11S01        BEGSR
186300051110      *
186400051110     c                   select
186500051110     c                   when      WrkSort =  *off
186600051110     c                   eval      WrkSort =  *on
186700051110     c                   when      WrkSort =  *on
186800051110     c                   eval      WrkSort =  *off
186900051110     c                   endsl
187000051123     c                   eval      *in40   =  (WrkSort = *on)
187100051123if  1c                   if        C01rcd  >  *zeros
187200051110     c                   exsr      Sort
187300051123e   1c                   endif
187400051110      *
187500051110     c                   ENDSR
187600051110      *
187700051110      *---------------------------------------------------------------*
187800051110      *?Gestione tasto funzionale F12 da videata C01 / S01           ?*
187900051110      *?F12-Ritorno                                                  ?*
188000051110      *---------------------------------------------------------------*
188100051110     c     F12S01        BEGSR
188200051110      *
188300130129     c*//                if        Not $Called
188400051110      * Torno alla videata precedente
188500051110     c                   eval      $Video  =  '1'
188600130129     c                   eval      *in51   =  ($Called = *on)
188700130129     c*//                else
188800091214      * Esco dal programma
188900130129     c*//                eval      $Fine   =  *on
189000130129     c*//                endif
189100051110      *
189200051110     c                   ENDSR
189300051110      *
189400051110      *---------------------------------------------------------------*
189500051110      *?Gestione opzioni da subfile S01                              ?*
189600051110      *---------------------------------------------------------------*
189700051110     c     OpzS01        BEGSR
189800051110      *
189900051110     c                   readc     TB06S1
190000051110      *
190100051110do  1c                   DOW       NOT %eof(TRTB06D)
190200051110      *
190300051110     c                   eval      *in32   =  *off
190400051110     c                   movea     *zeros        *in(50)
190500051110     c                   z-add     S01nrr        C01rcd
190600051110      *
190700051110sel 2c                   SELECT
190800051110      * - Nessuna opzione
190900051110w   2c                   WHEN      VS1opz  =  *blanks
191000051110      * - Gestione delle singola tabella
191100051110w   2c                   WHEN          VS1opz  =  '2'
191200051117     c***                          or  VS1opz  =  '5'
191300051110     c                   exsr      sr_OpzS1
191400051110      * - ? = Opzione NON valida
191500051110x   2c                   OTHER
191600051110     c                   seton                                        502890
191700051110     c                   eval      V1Dmsg  =  $Msg(4)
191800051110e   2c                   ENDSL
191900051110      *
192000051110      * Aggiornamento sfl
192100051110sel 2c                   select
192200051110w   2c                   when      *in28
192300051110     c                   eval      *in32   =  *on
192400051110     c                   z-add     C01rcd        C01csr
192500051110w   2c                   when      *in90
192600051110     c                   z-add     C01rcd        C01csr
192700051110     c                   clear                   VS1opz
192800051110x   2c                   other
192900051110     c                   clear                   VS1opz
193000051110e   2c                   endsl
193100130201     c                   eval      *in41 = ($Called = *on  and
193200130201     c                                      D06op0  = *blank)
193300130315     c                   eval      *in42 = (VH1abl  <> *blanks)
193400051110     c                   UPDATE    TB06S1
193500051110if  2c                   if        *in28  OR  *in90
193600051110     c                   leave
193700051110e   2c                   endif
193800051110      *
193900051110     c                   readc     TB06S1
194000051110      *
194100051110e   1c                   ENDDO
194200051110      *
194300051110     c                   ENDSR
194400051110      *
194500051110      *---------------------------------------------------------------*
194600051110      *?Gestione singola opzione dal subfile S01                     ?*
194700051110      *---------------------------------------------------------------*
194800051110     c     sr_OpzS1      BEGSR
194900051110      *
195000051110sel 1c                   select
195100051216      * - 1ª tabella ('3C')
195200051216w   1c                   when           VS1cod =  V1Ctb1
195300051110     c                             and (VS1opz =  '2'
195400051117     c                              or  VS1opz =  '5')
195500120807     c*//                movel(p)  VS1key        KPJBU
195600120807     c                   eval      KPJBU = VS1key + SDSpgm
195700051216     c                   call      V1Hpg1
195800051110     c                   parm                    KPJBA
195900051216      * - 2ª tabella ('3K')
196000051216w   1c                   when           VS1cod =  V1Ctb2
196100051110     c                             and (VS1opz =  '2'
196200051117     c                              or  VS1opz =  '5')
196300051110     c                   movel(p)  VS1key        KPJBU
196400051216     c                   call      V1Hpg2
196500051110     c                   parm                    KPJBA
196600051216      * - 3ª tabella ('3R')
196700051216w   1c                   when           VS1cod =  V1Ctb3
196800051110     c                             and (VS1opz =  '2'
196900051117     c                              or  VS1opz =  '5')
197000051110     c                   movel(p)  VS1key        KPJBU
197100051216     c                   call      V1Hpg3
197200051110     c                   parm                    KPJBA
197300051216      * - 4ª tabella ('4C')
197400051216w   1c                   when           VS1cod =  V1Ctb4
197500051110     c                             and (VS1opz =  '2'
197600051117     c                              or  VS1opz =  '5')
197700051110     c                   movel(p)  VS1key        KPJBU
197800051216     c                   call      V1Hpg4
197900051110     c                   parm                    KPJBA
198000060217      * - 5ª tabella ('3Q')
198100060217w   1c                   when           VS1cod =  V1Ctb5
198200060217     c                             and (VS1opz =  '2'
198300060217     c                              or  VS1opz =  '5')
198400071016     c                   movel(p)  VS1key        KPJBU
198500071016     c                   call      V1Hpg5
198600071016     c                   parm                    KPJBA
198700060217      * - 6ª tabella ('VAP')
198800060217w   1c                   when           VS1cod =  V1Ctb6
198900060202     c                             and (VS1opz =  '2'
199000060202     c                              or  VS1opz =  '5')
199100060202     c                   clear                   KPJBU
199200060202     c                   eval      KPJBU = '0' + VS1key
199300060217     c                   call      V1Hpg6
199400060202     c                   parm                    KPJBA
199500120111      * - 7ª tabella ('DSC')
199600060410w   1c                   when           VS1cod =  V1Ctb7
199700060410     c                             and (VS1opz =  '2'
199800060410     c                              or  VS1opz =  '5')
199900060410     c                   clear                   KPJBU
200000060410     c                   movel(p)  VS1key        KPJBU
200100060410     c                   call      V1Hpg7
200200060410     c                   parm                    KPJBA
200300120111      * - 8ª tabella ('LAC')
200400120111w   1c                   when           VS1cod =  V1Ctb8
200500120111     c                             and (VS1opz =  '2'
200600120111     c                              or  VS1opz =  '5')
200700120111     c                   clear                   TNTB46ds
200800120111     c                   eval      B46ke1 = VS1key
200900120111     c                   movel(p)  TNTB46ds      KPJBU
201000120111     c                   call      V1Hpg8
201100120111     c                   parm                    KPJBA
201200130129      * - 9ª tabella ('DKC')
201300120112w   1c                   when           VS1cod =  V1Ctb9
201400120112     c                             and (VS1opz =  '2'
201500120112     c                              or  VS1opz =  '5')
201600120112     c                   clear                   KPJBU
201700120112     c                   movel(p)  VS1key        KPJBU
201800120112     c                   call      V1Hpg9
201900120112     c                   parm                    KPJBA
202000130129      * - 10ª tabella ('OSR')
202100130129w   1c                   when           VS1cod =  V1Ctb10
202200130129     c                             and (VS1opz =  '2'
202300130129     c                              or  VS1opz =  '5')
202400130129     c                   clear                   KPJBU
202500130129     c                   movel(p)  VS1key        KPJBU
202600130129     c                   call      V1Hpg10
202700130129     c                   parm                    KPJBA
202800051110      *
202900051110e   1c                   endsl
203000051110      *
203100051110     c                   ENDSR
203200051110      *
203300051110      *---------------------------------------------------------------*
203400051110      *?Decodifica cliente                                           ?*
203500051110      *---------------------------------------------------------------*
203600051110     c     Decod_Cli     BEGSR
203700051110      *
203800051110     c                   clear                   TIBS69ds
203900051110     c                   clear                   ds_CNACO
204000051110     c                   clear                   ds_CNIND
204100051110     c                   clear                   ds_CNCLP
204200051110     c                   clear                   ds_FNCLS
204300051110      *
204400051110     c                   z-add     Wksc          I69kac
204500051110      *
204600051110     c                   call      'TIBS69R'
204700051110     c                   parm                    TIBS69ds
204800051110     c                   parm                    ds_CNACO
204900051110     c                   parm                    ds_CNIND
205000051110     c                   parm                    ds_CNCLP
205100051110     c                   parm                    ds_FNCLS
205200051110      *
205300051110     c                   ENDSR
205400051110      *
205500051110      *---------------------------------------------------------------*
205600051110      *?Ordinamento subfile                                          ?*
205700051110      *?  Subroutine - Sort                                          ?*
205800051110      *?  This subroutine sorts the subfile records.                 ?*
205900051110      *---------------------------------------------------------------*
206000051110     c     Sort          BEGSR
206100051110      *
206200051122     c                   eval      C01rcd  =  1
206300051122     c                   clear                   C01csr
206400051122     c                   clear                   S01nrr
206500051122     c                   clear                   V1Dmsg
206600051122     c                   setoff                                       28  90
206700051122     c                   eval      *in32   =  *off
206800051122     c                   movea     *zeros        *in(50)
206900051110      *
207000051110      * Initialize the key fields to sort on. There is one extra field not in
207100051110      * the subfile -- Selected -- that is added to the record. This field
207200051110      * is used to place selected records in front of nonselected records.
207300051110      *
207400051110     c                   clear                   QLGKL
207500051110     c                   clear                   QLGSKL
207600051110     c                   clear                   QLGSCB
207700051110     c                   clear                   QLGSCB00
207800051110      *
207900051110      * Gestione della scelta ordinamento.
208000051110sel 1c                   SELECT
208100051110      * Ordinamento per Chiave_Tabella/Tipo_Tabella
208200051122w   1c                   WHEN      WrkSort  = *on
208300051110      *   2 campi chiave
208400051110     c                   eval      QLGNBRK  = 2
208500051122      *   1° campo: chiave tabella (VS1key - codice cliente)
208600120807      *             a posizone  111,  7 byte, char, ascending.
208700120807     c                   eval      QLGSP    = 111
208800051110     c                   eval      QLGSS    = %size(VS1key)
208900051110     c                   eval      QLGDT    = Carattere
209000051110     c                   eval      QLGSO    = Ascendente
209100051110     c                   eval      QLGKL(1) = QLGSKL
209200051122      *   2° campo: tipo tabella (VS1cod - 3C/3K/3R/4C)
209300120807      *             a posizone  108,  3 byte, char, ascending.
209400120807     c                   eval      QLGSP    = 108
209500051110     c                   eval      QLGSS    = %size(VS1cod)
209600051110     c                   eval      QLGDT    = Carattere
209700051110     c                   eval      QLGSO    = Ascendente
209800051110     c                   eval      QLGKL(2) = QLGSKL
209900051110      *
210000051110      * Ordinamento per Tipo_Tabella/Chiave_Tabella
210100051110w   1c                   WHEN      WrkSort  = *off
210200051110      *   2 campi chiave
210300051110     c                   eval      QLGNBRK  = 2
210400051122      *   1° campo: tipo tabella (VS1cod - 3C/3K/3R/4C)
210500120807      *             a posizone  108,  3 byte, char, ascending.
210600120807     c                   eval      QLGSP    = 108
210700051110     c                   eval      QLGSS    = %size(VS1cod)
210800051110     c                   eval      QLGDT    = Carattere
210900051110     c                   eval      QLGSO    = Ascendente
211000051110     c                   eval      QLGKL(1) = QLGSKL
211100051122      *   2° campo: chiave tabella (VS1key - codice cliente)
211200120807      *             a posizone  111,  7 byte, char, ascending.
211300120807     c                   eval      QLGSP    = 111
211400051110     c                   eval      QLGSS    = %size(VS1key)
211500051110     c                   eval      QLGDT    = Carattere
211600051110     c                   eval      QLGSO    = Ascendente
211700051110     c                   eval      QLGKL(2) = QLGSKL
211800051110      *
211900051110e   1c                   ENDSL
212000051110      *
212100051110      * Load other sort parameters.
212200051110     c                   eval      QLGLB    = 80 + 16 * MaxKey
212300051110     c                   eval      QLGRL    = %size(SflRcd) - 1
212400051110     c                   eval      QLGRT    = 8
212500051110     c                   eval      QLGOKL   = 80
212600051110     c                   eval      QLGLKE   = 16
212700051110     c                   eval      QLGLSS   = 290
212800051110      * Initialize Sort I/O API fields.
212900051110     c                   eval      QLGRL00  = QLGRL
213000051110     c                   eval      QLGRC00  = 1
213100051110     c                   clear                   QUSEI
213200051110     c                   eval      QUSBPRV  = %size(QUSEC)
213300051110      * First step - Initialize the sort routine.
213400051110     c                   call      'QLGSORT'
213500051110     c                   parm                    QLGSCB
213600051110     c                   parm                    NotUsed
213700051110     c                   parm                    NotUsed
213800051110     c                   parm                    SizeList
213900051110     c                   parm                    ReturnSize
214000051110     c                   parm                    QUSEC
214100051110      *
214200051110      * Next step - Write records to I/O routine.
214300051110     c                   eval      QLGRT00  = Put
214400051122do  1c                   DO        RrnLast       Nrr
214500051122     c     Nrr           chain     TB06S1
214600051110      * Solo le righe con Selected = 'Y' sono riordinate,
214700051110      * quindi per fare un ordinamento di tutte le righe
214800051110      * metto 'Y' sempre.
214900051110     c                   eval      Selected = 'Y'
215000051110     c                   clear                   QUSEI
215100051110     c                   eval      QUSBPRV  = %size(QUSEC)
215200051110     c                   call      'QLGSRTIO'
215300051110     c                   parm                    QLGSCB00
215400051110     c                   parm                    SflRcd
215500051110     c                   parm                    NotUsed
215600051110     c                   parm                    SizeList
215700051110     c                   parm                    NotUsed
215800051110     c                   parm                    QUSEC
215900051110e   1c                   ENDDO
216000051110      * Next step - Signal end of input, clear subfile for reload.
216100051110     c                   eval      QLGRT00  = EndPut
216200051110     c                   clear                   QUSEI
216300051110     c                   eval      QUSBPRV  = %size(QUSEC)
216400051110      *
216500051110     c                   call      'QLGSRTIO'
216600051110     c                   parm                    QLGSCB00
216700051110     c                   parm                    SflRcd
216800051110     c                   parm                    NotUsed
216900051110     c                   parm                    SizeList
217000051110     c                   parm                    NotUsed
217100051110     c                   parm                    QUSEC
217200051110      *
217300051110      * Pulizia subfile
217400051110     c                   seton                                        3031
217500051110     c                   write     TB06C1
217600051110     c                   setoff                                         3133
217700051110      *
217800051110      * Final step - Write the records back to the subfile.
217900051122     c                   eval      QLGRT00  = Get
218000051122do  1c                   do        RrnLast       Nrr
218100051110     c                   clear                   QUSEI
218200051122     c                   eval      QUSBPRV  = %size(QUSEC)
218300051110     c                   call      'QLGSRTIO'
218400051110     c                   parm                    QLGSCB00
218500051110     c                   parm                    NotUsed
218600051110     c                   parm                    SflRcd
218700051110     c                   parm                    QLGRL00
218800051110     c                   parm                    NotUsed
218900051110     c                   parm                    QUSEC
219000051122      * - Ripristino indicatori utilizzati nel sfl rec
219100051122     c                   eval      *in32   =  (VS1opz <> *blanks)
219200130201     c                   eval      *in41   =  ($Called = *on  and
219300130201     c                                         D06op0  = *blank)
219400130315     c                   eval      *in42   =  (VH1abl  <> *blanks)
219500051122     c                   z-add     Nrr           S01nrr
219600051110     c                   WRITE     TB06S1
219700051110e   1c                   enddo
219800051122      *
219900051122      * Visualizzazione del SFL (se ci sono dati)
220000051122     c                   eval      *in30   =  (S01nrr = *zeros)
220100051122      *
220200051122      * Attivazione (eventuale) del SFLEND
220300051122     c                   eval      *in33   =  $EoF
220400051110      *
220500051110     c                   ENDSR
220600051110
220700051110      *---------------------------------------------------------------*
220800051110
220900051110     oPRTF198   e            PRTini            2
221000051110     o                       RSUT
221100051110     o                                        +  20 'ELENCO LEGAMI TAB-
221200051110     o                                              ELLE   V A S'
221300051110     o                       SDSpgm           +  20
221400051110     o                       Wdate         Y  +   5
221500051110     o                                        +   5 'Pag.'
221600051110     o                       Page          Z  +   0
221700051110     o          e            PRTini      0
221800051110     o                                        +  40 'ELENCO LEGAMI TAB-
221900051110     o                                              ELLE   V A S'
222000051110     o          e            PRTini      1
222100051110     o                       KNSIF
222200051110     o                       SDSusr           +   1
222300051110     o                                        +  20 '------------------
222400051110     o                                              ------------'
222500051110     o                       Wtime            +  35 '  :  :  '
222600051110     o          e            PRTini      0
222700051110     o                                        +  40 '------------------
222800051110     o                                              ------------'
222900051110      *
223000051110     o          e            PRTpar      5
223100051110     o                                              'PARAMETRI DI LANC-
223200051110     o                                              IO:'
223300051110     o                                        +   5 'Codice cliente ..-
223400051110     o                                              ...:'
223500051110     o                       V1Cksc           +   2
223600051110     o                       V1Dksc           +   2
223700051110     o          e            PRTpar      0  0
223800051110     o                                              '_________________-
223900051110     o                                              ___'
224000051110      *
224100051110     o          e            PRTpar      5
224200051110     o                                        +  25 'Tabelle da esamin-
224300051110     o                                              are:'
224400051110     o                       V1Ctb1           +   2
224500051110     o                       V1Dtb1           +   2
224600051110     o                       V1Cse1           +   2
224700051110     o                                        +   2 '(S=Si)'
224800051110     o          e            PRTpar      1
224900051110     o                       V1Ctb2           +  48
225000051110     o                       V1Dtb2           +   2
225100051110     o                       V1Cse2           +   2
225200051110     o                                        +   2 '(S=Si)'
225300051110     o          e            PRTpar      1
225400051110     o                       V1Ctb3           +  48
225500051110     o                       V1Dtb3           +   2
225600051110     o                       V1Cse3           +   2
225700051110     o                                        +   2 '(S=Si)'
225800051110     o          e            PRTpar      1
225900051110     o                       V1Ctb4           +  48
226000051110     o                       V1Dtb4           +   2
226100051110     o                       V1Cse4           +   2
226200051110     o                                        +   2 '(S=Si)'
226300060202     o          e            PRTpar      1
226400060202     o                       V1Ctb5           +  48
226500060202     o                       V1Dtb5           +   2
226600060202     o                       V1Cse5           +   2
226700060202     o                                        +   2 '(S=Si)'
226800060217     o          e            PRTpar      1
226900060217     o                       V1Ctb6           +  48
227000060217     o                       V1Dtb6           +   2
227100060217     o                       V1Cse6           +   2
227200060217     o                                        +   2 '(S=Si)'
227300060410     o          e            PRTpar      1
227400060410     o                       V1Ctb7           +  48
227500060410     o                       V1Dtb7           +   2
227600060410     o                       V1Cse7           +   2
227700060410     o                                        +   2 '(S=Si)'
227800120111     o          e            PRTpar      1
227900120111     o                       V1Ctb8           +  48
228000120111     o                       V1Dtb8           +   2
228100120111     o                       V1Cse8           +   2
228200120111     o                                        +   2 '(S=Si)'
228300120228     o          e            PRTpar      1
228400120228     o                       V1Ctb9           +  48
228500120228     o                       V1Dtb9           +   2
228600120228     o                       V1Cse9           +   2
228700120228     o                                        +   2 '(S=Si)'
228800130129     o          e            PRTpar      1
228900130129     o                       V1Ctb10          +  48
229000130129     o                       V1Dtb10          +   2
229100130129     o                       V1Cse10          +   2
229200130129     o                                        +   2 '(S=Si)'
229300051110      *
229400051110     o          e            PRTtxt      3
229500051110     o                       $Txt(1)
229600051110     o                       $Txt(4)          +   0
229700051110     o                       $Txt(7)          +   0
229800051110     o          e            PRTtxt      1
229900051110     o                       $Txt(2)
230000051110     o                       $Txt(5)          +   0
230100051110     o                       $Txt(8)          +   0
230200051110      *
230300051110     o          e            PRTdet      1
230400060202     o                       VS1cod
230500051110     o                       VS1key           +   4
230600051110     o                       VH1dke           +   1
230700051110     o                       VS1cu1           +   3
230800051110     o                       VH1du1           +   1
230900051110     o                       VS1cu2           +   3
231000051110     o                       VH1du2           +   1
231100051110      *
231200051110     o          e            PRTend      3
231300051110     o                                        +  40 '***  FINE  LISTA -
231400051110     o                                               ***'
231500051110     o          e            PRTend      0
231600051110     o                                        +  40 '***  FINE  LISTA -
231700051110     o                                               ***'
231800051109
231900051109      *---------------------------------------------------------------*
232000051109
232100051109**   $Msg -------------------------------------------------------------------*
232200051110Immettere un codice cliente                                                    1
232300051110Codice cliente errato                                                          2
232400051110Selezionare almeno una tabella da esaminare                                    3
232500051110Opzione non valida                                                             4
232600060202**   $Sql_TABEL -------------------------------------------------------------*
232700100803SELECT ' ' concat TBLcod as TABcod, TBLkey as TABke1, TBLuni as TABdat         1
232800100803  FROM TABEL00F                                                                2
232900100803 WHERE TBLflg   =  ' '                                                         3
233000051110       (TBLcod  =  '                                                           4
233100051110   and  (TBLkey =  '                                                           5
233200051110    or   substr(TBLuni,                                                        6
233300060202**   $Sql_TNTBE -------------------------------------------------------------*
233400060202SELECT TBEcod as TABcod, TBEke1 as TABke1, TBEke2 as TABdat                    1
233500060410SELECT TBEcod as TABcod, TBEke1 as TABke1, TBEuni as TABdat                    2
233600100803  FROM TNTBE00F                                                                3
233700100803 WHERE TBEatb   =  ' '                                                         4
233800060410       (TBEcod  =  '                                                           5
233900060410   and  (TBEke1 =  '                                                           6
234000060410    or   TBEke2 =  '                                                           7
234100060410    or   substr(TBEuni,                                                        8
234200060202**   $Cmd -------------------------------------------------------------------*
234300051109OVRPRTF FILE(PRTF198) USRDTA('Legami_VAS')
234400051109DLTOVR  FILE(PRTF198)
234500051110**   $Txt -------------------------------------------------------*
234600051110Tab.   Cliente                                       Cliente Unifi
234700051110----   -------------------------------------------   -------------
234800051110 12    1234567 *...+....1....+....2....+....3....+   1234567 *...+
234900051110cante (1)                        Cliente Unificante (2)
235000051110------------------------------   ---------------------------------
235100051110....1....+....2....+....3....+   1234567 *...+....1....+....2....+
235200051110
235300051110----------
235400051110....3....+
