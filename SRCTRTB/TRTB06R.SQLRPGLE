000100070828      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200091214     /*PRM closqlcsr(*endmod) dbgview(*source) dynusrprf(*owner)
000300091214     /*END
000400051109      *===============================================================*
000500051109      *?TRTB06R * Interrogazione legami tabelle VAS                  ?*
000600051109      *===============================================================*
000700051109      *
000800150813     h decedit('0,') datedit(*dmy/) option(*nodebugio) actgrp(*new)
000900150813     h alwnull(*inputonly) dftactgrp(*no)
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
006700150812     d  VH1SUN                             inz
006800051122     d  VS1opz                             inz
006900051110     d  VS1cod                             inz
007000051110     d  VS1key                             inz
007100051122     d  VS1dke                             inz
007200051110     d  VS1cu1                             inz
007300051122     d  VS1du1                             inz
007400051110     d  VS1cu2                             inz
007500051122     d  VS1du2                             inz
007600051123     d  VS1dc1                             inz
007700051123     d  VS1dc2                             inz
007800051110     d  Selected                      1A
007900051110      /COPY QSYSINC/QRPGLESRC,QLGSORT
008000051110     d QLGKL                         16    dim(MaxKey)
008100051110     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
008200051110     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
008300051110     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
008400051110     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
008500051110      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008600051110      /COPY QSYSINC/QRPGLESRC,QUSEC
008700051110      *
008800051110      *****************************************************************
008900051110      * Standalone fields
009000051110      *
009100051122      *   Nrr        - Relative record number for subfile
009200051110      *   SizeList   - Size of list
009300051110      *   ReturnSize - Size of list returned by sort API
009400051110      *****************************************************************
009500051110     d NotUsed         s             16a   inz
009600051110     d ReturnSize      s              9b 0 inz
009700051110     d SizeList        s              9b 0 inz
009800051110     d RrnLast         s              5i 0 inz
009900051122     d Nrr             s              5i 0 inz
010000051122      * WrkSort    - Ordinamento impostato a video
010100051122     d WrkSort         s              1    inz(*off)
010200051109      *
010300051109      *?C O S T A N T I?----------------------------------------------*
010400051109      *
010500051110      * - Nr. di righe del sfl (SFLPAG)
010600051110     d C_SflPag        c                   const(16)
010700051110      * - Tabelle elaborate con questo pgm
010800051109     d C_tab1          c                   const('3C')
010900051109     d C_tab2          c                   const('3K')
011000140423     d*C_tab3          c                   const('3R')
011100051109     d C_tab4          c                   const('4C')
011200060217     d C_tab5          c                   const('3Q')
011300060217     d C_tab6          c                   const('VAP')
011400060410     d C_tab7          c                   const('DSC')
011500120111     d C_tab8          c                   const('LAC')
011600130129     d C_tab9          c                   const('DKC')
011700130129     d C_tab10         c                   const('OSR')
011800150810     d C_tab11         c                   const('3EW')
011900051123     d C_tab1_a        c                   const('Cliente Unificante ')
012000051123     d C_tab2_a        c                   const('Unificante Consegna')
012100051123     d C_tab2_b        c                   const('Unificante Giacenza')
012200051123     d C_tab4_a        c                   const('Unificante Consegna')
012300051123     d C_tab4_b        c                   const('Unificante Fatturaz')
012400060217     d C_tab5_a        c                   const('Cliente Unificante ')
012500060217     d C_tab6_a        c                   const('Cliente di invio   ')
012600060410     d C_tab7_a        c                   const('Cliente Unificante ')
012700120111     d C_tab8_a        c                   const('Cliente Raggruppam.')
012800120111     d C_tab8_b        c                   const('Cliente Forzato    ')
012900120112     d C_tab9_a        c                   const('Cliente 1          ')
013000120112     d C_tab9_b        c                   const('Cliente 2          ')
013100060202      * - Costanti per l'SQL
013200060202     d C_Union         c                   const('UNION')
013300051109     d C_OrderBy       c                   const('ORDER BY +
013400060202     d                                            TABcod, +
013500060202     d                                            TABke1, +
013600060202     d                                            TABdat')
013700051109     d C_ForFetch      c                   const('FOR FETCH ONLY')
013800051109      *
013900051109      *?S C H I E R E?------------------------------------------------*
014000051109      *
014100051110     d $Msg            s             78    dim( 4) ctdata perrcd(1)
014200060202     d $Sql_TABEL      s             78    dim( 6) ctdata perrcd(1)
014300060410     d $Sql_TNTBE      s             78    dim( 8) ctdata perrcd(1)
014400051109     d $Cmd            s                   like(Qcmd)
014500060217     d                                     dim( 2) ctdata perrcd(1)
014600051110     d $Txt            s             66    dim( 9) ctdata perrcd(1)
014700051109      *
014800051109      *?S T R U T T U R E   D A T I?----------------------------------*
014900051109      *
015000051109      * - Parametri
015100051109     d KPJBA         e ds
015200100211     d TRTB06ds      e ds                  inz
015300051109      *
015400150812     d TNTB79ds      e ds                  inz
015500150812      *
015600051109      * - Parametri x Controllo profilo utenti
015700051109     d TIBS34ds      e ds                  inz
015800051109      * - Ds di riferimento al file esterno AZUTE00F
015900051109     d AZUTEds       e ds                  extname(AZUTE00F)
016000051109      * - Ds per dati organigramma
016100051109     d DDatiUte      e ds
016200051109      *
016300051109     d DS            e ds                  inz
016400051109     d ds3C          e ds                  inz
016500051109     d ds3K          e ds                  inz
016600060217     d ds3Q          e ds                  inz
016700140424     d*ds3R          e ds                  inz
016800051109     d ds4C          e ds                  inz
016900130129     d*dDKC*****     e ds                  inz
017000060410     d dDSC          e ds                  inz
017100120111     d dLAC          e ds                  inz
017200130129     d*dVAP*****     e ds                  inz
017300120112     d dOSR          e ds                  inz
017400051109      *
017500051109     d TIBS69ds      e ds                  inz
017600051109     d DS_cnaco      e ds                  inz  extname(CNACO00F)
017700051109     d DS_cnind      e ds                  inz  extname(CNIND00F)
017800051109     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
017900051109     d DS_fncls      e ds                  inz  extname(FNCLS00F)
018000120111      *
018100120111      * - Parametri x manutenzione tab. "LAC"
018200120111     d TNTB46ds      e ds                  inz
018300051109      *
018400051109     d Status         sds
018500130129     d   SDSpgm          *proc
018600130129     d   SDSusr              254    263
018700051110      *
018800051110     d DSFMT           ds           512
018900130129     d   £Tasto              369    369
019000130129     d   £SFLnrr             378    379B 0
019100051110      *
019200051117     d ds_TABEL        ds                  inz
019300130129     d   wSQLcod                           inz like(TBEcod)
019400130129     d   wSQLkey                           inz like(TBEke1)
019500130129     d   wSQLuni                           inz like(TBEuni)
019600120112      *
019700120112     d ds_Comodo       ds          6000    inz
019800120112     d   sk_Comodo                   60    inz  dim(100)
019900051109      *
020000051109      *?V A R I A B I L I?--------------------------------------------*
020100051109      *
020200051109      * - Flags booleani
020300120112     d $EoF            s               n   inz(*off)
020400120112     d $Err            s               n   inz(*off)
020500120112     d $Fine           s               n   inz(*off)
020600120112     d $InzD01         s               n   inz(*on)
020700120112     d $InzS01         s               n   inz(*on)
020800120112     d WinKG           s               n   inz(*off)
020900120112     d $Tabel          s               n   inz(*off)
021000120112     d $Tntbe1         s               n   inz(*off)
021100120112     d $Tntbe2         s               n   inz(*off)
021200120112     d $Tntbe3         s               n   inz(*off)
021300091214     d $Called         s               n   inz
021400051109      *
021500051109     d $Video          s              1    inz('1')
021600051109      * - Indici di schiera / Contatori
021700051109     d xx              s              3  0 inz
021800051109     d yy              s              3  0 inz
021900051109      * - Parametri per SQL
022000120112     d wSQL            s           5000    inz  varying
022100051215     d wAndOr          s              5    inz('AND (')
022200051109      * - Parametri per QCMDEXC
022300051109     d Qcmd            s             80    inz
022400051109     d Qlen            s             15  5 inz(80)
022500051109      * - Variabili riferite al data base o al display file
022600051122     d S01nrr          s                   like(C01rcd) inz
022700051110     d Wksc            s                   like(I69kac) inz
022800051109      * - Campi di comodo
022900051123     d WkscA           s              7    inz(*zeros)
023000051109     d Data_Iso        s               d   inz  datfmt(*iso)
023100051110     d Data_Eur        s               d   inz  datfmt(*eur)
023200051110     d wDate           s              8  0 inz
023300051110     d wTime           s              6  0 inz
023400051110     d wPag            s              4  0 inz
023500051110     d wRig            s              3  0 inz
023600051110      *
023700051110      *?K E Y - L I S T?----------------------------------------------*
023800051110      *
023900051110     c     K03TABEL      klist
024000051110     c                   kfld                    TBLkut
024100051110     c                   kfld                    TBLcod
024200051110     c                   kfld                    TBLkey
024300060202      *
024400060202     c     K05TBE01      klist
024500060202     c                   kfld                    TBEcod
024600060202     c                   kfld                    TBEke1
024700060202     c                   kfld                    TBEke2
024800060202     c                   kfld                    TBElin
024900060202     c                   kfld                    TBEsif
025000060202     c     K02TNTBE      klist
025100060202     c                   kfld                    TBEcod
025200060202     c                   kfld                    TBEke1
025300051109      *
025400051109      *===============================================================*
025500051109      *                                                               *
025600051109      *?I N D I C A T O R I?------------------------------------------*
025700051109      *                                                               *
025800051109      *  28     - Emette il messaggio di errore a video               *
025900051109      *  30     - Ripulisce il subfile S1                             *
026000051109      *  31     - NON emette il subfile S1                            *
026100051109      *  32     - Record modificato nel subfile S1  (sflnxtchg)       *
026200051109      *  33     - Fine dati nel subfile S1          (sflend)          *
026300051110      *  39     - subfile S1 senza records                            *
026400051110      *  40     - condiziona ordinamento per codice tabella/cliente   *
026500100216      *  41     - condiziona gestione opzioni                         *
026600120807      *  42     - evidenzia a video S1 cliente BLOCCATO               *
026700100216      *  45     - abilita ricerca cliente per ragione sociale         *
026800051109      *  50     - fmt D1: Codice cliente errato                       *
026900051109      *           fmt S1: Opzione errata                              *
027000051109      *  51     - fmt D1: Selezione 1ª tabella errata                 *
027100051109      * (52     - fmt D1: Selezione 2ª tabella errata)                *
027200051109      * (53     - fmt D1: Selezione 3ª tabella errata)                *
027300051109      * (54     - fmt D1: Selezione 4ª tabella errata)                *
027400130129      * (55     - fmt D1: Selezione 5ª tabella errata)                *
027500130129      * (56     - fmt D1: Selezione 6ª tabella errata)                *
027600130129      * (57     - fmt D1: Selezione 7ª tabella errata)                *
027700130129      * (58     - fmt D1: Selezione 8ª tabella errata)                *
027800130129      * (59     - fmt D1: Selezione 9ª tabella errata)                *
027900130129      * (60     - fmt D1: Selezione 10ª tabella errata)               *
028000051109      *                                                               *
028100051109      *===============================================================*
028200051109      *
028300051109     c     *Entry        plist
028400051109     c                   parm                    KPJBA
028500140115     C
028600140115     C/EXEC SQL
028700140115     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
028800140115     C/END-EXEC
028900140115     C
029000051109      *
029100051109      * Operazioni iniziali
029200051110     c                   exsr      OpeIni
029300051109      *
029400051109      * Gestione Video
029500051109do  1c                   DOW       $Fine   =  *off
029600051109      * - Filtro di lancio
029700051109cas 2c     $Video        caseq     '1'           GesD01
029800051109      * - SubFile
029900051109cas 2c     $Video        caseq     '2'           GesS01
030000051109e   2c                   endcs
030100051109e   1c                   ENDDO
030200051109      *
030300051110      * Fine programma
030400051110     c                   eval      *inLR   =  *on
030500051109      *
030600051109      *
030700051109      *
030800051109      *?S U B R O U T I N E S?----------------------------------------*
030900051109      *
031000051109      *---------------------------------------------------------------*
031100051109      *?Operazioni iniziali                                          ?*
031200051109      *---------------------------------------------------------------*
031300051110     c     OpeIni        BEGSR
031400051109      *
031500051109      * Reperisce dati job
031600051109     c     *dtaara       define    §azute        azuteds
031700051109     c     *dtaara       define    §datiute      ddatiute
031800051109      *
031900051109     c                   in(E)     *dtaara
032000051109     c                   IF        %ERROR or RSUT = *blanks
032100051109     c                   clear                   Tibs34Ds
032200051109     c                   call      'TIBS34R'
032300051109     c                   parm                    Tibs34Ds
032400051109     c                   in        *dtaara
032500051109     c                   ENDIF
032600051110      *
032700060217     c                   movel     SDSpgm        T1pgm
032800091214      *
032900091214      /free
033000091214         // -?Verifica se chiamato (con codice cliente come parametro);?
033100091214         //  ?se chiamato: salta la 1ª videata?
033200100211         if  %subst( KPJBU : 1 : 7 ) > *zero;
033300091214           $Called = *on;
033400091214           $InzD01 = *off;
033500091214           exsr  InzD01;
033600100211           //V1Cksc = %int( %subst( KPJBU : 1 : %len(V1Cksc) ) );
033700100211           TRTB06ds = kpjbu;
033800100211           V1Cksc = D06ksc;
033900100211           if  D063C  <> *blank   or
034000100211               D063K  <> *blank   or
034100140424         //    D063R  <> *blank   or
034200100211               D064C  <> *blank   or
034300100211               D063Q  <> *blank   or
034400100211               D06VAP <> *blank   or
034500120111               D06DSC <> *blank   or
034600120112               D06LAC <> *blank   or
034700130129               D06DKC <> *blank   or
034800150813               D06OSR <> *blank   or
034900150813               D063EW <> *blank;
035000100211             V1Cse1 = D063C;
035100100211             V1Cse2 = D063K;
035200140423           //V1Cse3 = D063R;
035300100211             V1Cse4 = D064C;
035400100211             V1Cse5 = D063Q;
035500100211             V1Cse6 = D06VAP;
035600100211             V1Cse7 = D06DSC;
035700120111             V1Cse8 = D06LAC;
035800130129             V1Cse9 = D06DKC;
035900130129             V1Cse10 = D06OSR;
036000150813             V1Cse11 = D063EW;
036100100211           endif;
036200091214           exsr  CtrD01;
036300091214           if  *in90;
036400091214             write  TB06T1;
036500130129           //else;
036600130129           //  $Video = '2';
036700091214           endif;
036800130129           *in51 = *on;
036900091214         endif;
037000091214      /end-free
037100051109      *
037200051109     c                   ENDSR
037300051109      *
037400051109      *---------------------------------------------------------------*
037500051110      *?Gestione videata D01                                         ?*
037600051109      *---------------------------------------------------------------*
037700051109     c     GesD01        BEGSR
037800051109      *
037900051109      * Inizializzo la videata
038000051109if  1c                   if        $InzD01 =  *on
038100051109     c                   exsr      InzD01
038200051110     c                   eval      $InzD01 =  *off
038300051109e   1c                   endif
038400051109      *
038500051109      * Scrivo la testata e la riga tasti funzionali abilitati
038600051109if  1c                   if        NOT *in90
038700051109     c                   write     TB06T1
038800051109e   1c                   endif
038900051109      * Emetto la videata
039000051109     c                   exfmt     TB06D1
039100051109     c                   setoff                                       28  90
039200051109     c                   clear                   V1Dmsg
039300051109      *
039400051109sel 1c                   select
039500051109      * F3=Fine
039600051109w   1c                   when      *inKC
039700051109     c                   exsr      F03D01
039800051109     c                   leavesr
039900051123      * F7=Ricerca cliente
040000051123w   1c                   when          *inKG
040100051123     c                             and *in45  =  *off
040200051123     c                   eval      *in45   =  *on
040300051123     c                   eval      WinKG   =  *on
040400051123     c                   leavesr
040500051123      * Ricerca alfabetica se:
040600051123      *   · è stato premuto F7
040700051123w   1c                   when          *in45  =  *on
040800051123     c                             and WinKG  =  *on
040900051123      *   · il codice cliente è vuoto
041000051123     c                             and V1Cksc =  *zeros
041100051123      *   · la ragione sociale è piena
041200051123     c                             and V1Dksc <> *blanks
041300051123     c                   exsr      SearchCli
041400051124     c                   leavesr
041500051109      *
041600051109e   1c                   endsl
041700051109      *
041800051109      * Controllo dati immessi a video
041900051109     c                   exsr      CtrD01
042000051109     c                   if        *in90
042100051109     c                   leavesr
042200051109     c                   endif
042300051109      *
042400051109      * Passaggio al subfile
042500051109     c                   eval      $InzS01 =  *on
042600051109     c                   eval      $Video  =  '2'
042700051109      *
042800051109     c                   ENDSR
042900051109      *
043000051109      *---------------------------------------------------------------*
043100051110      *?Inizializzazione videata D01                                 ?*
043200051109      *---------------------------------------------------------------*
043300051109     c     InzD01        BEGSR
043400051109      *
043500051109     c                   clear                   TB06D1
043600051109      *
043700051109      * Imposto i dati di default
043800051123     c                   eval      *in45   =  *off
043900051109     c                   eval      V1Ctb1  =  C_tab1
044000051109     c                   eval      V1Ctb2  =  C_tab2
044100140423     c*****              eval      V1Ctb3  =  C_tab3
044200051109     c                   eval      V1Ctb4  =  C_tab4
044300060202     c                   eval      V1Ctb5  =  C_tab5
044400060217     c                   eval      V1Ctb6  =  C_tab6
044500060410     c                   eval      V1Ctb7  =  C_tab7
044600120111     c                   eval      V1Ctb8  =  C_tab8
044700120112     c                   eval      V1Ctb9  =  C_tab9
044800130129     c                   eval      V1Ctb10 =  C_tab10
044900150810     c                   eval      V1Ctb11 =  C_tab11
045000051109     c                   eval      V1Cse1  =  'S'
045100051109     c                   eval      V1Cse2  =  'S'
045200140423     c*****              eval      V1Cse3  =  'S'
045300051109     c                   eval      V1Cse4  =  'S'
045400060202     c                   eval      V1Cse5  =  'S'
045500060217     c                   eval      V1Cse6  =  'S'
045600060410     c                   eval      V1Cse7  =  'S'
045700120111     c                   eval      V1Cse8  =  'S'
045800120112     c                   eval      V1Cse9  =  'S'
045900130129     c                   eval      V1Cse10 =  'S'
046000150810     c                   eval      V1Cse11 =  'S'
046100051109      *
046200051109      * Decodifica tabelle
046300051109     c                   eval      TBLkut  =  1
046400051109     c                   clear                   TBLcod
046500051109     c                   eval      TBLkey  =  *zeros
046600060202     c                   clear                   TBEcod
046700060202     c                   movel     *zeros        TBEke1
046800060202     c                   clear                   TBEke2
046900060202     c                   clear                   TBElin
047000060202     c                   movel     KNSIF         TBEsif
047100051109      * - 1ª tabella ('3C')
047200051109if  1c                   if        V1Ctb1  <> *blanks
047300060202     c***                move      V1Ctb1        TBLkey
047400060202     c                   eval      %subst(TBLkey:7:2) =
047500060202     c                             %subst(V1Ctb1:1:2)
047600051109     c     K03TABEL      chain     TABEL
047700130808     c                   if        %found(TABEL00F) and
047800130808     c                             TBLflg  =  *blanks
047900051109     c                   movel     TBLuni        DS
048000060202     c*** già impostato: move      TBLkey        V1Ctb1
048100051109     c                   movel     §des1         V1Dtb1
048200051109     c                   movel     §pgm          V1Hpg1
048300051109     c                   endif
048400051109e   1c                   endif
048500051109      * - 2ª tabella ('3K')
048600051123if  1c                   if        V1Ctb2  <> *blanks
048700060202     c***                move      V1Ctb2        TBLkey
048800060202     c                   eval      %subst(TBLkey:7:2) =
048900060202     c                             %subst(V1Ctb2:1:2)
049000051109     c     K03TABEL      chain     TABEL
049100130808     c                   if        %found(TABEL00F) and
049200130808     c                             TBLflg  =  *blanks
049300051109     c                   movel     TBLuni        DS
049400060202     c*** già impostato: move      TBLkey        V1Ctb2
049500051109     c                   movel     §des1         V1Dtb2
049600051109     c                   movel     §pgm          V1Hpg2
049700051109     c                   endif
049800051123e   1c                   endif
049900051109      * - 3ª tabella ('3R')
050000140423if  1c****               if        V1Ctb3  <> *blanks
050100060202     c***                move      V1Ctb3        TBLkey
050200140423     c****               eval      %subst(TBLkey:7:2) =
050300140423     c****                         %subst(V1Ctb3:1:2)
050400140423     c**** K03TABEL      chain     TABEL
050500140423     c****               if        %found(TABEL00F) and
050600140423     c****                         TBLflg  =  *blanks
050700140423     c****               movel     TBLuni        DS
050800060202     c*** già impostato: move      TBLkey        V1Ctb3
050900140423     c****               movel     §des1         V1Dtb3
051000140423     c****               movel     §pgm          V1Hpg3
051100140423     c****               endif
051200140423e   1c****               endif
051300051109      * - 4ª tabella ('4C')
051400051123if  1c                   if        V1Ctb4  <> *blanks
051500060202     c***                move      V1Ctb4        TBLkey
051600060202     c                   eval      %subst(TBLkey:7:2) =
051700060202     c                             %subst(V1Ctb4:1:2)
051800051109     c     K03TABEL      chain     TABEL
051900130808     c                   if        %found(TABEL00F) and
052000130808     c                             TBLflg  =  *blanks
052100051109     c                   movel     TBLuni        DS
052200060202     c*** già impostato: move      TBLkey        V1Ctb4
052300051109     c                   movel     §des1         V1Dtb4
052400051109     c                   movel     §pgm          V1Hpg4
052500051109     c                   endif
052600051123e   1c                   endif
052700060217      * - 5ª tabella ('3Q')
052800060217if  1c                   if        V1Ctb5  <> *blanks
052900060217     c***                move      V1Ctb5        TBLkey
053000060217     c                   eval      %subst(TBLkey:7:2) =
053100060217     c                             %subst(V1Ctb5:1:2)
053200060217     c     K03TABEL      chain     TABEL
053300130808     c                   if        %found(TABEL00F) and
053400130808     c                             TBLflg  =  *blanks
053500060217     c                   movel     TBLuni        DS
053600060217     c*** già impostato: move      TBLkey        V1Ctb5
053700060217     c                   movel     §des1         V1Dtb5
053800060217     c                   movel     §pgm          V1Hpg5
053900060217     c                   endif
054000060217e   1c                   endif
054100060217      * - 6ª tabella ('VAP')
054200060217if  1c                   if        V1Ctb6  <> *blanks
054300060217     c                   move      V1Ctb6        TBEke1
054400060202     c                   movel     KNSIF         TBEsif
054500060202     c     K05TBE01      chain     TNTBE000
054600060202     c                   if        NOT %found(TNTBE01L)
054700060202     c                   clear                   TBEsif
054800060202     c     K05TBE01      chain     TNTBE000
054900060202     c                   endif
055000130808     c                   if        %found(TNTBE01L) and
055100130808     c                             TBLflg  =  *blanks
055200060217     c                   move      TBEke1        V1Ctb6
055300060217     c                   movel     TBEuni        V1Dtb6
055400060217     c                   move      TBEuni        V1Hpg6
055500060202     c                   endif
055600060202e   1c                   endif
055700060410      * - 7ª tabella ('DSC')
055800060410if  1c                   if        V1Ctb7  <> *blanks
055900060410     c                   move      V1Ctb7        TBEke1
056000060410     c                   movel     KNSIF         TBEsif
056100060410     c     K05TBE01      chain     TNTBE000
056200060410     c                   if        NOT %found(TNTBE01L)
056300060410     c                   clear                   TBEsif
056400060410     c     K05TBE01      chain     TNTBE000
056500060410     c                   endif
056600130808     c                   if        %found(TNTBE01L) and
056700130808     c                             TBLflg  =  *blanks
056800060410     c                   move      TBEke1        V1Ctb7
056900060410     c                   movel     TBEuni        V1Dtb7
057000060410     c                   move      TBEuni        V1Hpg7
057100060410     c                   endif
057200060410e   1c                   endif
057300120111      * - 8ª tabella ('LAC')
057400120111if  1c                   if        V1Ctb8  <> *blanks
057500120111     c                   move      V1Ctb8        TBEke1
057600120111     c                   movel     KNSIF         TBEsif
057700120111     c     K05TBE01      chain     TNTBE000
057800120111     c                   if        NOT %found(TNTBE01L)
057900120111     c                   clear                   TBEsif
058000120111     c     K05TBE01      chain     TNTBE000
058100120111     c                   endif
058200130808     c                   if        %found(TNTBE01L) and
058300130808     c                             TBLflg  =  *blanks
058400120111     c                   move      TBEke1        V1Ctb8
058500120111     c                   movel     TBEuni        V1Dtb8
058600120111     c                   move      TBEuni        V1Hpg8
058700120111     c                   endif
058800120111e   1c                   endif
058900130129      * - 9ª tabella ('DKC')
059000120112if  1c                   if        V1Ctb9  <> *blanks
059100120112     c                   move      V1Ctb9        TBEke1
059200120112     c                   movel     KNSIF         TBEsif
059300120112     c     K05TBE01      chain     TNTBE000
059400120112     c                   if        NOT %found(TNTBE01L)
059500120112     c                   clear                   TBEsif
059600120112     c     K05TBE01      chain     TNTBE000
059700120112     c                   endif
059800130808     c                   if        %found(TNTBE01L) and
059900130808     c                             TBLflg  =  *blanks
060000120112     c                   move      TBEke1        V1Ctb9
060100120112     c                   movel     TBEuni        V1Dtb9
060200120112     c                   move      TBEuni        V1Hpg9
060300120112     c                   endif
060400120112e   1c                   endif
060500130129      * - 10ª tabella ('OSR')
060600130129if  1c                   if        V1Ctb10 <> *blanks
060700130129     c                   move      V1Ctb10       TBEke1
060800130129     c                   movel     KNSIF         TBEsif
060900130129     c     K05TBE01      chain     TNTBE000
061000130129     c                   if        NOT %found(TNTBE01L)
061100130129     c                   clear                   TBEsif
061200130129     c     K05TBE01      chain     TNTBE000
061300130129     c                   endif
061400130808     c                   if        %found(TNTBE01L) and
061500130808     c                             TBLflg  =  *blanks
061600130129     c                   move      TBEke1        V1Ctb10
061700130129     c                   movel     TBEuni        V1Dtb10
061800130129     c                   move      TBEuni        V1Hpg10
061900130129     c                   endif
062000130129e   1c                   endif
062100150810      * - 11ª tabella ('3EW')
062200150810if  1c                   if        V1Ctb11 <> *blanks
062300150810     c                   move      V1Ctb11       TBEke1
062400150810     c                   movel     KNSIF         TBEsif
062500150810     c     K05TBE01      chain     TNTBE000
062600150810     c                   if        NOT %found(TNTBE01L)
062700150810     c                   clear                   TBEsif
062800150810     c     K05TBE01      chain     TNTBE000
062900150810     c                   endif
063000150810     c                   if        %found(TNTBE01L) and
063100150810     c                             TBLflg  =  *blanks
063200150810     c                   move      TBEke1        V1Ctb11
063300150810     c                   movel     TBEuni        V1Dtb11
063400150810     c                   move      TBEuni        V1Hpg11
063500150810     c                   endif
063600150810e   1c                   endif
063700051109      *
063800051109     c                   ENDSR
063900051109      *
064000051109      *---------------------------------------------------------------*
064100051110      *?Gestione tasto funzionale F03 da videata D01                 ?*
064200051109      *---------------------------------------------------------------*
064300051109     c     F03D01        BEGSR
064400051109      *
064500051109      * Chiudo il programma
064600051109     c                   eval      $Fine   =  *on
064700051109      *
064800051109     c                   ENDSR
064900051109      *
065000051109      *---------------------------------------------------------------*
065100051110      *?Controllo dati immessi in videata D01                        ?*
065200051109      *---------------------------------------------------------------*
065300051109     c     CtrD01        BEGSR
065400051109      *
065500051109     c                   movea     *zeros        *in(50)
065600051109      *
065700051109      * Controllo/Decodifico codice cliente
065800051109     c                   clear                   V1Dksc
065900120807     c                   clear                   V1Dabl
066000051109if  1c                   IF        V1Cksc  =  *zeros
066100051109     c                   seton                                        285090
066200051109     c                   eval      V1Dmsg  =  $Msg(1)
066300051109x   1c                   ELSE
066400051110     c                   move      V1Cksc        Wksc
066500051110     c                   exsr      Decod_Cli
066600051109if  2c                   if        O69err  =  *on
066700051109     c                   seton                                        285090
066800051109     c                   eval      V1Dmsg  =  $Msg(2)
066900051109x   2c                   else
067000051109     c                   movel     ACOrag        V1Dksc
067100130315     c                   if        ACOabl <> *blanks
067200120807     c                   eval      V1Dabl = 'BLOCCATO'
067300120807     c                   endif
067400051109e   2c                   endif
067500051109e   1c                   ENDIF
067600051109     c                   if        *in90
067700051109     c                   leavesr
067800051109     c                   endif
067900051109      *
068000051109      * Controllo selezioni
068100051109     c                   if            V1Cse1 = *blanks
068200051109     c                             and V1Cse2 = *blanks
068300140423     c*****                        and V1Cse3 = *blanks
068400051109     c                             and V1Cse4 = *blanks
068500060202     c                             and V1Cse5 = *blanks
068600060217     c                             and V1Cse6 = *blanks
068700060410     c                             and V1Cse7 = *blanks
068800120111     c                             and V1Cse8 = *blanks
068900120112     c                             and V1Cse9 = *blanks
069000130129     c                             and V1Cse10 = *blanks
069100150812     c                             and V1Cse11 = *blanks
069200051109     c                   seton                                        285190
069300051109     c                   eval      V1Dmsg  =  $Msg(3)
069400051109     c                   leavesr
069500051109     c                   endif
069600060202     c                   if            V1Cse1 = *blanks
069700060202     c                             and V1Cse2 = *blanks
069800140423     c*****                        and V1Cse3 = *blanks
069900060202     c                             and V1Cse4 = *blanks
070000060217     c                             and V1Cse5 = *blanks
070100060202     c                   eval      $Tabel  =  *off
070200060202     c                   else
070300060202     c                   eval      $Tabel  =  *on
070400060202     c                   endif
070500060217     c                   if            V1Cse6 = *blanks
070600150812     c                             and V1Cse11= *blanks
070700060410     c                   eval      $Tntbe2 =  *off
070800060202     c                   else
070900060410     c                   eval      $Tntbe2 =  *on
071000060202     c                   endif
071100060410     c                   if            V1Cse7 = *blanks
071200120111     c                             and V1Cse8 = *blanks
071300130129     c                             and V1Cse9 = *blanks
071400060410     c                   eval      $Tntbe1 =  *off
071500060410     c                   else
071600060410     c                   eval      $Tntbe1 =  *on
071700060410     c                   endif
071800130129     c                   if            V1Cse10 = *blanks
071900120112     c                   eval      $Tntbe3 =  *off
072000120112     c                   else
072100120112     c                   eval      $Tntbe3 =  *on
072200120112     c                   endif
072300051109      *
072400051109     c                   ENDSR
072500051123      *
072600051123      *---------------------------------------------------------------*
072700051123      *?Ricerca cliente                                              ?*
072800051123      *---------------------------------------------------------------*
072900051123     c     SearchCli     BEGSR
073000051123      *
073100051123     c                   call      'XALFA3BR'
073200051123     c                   parm      RSUT          PAXdut           30
073300051123     c                   parm                    TBLkut
073400051123     c                   parm      V1Dksc        PAXdmt           48
073500051123     c                   parm      DUTkci        PAXccc            4 0
073600051123     c                   parm      9             PAXsta            1 0
073700051123     c                   parm                    PAXflr           90
073800051123     c                   parm      *blanks       PAXdit            3
073900051123     c                   parm      1             PAXnum            2 0
074000051123     c                   parm                    PAXksc           80
074100051123     c                   parm                    PAXksm          140
074200051123     c                   parm                    PAXkdm           60
074300051123      *
074400051123      *   · Imposto il codice selezionato a video
074500051123      *     (se selezionato e non premuto F12)
074600051123if  1c                   if        PAXsta >= *zeros
074700051123     c                   movel     PAXksm        V1Cksc
074800051123     c                   movel     PAXdmt        V1Dksc
074900051123     c                   eval      *in45   =  *off
075000051123     c                   reset                   WinKG
075100051123e   1c                   endif
075200051123      *
075300051123     c                   ENDSR
075400051109      *
075500051109      *---------------------------------------------------------------*
075600051110      *?Gestione videata C01 / S01                                   ?*
075700051109      *---------------------------------------------------------------*
075800051109     c     GesS01        BEGSR
075900051109      *
076000051109      * Inizializzo la videata
076100051109if  1c                   if        $InzS01 =  *on
076200051109     c                   exsr      InzS01
076300051110     c                   eval      $InzS01 = *off
076400051109e   1c                   endif
076500100216      *
076600100216      * Impostazione attributi di visualizzazione
076700130201     c                   eval      *in41 = ($Called = *on  and
076800130201     c                                      D06op0  = *blank)
076900051110      *
077000051110      * Posiziono il cursore
077100051110if  1c                   if        C01csr  >  *zeros
077200051110     c                   z-add     C01csr        C01rcd
077300051110e   1c                   endif
077400051109      *
077500051109      * Scrivo la testata e la riga tasti funzionali abilitati
077600051110     c                   eval      *in39   =  (C01rcd <= *zeros)
077700051109if  1c                   if        NOT *in90
077800051109     c                   write     TB06T1
077900051110     c                   write     TB06Z1
078000051109e   1c                   endif
078100051110      *
078200051110      * Visualizzo (eventualmente) il msg per la mancanza di dati
078300051110if  1c                   if        C01rcd  <= *zeros
078400051110     c                   write     TB06D2
078500051110e   1c                   endif
078600051109      * Emetto la videata
078700051110     c                   exfmt     TB06C1
078800051110     c                   z-add     £SFLnrr       C01rcd
078900051109     c                   setoff                                       28  90
079000051109     c                   clear                   V1Dmsg
079100051109      *
079200051110sel 1c                   SELECT
079300051109      * F3=Fine
079400051110w   1c                   WHEN      *inKC
079500051109     c                   exsr      F03D01
079600051110      * F8=Stampa elenco legami
079700051110w   1c                   WHEN      *inKH
079800051110     c                   exsr      F08S01
079900051122      * F11=Ordinamento x codice cliente / codice tabella
080000051122w   1c                   WHEN      *inKK
080100051122     c                   exsr      F11S01
080200051110      * F12=Ritorno
080300051110w   1c                   WHEN      *inKL
080400051110     c                   exsr      F12S01
080500051110      * NO rec.
080600051110w   1c                   WHEN      *in39
080700051110      * Gestione opzioni inserite
080800051110x   1c                   OTHER
080900051110     c                   exsr      OpzS01
081000051110      *
081100051110e   1c                   ENDSL
081200051109      *
081300051109     c                   ENDSR
081400051109      *
081500051109      *---------------------------------------------------------------*
081600051110      *?Inizializzazione videata C01 / S01                           ?*
081700051109      *---------------------------------------------------------------*
081800051110     c     InzS01        BEGSR
081900051110      *
082000051110      * Pulizia subfile
082100051110     c                   seton                                        3031
082200051110     c                   write     TB06C1
082300051110     c                   setoff                                         3133
082400051110      *
082500051110     c                   reset                   $EoF
082600120807     c                   reset                   WrkSort
082700051110     c                   clear                   C01rcd
082800051110     c                   clear                   C01csr
082900051110     c                   clear                   S01nrr
083000051110     c                   clear                   V1Dmsg
083100051110     c                   setoff                                       28  90
083200051110     c                   eval      *in32   =  *off
083300051122     c                   eval      *in40   =  (WrkSort = *on)
083400051110     c                   movea     *zeros        *in(50)
083500051110      *
083600051110      * Caricamento dei dati da presentare nel sfl (NON a pagine)
083700051110      *
083800051110      * - Preparazione stringa SQL
083900060202     c                   clear                   wSQL
084000060202      *   - da TABEL00F
084100060202     c                   if        $Tabel  =  *on
084200060202     c                   exsr      PrepSQL1
084300060202     c                   endif
084400060202      *   - da TNTBE00F
084500060410     c                   if            $Tntbe1 =  *on
084600060410     c                             or  $Tntbe2 =  *on
084700120112     c                             or  $Tntbe3 =  *on
084800060202     c                   exsr      PrepSQL2
084900060202     c                   endif
085000060202      *   - impostazioni finali
085100060202     c                   exsr      PrepSQL3
085200051110      *
085300051110      * - Apertura cursore
085400051110     c                   exsr      OpenCursor
085500051110      *
085600051110      * - Esecuzione SQL
085700051110     c                   dou       $Eof    =  *on
085800051110     c                   exsr      ReadCursor
085900051110     c                   enddo
086000051110      *
086100051110      * Chiusura cursore
086200051110     c                   exsr      CloseCursor
086300051110      *
086400051110      * Memorizzo NRR dell'ultimo record scritto nel sfl
086500051110     c                   eval      RRNlast =  S01nrr
086600051110      *
086700051110      * Avendo caricato TUTTI i dati (NON SOLO quelli della 1ª pagina)
086800051110      *  occorre impostare il posizionamento cursore sul 1° rec del sfl
086900051110if  1c                   if        C01csr  >  *zeros
087000051110     c                   eval      C01csr  =  1
087100051110e   1c                   endif
087200051109      *
087300051109     c                   ENDSR
087400051109      *
087500051109      *---------------------------------------------------------------*
087600060202      *?Preparazione stringa SQL per file TABEL00F                   ?*
087700051109      *---------------------------------------------------------------*
087800060202     c     PrepSQL1      BEGSR
087900051109      *
088000051109     c                   movel(p)  V1Cksc        TBLkey
088100051110     c                   reset                   wAndOr
088200051123      *
088300051123      * Reperisco eventuali codici clienti unificanti da tab. 3C
088400051123     c                   clear                   ds3C
088500060410if  1c                   if        V1Cse1  =  'S'
088600060410     c                   eval      TBLcod  = '3C'
088700051123     c     K03TABEL      chain     TABEL
088800051123     c                   if            %found(TABEL00F)
088900051123     c                             and TBLflg = *blanks
089000051123     c                   movel     TBLuni        ds3C
089100051123     c                   endif
089200060410e   1c                   endif
089300051123      * Reperisco eventuali codici clienti unificanti da tab. 3K
089400051123     c                   clear                   ds3K
089500060410if  1c                   if        V1Cse2  =  'S'
089600060410     c                   eval      TBLcod  = '3K'
089700051123     c     K03TABEL      chain     TABEL
089800051123     c                   if            %found(TABEL00F)
089900051123     c                             and TBLflg = *blanks
090000051123     c                   movel     TBLuni        ds3K
090100051123     c                   endif
090200060410e   1c                   endif
090300060217      * Reperisco eventuali codici clienti unificanti da tab. 3Q
090400060217     c                   clear                   ds3Q
090500060410if  1c                   if        V1Cse5  =  'S'
090600060410     c                   eval      TBLcod  = '3Q'
090700060217     c     K03TABEL      chain     TABEL
090800060217     c                   if            %found(TABEL00F)
090900060217     c                             and TBLflg = *blanks
091000060217     c                   movel     TBLuni        ds3Q
091100060217     c                   endif
091200060410e   1c                   endif
091300051123      * Reperisco eventuali codici clienti unificanti da tab. 3R
091400140423     c****               clear                   ds3R
091500140423if  1c****               if        V1Cse3  =  'S'
091600140423     c****               eval      TBLcod  = '3R'
091700140423     c**** K03TABEL      chain     TABEL
091800140423     c****               if            %found(TABEL00F)
091900140423     c****                         and TBLflg = *blanks
092000140423     c****               movel     TBLuni        ds3R
092100140423     c****               endif
092200140423e   1c****               endif
092300051123      * Reperisco eventuali codici clienti unificanti da tab. 4C
092400051123     c                   clear                   ds4C
092500060410if  1c                   if        V1Cse4  =  'S'
092600060410     c                   eval      TBLcod  = '4C'
092700051123     c     K03TABEL      chain     TABEL
092800051123     c                   if            %found(TABEL00F)
092900051123     c                             and TBLflg = *blanks
093000051123     c                   movel     TBLuni        ds4C
093100051123     c                   endif
093200060410e   1c                   endif
093300051109      *
093400051109      *? E S E M P I O : ?
093500051109      *
093600051109      * select TBLcod, TBLkey, TBLuni
093700051109      * from   TABEL00F
093800051109      * where  TBLflg   =  ' '
093900051215      *   AND ((TBLcod  =  '3C'
094000051109      *    and  (TBLkey =  '0430839 '
094100051109      *     or   substr(TBLuni, 79, 7) = '0430839'))
094200051109      *   OR   (TBLcod  =  '3K'
094300051109      *    and  (TBLkey =  '0430839 '
094400051109      *     or   substr(TBLuni, 27, 7) = '0430839'
094500051109      *     or   substr(TBLuni, 55, 7) = '0430839'))
094600051109      *   OR   (TBLcod  =  '3R'
094700051109      *    and  (TBLkey =  '0430839 '))
094800051109      *   OR   (TBLcod  =  '4C'
094900051109      *    and  (TBLkey =  '0430839 '
095000051109      *     or   substr(TBLuni, 29, 7) = '0430839'
095100051215      *     or   substr(TBLuni, 58, 7) = '0430839')))
095200051109      * order  by  TBLcod, TBLkey
095300051109      *
095400060202      *?Impostazioni iniziali per estrazione da TABEL?
095500051109     c                   eval      wSQL    =  %trim($Sql_TABEL(1))
095600051109     c                                     +  ' '
095700051109     c                                     +  %trim($Sql_TABEL(2))
095800051109     c                                     +  ' '
095900051109     c                                     +  %trim($Sql_TABEL(3))
096000051215     c*** ancora così:   eval      wAndOr  =  'AND ('
096100051109      *
096200051109      *?Impostazione estrazione dati da tabella "3C"?
096300051109      *
096400051110if  1c                   if        V1Cse1  =  'S'
096500120112     c                   eval      wSQL   +=  ' '
096600051109     c                                     +  %trim(wAndOr)
096700060202     c                                     +  ' '
096800051109     c                                     +  %trim($Sql_TABEL(4))
096900051109     c                                     +  C_tab1 +  ''''
097000051109     c                                     +  ' '
097100051109     c                                     +  %trim($Sql_TABEL(5))
097200051109     c                                     +  TBLkey +  ''''
097300140115     C                   IF        VidSoloCli = *blank
097400051123     c                   exsr      Add_Sel_3C
097500140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
097600140115     C                   ELSE
097700140115      * chiusura
097800140115     c                   eval      wSQL   +=  '))'
097900140115     C                   ENDIF
098000051215     c                   eval      wAndOr  =  'OR   '
098100051109e   1c                   endif
098200051109      *
098300051109      *?Impostazione estrazione dati da tabella "3K"?
098400051109      *
098500051110if  1c                   if        V1Cse2  =  'S'
098600120112     c                   eval      wSQL   +=  ' '
098700051109     c                                     +  %trim(wAndOr)
098800051109     c                                     +  ' '
098900051109     c                                     +  %trim($Sql_TABEL(4))
099000051109     c                                     +  C_tab2 +  ''''
099100051109     c                                     +  ' '
099200051109     c                                     +  %trim($Sql_TABEL(5))
099300051109     c                                     +  TBLkey +  ''''
099400140115     C                   IF        VidSoloCli = *blank
099500051123     c                   exsr      Add_Sel_3K
099600140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
099700140115     C                   ELSE
099800140115      * chiusura
099900140115     c                   eval      wSQL   +=  '))'
100000140115     C                   ENDIF
100100051215     c                   eval      wAndOr  =  'OR   '
100200051109e   1c                   endif
100300051109      *
100400051109      *?Impostazione estrazione dati da tabella "3R"?
100500051109      *
100600140423if  1c***                if        V1Cse3  =  'S'
100700140423     c***                eval      wSQL   +=  ' '
100800140423     c***                                  +  %trim(wAndOr)
100900140423     c***                                  +  ' '
101000140423     c***                                  +  %trim($Sql_TABEL(4))
101100140423     c***                                  +  C_tab3 +  ''''
101200140423     c***                                  +  ' '
101300140423     c***                                  +  %trim($Sql_TABEL(5))
101400140423     c***                                  +  TBLkey +  ''''
101500140423     C***                IF        VidSoloCli = *blank
101600140423     c***                exsr      Add_Sel_3R
101700140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
101800140423     C***                ELSE
101900140115      * chiusura
102000140423     c***                eval      wSQL   +=  '))'
102100140423     C***                ENDIF
102200140423     c***                eval      wAndOr  =  'OR   '
102300140423e   1c***                endif
102400051109      *
102500051109      *?Impostazione estrazione dati da tabella "4C"?
102600051109      *
102700051110if  1c                   if        V1Cse4  =  'S'
102800120112     c                   eval      wSQL   +=  ' '
102900051109     c                                     +  %trim(wAndOr)
103000051109     c                                     +  ' '
103100051109     c                                     +  %trim($Sql_TABEL(4))
103200051109     c                                     +  C_tab4 +  ''''
103300051109     c                                     +  ' '
103400051109     c                                     +  %trim($Sql_TABEL(5))
103500051109     c                                     +  TBLkey +  ''''
103600140115     C                   IF        VidSoloCli = *blank
103700051123     c                   exsr      Add_Sel_4C
103800140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
103900140115     C                   ELSE
104000140115      * chiusura
104100140115     c                   eval      wSQL   +=  '))'
104200140115     C                   ENDIF
104300051215     c                   eval      wAndOr  =  'OR   '
104400051109e   1c                   endif
104500060217      *
104600060217      *?Impostazione estrazione dati da tabella "3Q"?
104700060217      *
104800060217if  1c                   if        V1Cse5  =  'S'
104900120112     c                   eval      wSQL   +=  ' '
105000060217     c                                     +  %trim(wAndOr)
105100060217     c                                     +  ' '
105200060217     c                                     +  %trim($Sql_TABEL(4))
105300060217     c                                     +  C_tab5 +  ''''
105400060217     c                                     +  ' '
105500060217     c                                     +  %trim($Sql_TABEL(5))
105600060217     c                                     +  TBLkey +  ''''
105700140115     C                   IF        VidSoloCli = *blank
105800060217     c                   exsr      Add_Sel_3Q
105900140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
106000140115     C                   ELSE
106100140115      * chiusura
106200140115     c                   eval      wSQL   +=  '))'
106300140115     C                   ENDIF
106400060217     c                   eval      wAndOr  =  'OR   '
106500060217e   1c                   endif
106600051109      *
106700060202      *?chiusura?
106800051109      *
106900120112     c                   eval      wSQL   +=  ')'
107000051109      *
107100051109     c                   ENDSR
107200060202      *
107300060202      *---------------------------------------------------------------*
107400060202      *?Preparazione stringa SQL per file TNTBE00F                   ?*
107500060202      *---------------------------------------------------------------*
107600060202     c     PrepSQL2      BEGSR
107700060202      *
107800060202     c                   reset                   wAndOr
107900060202      *
108000060202      * Reperisco codice cliente di invio da tab. VAP
108100060410     c*****              clear                   dVAP
108200060410if  1c                   if        V1Cse6  =  'S'
108300060410     c                   eval      TBEcod  =  C_tab6
108400060202     c                   move      V1Cksc        WkscA
108500060410     c                   eval      TBEke1  =  '0' + WkscA
108600060202     c                   clear                   TBEke2
108700060202     c     K02TNTBE      chain     TNTBE000
108800060202     c                   if        NOT %found(TNTBE01L)
108900060202     c                             or  TBEatb <> *blanks
109000060202     c                   clear                   TBEke2
109100060202     c                   endif
109200060410e   1c                   endif
109300060410      * Reperisco eventuali codici clienti unificanti da tab. DSC
109400060410     c                   clear                   dDSC
109500060410if  1c                   if        V1Cse7  =  'S'
109600060410     c                   eval      TBEcod  =  C_tab7
109700060410     c                   movel(p)  V1Cksc        TBEke1
109800060410     c                   clear                   TBEke2
109900060410     c                   clear                   TBElin
110000060410     c                   clear                   TBEsif
110100060410     c     K05TBE01      chain     TNTBE000
110200060410     c                   if            %found(TNTBE01L)
110300060410     c                             and TBEatb = *blanks
110400060410     c                   movel     TBEuni        dDSC
110500060410     c                   endif
110600060410e   1c                   endif
110700120111      * Reperisco eventuali codici clienti da tab. LAC
110800120111     c                   clear                   dLAC
110900120111if  1c                   if        V1Cse8  =  'S'
111000120111     c                   eval      TBEcod  =  C_tab8
111100120111     c                   movel(p)  V1Cksc        TBEke1
111200120111     c                   clear                   TBEke2
111300120111     c                   clear                   TBElin
111400120111     c                   clear                   TBEsif
111500120111     c     K05TBE01      chain     TNTBE000
111600120111     c                   if            %found(TNTBE01L)
111700120111     c                             and TBEatb = *blanks
111800120111     c                   movel     TBEuni        dLAC
111900120111     c                   endif
112000120111e   1c                   endif
112100130129      * La tab. DKC non ha altri codici...
112200060202      *
112300060202      *?UNION?
112400060202if  1c                   if        $Tabel  =  *on
112500120112     c                   eval      wSQL   +=  ' '
112600060202     c                                     +  C_Union
112700060202e   1c                   endif
112800060202      *
112900060410      *?Impostazioni iniziali per estrazione da TNTBE (ke1+ke2)?
113000060410if  1c                   if        $Tntbe2 =  *on
113100120112     c                   eval      wSQL   +=  ' '
113200060202     c                                     +  %trim($Sql_TNTBE(1))
113300060202     c                                     +  ' '
113400060410     c                                     +  %trim($Sql_TNTBE(3))
113500060202     c                                     +  ' '
113600060410     c                                     +  %trim($Sql_TNTBE(4))
113700060202     c                   eval      wAndOr  =  'AND ('
113800060410e   1c                   endif
113900060202      *
114000060202      *?Impostazione estrazione dati da tabella "VAP"?
114100060202      *
114200060217if  1c                   if        V1Cse6  =  'S'
114300120112     c                   eval      wSQL   +=  ' '
114400060202     c                                     +  %trim(wAndOr)
114500060202     c                                     +  ' '
114600060410     c                                     +  %trim($Sql_TNTBE(5))
114700060217     c                                     +  C_tab6 +  ''''
114800060202     c                                     +  ' '
114900060410     c                                     +  %trim($Sql_TNTBE(6))
115000060410     c                                     +  '0' + WkscA
115100060410     c                                     +  ''''
115200140115     C                   IF        VidSoloCli = *blank
115300060202     c                   exsr      Add_Sel_VAP
115400140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
115500140115     C                   ELSE
115600140115      * chiusura
115700140115     c                   eval      wSQL   +=  '))'
115800140115     C                   ENDIF
115900060202     c                   eval      wAndOr  =  'OR   '
116000060202e   1c                   endif
116100150812      *
116200150812      *?Impostazione estrazione dati da tabella "3EW"?
116300150812      *
116400150812if  1c                   if        V1Cse11 =  'S'
116500150812     c                   eval      wSQL   +=  ' '
116600150812     c                                     +  %trim(wAndOr)
116700150812     c                                     +  ' '
116800150812     c                                     +  %trim($Sql_TNTBE(5))
116900150812     c                                     +  C_tab11 +  ''''
117000150812     c                                     +  ' '
117100150812     c                                     +  %trim($Sql_TNTBE(6))
117200150812     c                                     +  '0'
117300150812     c                                     +  %editc( V1Cksc : 'X' )
117400150812     c                                     +  ''''
117500150812     c*//                exsr      Add_Sel_3EW
117600150812     c                   eval      wSQL   +=  '))'
117700150812     c                   eval      wAndOr  =  'OR   '
117800150812e   1c                   endif
117900100803      *
118000060410if  1c                   if            $Tntbe1 =  *on
118100060410     c                             and $Tntbe2 =  *on
118200100803      *
118300100803      *?chiusura 1ª selezione da TNTBE00F (ke1+ke2)?
118400100803      *
118500120112     c                   eval      wSQL   +=  ')'
118600100803      *
118700100803      *?UNION?
118800120112     c                   eval      wSQL   +=  ' '
118900060410     c                                     +  C_Union
119000100803      *
119100060410e   1c                   endif
119200060410      *
119300060410      *?Impostazioni iniziali per estrazione da TNTBE (ke1+uni)?
119400060410if  1c                   if        $Tntbe1 =  *on
119500120112     c                   eval      wSQL   +=  ' '
119600060410     c                                     +  %trim($Sql_TNTBE(2))
119700060410     c                                     +  ' '
119800060410     c                                     +  %trim($Sql_TNTBE(3))
119900060410     c                                     +  ' '
120000060410     c                                     +  %trim($Sql_TNTBE(4))
120100060410     c                   eval      wAndOr  =  'AND ('
120200060410e   1c                   endif
120300060410      *
120400060410      *?Impostazione estrazione dati da tabella "DSC"?
120500060410      *
120600060410if  1c                   if        V1Cse7  =  'S'
120700120112     c                   eval      wSQL   +=  ' '
120800060410     c                                     +  %trim(wAndOr)
120900060410     c                                     +  ' '
121000060410     c                                     +  %trim($Sql_TNTBE(5))
121100060410     c                                     +  C_tab7 +  ''''
121200060410     c                                     +  ' '
121300060410     c                                     +  %trim($Sql_TNTBE(6))
121400120112     c                                     +  %trimr(TBEke1) +  ''''
121500140115     C                   IF        VidSoloCli = *blank
121600060410     c                   exsr      Add_Sel_DSC
121700140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
121800140115     C                   ELSE
121900140115      * chiusura
122000140115     c                   eval      wSQL   +=  '))'
122100140115     C                   ENDIF
122200060410     c                   eval      wAndOr  =  'OR   '
122300060410e   1c                   endif
122400120111      *
122500120111      *?Impostazione estrazione dati da tabella "LAC"?
122600120111      *
122700120111if  1c                   if        V1Cse8  =  'S'
122800120112     c                   eval      wSQL   +=  ' '
122900120111     c                                     +  %trim(wAndOr)
123000120111     c                                     +  ' '
123100120111     c                                     +  %trim($Sql_TNTBE(5))
123200120111     c                                     +  C_tab8 +  ''''
123300120111     c                                     +  ' '
123400120111     c                                     +  %trim($Sql_TNTBE(6))
123500120112     c                                     +  %trimr(TBEke1) +  ''''
123600140115     C                   IF        VidSoloCli = *blank
123700140115     c                   exsr      Add_Sel_LAC
123800140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
123900140115     C                   ELSE
124000140115      * chiusura
124100140115     c                   eval      wSQL   +=  '))'
124200140115     C                   ENDIF
124300120111     c                   eval      wAndOr  =  'OR   '
124400120111e   1c                   endif
124500130129      *
124600130129      *?Impostazione estrazione dati da tabella "DKC"?
124700130129      *
124800130129if  1c                   if        V1Cse9  =  'S'
124900130129     c                   eval      wSQL   +=  ' '
125000130129     c                                     +  %trim(wAndOr)
125100130129     c                                     +  ' '
125200130129     c                                     +  %trim($Sql_TNTBE(5))
125300130129     c                                     +  C_tab9 +  ''''
125400130129     c                                     +  ' '
125500130129     c                                     +  %trim($Sql_TNTBE(6))
125600130201     c                                     +  %editc( V1Cksc : 'X' )
125700130201     c                                     +  ''''
125800130129     c*//                exsr      Add_Sel_DKC
125900130129     c                   eval      wSQL   +=  '))'
126000130129     c                   eval      wAndOr  =  'OR   '
126100130129e   1c                   endif
126200120112      *
126300120112if  1c                   if           ($Tntbe1 =  *on
126400120112     c                             or  $Tntbe2 =  *on)
126500120112     c                             and $tntbe3 =  *on
126600120112      *
126700120112      *?chiusura 2ª selezione da TNTBE00F (ke1+uni)?
126800120112      *
126900120112     c                   eval      wSQL   +=  ')'
127000120112      *
127100120112      *?UNION?
127200120112     c                   eval      wSQL   +=  ' '
127300120112     c                                     +  C_Union
127400120112      *
127500120112e   1c                   endif
127600120112      *
127700120112      *?Impostazioni iniziali per estrazione da TNTBE (uni)?
127800120112if  1c                   if        $Tntbe3 =  *on
127900120112     c                   eval      wSQL   +=  ' '
128000120112     c                                     +  %trim($Sql_TNTBE(2))
128100120112     c                                     +  ' '
128200120112     c                                     +  %trim($Sql_TNTBE(3))
128300120112     c                                     +  ' '
128400120112     c                                     +  %trim($Sql_TNTBE(4))
128500120112     c                   eval      wAndOr  =  'AND ('
128600120112e   1c                   endif
128700120112      *
128800120112      *?Impostazione estrazione dati da tabella "OSR"?
128900120112      *
129000130129if  1c                   if        V1Cse10 =  'S'
129100120112     c                   eval      wSQL   +=  ' '
129200120112     c                                     +  %trim(wAndOr)
129300120112     c                                     +  ' '
129400120112     c                                     +  %trim($Sql_TNTBE(5))
129500130129     c                                     +  C_tab10 +  ''''
129600140115     C***                IF        VidSoloCli = *blank
129700120112     c                   exsr      Add_Sel_OSR
129800140115     C* se è richiesto solo il codice principale, chiudo l'istruzione
129900140115     C***                ELSE
130000140115      * chiusura
130100140115     c***                eval      wSQL   +=  '))'
130200140115     C***                ENDIF
130300120112     c                   eval      wAndOr  =  'OR   '
130400120112e   1c                   endif
130500120112      *
130600120112      *?chiusura 3ª selezione da TNTBE00F (uni)?
130700120112      *
130800120112     c                   eval      wSQL   +=  ')'
130900060202      *
131000060202     c                   ENDSR
131100060202      *
131200060202      *---------------------------------------------------------------*
131300060202      *?Impostazioni finali nella stringa SQL                        ?*
131400060202      *---------------------------------------------------------------*
131500060202     c     PrepSQL3      BEGSR
131600060202      *
131700060202      *
131800120112     c                   eval      wSQL   +=  ' '
131900060202      *?order by?
132000120112     c                                     +  C_OrderBy
132100060202      *?for fatch only?
132200060202     c                                     +  ' '
132300060202     c                                     +  C_ForFetch
132400120112      *
132500120112     c                   movel     wSQL          ds_Comodo
132600060202      *
132700060202     c                   ENDSR
132800051109      *
132900051109      *---------------------------------------------------------------*
133000051109      *?Aggiunta selezione tab. "3C"                                 ?*
133100051109      *---------------------------------------------------------------*
133200051123     c     Add_Sel_3C    BEGSR
133300051109      *
133400120112     c                   eval      wSQL   +=  ' '
133500051123      * aggiunta selezione per §3CCKS
133600051109     c                                     +  %trim($Sql_TABEL(6))
133700051109     c                                     +  ' 79, 7) = '''
133800051109     c                                     +  %trim(TBLkey)
133900051109     c                                     +  ''''
134000051123      * aggiunta selezione per stesso unificante (se diverso)
134100051123     c                   move      §3Ccks        WkscA
134200051123     c                   if            WkscA   > *zeros
134300051123     c                             and WkscA  <> TBLkey
134400120112     c                   eval      wSQL   +=  ' '
134500051123     c                                     +  %trim($Sql_TABEL(6))
134600051123     c                                     +  ' 79, 7) = '''
134700051123     c                                     +  WkscA
134800051123     c                                     +  ''''
134900051123     c                   endif
135000051109      * chiusura
135100120112     c                   eval      wSQL   +=  '))'
135200051109      *
135300051109     c                   ENDSR
135400051109      *
135500051109      *---------------------------------------------------------------*
135600051109      *?Aggiunta selezione tab. "3K"                                 ?*
135700051109      *---------------------------------------------------------------*
135800051123     c     Add_Sel_3K    BEGSR
135900051109      *
136000120112     c                   eval      wSQL   +=  ' '
136100051109      * aggiunta selezione per §3KCKS
136200051109     c                                     +  %trim($Sql_TABEL(6))
136300051109     c                                     +  ' 27, 7) = '''
136400051109     c                                     +  %trim(TBLkey)
136500051109     c                                     +  ''''
136600051109      * aggiunta selezione per §3KGKS
136700051109     c                                     +  ' '
136800051109     c                                     +  %trim($Sql_TABEL(6))
136900051109     c                                     +  ' 55, 7) = '''
137000051109     c                                     +  %trim(TBLkey)
137100051109     c                                     +  ''''
137200051123      * aggiunta selezione per stesso unificante-consegna (se diverso)
137300051123     c                   move      §3Kcks        WkscA
137400051123     c                   if            WkscA   > *zeros
137500051123     c                             and WkscA  <> TBLkey
137600120112     c                   eval      wSQL   +=  ' '
137700051123     c                                     +  %trim($Sql_TABEL(6))
137800051123     c                                     +  ' 27, 7) = '''
137900051123     c                                     +  WkscA
138000051123     c                                     +  ''''
138100051123     c                   endif
138200051123      * aggiunta selezione per stesso unificante-giacenza (se diverso)
138300051123     c                   move      §3Kgks        WkscA
138400051123     c                   if            WkscA   > *zeros
138500051123     c                             and WkscA  <> TBLkey
138600120112     c                   eval      wSQL   +=  ' '
138700051123     c                                     +  %trim($Sql_TABEL(6))
138800051123     c                                     +  ' 55, 7) = '''
138900051123     c                                     +  WkscA
139000051123     c                                     +  ''''
139100051123     c                   endif
139200051109      * chiusura
139300120112     c                   eval      wSQL   +=  '))'
139400051109      *
139500051109     c                   ENDSR
139600060217      *
139700060217      *---------------------------------------------------------------*
139800060217      *?Aggiunta selezione tab. "3Q"                                 ?*
139900060217      *---------------------------------------------------------------*
140000060217     c     Add_Sel_3Q    BEGSR
140100060217      *
140200120112     c                   eval      wSQL   +=  ' '
140300060217      * aggiunta selezione per §3QCKS
140400060217     c                                     +  %trim($Sql_TABEL(6))
140500060217     c                                     +  ' 13, 7) = '''
140600060217     c                                     +  %trim(TBLkey)
140700060217     c                                     +  ''''
140800060217      * chiusura
140900120112     c                   eval      wSQL   +=  '))'
141000060217      *
141100060217     c                   ENDSR
141200051109      *
141300051109      *---------------------------------------------------------------*
141400051109      *?Aggiunta selezione tab. "3R"                                 ?*
141500051109      *---------------------------------------------------------------*
141600051123     c     Add_Sel_3R    BEGSR
141700051109      *
141800051123      * chiusura
141900120112     c                   eval      wSQL   +=  '))'
142000051109      *
142100051109     c                   ENDSR
142200051109      *
142300051109      *---------------------------------------------------------------*
142400051109      *?Aggiunta selezione tab. "4C"                                 ?*
142500051109      *---------------------------------------------------------------*
142600051123     c     Add_Sel_4C    BEGSR
142700051109      *
142800120112     c                   eval      wSQL   +=  ' '
142900051109      * aggiunta selezione per §4CCKS
143000051109     c                                     +  %trim($Sql_TABEL(6))
143100051109     c                                     +  ' 29, 7) = '''
143200051109     c                                     +  %trim(TBLkey)
143300051109     c                                     +  ''''
143400051109      * aggiunta selezione per §4CFKS
143500051109     c                                     +  ' '
143600051109     c                                     +  %trim($Sql_TABEL(6))
143700051109     c                                     +  ' 58, 7) = '''
143800051109     c                                     +  %trim(TBLkey)
143900051109     c                                     +  ''''
144000051123      * aggiunta selezione per stesso unificante-consegna (se diverso)
144100051123     c                   move      §4Ccks        WkscA
144200051123     c                   if            WkscA   > *zeros
144300051123     c                             and WkscA  <> TBLkey
144400120112     c                   eval      wSQL   +=  ' '
144500051123     c                                     +  %trim($Sql_TABEL(6))
144600051123     c                                     +  ' 29, 7) = '''
144700051123     c                                     +  WkscA
144800051123     c                                     +  ''''
144900051123     c                   endif
145000051123      * aggiunta selezione per stesso unificante-fatturaz (se diverso)
145100051123     c                   move      §4Cfks        WkscA
145200051123     c                   if            WkscA   > *zeros
145300051123     c                             and WkscA  <> TBLkey
145400120112     c                   eval      wSQL   +=  ' '
145500051123     c                                     +  %trim($Sql_TABEL(6))
145600051123     c                                     +  ' 58, 7) = '''
145700051123     c                                     +  WkscA
145800051123     c                                     +  ''''
145900051123     c                   endif
146000051109      * chiusura
146100120112     c                   eval      wSQL   +=  '))'
146200051109      *
146300051109     c                   ENDSR
146400060202      *
146500060202      *---------------------------------------------------------------*
146600060202      *?Aggiunta selezione tab. "VAP"                                ?*
146700060202      *---------------------------------------------------------------*
146800060202     c     Add_Sel_VAP   BEGSR
146900060202      *
147000120112     c                   eval      wSQL   +=  ' '
147100060202      * aggiunta selezione per TBEke2
147200060410     c                                     +  %trim($Sql_TNTBE(7))
147300060410     c                                     +  '0' + WkscA
147400060410     c                                     +  ''''
147500060202      * chiusura
147600120112     c                   eval      wSQL   +=  '))'
147700060202      *
147800060202     c                   ENDSR
147900060410      *
148000060410      *---------------------------------------------------------------*
148100060410      *?Aggiunta selezione tab. "DSC"                                ?*
148200060410      *---------------------------------------------------------------*
148300060410     c     Add_Sel_DSC   BEGSR
148400060410      *
148500120112     c                   eval      wSQL   +=  ' '
148600120111      * aggiunta selezione per §DSCKSU
148700060410     c                                     +  %trim($Sql_TNTBE(8))
148800060410     c                                     +  ' 3, 7) = '''
148900060410     c                                     +  %trim(TBEke1)
149000060410     c                                     +  ''''
149100060410      * aggiunta selezione per stesso unificante (se diverso)
149200060410     c                   move      §DSCksu       WkscA
149300060410     c                   if            WkscA   > *zeros
149400060410     c                             and WkscA  <> TBEke1
149500120112     c                   eval      wSQL   +=  ' '
149600060410     c                                     +  %trim($Sql_TNTBE(8))
149700060410     c                                     +  ' 3, 7) = '''
149800060410     c                                     +  WkscA
149900060410     c                                     +  ''''
150000060410     c                   endif
150100060410      * chiusura
150200120112     c                   eval      wSQL   +=  '))'
150300060410      *
150400060410     c                   ENDSR
150500120111      *
150600120111      *---------------------------------------------------------------*
150700120111      *?Aggiunta selezione tab. "LAC"                                ?*
150800120111      *---------------------------------------------------------------*
150900120111     c     Add_Sel_LAC   BEGSR
151000120111      *
151100120112     c                   eval      wSQL   +=  ' '
151200120111      * aggiunta selezione per §LACKSU
151300120111     c                                     +  %trim($Sql_TNTBE(8))
151400120111     c                                     +  ' 156, 7) = '''
151500120111     c                                     +  %trim(TBEke1)
151600120111     c                                     +  ''''
151700120111      * aggiunta selezione per stesso unificante (se diverso)
151800120111     c                   move      §LACksu       WkscA
151900120111     c                   if            WkscA   > *zeros
152000120111     c                             and WkscA  <> TBEke1
152100120112     c                   eval      wSQL   +=  ' '
152200120111     c                                     +  %trim($Sql_TNTBE(8))
152300120111     c                                     +  ' 156, 7) = '''
152400120111     c                                     +  WkscA
152500120111     c                                     +  ''''
152600120111     c                   endif
152700120111      * aggiunta selezione per §LACKSC
152800120112     c                   eval      wSQL   +=  ' '
152900120111     c                                     +  %trim($Sql_TNTBE(8))
153000120111     c                                     +  ' 178, 7) = '''
153100120111     c                                     +  %trim(TBEke1)
153200120111     c                                     +  ''''
153300120111      * aggiunta selezione per cliente forzato (se diverso)
153400120111     c                   move      §LACksc       WkscA
153500120111     c                   if            WkscA   > *zeros
153600120111     c                             and WkscA  <> TBEke1
153700120112     c                   eval      wSQL   +=  ' '
153800120111     c                                     +  %trim($Sql_TNTBE(8))
153900120111     c                                     +  ' 178, 7) = '''
154000120111     c                                     +  WkscA
154100120111     c                                     +  ''''
154200120111     c                   endif
154300120111      *
154400120111      * chiusura
154500120112     c                   eval      wSQL   +=  '))'
154600120111      *
154700120111     c                   ENDSR
154800120112      /free
154900120112
155000120112       //--------------------------------------------------------------
155100120112       //?Aggiunta selezione tab. "OSR"                                ?
155200120112       //--------------------------------------------------------------
155300120112       BEGSR  Add_Sel_OSR;
155400120112
155500120112         // -?aggiunta selezione per D§OSRCLI/CL2/3-16?
155600120112         wSQL += ' and (substr(TBEuni,   1, 7) = ''' + %editc(V1Cksc:'X') +
155700120112                 ''' or substr(TBEuni,  42, 7) = ''' + %editc(V1Cksc:'X') +
155800120112                 ''' or substr(TBEuni,  74, 7) = ''' + %editc(V1Cksc:'X') +
155900120112                 ''' or substr(TBEuni, 106, 7) = ''' + %editc(V1Cksc:'X') +
156000120112                 ''' or substr(TBEuni, 138, 7) = ''' + %editc(V1Cksc:'X') +
156100120112                 ''' or substr(TBEuni, 171, 7) = ''' + %editc(V1Cksc:'X') +
156200120112                 ''' or substr(TBEuni, 178, 7) = ''' + %editc(V1Cksc:'X') +
156300120112                 ''' or substr(TBEuni, 185, 7) = ''' + %editc(V1Cksc:'X') +
156400120112                 ''' or substr(TBEuni, 192, 7) = ''' + %editc(V1Cksc:'X') +
156500120112                 ''' or substr(TBEuni, 199, 7) = ''' + %editc(V1Cksc:'X') +
156600120112                 ''' or substr(TBEuni, 206, 7) = ''' + %editc(V1Cksc:'X') +
156700120112                 ''' or substr(TBEuni, 213, 7) = ''' + %editc(V1Cksc:'X') +
156800120112                 ''' or substr(TBEuni, 220, 7) = ''' + %editc(V1Cksc:'X') +
156900120112                 ''' or substr(TBEuni, 227, 7) = ''' + %editc(V1Cksc:'X') +
157000120112                 ''' or substr(TBEuni, 234, 7) = ''' + %editc(V1Cksc:'X') +
157100120112                 ''' or substr(TBEuni, 241, 7) = ''' + %editc(V1Cksc:'X') +
157200120112                 '''';
157300120112         // -?aggiunta selezione per codice ordinante ORM?
157400120112         wSQL += ' or substr(TBEuni, 146, 7) = ''' + %editc(V1Cksc:'X') +
157500120112                 '''';
157600120112         // -?aggiunta selezione per codice destinatario ORM?
157700120112         wSQL += ' or substr(TBEuni, 153, 7) = ''' + %editc(V1Cksc:'X') +
157800120112                 '''';
157900120112
158000120112         // -?chiusura?
158100120112         wSQL += '))';
158200120112
158300120112       ENDSR;
158400120112
158500120112      /end-free
158600051109      *
158700051109      *---------------------------------------------------------------*
158800051109      *?Dichiarazione e Apertura cursore                             ?*
158900051109      *---------------------------------------------------------------*
159000051109     c     OpenCursor    BEGSR
159100051109      *
159200051109      * Dichiarazione cursore
159300051109      *
159400051109     c/exec sql
159500051109     c+     PREPARE S1 FROM :wSQL
159600051109     c/end-exec
159700051109     c*
159800051109     c/exec sql
159900051109     c+     DECLARE C1 CURSOR FOR S1
160000051109     c/end-exec
160100051109      *
160200051109      * Apertura del cursore
160300051109      *
160400051109     c/exec sql
160500051109     c+     OPEN C1
160600051109     c/end-exec
160700051109      *
160800051109     c                   ENDSR
160900051109      *
161000051109      *---------------------------------------------------------------*
161100051109      *?Lettura cursore                                              ?*
161200051109      *---------------------------------------------------------------*
161300051109     c     ReadCursor    BEGSR
161400051109      *
161500051110     c                   clear                   ds_TABEL
161600051109     c/exec sql
161700051110     c+     FETCH NEXT FROM C1 INTO :ds_TABEL
161800051109     c/end-exec
161900051109      *
162000051109sel 1c                   select
162100051109w   1c                   when      SQLcod  =  100
162200051109     c                   eval      $EoF    =  *on
162300051109w   1c                   when      SQLcod  <  *zeros
162400051109     c                   eval      $Err    =  *on
162500051109     c                   eval      $Eof    =  *on
162600051109x   1c                   other
162700051110     c                   exsr      RieS01
162800051109e   1c                   endsl
162900051110      *
163000051110      * Visualizzazione del SFL (se ci sono dati)
163100051110     c                   eval      *in30   =  (S01nrr = *zeros)
163200051110      *
163300051110      * Attivazione (eventuale) del SFLEND
163400051110     c                   eval      *in33   =  $EoF
163500051110      *
163600051110      * Posizionamento cursore al 1° rcd della pagina
163700051110if  1c                   if        S01nrr  >  *zeros
163800051110     c     S01nrr        div       C_SflPag      wPag
163900051110     c                   mvr                     wRig
164000051110     c                   eval      C01rcd  =  wPag * C_SflPag
164100051110     c                   if        wRig    >  *zeros
164200051110     c                   eval      C01rcd  =  C01rcd + 1
164300051110     c                   else
164400051110     c                   eval      C01rcd  =  C01rcd - C_SflPag + 1
164500051110     c                   endif
164600051110x   1c                   else
164700051110     c                   clear                   C01rcd
164800051110e   1c                   endif
164900051110     c                   eval      C01csr  =  C01rcd
165000051109      *
165100051109     c                   ENDSR
165200051109      *
165300051109      *---------------------------------------------------------------*
165400051110      *?Caricamento dati nel record del subfile S01                  ?*
165500051109      *---------------------------------------------------------------*
165600051110     c     RieS01        BEGSR
165700051110      *
165800051110     c                   clear                   TB06S1
165900060202      *
166000060202     c                   eval      VS1cod  = %trim(wSQLcod)
166100051110      *
166200051110      * Impostazione DS corretta (in base al tipo tabella)
166300051110     c                   SELECT
166400051110      * - tab. 3C
166500060202     c                   WHEN      %trim(wSQLcod) = C_tab1
166600060202     c                   movel     wSQLkey       VS1key
166700051117     c                   movel     wSQLuni       ds3C
166800051110     c                   movel     §3Crag        VS1dke
166900051110     c                   movel     §3Ccks        VS1cu1
167000051110     c***                movel                   VS1du1
167100051110     c***                movel                   VS1cu2
167200051110     c***                movel                   VS1du2
167300060202     c                   if        §3Ccks  > *zeros
167400051123     c                   movel     C_tab1_a      VS1dc1
167500051123     c                   endif
167600051110      * - tab. 3K
167700060202     c                   WHEN      %trim(wSQLcod) = C_tab2
167800060202     c                   movel     wSQLkey       VS1key
167900051117     c                   movel     wSQLuni       ds3K
168000051110     c***                movel                   VS1dke
168100051110     c                   movel     §3Kcks        VS1cu1
168200051110     c***                movel                   VS1du1
168300051110     c                   movel     §3Kgks        VS1cu2
168400051110     c***                movel                   VS1du2
168500060202     c                   if        §3Kcks  > *zeros
168600051123     c                   movel     C_tab2_a      VS1dc1
168700051123     c                   endif
168800060202     c                   if        §3Kgks  > *zeros
168900051123     c                   movel     C_tab2_b      VS1dc2
169000051123     c                   endif
169100051110      * - tab. 3R
169200140423     c****               WHEN      %trim(wSQLcod) = C_tab3
169300140423     c****               movel     wSQLkey       VS1key
169400140423     c****               movel     wSQLuni       ds3R
169500051110     c***                movel                   VS1dke
169600051110     c***                movel                   VS1cu1
169700051110     c***                movel                   VS1du1
169800051110     c***                movel                   VS1cu2
169900051110     c***                movel                   VS1du2
170000051110      * - tab. 4C
170100060202     c                   WHEN      %trim(wSQLcod) = C_tab4
170200060202     c                   movel     wSQLkey       VS1key
170300051117     c                   movel     wSQLuni       ds4C
170400051110     c***                movel                   VS1dke
170500051122     c                   if        §4Ccks <> *zeros
170600051110     c                   movel     §4Ccks        VS1cu1
170700051122     c                   endif
170800051110     c***                movel                   VS1du1
170900051122     c                   if        §4Cfks <> *zeros
171000051110     c                   movel     §4Cfks        VS1cu2
171100051122     c                   endif
171200051110     c***                movel                   VS1du2
171300060202     c                   if        §4Ccks  > *zeros
171400051123     c                   movel     C_tab4_a      VS1dc1
171500051123     c                   endif
171600060202     c                   if        §4Cfks  > *zeros
171700051123     c                   movel     C_tab4_b      VS1dc2
171800051123     c                   endif
171900060217      * - tab. 3Q
172000060217     c                   WHEN      %trim(wSQLcod) = C_tab5
172100060217     c                   movel     wSQLkey       VS1key
172200060217     c                   movel     wSQLuni       ds3Q
172300060217     c***                movel                   VS1dke
172400060217     c                   movel     §3Qcks        VS1cu1
172500060217     c***                movel                   VS1du1
172600060217     c***                movel                   VS1cu2
172700060217     c***                movel                   VS1du2
172800060217     c                   if        §3Qcks  > *zeros
172900060217     c                   movel     C_tab5_a      VS1dc1
173000060217     c                   endif
173100060202      * - tab. VAP
173200060217     c                   WHEN      %trim(wSQLcod) = C_tab6
173300060202     c                   eval      VS1key  = %subst(wSQLkey:2:7)
173400060410     c*** inesistente:   movel     wSQLuni       dVAP
173500060202     c***                movel                   VS1dke
173600060202     c                   eval      VS1cu1  = %subst(wSQLuni:2:7)
173700060202     c***                movel                   VS1du1
173800060202     c***                movel                   VS1cu2
173900060202     c***                movel                   VS1du2
174000060202     c*** sempre:        if        VS1cu1  > *zeros
174100060217     c                   movel     C_tab6_a      VS1dc1
174200060202     c*** sempre:        endif
174300060410      * - tab. DSC
174400060410     c                   WHEN      %trim(wSQLcod) = C_tab7
174500060410     c                   movel     wSQLkey       VS1key
174600060410     c                   movel     wSQLuni       dDSC
174700060410     c***                movel                   VS1dke
174800060410     c                   if        §DSCksu > *zeros
174900060410     c                   movel     §DSCksu       VS1cu1
175000060410     c                   endif
175100060410     c***                movel                   VS1du1
175200060410     c***                movel                   VS1cu2
175300060410     c***                movel                   VS1du2
175400060410     c*** sempre:        if        VS1cu1  > *zeros
175500060410     c                   movel     C_tab7_a      VS1dc1
175600060410     c*** sempre:        endif
175700120111      * - tab. LAC
175800120111     c                   WHEN      %trim(wSQLcod) = C_tab8
175900120111     c                   movel     wSQLkey       VS1key
176000120111     c                   movel     wSQLuni       dLAC
176100120111     c***                movel     §LACrag       VS1dke
176200120111     c                   if        §LACksu > *zeros
176300120111     c                   movel     §LACksu       VS1cu1
176400120111     c                   endif
176500120111     c***                movel                   VS1du1
176600120111     c                   if        §LACksc > *zeros
176700120111     c                   movel     §LACksc       VS1cu2
176800120111     c                   endif
176900120111     c***                movel                   VS1du2
177000120111     c*** sempre:        if        VS1cu1  > *zeros
177100120111     c                   movel     C_tab8_a      VS1dc1
177200120111     c*** sempre:        endif
177300120111     c*** sempre:        if        VS1cu2  > *zeros
177400120111     c                   movel     C_tab8_b      VS1dc2
177500120111     c*** sempre:        endif
177600130129      * - tab. DKC
177700130129     c                   WHEN      %trim(wSQLcod) = C_tab9
177800130129     c                   movel     wSQLkey       VS1key
177900130129     c*// No altri cod.  movel     wSQLuni       dDKC
178000130129     c*//                movel                   VS1dke
178100130129     c*//                movel                   VS1cu1
178200130129     c*//                movel                   VS1du1
178300130129     c*//                movel                   VS1cu2
178400130129     c*//                movel                   VS1du2
178500130129     c*// mai:           if        VS1cu1  > *zeros
178600130129     c*//                movel     C_tab?_a      VS1dc1
178700130129     c*// mai:           endif
178800120112      * - tab. OSR
178900130129     c                   WHEN      %trim(wSQLcod) = C_tab10
179000120112     c                   movel     wSQLkey       VS1key
179100120112     c                   movel     wSQLuni       dOSR
179200120112     c                   eval      VS1dke = '-Fil.emiss.ORM++
179300120112     c                                        Serie'
179400120112     c                   if            d§OSRcl3  > *zeros
179500120112     c                             or  d§OSRcl4  > *zeros
179600120112     c                             or  d§OSRcl5  > *zeros
179700120112     c                             or  d§OSRcl6  > *zeros
179800120112     c                             or  d§OSRcl7  > *zeros
179900120112     c                             or  d§OSRcl8  > *zeros
180000120112     c                             or  d§OSRcl9  > *zeros
180100120112     c                             or  d§OSRcl10 > *zeros
180200120112     c                             or  d§OSRcl11 > *zeros
180300120112     c                             or  d§OSRcl12 > *zeros
180400120112     c                             or  d§OSRcl13 > *zeros
180500120112     c                             or  d§OSRcl14 > *zeros
180600120112     c                             or  d§OSRcl15 > *zeros
180700120112     c                             or  d§OSRcl16 > *zeros
180800120112     c                             or  d§OSRcor  > *zeros
180900120112     c                             or  d§OSRcrc  > *zeros
181000120112     c                   eval      VS1dke = '   (altri clienti)'
181100120112     c                   endif
181200120112     c                   if        d§OSRcli > *zeros
181300120112     c                   movel     d§OSRcli      VS1cu1
181400120112     c                   endif
181500120112     c***                movel     d§OSRrag      VS1du1
181600120112     c                   if        d§OSRcl2 > *zeros
181700120112     c                   movel     d§OSRcl2      VS1cu2
181800120112     c                   endif
181900120112     c***                movel     d§OSRra2      VS1du2
182000120112     c*** sempre:        if        VS1cu1  > *zeros
182100120112     c                   movel     C_tab8_a      VS1dc1
182200120112     c*** sempre:        endif
182300120112     c*** sempre:        if        VS1cu2  > *zeros
182400120112     c                   movel     C_tab8_b      VS1dc2
182500120112     c*** sempre:        endif
182600150812      * - tab. 3EW
182700150812     c                   WHEN      %trim(wSQLcod) = C_tab11
182800150812     c                   EVAL      VS1key = %subst(wSQLkey:2:7)
182900150812     c                   EVAL      VH1SUN = %subst(wSQLuni:1:9)
183000150812     c*// No altri cod.  movel     wSQLuni       dDKC
183100150812     c*//                movel                   VS1dke
183200150812     c*//                movel                   VS1cu1
183300150812     c*//                movel                   VS1du1
183400150812     c*//                movel                   VS1cu2
183500150812     c*//                movel                   VS1du2
183600150812     c*// mai:           if        VS1cu1  > *zeros
183700150812     c*//                movel     C_tab?_a      VS1dc1
183800150812     c*// mai:           endif
183900051110     c                   ENDSL
184000051110      *
184100051110      * Decodifica cliente - chiave
184200060202if  1c                   if        VS1key  > *zeros
184300130129     c                             and  %trim(wSQLcod) <> C_tab10
184400051110     c                   move      VS1key        Wksc
184500051110     c                   exsr      Decod_Cli
184600051110if  2c                   if        O69err  =  *on
184700051110     c                   movel     *all'? '      VH1dke
184800051110x   2c                   else
184900051110     c                   movel     ACOrag        VH1dke
185000120807     c                   movel     ACOabl        VH1abl
185100051110e   2c                   endif
185200060202if  2c                   if        VS1dke  = *blanks
185300051110     c                   movel     VH1dke        VS1dke
185400051110e   2c                   endif
185500051110e   1c                   endif
185600051110      * Decodifica cliente - unificante 1
185700060202if  1c                   if        VS1cu1  > *zeros
185800051110     c                   move      VS1cu1        Wksc
185900051110     c                   exsr      Decod_Cli
186000051110if  2c                   if        O69err  =  *on
186100051110     c                   movel     *all'? '      VH1du1
186200051110x   2c                   else
186300051110     c                   movel     ACOrag        VH1du1
186400051110e   2c                   endif
186500060202if  2c                   if        VS1du1  = *blanks
186600051110     c                   movel     VH1du1        VS1du1
186700051110e   2c                   endif
186800051110e   1c                   endif
186900051110      * Decodifica cliente - unificante 2
187000051110if  1c                   if        VS1cu2 >  *zeros
187100051110     c                   move      VS1cu2        Wksc
187200051110     c                   exsr      Decod_Cli
187300051110if  2c                   if        O69err  =  *on
187400051110     c                   movel     *all'? '      VH1du2
187500051110x   2c                   else
187600051110     c                   movel     ACOrag        VH1du2
187700051110e   2c                   endif
187800060202if  2c                   if        VS1du2  = *blanks
187900051110     c                   movel     VH1du2        VS1du2
188000051110e   2c                   endif
188100051110e   1c                   endif
188200100216      *
188300100216      * Impostazione attributi di visualizzazione
188400130201     c                   eval      *in41 = ($Called = *on  and
188500130201     c                                      D06op0  = *blank)
188600130315     c                   eval      *in42 = (VH1abl  <> *blanks)
188700051110      *
188800051110      * Scrittura record subfile
188900051110     c                   add       1             S01nrr
189000051110     c                   write     TB06S1
189100051110      *
189200051110     c                   ENDSR
189300051109      *
189400051109      *---------------------------------------------------------------*
189500051109      *?Chiusura cursore                                             ?*
189600051109      *---------------------------------------------------------------*
189700051109     c     CloseCursor   BEGSR
189800051109      *
189900051109     c/exec sql
190000051109     c+     CLOSE C1
190100051109     c/end-exec
190200051109      *
190300051109     c                   ENDSR
190400051110      *
190500051110      *---------------------------------------------------------------*
190600051110      *?Gestione tasto funzionale F08 da videata C01 / S01           ?*
190700120111      *?F8-Stampa elenco                                             ?*
190800051110      *---------------------------------------------------------------*
190900051110     c     F08S01        BEGSR
191000051110      *
191100051110      * Override al prtf
191200051110     c                   call      'QCMDEXC'
191300051110     c                   parm      $Cmd(1)       Qcmd
191400051110     c                   parm                    Qlen
191500051110      *
191600051110      * Apertura prtf
191700051110     c                   open      PRTF198
191800051110      *
191900051110      * Stampa parametri - stampa della testata iniziale
192000051110     c                   move      *date         Data_Eur
192100051110     c     *eur          movel     Data_Eur      wDate
192200051110     c                   time                    wTime
192300051110      * Stampa parametri - stampa dei parametri di lancio
192400051110     c                   except    PRTini
192500051110     c                   except    PRTpar
192600051110      *
192700051110      * Stampa lista     - stampa della testata ininiziale
192800051110     c                   except    PRTini
192900051110     c                   except    PRTtxt
193000051110     c                   eval      *inOF   =  *off
193100070828      *
193200070828if  0c                   IF        C01rcd  >  *zeros
193300070828      *
193400051110      * Stampa lista     - stampa dettagio
193500051110      * - ciclo di lettura sfl
193600051110if  1c                   DO        *hival        S01nrr
193700051110     c     S01nrr        chain     TB06S1
193800051110if  2c                   if        NOT %found(TRTB06D)
193900051110     c                   leave
194000051110e   2c                   endif
194100051110      * - stampa riga di dettaglio
194200051110     c                   if        *inOF    = *on
194300051110     c                   except    PRTini
194400051110     c                   except    PRTtxt
194500051110     c                   eval      *inOF    = *off
194600051110     c                   endif
194700051110     c                   except    PRTdet
194800051110      * - riattivazione sflnxtchg (se immessa opzione)
194900051110if  2c                   if        VS1opz  <> *blanks
195000051110     c                   eval      *in32   =  *on
195100130201     c                   eval      *in41 = ($Called = *on  and
195200130201     c                                      D06op0  = *blank)
195300130315     c                   eval      *in42 = (VH1abl  <> *blanks)
195400051110     c                   UPDATE    TB06S1
195500051110e   2c                   endif
195600051110e   1c                   ENDDO
195700070828      *
195800070828e   0c                   ENDIF
195900051110      *
196000051110      * Stampa "Fine Lista"
196100051110     c                   time                    wTime
196200051110     c                   except    PRTend
196300051110      *
196400051110      * Chiusura prtf
196500051110     c                   close     PRTF198
196600051110      *
196700051110      * Cancellazione override
196800051110     c                   call      'QCMDEXC'
196900051110     c                   parm      $Cmd(2)       Qcmd
197000051110     c                   parm                    Qlen
197100051110      *
197200051110     c                   ENDSR
197300051110      *
197400051110      *---------------------------------------------------------------*
197500051110      *?Gestione tasto funzionale F11 da videata C01 / S01           ?*
197600051110      *?F11-Cambia ordinamento sfl                                   ?*
197700051110      *---------------------------------------------------------------*
197800051110     c     F11S01        BEGSR
197900051110      *
198000051110     c                   select
198100051110     c                   when      WrkSort =  *off
198200051110     c                   eval      WrkSort =  *on
198300051110     c                   when      WrkSort =  *on
198400051110     c                   eval      WrkSort =  *off
198500051110     c                   endsl
198600051123     c                   eval      *in40   =  (WrkSort = *on)
198700051123if  1c                   if        C01rcd  >  *zeros
198800051110     c                   exsr      Sort
198900051123e   1c                   endif
199000051110      *
199100051110     c                   ENDSR
199200051110      *
199300051110      *---------------------------------------------------------------*
199400051110      *?Gestione tasto funzionale F12 da videata C01 / S01           ?*
199500051110      *?F12-Ritorno                                                  ?*
199600051110      *---------------------------------------------------------------*
199700051110     c     F12S01        BEGSR
199800051110      *
199900130129     c*//                if        Not $Called
200000051110      * Torno alla videata precedente
200100051110     c                   eval      $Video  =  '1'
200200130129     c                   eval      *in51   =  ($Called = *on)
200300130129     c*//                else
200400091214      * Esco dal programma
200500130129     c*//                eval      $Fine   =  *on
200600130129     c*//                endif
200700051110      *
200800051110     c                   ENDSR
200900051110      *
201000051110      *---------------------------------------------------------------*
201100051110      *?Gestione opzioni da subfile S01                              ?*
201200051110      *---------------------------------------------------------------*
201300051110     c     OpzS01        BEGSR
201400051110      *
201500051110     c                   readc     TB06S1
201600051110      *
201700051110do  1c                   DOW       NOT %eof(TRTB06D)
201800051110      *
201900051110     c                   eval      *in32   =  *off
202000051110     c                   movea     *zeros        *in(50)
202100051110     c                   z-add     S01nrr        C01rcd
202200051110      *
202300051110sel 2c                   SELECT
202400051110      * - Nessuna opzione
202500051110w   2c                   WHEN      VS1opz  =  *blanks
202600051110      * - Gestione delle singola tabella
202700051110w   2c                   WHEN          VS1opz  =  '2'
202800051117     c***                          or  VS1opz  =  '5'
202900051110     c                   exsr      sr_OpzS1
203000051110      * - ? = Opzione NON valida
203100051110x   2c                   OTHER
203200051110     c                   seton                                        502890
203300051110     c                   eval      V1Dmsg  =  $Msg(4)
203400051110e   2c                   ENDSL
203500051110      *
203600051110      * Aggiornamento sfl
203700051110sel 2c                   select
203800051110w   2c                   when      *in28
203900051110     c                   eval      *in32   =  *on
204000051110     c                   z-add     C01rcd        C01csr
204100051110w   2c                   when      *in90
204200051110     c                   z-add     C01rcd        C01csr
204300051110     c                   clear                   VS1opz
204400051110x   2c                   other
204500051110     c                   clear                   VS1opz
204600051110e   2c                   endsl
204700130201     c                   eval      *in41 = ($Called = *on  and
204800130201     c                                      D06op0  = *blank)
204900130315     c                   eval      *in42 = (VH1abl  <> *blanks)
205000051110     c                   UPDATE    TB06S1
205100051110if  2c                   if        *in28  OR  *in90
205200051110     c                   leave
205300051110e   2c                   endif
205400051110      *
205500051110     c                   readc     TB06S1
205600051110      *
205700051110e   1c                   ENDDO
205800051110      *
205900051110     c                   ENDSR
206000051110      *
206100051110      *---------------------------------------------------------------*
206200051110      *?Gestione singola opzione dal subfile S01                     ?*
206300051110      *---------------------------------------------------------------*
206400051110     c     sr_OpzS1      BEGSR
206500051110      *
206600051110sel 1c                   select
206700051216      * - 1ª tabella ('3C')
206800051216w   1c                   when           VS1cod =  V1Ctb1
206900051110     c                             and (VS1opz =  '2'
207000051117     c                              or  VS1opz =  '5')
207100120807     c*//                movel(p)  VS1key        KPJBU
207200120807     c                   eval      KPJBU = VS1key + SDSpgm
207300051216     c                   call      V1Hpg1
207400051110     c                   parm                    KPJBA
207500051216      * - 2ª tabella ('3K')
207600051216w   1c                   when           VS1cod =  V1Ctb2
207700051110     c                             and (VS1opz =  '2'
207800051117     c                              or  VS1opz =  '5')
207900051110     c                   movel(p)  VS1key        KPJBU
208000051216     c                   call      V1Hpg2
208100051110     c                   parm                    KPJBA
208200051216      * - 3ª tabella ('3R')
208300140423w   1c***                when           VS1cod =  V1Ctb3
208400140423     c***                          and (VS1opz =  '2'
208500140423     c***                           or  VS1opz =  '5')
208600140423     c***                movel(p)  VS1key        KPJBU
208700140423     c***                call      V1Hpg3
208800140423     c***                parm                    KPJBA
208900051216      * - 4ª tabella ('4C')
209000051216w   1c                   when           VS1cod =  V1Ctb4
209100051110     c                             and (VS1opz =  '2'
209200051117     c                              or  VS1opz =  '5')
209300051110     c                   movel(p)  VS1key        KPJBU
209400051216     c                   call      V1Hpg4
209500051110     c                   parm                    KPJBA
209600060217      * - 5ª tabella ('3Q')
209700060217w   1c                   when           VS1cod =  V1Ctb5
209800060217     c                             and (VS1opz =  '2'
209900060217     c                              or  VS1opz =  '5')
210000071016     c                   movel(p)  VS1key        KPJBU
210100071016     c                   call      V1Hpg5
210200071016     c                   parm                    KPJBA
210300060217      * - 6ª tabella ('VAP')
210400060217w   1c                   when           VS1cod =  V1Ctb6
210500060202     c                             and (VS1opz =  '2'
210600060202     c                              or  VS1opz =  '5')
210700060202     c                   clear                   KPJBU
210800150819     c                   eval      KPJBU = '0' + VS1key
210900060217     c                   call      V1Hpg6
211000060202     c                   parm                    KPJBA
211100120111      * - 7ª tabella ('DSC')
211200060410w   1c                   when           VS1cod =  V1Ctb7
211300060410     c                             and (VS1opz =  '2'
211400060410     c                              or  VS1opz =  '5')
211500060410     c                   clear                   KPJBU
211600060410     c                   movel(p)  VS1key        KPJBU
211700060410     c                   call      V1Hpg7
211800060410     c                   parm                    KPJBA
211900120111      * - 8ª tabella ('LAC')
212000120111w   1c                   when           VS1cod =  V1Ctb8
212100120111     c                             and (VS1opz =  '2'
212200120111     c                              or  VS1opz =  '5')
212300120111     c                   clear                   TNTB46ds
212400120111     c                   eval      B46ke1 = VS1key
212500120111     c                   movel(p)  TNTB46ds      KPJBU
212600120111     c                   call      V1Hpg8
212700120111     c                   parm                    KPJBA
212800130129      * - 9ª tabella ('DKC')
212900120112w   1c                   when           VS1cod =  V1Ctb9
213000120112     c                             and (VS1opz =  '2'
213100120112     c                              or  VS1opz =  '5')
213200120112     c                   clear                   KPJBU
213300120112     c                   movel(p)  VS1key        KPJBU
213400120112     c                   call      V1Hpg9
213500120112     c                   parm                    KPJBA
213600130129      * - 10ª tabella ('OSR')
213700130129w   1c                   when           VS1cod =  V1Ctb10
213800130129     c                             and (VS1opz =  '2'
213900130129     c                              or  VS1opz =  '5')
214000130129     c                   clear                   KPJBU
214100130129     c                   movel(p)  VS1key        KPJBU
214200130129     c                   call      V1Hpg10
214300130129     c                   parm                    KPJBA
214400150812      * - 11ª tabella ('3EW')
214500150812w   1c                   when           VS1cod =  V1Ctb11
214600150812     c                             and (VS1opz =  '2'
214700150812     c                              or  VS1opz =  '5')
214800150812     c                   clear                   KPJBU
214900150812     c                   clear                   TNTB79DS
215000150812     C                   eval      I3EWTLA = 'L'
215100150812     c                   EVAL      I3EWKSU = '0' +
215200150812     c                             VS1key
215300150812     c                   EVAL      I3EWSUN = VH1SUN
215400150812     c                   call      V1Hpg11
215500150812     c                   parm                    KPJBA
215600150812     c                   parm                    TNTB79DS
215700150819      *
215800150819     c                   IF        O3EWERR = '1'
215900150819     c                   seton                                        5028
216000150819     c                   eval      V1Dmsg  =  O3EWMSG
216100150819     c                   endif
216200051110      *
216300051110e   1c                   endsl
216400051110      *
216500051110     c                   ENDSR
216600051110      *
216700051110      *---------------------------------------------------------------*
216800051110      *?Decodifica cliente                                           ?*
216900051110      *---------------------------------------------------------------*
217000051110     c     Decod_Cli     BEGSR
217100051110      *
217200051110     c                   clear                   TIBS69ds
217300051110     c                   clear                   ds_CNACO
217400051110     c                   clear                   ds_CNIND
217500051110     c                   clear                   ds_CNCLP
217600051110     c                   clear                   ds_FNCLS
217700051110      *
217800051110     c                   z-add     Wksc          I69kac
217900051110      *
218000051110     c                   call      'TIBS69R'
218100051110     c                   parm                    TIBS69ds
218200051110     c                   parm                    ds_CNACO
218300051110     c                   parm                    ds_CNIND
218400051110     c                   parm                    ds_CNCLP
218500051110     c                   parm                    ds_FNCLS
218600051110      *
218700051110     c                   ENDSR
218800051110      *
218900051110      *---------------------------------------------------------------*
219000051110      *?Ordinamento subfile                                          ?*
219100051110      *?  Subroutine - Sort                                          ?*
219200051110      *?  This subroutine sorts the subfile records.                 ?*
219300051110      *---------------------------------------------------------------*
219400051110     c     Sort          BEGSR
219500051110      *
219600051122     c                   eval      C01rcd  =  1
219700051122     c                   clear                   C01csr
219800051122     c                   clear                   S01nrr
219900051122     c                   clear                   V1Dmsg
220000051122     c                   setoff                                       28  90
220100051122     c                   eval      *in32   =  *off
220200051122     c                   movea     *zeros        *in(50)
220300051110      *
220400051110      * Initialize the key fields to sort on. There is one extra field not in
220500051110      * the subfile -- Selected -- that is added to the record. This field
220600051110      * is used to place selected records in front of nonselected records.
220700051110      *
220800051110     c                   clear                   QLGKL
220900051110     c                   clear                   QLGSKL
221000051110     c                   clear                   QLGSCB
221100051110     c                   clear                   QLGSCB00
221200051110      *
221300051110      * Gestione della scelta ordinamento.
221400051110sel 1c                   SELECT
221500051110      * Ordinamento per Chiave_Tabella/Tipo_Tabella
221600051122w   1c                   WHEN      WrkSort  = *on
221700051110      *   2 campi chiave
221800051110     c                   eval      QLGNBRK  = 2
221900051122      *   1° campo: chiave tabella (VS1key - codice cliente)
222000120807      *             a posizone  111,  7 byte, char, ascending.
222100120807     c                   eval      QLGSP    = 111
222200051110     c                   eval      QLGSS    = %size(VS1key)
222300051110     c                   eval      QLGDT    = Carattere
222400051110     c                   eval      QLGSO    = Ascendente
222500051110     c                   eval      QLGKL(1) = QLGSKL
222600051122      *   2° campo: tipo tabella (VS1cod - 3C/3K/3R/4C)
222700120807      *             a posizone  108,  3 byte, char, ascending.
222800120807     c                   eval      QLGSP    = 108
222900051110     c                   eval      QLGSS    = %size(VS1cod)
223000051110     c                   eval      QLGDT    = Carattere
223100051110     c                   eval      QLGSO    = Ascendente
223200051110     c                   eval      QLGKL(2) = QLGSKL
223300051110      *
223400051110      * Ordinamento per Tipo_Tabella/Chiave_Tabella
223500051110w   1c                   WHEN      WrkSort  = *off
223600051110      *   2 campi chiave
223700051110     c                   eval      QLGNBRK  = 2
223800051122      *   1° campo: tipo tabella (VS1cod - 3C/3K/3R/4C)
223900120807      *             a posizone  108,  3 byte, char, ascending.
224000120807     c                   eval      QLGSP    = 108
224100051110     c                   eval      QLGSS    = %size(VS1cod)
224200051110     c                   eval      QLGDT    = Carattere
224300051110     c                   eval      QLGSO    = Ascendente
224400051110     c                   eval      QLGKL(1) = QLGSKL
224500051122      *   2° campo: chiave tabella (VS1key - codice cliente)
224600120807      *             a posizone  111,  7 byte, char, ascending.
224700120807     c                   eval      QLGSP    = 111
224800051110     c                   eval      QLGSS    = %size(VS1key)
224900051110     c                   eval      QLGDT    = Carattere
225000051110     c                   eval      QLGSO    = Ascendente
225100051110     c                   eval      QLGKL(2) = QLGSKL
225200051110      *
225300051110e   1c                   ENDSL
225400051110      *
225500051110      * Load other sort parameters.
225600051110     c                   eval      QLGLB    = 80 + 16 * MaxKey
225700051110     c                   eval      QLGRL    = %size(SflRcd) - 1
225800051110     c                   eval      QLGRT    = 8
225900051110     c                   eval      QLGOKL   = 80
226000051110     c                   eval      QLGLKE   = 16
226100051110     c                   eval      QLGLSS   = 290
226200051110      * Initialize Sort I/O API fields.
226300051110     c                   eval      QLGRL00  = QLGRL
226400051110     c                   eval      QLGRC00  = 1
226500051110     c                   clear                   QUSEI
226600051110     c                   eval      QUSBPRV  = %size(QUSEC)
226700051110      * First step - Initialize the sort routine.
226800051110     c                   call      'QLGSORT'
226900051110     c                   parm                    QLGSCB
227000051110     c                   parm                    NotUsed
227100051110     c                   parm                    NotUsed
227200051110     c                   parm                    SizeList
227300051110     c                   parm                    ReturnSize
227400051110     c                   parm                    QUSEC
227500051110      *
227600051110      * Next step - Write records to I/O routine.
227700051110     c                   eval      QLGRT00  = Put
227800051122do  1c                   DO        RrnLast       Nrr
227900051122     c     Nrr           chain     TB06S1
228000051110      * Solo le righe con Selected = 'Y' sono riordinate,
228100051110      * quindi per fare un ordinamento di tutte le righe
228200051110      * metto 'Y' sempre.
228300051110     c                   eval      Selected = 'Y'
228400051110     c                   clear                   QUSEI
228500051110     c                   eval      QUSBPRV  = %size(QUSEC)
228600051110     c                   call      'QLGSRTIO'
228700051110     c                   parm                    QLGSCB00
228800051110     c                   parm                    SflRcd
228900051110     c                   parm                    NotUsed
229000051110     c                   parm                    SizeList
229100051110     c                   parm                    NotUsed
229200051110     c                   parm                    QUSEC
229300051110e   1c                   ENDDO
229400051110      * Next step - Signal end of input, clear subfile for reload.
229500051110     c                   eval      QLGRT00  = EndPut
229600051110     c                   clear                   QUSEI
229700051110     c                   eval      QUSBPRV  = %size(QUSEC)
229800051110      *
229900051110     c                   call      'QLGSRTIO'
230000051110     c                   parm                    QLGSCB00
230100051110     c                   parm                    SflRcd
230200051110     c                   parm                    NotUsed
230300051110     c                   parm                    SizeList
230400051110     c                   parm                    NotUsed
230500051110     c                   parm                    QUSEC
230600051110      *
230700051110      * Pulizia subfile
230800051110     c                   seton                                        3031
230900051110     c                   write     TB06C1
231000051110     c                   setoff                                         3133
231100051110      *
231200051110      * Final step - Write the records back to the subfile.
231300051122     c                   eval      QLGRT00  = Get
231400051122do  1c                   do        RrnLast       Nrr
231500051110     c                   clear                   QUSEI
231600051122     c                   eval      QUSBPRV  = %size(QUSEC)
231700051110     c                   call      'QLGSRTIO'
231800051110     c                   parm                    QLGSCB00
231900051110     c                   parm                    NotUsed
232000051110     c                   parm                    SflRcd
232100051110     c                   parm                    QLGRL00
232200051110     c                   parm                    NotUsed
232300051110     c                   parm                    QUSEC
232400051122      * - Ripristino indicatori utilizzati nel sfl rec
232500051122     c                   eval      *in32   =  (VS1opz <> *blanks)
232600130201     c                   eval      *in41   =  ($Called = *on  and
232700130201     c                                         D06op0  = *blank)
232800130315     c                   eval      *in42   =  (VH1abl  <> *blanks)
232900051122     c                   z-add     Nrr           S01nrr
233000051110     c                   WRITE     TB06S1
233100051110e   1c                   enddo
233200051122      *
233300051122      * Visualizzazione del SFL (se ci sono dati)
233400051122     c                   eval      *in30   =  (S01nrr = *zeros)
233500051122      *
233600051122      * Attivazione (eventuale) del SFLEND
233700051122     c                   eval      *in33   =  $EoF
233800051110      *
233900051110     c                   ENDSR
234000051110
234100051110      *---------------------------------------------------------------*
234200051110
234300051110     oPRTF198   e            PRTini            2
234400051110     o                       RSUT
234500051110     o                                        +  20 'ELENCO LEGAMI TAB-
234600051110     o                                              ELLE   V A S'
234700051110     o                       SDSpgm           +  20
234800051110     o                       Wdate         Y  +   5
234900051110     o                                        +   5 'Pag.'
235000051110     o                       Page          Z  +   0
235100051110     o          e            PRTini      0
235200051110     o                                        +  40 'ELENCO LEGAMI TAB-
235300051110     o                                              ELLE   V A S'
235400051110     o          e            PRTini      1
235500051110     o                       KNSIF
235600051110     o                       SDSusr           +   1
235700051110     o                                        +  20 '------------------
235800051110     o                                              ------------'
235900051110     o                       Wtime            +  35 '  :  :  '
236000051110     o          e            PRTini      0
236100051110     o                                        +  40 '------------------
236200051110     o                                              ------------'
236300051110      *
236400051110     o          e            PRTpar      5
236500051110     o                                              'PARAMETRI DI LANC-
236600051110     o                                              IO:'
236700051110     o                                        +   5 'Codice cliente ..-
236800051110     o                                              ...:'
236900051110     o                       V1Cksc           +   2
237000051110     o                       V1Dksc           +   2
237100140115     o          e            PRTpar      1
237200140115     o                                           46 'Solo cliente rich-
237300140115     o                                              . .:'
237400140115     o                       VidSoloCli       +   2
237500051110     o          e            PRTpar      0  0
237600051110     o                                              '_________________-
237700051110     o                                              ___'
237800051110      *
237900051110     o          e            PRTpar      5
238000051110     o                                        +  25 'Tabelle da esamin-
238100051110     o                                              are:'
238200051110     o                       V1Ctb1           +   2
238300051110     o                       V1Dtb1           +   2
238400051110     o                       V1Cse1           +   2
238500051110     o                                        +   2 '(S=Si)'
238600051110     o          e            PRTpar      1
238700051110     o                       V1Ctb2           +  48
238800051110     o                       V1Dtb2           +   2
238900051110     o                       V1Cse2           +   2
239000051110     o                                        +   2 '(S=Si)'
239100140423     o***       e            PRTpar      1
239200140423     o***                    V1Ctb3           +  48
239300140423     o***                    V1Dtb3           +   2
239400140423     o***                    V1Cse3           +   2
239500051110     o                                        +   2 '(S=Si)'
239600051110     o          e            PRTpar      1
239700051110     o                       V1Ctb4           +  48
239800051110     o                       V1Dtb4           +   2
239900051110     o                       V1Cse4           +   2
240000051110     o                                        +   2 '(S=Si)'
240100060202     o          e            PRTpar      1
240200060202     o                       V1Ctb5           +  48
240300060202     o                       V1Dtb5           +   2
240400060202     o                       V1Cse5           +   2
240500060202     o                                        +   2 '(S=Si)'
240600060217     o          e            PRTpar      1
240700060217     o                       V1Ctb6           +  48
240800060217     o                       V1Dtb6           +   2
240900060217     o                       V1Cse6           +   2
241000060217     o                                        +   2 '(S=Si)'
241100060410     o          e            PRTpar      1
241200060410     o                       V1Ctb7           +  48
241300060410     o                       V1Dtb7           +   2
241400060410     o                       V1Cse7           +   2
241500060410     o                                        +   2 '(S=Si)'
241600120111     o          e            PRTpar      1
241700120111     o                       V1Ctb8           +  48
241800120111     o                       V1Dtb8           +   2
241900120111     o                       V1Cse8           +   2
242000120111     o                                        +   2 '(S=Si)'
242100120228     o          e            PRTpar      1
242200120228     o                       V1Ctb9           +  48
242300120228     o                       V1Dtb9           +   2
242400120228     o                       V1Cse9           +   2
242500120228     o                                        +   2 '(S=Si)'
242600130129     o          e            PRTpar      1
242700130129     o                       V1Ctb10          +  48
242800130129     o                       V1Dtb10          +   2
242900130129     o                       V1Cse10          +   2
243000130129     o                                        +   2 '(S=Si)'
243100051110      *
243200051110     o          e            PRTtxt      3
243300051110     o                       $Txt(1)
243400051110     o                       $Txt(4)          +   0
243500051110     o                       $Txt(7)          +   0
243600051110     o          e            PRTtxt      1
243700051110     o                       $Txt(2)
243800051110     o                       $Txt(5)          +   0
243900051110     o                       $Txt(8)          +   0
244000051110      *
244100051110     o          e            PRTdet      1
244200060202     o                       VS1cod
244300051110     o                       VS1key           +   4
244400051110     o                       VH1dke           +   1
244500051110     o                       VS1cu1           +   3
244600051110     o                       VH1du1           +   1
244700051110     o                       VS1cu2           +   3
244800051110     o                       VH1du2           +   1
244900051110      *
245000051110     o          e            PRTend      3
245100051110     o                                        +  40 '***  FINE  LISTA -
245200051110     o                                               ***'
245300051110     o          e            PRTend      0
245400051110     o                                        +  40 '***  FINE  LISTA -
245500051110     o                                               ***'
245600051109
245700051109      *---------------------------------------------------------------*
245800051109
245900051109**   $Msg -------------------------------------------------------------------*
246000051110Immettere un codice cliente                                                    1
246100051110Codice cliente errato                                                          2
246200051110Selezionare almeno una tabella da esaminare                                    3
246300051110Opzione non valida                                                             4
246400060202**   $Sql_TABEL -------------------------------------------------------------*
246500100803SELECT ' ' concat TBLcod as TABcod, TBLkey as TABke1, TBLuni as TABdat         1
246600100803  FROM TABEL00F                                                                2
246700100803 WHERE TBLflg   =  ' '                                                         3
246800051110       (TBLcod  =  '                                                           4
246900051110   and  (TBLkey =  '                                                           5
247000051110    or   substr(TBLuni,                                                        6
247100060202**   $Sql_TNTBE -------------------------------------------------------------*
247200060202SELECT TBEcod as TABcod, TBEke1 as TABke1, TBEke2 as TABdat                    1
247300060410SELECT TBEcod as TABcod, TBEke1 as TABke1, TBEuni as TABdat                    2
247400100803  FROM TNTBE00F                                                                3
247500100803 WHERE TBEatb   =  ' '                                                         4
247600060410       (TBEcod  =  '                                                           5
247700060410   and  (TBEke1 =  '                                                           6
247800060410    or   TBEke2 =  '                                                           7
247900060410    or   substr(TBEuni,                                                        8
248000060202**   $Cmd -------------------------------------------------------------------*
248100051109OVRPRTF FILE(PRTF198) USRDTA('Legami_VAS')
248200051109DLTOVR  FILE(PRTF198)
248300051110**   $Txt -------------------------------------------------------*
248400051110Tab.   Cliente                                       Cliente Unifi
248500051110----   -------------------------------------------   -------------
248600051110 12    1234567 *...+....1....+....2....+....3....+   1234567 *...+
248700051110cante (1)                        Cliente Unificante (2)
248800051110------------------------------   ---------------------------------
248900051110....1....+....2....+....3....+   1234567 *...+....1....+....2....+
249000051110
249100051110----------
249200051110....3....+
