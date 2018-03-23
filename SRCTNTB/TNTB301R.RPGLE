000100110608     /*END
000200030806      *---------------------------------------------------------------*
000300030806      * Interrogazione tabella "OSR" = Serie O.R.M.                   *
000400030806      *---------------------------------------------------------------*
000500030806
000600091204     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700091204     h dftactgrp(*no)
000800091204     h bnddir('UBRTVNETA')
000900030806
001000030806      *---------------------------------------------------------------*
001100030806      *   A R C H I V I                                               *
001200030806      *---------------------------------------------------------------*
001300030806      *
001400090623     fazorg01l  if   e           k disk
001500091204     fTABEL00F  if   e           k disk
001600091204     fTNTBE01L  if a e           k disk
001700091204     f                                     extfile(wLibFile)  usropn
001800030806      *
001900030806     fTNTB301D  cf   e             workstn usropn
002000030806     f                                     sfile(TB301S1:nrr)
002100030806
002200030806      *---------------------------------------------------------------*
002300030806      *   C O S T A N T I                                             *
002400030806      *---------------------------------------------------------------*
002500030806      *
002600030806      * MaxKey - Maximum number of key fields allowed by this program
002700030806     d MaxKey          c                   2
002800030806      * Sort order:
002900030806      *  1 = Sort in an ascending sequence
003000030806      *  2 = Sort in a descending sequence
003100030806     d Ascendente      c                   1
003200030806     d Discendente     c                   2
003300030806      * Key data type:
003400030806      *  0 = Signed binary
003500030806      *  2 = Signed zoned decimal
003600030806      *  3 = Signed packed decimal
003700030806      *  6 = Character with no national language sort sequence applied,
003800030806      *      if specified
003900030806      *  7 = Unsigned packed decimal
004000030806      *      All numbers will have the sign forced positive ('F'X).
004100030806      *  8 = Unsigned zoned decimal
004200030806      *      All numbers will have the sign forced positive ('F'X).
004300030806      *  9 = Unsigned binary
004400030806      * 14 = Date in form of DD/MM/YY
004500030806      * 15 = Date in form of DD.MM.YYYY
004600030806      *
004700030806     d Numero          c                   2
004800030806     d Carattere       c                   6
004900030806     d Put             c                   1
005000030806     d EndPut          c                   2
005100030806     d Get             c                   3
005200030806      *
005300030806     d Ord_CodCli      c                   '1'
005400030806     d Kcod            c                   'OSR'
005500091204       //
005600091204       // -?Costanti per la definizione delle schiere con i nomi?
005700091204       //  ?degli iSeries da elaborare e delle relative librerie?
005800091204     d c_NrSyst        c                   const(2)
005900091204     d c_NrLibr        c                   const(2)
006000091204
006100091204      *---------------------------------------------------------------*
006200091204      *   S C H I E R E                                               *
006300091204      *---------------------------------------------------------------*
006400091204      *
006500091204       // -?iSeries  &  Librerie con entrambi i file tabelle?
006600091204     d $iSystem        s                   like(currSysNetA)
006700091204     d                                     dim(c_NrSyst)
006800091204     d                                     ctdata   perrcd( 1)
006900091204     d $Libraries      s                   like(ds_Libr)
007000091204     d                                     dim(c_NrSyst)
007100091204     d                                     alt($iSystem)
007200091204
007300091204       //--------------------------------------------------------------
007400091204       //?Definizione aree dati.                                       ?
007500091204       //--------------------------------------------------------------
007600091204
007700091204       // -?Dati utente?
007800091204     d §AzUte        e ds                  extname(AZUTE00F)
007900091204     d                                     dtaara
008000091204     d §DatiUte      e ds                  extname(dDatiUte)
008100091204     d                                     dtaara
008200030806
008300030806      *---------------------------------------------------------------*
008400030806      *   S T R U T T U R E   D A T I                                 *
008500030806      *---------------------------------------------------------------*
008600030806      *
008700030806      * Parametri
008800030806     d KPJBA         e ds
008900030806     d TNTB301ds     e ds
009000091204       //
009100091204       // -?Parametri per Reperimento dati utente?
009200091204     d TIBS34ds      e ds                  inz
009300030806      *
009400030806      * Passaggio Parametri al pgm TIBS02R
009500030806     d TIBS02DS      e ds                  inz
009600030806     d  T02mod       e                     inz('R')
009700030806     d  T02cod       e                     inz('OSR')
009800030806      *
009900030806      * Tabella OSR = Serie O.R.M.
010000030806     d dOSR          e ds                  inz
010100030806      *
010200030806     d DSkeyOSR        ds             5    inz
010300030806     d  DSKcd1                        3a   inz(*zeros)
010400030806     d  DSKcd2                             like(VS1cd2) inz
010500091204       //
010600091204       // -?Ridefinizione elenco librerie in elaborare le tabelle?
010700091204     d ds_Libr         ds                  inz
010800091204     d  $Libr                        10    dim(c_NrLibr) inz
010900030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
011000030806      * Data Structures                                               *
011100030806      *   SflRcd     - The entire subfile record to pass to the sort  *
011200030806      *   QLGSCB     - The sort request block for the QLGSORT API     *
011300030806      *   QLGSCB00   - The sort request block for the QLGSRTIO API    *
011400030806      *   QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB *
011500030806      *   QUSEC      - Error structure for the QLGSORT API            *
011600030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
011700030806     d SflRcd          ds
011800030806     d  VS1sce
011900030806     d  VS1cd1
012000030806     d  VS1cd2
012100030806     d  VS1cli
012200030806     d  VS1rag
012300130418     d  VS1cli2
012400130418     d  VS1cli3
012500130418     d  VS1cli4
012600130418     d  VS1altri
012700091204     d  Selected                      1a
012800030806      /COPY QSYSINC/QRPGLESRC,QLGSORT
012900030806     d QLGKL                         16    dim(MaxKey)
013000030806     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
013100030806     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
013200030806     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
013300030806     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
013400030806      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
013500030806      /COPY QSYSINC/QRPGLESRC,QUSEC
013600030806      *
013700030806     d                sds
013800030806     d  VTCpgm           *proc
013900030806
014000030806      *---------------------------------------------------------------*
014100030806      *   V A R I A B I L I                                           *
014200030806      *---------------------------------------------------------------*
014300030806      *
014400030806     d xx              s              3  0 inz
014500030808     d w02a            s              2    inz(*zeros)
014600030806     d STRke1          s                   like(B30ke1) inz
014700030806     d SAVcd1          s                   like(VC1cd1) inz
014800030806     d SAVcd2          s                   like(VC1cd2) inz
014900030806     d SAVcli          s                   like(VC1cli) inz
015000091204      *
015100091204     d wrknsr          s                   like(w01nsrn)
015200091204     d savtbesif       s                   like(tbesif)
015300091204     d savtbeapl       s                   like(tbeapl)
015400091204     d savtbeftt       s                   like(tbeftt)
015500091204       // -?Nome del sistema?
015600091204     d currSysNeta     s              8a   inz
015700091204       // -?Nome esteso Libreria/File del file tabella?
015800091204     d wLibFile        s             21a   inz
015900091204       // -?Campi di comodo?
016000091204     d w_iSystem       s              1  0 inz
016100091204     d w_SisInf        s              3  0 inz
016200030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
016300030806      * Standalone fields                                             *
016400030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
016500091204     d NotUsed         s             16a   inz
016600030806      * ReturnSize - Size of list returned by sort API
016700030806     d ReturnSize      s              9b 0 inz
016800030806      * SizeList   - Size of list
016900030806     d SizeList        s              9b 0 inz
017000030806      * Nrr        - Relative record number for subfile
017100030806     d Nrr             s              5i 0 inz
017200030806     d RrnLast         s              5i 0 inz
017300030806      * WrkSort    - Ordinamento impostato a video
017400030806     d WrkSort         s              1    inz
017500091204
017600091204       //--------------------------------------------------------------
017700091204       //?Definizione prototipi procedure.                             ?
017800091204       //--------------------------------------------------------------
017900091204
018000091204       // -?Reperimento dati utente?
018100091204      /copy gaitrasrc/srcProtoPR,TIBS34R
018200091204
018300091204       // -?Reperimento NETA sistema AS/400 corrente?
018400091204      /copy gaitrasrc/srcProtoPr,UBRTVNETA
018500030806
018600030806      *---------------------------------------------------------------*
018700030806      *   M A I N   L I N E                                           *
018800030806      *---------------------------------------------------------------*
018900030806      *  Riepilogo indicatori utilizzati:                             *
019000030806      *  --------------------------------                             *
019100130418      * N03 - richiamato per int. Filiale/Serie                       *
019200130418      *  03 - richiamato per int. Cliente                             *
019300030806      *  20 - Comodo                                                  *
019400030806      *  40 - SflClr                                                  *
019500030806      *  41 - NOT SflDsp                                              *
019600030808      *  42 - SflEnd                                                  *
019700030806      *  51 - Ordinamento per P.O. emissione + numero serie O.R.M.    *
019800030806      *  52 - Ordinamento per codice cliente                          *
019900030806      *---------------------------------------------------------------*
020000030806      *  PARAMETRI:                                                   *
020100030806      *  ----------                                                   *
020200030806      *  Opzione richiesta             1  I                           *
020300030806      *  Chiave 1 tabella             15  I                           *
020400030806      *  Chiave 2 tabella             15  I                           *
020500030806      *  Codice lingua                 1  I                           *
020600030806      *  Sistema Informativo           5  I                           *
020700030806      *  Dati tabella selezionata    256  O                           *
020800030806      *  Tasto funzionale premuto      3  O      F03, F12             *
020900030806      *  Flag di ritorno               1  O      1=Err                *
021000030806      *  Messaggio di ritorno         78  O                           *
021100030806      *---------------------------------------------------------------*
021200030806
021300091204      * Operazioni iniziali
021400091204     c                   exsr      sr_RutInz
021500091204      *
021600030806      * Restituisco il 1° numero serie libero per il P.O. di emissione
021700030806      *  O.R.M. e vado a fine - se richiesto con opz. '2'.
021800030806if  1c                   if        B30opz = '2'
021900030806     c                   exsr      Rep1free
022000030806     c                   goto      Fine
022100030806e   1c                   endif
022200130418
022300130418     c                   eval      *in03 = (B30opz = '3')
022400030806
022500030806if  1c                   if        not %open(TnTb301d)
022600030806     c                   open      TnTb301d
022700030806e   1c                   endif
022800030806      * Ordino il sfl per codice tabella
022900030806     c                   exsr      Initialize
023000030806     c                   if        Nrr   = 0
023100030806     c                   eval      *in41 = *on
023200030806     c                   endif
023300030806      *
023400030806do  1c                   DO        *hival
023500030806      *
023600030806      * Imposto la descrizione dell'ordinamento
023700030806sel 1c                   select
023800030806w   1c                   when      WrkSort = *blanks
023900030806     c                   movea     '10'          *in(51)
024000030806     c                   eval      VC1ied  = 'codice '
024100030806w   1c                   when      WrkSort = Ord_CodCli
024200030806     c                   movea     '01'          *in(51)
024300030806     c                   eval      VC1ied  = 'cliente'
024400030806e   1c                   endsl
024500030806      *
024600030806      * Emetto videata
024700030806     c                   write     tb301t1
024800030806     c                   write     tb301z1
024900030806     c                   exfmt     tb301c1
025000030806      *
025100030806sel 2c                   select
025200030806      * F3=Fine
025300030806w   2c                   when      *inKC
025400030806     c                   eval      B30fxx = 'F03'
025500030806     c                   leave
025600030806      * F12=Ritorno
025700030806w   2c                   when      *inKL
025800030806     c                   eval      B30fxx = 'F12'
025900030806     c                   leave
026000030806      * F5=Rivisualizza
026100030806w   2c                   when      *inKE
026200030806     c                   exsr      Initialize
026300030806      * F11=Cambio ordinamento
026400030806w   2c                   when      *inKK
026500030806     c                   exsr      SortSfl
026600030806e   2c                   endsl
026700030806      *
026800030806      * Controlli
026900130129if  2c                   if        RrnLast > *zeros
027000030806     c                   exsr      Ctr01
027100130129e   2c                   endif
027200030806      *
027300030806      * Effettuata una scelta
027400030806if  2c                   if        B30ke1 <> *blanks
027500030806     c                   leave
027600030806e   2c                   endif
027700030806      *
027800030806e   1c                   ENDDO
027900030806      *
028000030806      * Fine
028100030806     c     Fine          tag
028200091204     c                   exsr      sr_RutEnd
028300030806      *
028400030806      *---------------------------------------------------------------*
028500091204      * RutInz                                                        *
028600030806      *---------------------------------------------------------------*
028700091204     c     sr_RutInz     BegSr
028800030806      *
028900030806     c     *entry        plist
029000030806     c                   parm                    KPJBA
029100091204      *
029200091204     c                   eval      *inLR = *on
029300030806      *
029400030806     c                   movel     KPJBU         TNTB301ds
029500030806     c                   clear                   B30fxx
029600030806     c                   clear                   B30err
029700030806     c                   clear                   B30msg
029800030806     c                   movel     B30ke1        STRke1
029900030806     c                   clear                   B30ke1
030000091204      /free
030100091204
030200091204         // -?Verifica del sistema AS/400 corrente?
030300091204         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
030400091204           exsr  sr_RutEnd;
030500091204         endif;
030600091204
030700091204         // -?Impostazione elenco librerie in cui gestire le tabelle?
030800091204         //  ?(a seconda del sistema in cui si stà lavorando)?
030900091204         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
031000091204         if  w_iSystem = *zero;
031100091204           exsr sr_RutEnd;
031200091204         endif;
031300091204
031400091204         // -?Reperimento dati job?
031500091204         exsr  sr_DatiJob;
031600091204
031700091204         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
031800091204         w_SisInf = 1;
031900091204         exsr  sr_OpenFileTab;
032000091204
032100091204      /end-free
032200030806      *
032300091204     c                   eval      TBLkut = 1
032400030806     c                   movel     Kcod          TBEcod
032500030806      *
032600030806     c     K02TBE01      klist
032700030806     c                   kfld                    TBEcod
032800030806     c                   kfld                    TBEke1
032900030806      *
033000030806     c     K03TAB00      klist
033100091204     c                   kfld                    TBLkut
033200030806     c                   kfld                    TBLcod
033300030806     c                   kfld                    TBLkey
033400030806      *
033500030806     c                   EndSr
033600091204      /free
033700091204
033800091204       //--------------------------------------------------------------
033900091204       //?Reperimento dati del job (Utente/Operativi).                 ?
034000091204       //--------------------------------------------------------------
034100091204       BEGSR  sr_DatiJob;
034200091204
034300091204         in(e) §AzUte;
034400091204         if NOT %error;
034500091204           in(e) §DatiUte;
034600091204         endif;
034700091204         if %error or RSut = *blank;
034800091204           tibs34r ( tibs34ds );
034900091204           in §AzUte;
035000091204           in §DatiUte;
035100091204         endif;
035200091204
035300091204       ENDSR;
035400091204
035500091204       //--------------------------------------------------------------
035600091204       //?Operazioni finali / Uscita dal programma.                    ?
035700091204       //--------------------------------------------------------------
035800091204       BEGSR  sr_RutEnd;
035900091204
036000091204         kpjbu = TnTb301ds;
036100091204
036200091204         return;
036300091204
036400091204       ENDSR;
036500091204
036600091204       //--------------------------------------------------------------
036700091204       //?Apertura dei files tabelle nel sistema informativo impostato.?
036800091204       //--------------------------------------------------------------
036900091204       BEGSR  sr_OpenFileTab;
037000091204
037100091204         // -?Chiusura (eventuale) file tabelle?
037200091204         if  %open(TNTBE01L);
037300091204           close  TNTBE01L;
037400091204         endif;
037500091204
037600091204         // -?Apertura file tabelle?
037700091204         ds_Libr  = $Libraries(w_iSystem);
037800091204         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
037900091204         open  TNTBE01L;
038000091204
038100091204       ENDSR;
038200091204
038300091204      /end-free
038400030806      *
038500030806      *---------------------------------------------------------------*
038600030806      * Rep1free                                                      *
038700030806      *---------------------------------------------------------------*
038800030806     c     Rep1free      BegSr
038900030806      *
039000030806     c                   movel     STRke1        DSkeyOSR
039100030808     c                   clear                   DSKcd2
039200030806     c                   movel(p)  DSkeyOSR      TBEke1
039300030808     c     K02tbe01      setll     TNTBE000
039400030808     c     Kcod          reade     TNTBE000
039500030808do  1c                   dou       %eof(TNTBE01L)     or
039600030806     c                             %subst(TBEke1:1:3) <> DSKcd1
039700030808     c                   add       1             DSKcd2
039800030808     c                   move      DSKcd2        w02a
039900130418if  2c                   if        (w02a < %subst(TBEke1:4:2) and
040000130418     c                             %subst(TBEke1:1:3) = DSKcd1) or
040100130418     c                             %subst(TBEke1:1:3) <> DSKcd1
040200030806     c                   movel(p)  DSkeyOSR      B30ke1
040300030806     c                   clear                   B30ke2
040400030808     c                   movel     TBElin        B30lin
040500030808     c                   movel     TBEsif        B30sif
040600110608     c                   leavesr
040700030808e   2c                   endif
040800030808     c     Kcod          reade     TNTBE000
040900030808e   1c                   enddo
041000110608      *
041100110608if  1c                   if        %eof(TNTBE01L)     or
041200110608     c                             %subst(TBEke1:1:3) <> DSKcd1
041300110608     c                   add       1             DSKcd2
041400110608     c                   move      DSKcd2        w02a
041500110608     c                   movel(p)  DSkeyOSR      B30ke1
041600110608     c                   clear                   B30ke2
041700110608     c                   movel     TBElin        B30lin
041800110608     c                   movel     TBEsif        B30sif
041900110608e   1c                   endif
042000030806      *
042100030806     c                   EndSr
042200030806      *
042300030806      *---------------------------------------------------------------*
042400030806      * Initialize                                                    *
042500030806      *---------------------------------------------------------------*
042600030806     c     Initialize    BegSr
042700030806      *
042800030806      * Inizializzazione videata
042900030806     c                   eval      *in40 = *on
043000030806     c                   write     tb301c1
043100030806     c                   eval      *in40 = *off
043200030806     c                   clear                   Nrr
043300030806
043400030806      * Carico il subfile in ordine di codice
043500030806     c                   movea     '10'          *in(51)
043600130418     c  n03              movel     STRke1        TBEke1
043700130418     c   03              clear                   TBEke1
043800130418     c  n03K02tbe01      setll     TNTBE000
043900130418     c   03Kcod          setll     TNTBE000
044000030806     c     Kcod          reade     TNTBE000
044100030806do  1c                   DOW       NOT %eof(TNTBE01L)
044200030806      * Escludo annullati
044300030806if  2c                   if        TBEatb = *blanks  and
044400030806      * Escludo se non del S.I. passato
044500030806     c                             (TBEsif = *blanks or
044600130418     c                              TBEsif = B30sif)
044700130418     c                   movel     TBEuni        Dosr
044800030808      * Escludo se non del P.O. emissione ORM
044900130418     c                   IF        (not *in03 and
045000130418     c                             (%subst(STRke1:1:3) <= *zeros  or
045100130418     c                              %subst(STRke1:1:3) =
045200130418     c                              %subst(TBEke1:1:3))) or
045300130418      * Escludo se non c'è il cliente richiesto
045400130418     c                             (*in03 and
045500130418     c                             (%subst(STRke1:1:7) =
045600130418     c                              %editc(d§OSRcli:'X') or
045700130418     c                              %subst(STRke1:1:7) =
045800130418     c                              %editc(d§OSRcl2:'X') or
045900130418     c                              %subst(STRke1:1:7) =
046000130418     c                              %editc(d§OSRcl3:'X') or
046100130418     c                              %subst(STRke1:1:7) =
046200130418     c                              %editc(d§OSRcl4:'X') or
046300130418     c                              %subst(STRke1:1:7) =
046400130418     c                              %editc(d§OSRcl5:'X') or
046500130418     c                              %subst(STRke1:1:7) =
046600130418     c                              %editc(d§OSRcl6:'X') or
046700130418     c                              %subst(STRke1:1:7) =
046800130418     c                              %editc(d§OSRcl7:'X') or
046900130418     c                              %subst(STRke1:1:7) =
047000130418     c                              %editc(d§OSRcl8:'X') or
047100130418     c                              %subst(STRke1:1:7) =
047200130418     c                              %editc(d§OSRcl9:'X') or
047300130418     c                              %subst(STRke1:1:7) =
047400130418     c                              %editc(d§OSRcl10:'X') or
047500130418     c                              %subst(STRke1:1:7) =
047600130418     c                              %editc(d§OSRcl11:'X') or
047700130418     c                              %subst(STRke1:1:7) =
047800130418     c                              %editc(d§OSRcl12:'X') or
047900130418     c                              %subst(STRke1:1:7) =
048000130418     c                              %editc(d§OSRcl13:'X') or
048100130418     c                              %subst(STRke1:1:7) =
048200130418     c                              %editc(d§OSRcl14:'X') or
048300130418     c                              %subst(STRke1:1:7) =
048400130418     c                              %editc(d§OSRcl15:'X') or
048500130418     c                              %subst(STRke1:1:7) =
048600130418     c                              %editc(d§OSRcl16:'X')))
048700030806      * Imposto campi chiave del record selezionato
048800030806     c                   movel     TBEke1        DSkeyOSR
048900030806     c                   move      DSKcd1        VS1cd1
049000030806     c                   eval      VS1cd2 = DSKcd2
049100030806      * Imposto dati del record selezionato
049200030806     c                   movel     d§OSRcli      VS1cli
049300030806     c                   movel     d§OSRrag      VS1rag
049400130418     c                   movel     d§OSRcl2      VS1cli2
049500130418     c                   movel     d§OSRcl3      VS1cli3
049600130418     c                   movel     d§OSRcl4      VS1cli4
049700130418     c                   clear                   VS1altri
049800130418     c                   IF        d§OSRcl5 > *zeros or d§OSRcl6 > *zeros or
049900130418     c                             d§OSRcl7 > *zeros or d§OSRcl8 > *zeros or
050000130418     c                             d§OSRcl9 > *zeros or d§OSRcl10 > *zeros or
050100130418     c                             d§OSRcl11 > *zeros or d§OSRcl12 > *zeros or
050200130418     c                             d§OSRcl13 > *zeros or d§OSRcl14 > *zeros or
050300130418     c                             d§OSRcl16 > *zeros or d§OSRcl16 > *zeros
050400130418     c                   eval      VS1altri = '+'
050500130418     c                   ENDIF
050600030806     c                   add       1             Nrr
050700030806     c                   write     tb301s1
050800030806e   2c                   endif
050900130418e   2c                   endif
051000030806     c     Kcod          reade     TNTBE000
051100030806e   1c                   ENDDO
051200030806      *
051300030808     c                   eval      *in42   = *on
051400030806     c                   eval      RrnLast = Nrr
051500030806     c                   eval      RcdNbr  = 1
051600030806      *
051700030806     c                   EndSr
051800030806      *
051900030806      *---------------------------------------------------------------*
052000030806      * SortSfl                                                       *
052100030806      * This subroutine sorts the subfile records.                    *
052200030806      *---------------------------------------------------------------*
052300030806     c     SortSfl       BegSr
052400030806      *
052500030806     c                   clear                   VC1cd1
052600030806     c                   clear                   VC1cd2
052700030806     c                   clear                   VC1cli
052800030806     c                   clear                   SAVcd1
052900030806     c                   clear                   SAVcd2
053000030806     c                   clear                   SAVcli
053100030806     c                   eval      RcdNbr = 1
053200030806      *
053300030806sel 1c                   select
053400030806      * Sono in ordine di codice: all'F11 passo al cliente
053500030806w   1c                   when      WrkSort  = *blanks
053600030806     c                   eval      WrkSort  = Ord_CodCli
053700030806      * Sono in prdine di cliente: all'F11 passo al codice
053800030806w   1c                   when      WrkSort  = Ord_CodCli
053900030806     c                   eval      WrkSort  = *blanks
054000030806e   1c                   endsl
054100030806      *
054200030806      * Initialize the key fields to sort on. There is one extra field
054300030806      * not in the subfile -- Selected -- that is added to the record.
054400030806      * This field is used to place selected records in front of
054500030806      * nonselected records.
054600030806      *
054700030806     c                   clear                   QLGSCB
054800030806     c                   clear                   QLGSCB00
054900030806      *
055000030806      * Gestione della scelta ordinamento:
055100030806sel 1c                   SELECT
055200030806      *
055300030806      * Ordinamento per: P.O. emissione ORM + Numero Serie
055400030806w   1c                   WHEN      WrkSort  = *blanks
055500030806      *   Numero campi chiave: 2
055600030806     c                   eval      QLGNBRK  = 2
055700030806      *   Dati relativi al 1° campo chiave:
055800030806      *   - Posizone iniziale: 2
055900030806      *   - Lunghezza        : 3
056000030806      *   - Tipo byte        : Zoned
056100030806      *   - Ordinamento      : Ascending
056200030806     c                   eval      QLGSP    = 2
056300030806     c                   eval      QLGSS    = %size(VS1cd1)
056400030806     c                   eval      QLGDT    = Numero
056500030806     c                   eval      QLGSO    = Ascendente
056600030806     c                   eval      QLGKL(1) = QLGSKL
056700030806      *   Dati relativi al 2° campo chiave:
056800030806      *   - Posizone iniziale: 5
056900030806      *   - Lunghezza        : 2
057000030806      *   - Tipo byte        : Zoned
057100030806      *   - Ordinamento      : Ascending
057200030806     c                   eval      QLGSP    = 5
057300030806     c                   eval      QLGSS    = %size(VS1cd2)
057400030806     c                   eval      QLGDT    = Numero
057500030806     c                   eval      QLGSO    = Ascendente
057600030806     c                   eval      QLGKL(2) = QLGSKL
057700030806      *
057800030806      * Ordinamento per Codice Cliente
057900030806w   1c                   WHEN      WrkSort  = Ord_CodCli
058000030806      *   Numero campi chiave: 1
058100030806     c                   eval      QLGNBRK  = 1
058200030806      *   Dati relativi all'unico campo chiave:
058300030806      *   - Posizone iniziale: 7
058400030806      *   - Lunghezza        : 7 byte
058500030806      *   - Tipo byte        : Zoned
058600030806      *   - Ordinamento      : Ascending
058700030806     c                   eval      QLGSP    = 7
058800030806     c                   eval      QLGSS    = %size(VS1cli)
058900030806     c                   eval      QLGDT    = Numero
059000030806     c                   eval      QLGSO    = Ascendente
059100030806     c                   eval      QLGKL(1) = QLGSKL
059200030806      *
059300030806e   1c                   ENDSL
059400030806      *
059500030806      * Load other sort parameters.
059600030806     c                   eval      QLGLB    = 80 + 16 * MaxKey
059700030806     c                   eval      QLGRL    = %size(SflRcd) - 1
059800030806     c                   eval      QLGRT    = 8
059900030806     c                   eval      QLGOKL   = 80
060000030806     c                   eval      QLGLKE   = 16
060100030806     c                   eval      QLGLSS   = 290
060200030806      * Initialize Sort I/O API fields.
060300030806     c                   eval      QLGRL00  = QLGRL
060400030806     c                   eval      QLGRC00  = 1
060500030806     c                   clear                   QUSEI
060600030806     c                   eval      QUSBPRV  = %size(QUSEC)
060700030806      *
060800030806      * First step - Initialize the sort routine.
060900030806     c                   call      'QLGSORT'
061000030806     c                   parm                    QLGSCB
061100030806     c                   parm                    NotUsed
061200030806     c                   parm                    NotUsed
061300030806     c                   parm                    SizeList
061400030806     c                   parm                    ReturnSize
061500030806     c                   parm                    QUSEC
061600030806      *
061700030806      * Next step - Write records to I/O routine.
061800030806     c                   eval      QLGRT00  = Put
061900030806do  1c                   DO        RrnLast       xx
062000030806     c     xx            chain     tb301s1
062100030806      * Solo le righe con Selected = 'Y' sono riordinate,
062200030806      * quindi per fare un ordinamento di tutte le righe
062300030806      * metto 'Y' sempre.
062400030806     c                   eval      Selected = 'Y'
062500030806     c                   clear                   QUSEI
062600030806     c                   eval      QUSBPRV  = %size(QUSEC)
062700030806     c                   call      'QLGSRTIO'
062800030806     c                   parm                    QLGSCB00
062900030806     c                   parm                    SflRcd
063000030806     c                   parm                    NotUsed
063100030806     c                   parm                    SizeList
063200030806     c                   parm                    NotUsed
063300030806     c                   parm                    QUSEC
063400030806e   1c                   ENDDO
063500030806      *
063600030806      * Next step - Signal end of input, clear subfile for reload.
063700030806     c                   eval      QLGRT00  = EndPut
063800030806     c                   clear                   QUSEI
063900030806     c                   eval      QUSBPRV  = %size(QUSEC)
064000030806     c                   call      'QLGSRTIO'
064100030806     c                   parm                    QLGSCB00
064200030806     c                   parm                    SflRcd
064300030806     c                   parm                    NotUsed
064400030806     c                   parm                    SizeList
064500030806     c                   parm                    NotUsed
064600030806     c                   parm                    QUSEC
064700030806     c                   eval      *in40 = *on
064800030806     c                   write     tb301c1
064900030806     c                   eval      *in40 = *off
065000030806      *
065100030806      * Final step - Write the records back to the subfile.
065200030806     c                   eval      QLGRT00 = Get
065300030806do  1c                   DO        RrnLast       xx
065400030806     c                   clear                   QUSEI
065500030806     c                   eval      QUSBPRV = %size(QUSEC)
065600030806     c                   call      'QLGSRTIO'
065700030806     c                   parm                    QLGSCB00
065800030806     c                   parm                    NotUsed
065900030806     c                   parm                    SflRcd
066000030806     c                   parm                    QLGRL00
066100030806     c                   parm                    NotUsed
066200030806     c                   parm                    QUSEC
066300030806     c                   z-add     xx            Nrr
066400030806     c                   write     tb301s1
066500030806e   1c                   ENDDO
066600030806      *
066700030806     c                   EndSr
066800030806      *
066900030806      *---------------------------------------------------------------*
067000030806      * Ctr01                                                         *
067100030806      *---------------------------------------------------------------*
067200030806     c     Ctr01         BegSr
067300030806      *
067400030806     c                   eval      *in41 = *off
067500030806      *
067600030806do  1c                   DO        *hival        xx
067700030806      *
067800030806     c     xx            chain     tb301s1                            01
067900030806if  2c                   if        NOT %found(tntb301d)
068000030806     c                   leave
068100030806e   2c                   endif
068200030806      *
068300030806if  2c                   if        VS1sce = '1'
068400030806     c                   move      VS1cd1        DSKcd1
068500030806     c                   move      VS1cd2        DSKcd2
068600030806     c                   movel(p)  DSkeyOSR      B30ke1
068700030806     c                   clear                   B30ke2
068800030806     c                   leave
068900030806e   2c                   endif
069000090623
069100090623      * se copia emetto window con richiesta nuova chiave
069200090623if  2c                   if        VS1sce = '3'
069300090623     c                   exsr      sr_w01
069400090623     c                   if        *inkf
069500090623     c                   move      w01poen       DSKcd1
069600090623     c                   move      w01nsrn       DSKcd2
069700090623     c                   movel(p)  DSkeyOSR      B30ke1
069800090623     c                   clear                   B30ke2
069900090623     c                   leave
070000090623     c                   endif
070100090623e   2c                   endif
070200030806      *
070300030806      * Controllo se effettuata ricerca
070400030806if  1c                   if           (VC1cd1 <> *zeros
070500030806     c                             and VC1cd1 <> SAVcd1)
070600030806     c                             or (VC1cd2 <> *zeros
070700030806     c                             and VC1cd2 <> SAVcd2)
070800030806     c                             or (VC1cli <> *zeros
070900030806     c                             and VC1cli <> SAVcli)
071000030806     c                   eval      *in41 = *on
071100030806if  2c                   if           (WrkSort = *blanks
071200030806     c                             and VC1cd1 <= VS1cd1
071300030806     c                             and VC1cd2 <= VS1cd2)
071400030806     c                             or (WrkSort = Ord_CodCli
071500030806     c                             and VC1cli <= VS1cli)
071600030806     c                   eval      *in41 = *off
071700030806     c                   z-add     xx            RcdNbr
071800030806sel 3c                   select
071900030806w   3c                   when      WrkSort = *blanks
072000030806     c                   move      VC1cd1        SAVcd1
072100030806     c                   move      VC1cd2        SAVcd2
072200030806w   3c                   when      WrkSort = Ord_CodCli
072300030806     c                   movel     VC1cli        SAVcli
072400030806e   3c                   endsl
072500030806     c                   leave
072600030806     c                   endif
072700030806     c                   endif
072800030806      *
072900030806e   1c                   ENDDO
073000030806      *
073100030806if  1c                   if        not *in41
073200030806     c                   clear                   VC1cd1
073300030806     c                   clear                   VC1cd2
073400030806     c                   clear                   VC1cli
073500030806e   1c                   endif
073600030806      *
073700030806     c                   EndSr
073800090623      *
073900090623      *---------------------------------------------------------------*
074000090623      * Gestione window per richiesta copia tabella                   *
074100090623      *---------------------------------------------------------------*
074200090623     c     sr_w01        begsr
074300091204      *
074400090623     c                   eval      w01poeo = vs1cd1
074500090623     c                   eval      w01nsro = vs1cd2
074600090623     c                   clear                   w01poen
074700090623     c                   clear                   w01nsrn
074800090623     c                   eval      *in60 = *off
074900090623     c                   eval      *in61 = *off
075000091204      *
075100091204     c                   DO        *hival
075200091204      *
075300090623     c                   exfmt     tb301w1
075400090623     c                   eval      *in28 = *on
075500090623     c                   clear                   w01msg
075600090623      * F12=Ritorno
075700091204     c   kl              leavesr
075800090623      * Controllo
075900090623     c                   if        w01poen = *zeros
076000090623     c                   eval      w01msg = 'Immettere la filiale'
076100090623     c                   eval      *in28 = *on
076200090623     c                   eval      *in60 = *on
076300090623     c                   iter
076400090623     c                   endif
076500090623     c     w01poen       chain     azorg01l
076600090623     c                   if        not %found(azorg01l) or orgfva <> *blanks
076700090623     c                   eval      w01msg = 'Filiale errata'
076800090623     c                   eval      *in28 = *on
076900090623     c                   eval      *in60 = *on
077000090623     c                   iter
077100090623     c                   endif
077200090623     c                   if        w01nsrn = *blanks
077300090623     c                   eval      w01msg = 'Immettere la serie'
077400090623     c                   eval      *in28 = *on
077500090623     c                   eval      *in61 = *on
077600090623     c                   iter
077700090623     c                   endif
077800090623     c                   if        w01nsrn = '99' or
077900090623     c                             %subst(w01nsrn:1:1) < '0' or
078000090623     c                             %subst(w01nsrn:2:1) < '0'
078100090623     c                   eval      w01msg = 'Serie errata'
078200090623     c                   eval      *in28 = *on
078300090623     c                   eval      *in61 = *on
078400090623     c                   iter
078500090623     c                   endif
078600090623     c                   move      w01nsro       wrknsr
078700090623     c                   if        w01poeo = w01poen and
078800090623     c                             wrknsr  = w01nsrn
078900090623     c                   eval      w01msg = 'Copia su stesso codice'
079000090623     c                   eval      *in28 = *on
079100090623     c                   eval      *in60 = *on
079200090623     c                   iter
079300090623     c                   endif
079400090623     c                   move      w01poen       DSKcd1
079500090623     c                   move      w01nsrn       DSKcd2
079600090623     c                   movel(p)  DSkeyOSR      tbeke1
079700091204     c     K02tbe01      chain     TNTBE000
079800090623     c                   if        %found(tntbe01l)
079900090623     c                   eval      w01msg = 'Codice già presente'
080000090623     c                   eval      *in28 = *on
080100090623     c                   eval      *in60 = *on
080200090623     c                   iter
080300090623     c                   endif
080400091204      * F6=Conferma
080500091204     c                   if        *inKF
080600091204     c                   exsr      sr_CopyOSR
080700090623     c                   leave
080800091204     c                   endif
080900091204      *
081000090623     c                   enddo
081100091204      *
081200090623     c                   endsr
081300091204
081400091204       //--------------------------------------------------------------
081500091204       //?Copia del record-tabella.                                    ?
081600091204       //--------------------------------------------------------------
081700091204     c     sr_CopyOSR    BEGSR
081800091204      *
081900091204     c                   move      w01poeo       DSKcd1
082000091204     c                   move      w01nsro       DSKcd2
082100091204     c                   movel(p)  DSkeyOSR      tbeke1
082200091204     c     K02tbe01      chain     TNTBE000
082300091204      *
082400091204     c                   if        %found(tntbe01l)
082500091204     c                   eval      dosr = tbeuni
082600091204     c                   eval      savtbesif = tbesif
082700091204     c                   eval      savtbeapl = tbeapl
082800091204     c                   eval      savtbeftt = tbeftt
082900091204     c                   endif
083000091204      *
083100091204      /free
083200091204
083300091204         // -?Ciclo di elaborazione per ogni sistema informativo?
083400091204         For  w_SisInf = 1  To  %elem($Libr);
083500091204
083600091204           // -?Apertura (eventuale) del file ed aggancio record?
083700091204           if  w_SisInf > 1;
083800091204             exsr  sr_OpenFileTab;
083900091204           endif;
084000091204
084100091204      /end-free
084200091204      *
084300091204      *
084400091204     c                   move      w01poen       DSKcd1
084500091204     c                   move      w01nsrn       DSKcd2
084600091204     c                   movel(p)  DSkeyOSR      TBEke1
084700091204     c     K02tbe01      chain     TNTBE000
084800091204      *
084900091204     c                   if        not %found(tntbe01l)
085000091204     c                   clear                   tntbe000
085100091204     c                   movel     kcod          tbecod
085200091204     c                   movel(p)  DSkeyOSR      tbeke1
085300091204     c                   eval      tbeuni = dosr
085400091204     c                   eval      tbesif = savtbesif
085500091204     c                   eval      tbeapl = savtbeapl
085600091204     c                   eval      tbeftt = savtbeftt
085700091204      /free
085800091204           // - Aggiornamento o Scrittura in sede
085900091204           if  w_SisInf = 1;
086000091204             TBEftr = 'T';
086100091204           // - Aggiornamento o Scrittura in filiale
086200091204           else;
086300091204             TBEftr = 'R';
086400091204           endif;
086500091204           TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
086600091204                                   : 1 : 8 ) );
086700091204      /end-free
086800091204     c                   write     tntbe000
086900091204     c                   endif
087000091204      /free
087100091204
087200091204         EndFor;
087300091204
087400091204      /end-free
087500091204      *
087600091204     c                   ENDSR
087700091204
087800091204       //--------------------------------------------------------------
087900091204       //?Schiere a tempo di compilazione.                             ?
088000091204       //--------------------------------------------------------------
088100091204
088200091204** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
088300091204SETRAS  GAITRAGRU FILTRAGRU
088400091204AS888   GAITRAGRPSFILTRAGRPF
