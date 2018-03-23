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
010200030807      * Tabella 3A = Codici bolla
010300030807     d ds3A          e ds                  inz
010400030806      *
010500030806     d DSkeyOSR        ds             5    inz
010600030806     d  DSKcd1                        3a   inz(*zeros)
010700030806     d  DSKcd2                             like(VS1cd2) inz
010800091204       //
010900091204       // -?Ridefinizione elenco librerie in elaborare le tabelle?
011000091204     d ds_Libr         ds                  inz
011100091204     d  $Libr                        10    dim(c_NrLibr) inz
011200030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
011300030806      * Data Structures                                               *
011400030806      *   SflRcd     - The entire subfile record to pass to the sort  *
011500030806      *   QLGSCB     - The sort request block for the QLGSORT API     *
011600030806      *   QLGSCB00   - The sort request block for the QLGSRTIO API    *
011700030806      *   QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB *
011800030806      *   QUSEC      - Error structure for the QLGSORT API            *
011900030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
012000030806     d SflRcd          ds
012100030806     d  VS1sce
012200030806     d  VS1cd1
012300030806     d  VS1cd2
012400030806     d  VS1cli
012500030806     d  VS1rag
012600030806     d  VS1cbo
012700030806     d  VS1dbo
012800090623     d  VS1trad
012900091204     d  Selected                      1a
013000030806      /COPY QSYSINC/QRPGLESRC,QLGSORT
013100030806     d QLGKL                         16    dim(MaxKey)
013200030806     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
013300030806     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
013400030806     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
013500030806     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
013600030806      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
013700030806      /COPY QSYSINC/QRPGLESRC,QUSEC
013800030806      *
013900030806     d                sds
014000030806     d  VTCpgm           *proc
014100030806
014200030806      *---------------------------------------------------------------*
014300030806      *   V A R I A B I L I                                           *
014400030806      *---------------------------------------------------------------*
014500030806      *
014600030806     d xx              s              3  0 inz
014700030808     d w02a            s              2    inz(*zeros)
014800030806     d STRke1          s                   like(B30ke1) inz
014900030806     d SAVcd1          s                   like(VC1cd1) inz
015000030806     d SAVcd2          s                   like(VC1cd2) inz
015100030806     d SAVcli          s                   like(VC1cli) inz
015200091204      *
015300091204     d wrknsr          s                   like(w01nsrn)
015400091204     d savtbesif       s                   like(tbesif)
015500091204     d savtbeapl       s                   like(tbeapl)
015600091204     d savtbeftt       s                   like(tbeftt)
015700091204       // -?Nome del sistema?
015800091204     d currSysNeta     s              8a   inz
015900091204       // -?Nome esteso Libreria/File del file tabella?
016000091204     d wLibFile        s             21a   inz
016100091204       // -?Campi di comodo?
016200091204     d w_iSystem       s              1  0 inz
016300091204     d w_SisInf        s              3  0 inz
016400030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
016500030806      * Standalone fields                                             *
016600030806      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
016700091204     d NotUsed         s             16a   inz
016800030806      * ReturnSize - Size of list returned by sort API
016900030806     d ReturnSize      s              9b 0 inz
017000030806      * SizeList   - Size of list
017100030806     d SizeList        s              9b 0 inz
017200030806      * Nrr        - Relative record number for subfile
017300030806     d Nrr             s              5i 0 inz
017400030806     d RrnLast         s              5i 0 inz
017500030806      * WrkSort    - Ordinamento impostato a video
017600030806     d WrkSort         s              1    inz
017700091204
017800091204       //--------------------------------------------------------------
017900091204       //?Definizione prototipi procedure.                             ?
018000091204       //--------------------------------------------------------------
018100091204
018200091204       // -?Reperimento dati utente?
018300091204      /copy gaitrasrc/srcProtoPR,TIBS34R
018400091204
018500091204       // -?Reperimento NETA sistema AS/400 corrente?
018600091204      /copy gaitrasrc/srcProtoPr,UBRTVNETA
018700030806
018800030806      *---------------------------------------------------------------*
018900030806      *   M A I N   L I N E                                           *
019000030806      *---------------------------------------------------------------*
019100030806      *  Riepilogo indicatori utilizzati:                             *
019200030806      *  --------------------------------                             *
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
022200030806
022300030806if  1c                   if        not %open(TnTb301d)
022400030806     c                   open      TnTb301d
022500030806e   1c                   endif
022600030806      * Ordino il sfl per codice tabella
022700030806     c                   exsr      Initialize
022800030806     c                   if        Nrr   = 0
022900030806     c                   eval      *in41 = *on
023000030806     c                   endif
023100030806      *
023200030806do  1c                   DO        *hival
023300030806      *
023400030806      * Imposto la descrizione dell'ordinamento
023500030806sel 1c                   select
023600030806w   1c                   when      WrkSort = *blanks
023700030806     c                   movea     '10'          *in(51)
023800030806     c                   eval      VC1ied  = 'codice '
023900030806w   1c                   when      WrkSort = Ord_CodCli
024000030806     c                   movea     '01'          *in(51)
024100030806     c                   eval      VC1ied  = 'cliente'
024200030806e   1c                   endsl
024300030806      *
024400030806      * Emetto videata
024500030806     c                   write     tb301t1
024600030806     c                   write     tb301z1
024700030806     c                   exfmt     tb301c1
024800030806      *
024900030806sel 2c                   select
025000030806      * F3=Fine
025100030806w   2c                   when      *inKC
025200030806     c                   eval      B30fxx = 'F03'
025300030806     c                   leave
025400030806      * F12=Ritorno
025500030806w   2c                   when      *inKL
025600030806     c                   eval      B30fxx = 'F12'
025700030806     c                   leave
025800030806      * F5=Rivisualizza
025900030806w   2c                   when      *inKE
026000030806     c                   exsr      Initialize
026100030806      * F11=Cambio ordinamento
026200030806w   2c                   when      *inKK
026300030806     c                   exsr      SortSfl
026400030806e   2c                   endsl
026500030806      *
026600030806      * Controlli
026700130129if  2c                   if        RrnLast > *zeros
026800030806     c                   exsr      Ctr01
026900130129e   2c                   endif
027000030806      *
027100030806      * Effettuata una scelta
027200030806if  2c                   if        B30ke1 <> *blanks
027300030806     c                   leave
027400030806e   2c                   endif
027500030806      *
027600030806e   1c                   ENDDO
027700030806      *
027800030806      * Fine
027900030806     c     Fine          tag
028000091204     c                   exsr      sr_RutEnd
028100030806      *
028200030806      *---------------------------------------------------------------*
028300091204      * RutInz                                                        *
028400030806      *---------------------------------------------------------------*
028500091204     c     sr_RutInz     BegSr
028600030806      *
028700030806     c     *entry        plist
028800030806     c                   parm                    KPJBA
028900091204      *
029000091204     c                   eval      *inLR = *on
029100030806      *
029200030806     c                   movel     KPJBU         TNTB301ds
029300030806     c                   clear                   B30fxx
029400030806     c                   clear                   B30err
029500030806     c                   clear                   B30msg
029600030806     c                   movel     B30ke1        STRke1
029700030806     c                   clear                   B30ke1
029800091204      /free
029900091204
030000091204         // -?Verifica del sistema AS/400 corrente?
030100091204         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
030200091204           exsr  sr_RutEnd;
030300091204         endif;
030400091204
030500091204         // -?Impostazione elenco librerie in cui gestire le tabelle?
030600091204         //  ?(a seconda del sistema in cui si stà lavorando)?
030700091204         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
030800091204         if  w_iSystem = *zero;
030900091204           exsr sr_RutEnd;
031000091204         endif;
031100091204
031200091204         // -?Reperimento dati job?
031300091204         exsr  sr_DatiJob;
031400091204
031500091204         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
031600091204         w_SisInf = 1;
031700091204         exsr  sr_OpenFileTab;
031800091204
031900091204      /end-free
032000030806      *
032100091204     c                   eval      TBLkut = 1
032200030806     c                   movel     Kcod          TBEcod
032300030806      *
032400030806     c     K02TBE01      klist
032500030806     c                   kfld                    TBEcod
032600030806     c                   kfld                    TBEke1
032700030806      *
032800030806     c     K03TAB00      klist
032900091204     c                   kfld                    TBLkut
033000030806     c                   kfld                    TBLcod
033100030806     c                   kfld                    TBLkey
033200030806      *
033300030806     c                   EndSr
033400091204      /free
033500091204
033600091204       //--------------------------------------------------------------
033700091204       //?Reperimento dati del job (Utente/Operativi).                 ?
033800091204       //--------------------------------------------------------------
033900091204       BEGSR  sr_DatiJob;
034000091204
034100091204         in(e) §AzUte;
034200091204         if NOT %error;
034300091204           in(e) §DatiUte;
034400091204         endif;
034500091204         if %error or RSut = *blank;
034600091204           tibs34r ( tibs34ds );
034700091204           in §AzUte;
034800091204           in §DatiUte;
034900091204         endif;
035000091204
035100091204       ENDSR;
035200091204
035300091204       //--------------------------------------------------------------
035400091204       //?Operazioni finali / Uscita dal programma.                    ?
035500091204       //--------------------------------------------------------------
035600091204       BEGSR  sr_RutEnd;
035700091204
035800091204         kpjbu = TnTb301ds;
035900091204
036000091204         return;
036100091204
036200091204       ENDSR;
036300091204
036400091204       //--------------------------------------------------------------
036500091204       //?Apertura dei files tabelle nel sistema informativo impostato.?
036600091204       //--------------------------------------------------------------
036700091204       BEGSR  sr_OpenFileTab;
036800091204
036900091204         // -?Chiusura (eventuale) file tabelle?
037000091204         if  %open(TNTBE01L);
037100091204           close  TNTBE01L;
037200091204         endif;
037300091204
037400091204         // -?Apertura file tabelle?
037500091204         ds_Libr  = $Libraries(w_iSystem);
037600091204         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
037700091204         open  TNTBE01L;
037800091204
037900091204       ENDSR;
038000091204
038100091204      /end-free
038200030806      *
038300030806      *---------------------------------------------------------------*
038400030806      * Rep1free                                                      *
038500030806      *---------------------------------------------------------------*
038600030806     c     Rep1free      BegSr
038700030806      *
038800030806     c                   movel     STRke1        DSkeyOSR
038900030808     c                   clear                   DSKcd2
039000030806     c                   movel(p)  DSkeyOSR      TBEke1
039100030808     c     K02tbe01      setll     TNTBE000
039200030808     c     Kcod          reade     TNTBE000
039300030808do  1c                   dou       %eof(TNTBE01L)     or
039400030806     c                             %subst(TBEke1:1:3) <> DSKcd1
039500030808     c                   add       1             DSKcd2
039600030808     c                   move      DSKcd2        w02a
039700030808if  2c                   if        w02a < %subst(TBEke1:4:2)
039800030806     c                   movel(p)  DSkeyOSR      B30ke1
039900030806     c                   clear                   B30ke2
040000030808     c                   movel     TBElin        B30lin
040100030808     c                   movel     TBEsif        B30sif
040200110608     c                   leavesr
040300030808e   2c                   endif
040400030808     c     Kcod          reade     TNTBE000
040500030808e   1c                   enddo
040600110608      *
040700110608if  1c                   if        %eof(TNTBE01L)     or
040800110608     c                             %subst(TBEke1:1:3) <> DSKcd1
040900110608     c                   add       1             DSKcd2
041000110608     c                   move      DSKcd2        w02a
041100110608     c                   movel(p)  DSkeyOSR      B30ke1
041200110608     c                   clear                   B30ke2
041300110608     c                   movel     TBElin        B30lin
041400110608     c                   movel     TBEsif        B30sif
041500110608e   1c                   endif
041600030806      *
041700030806     c                   EndSr
041800030806      *
041900030806      *---------------------------------------------------------------*
042000030806      * Initialize                                                    *
042100030806      *---------------------------------------------------------------*
042200030806     c     Initialize    BegSr
042300030806      *
042400030806      * Inizializzazione videata
042500030806     c                   eval      *in40 = *on
042600030806     c                   write     tb301c1
042700030806     c                   eval      *in40 = *off
042800030806     c                   clear                   Nrr
042900030806
043000030806      * Carico il subfile in ordine di codice
043100030806     c                   movea     '10'          *in(51)
043200030806     c                   movel     STRke1        TBEke1
043300030806     c     K02tbe01      setll     TNTBE000
043400030806     c     Kcod          reade     TNTBE000
043500030806do  1c                   DOW       NOT %eof(TNTBE01L)
043600030806      * Escludo annullati
043700030806if  2c                   if        TBEatb = *blanks  and
043800030806      * Escludo se non del S.I. passato
043900030806     c                             (TBEsif = *blanks or
044000030808     c                              TBEsif = B30sif) and
044100030808      * Escludo se non del P.O. emissione ORM
044200030808     c                             (%subst(STRke1:1:3) <= *zeros  or
044300030808     c                              %subst(STRke1:1:3) =
044400030808     c                                  %subst(TBEke1:1:3))
044500030806      * Imposto campi chiave del record selezionato
044600030806     c                   movel     TBEke1        DSkeyOSR
044700030806     c                   move      DSKcd1        VS1cd1
044800030806     c                   eval      VS1cd2 = DSKcd2
044900030806      * Imposto dati del record selezionato
045000030806     c                   movel     TBEuni        Dosr
045100030806     c                   exsr      DecodCBO
045200030806     c                   movel     d§OSRcli      VS1cli
045300030806     c                   movel     d§OSRrag      VS1rag
045400030806     c                   movel     d§OSRcbo      VS1cbo
045500030807     c                   movel     §3Ades        VS1dbo
045600090623     c                   movel     d§OSRvas      VS1trad
045700030806     c                   add       1             Nrr
045800030806     c                   write     tb301s1
045900030806e   2c                   endif
046000030806     c     Kcod          reade     TNTBE000
046100030806e   1c                   ENDDO
046200030806      *
046300030808     c                   eval      *in42   = *on
046400030806     c                   eval      RrnLast = Nrr
046500030806     c                   eval      RcdNbr  = 1
046600030806      *
046700030806     c                   EndSr
046800030806      *
046900030806      *---------------------------------------------------------------*
047000030806      * SortSfl                                                       *
047100030806      * This subroutine sorts the subfile records.                    *
047200030806      *---------------------------------------------------------------*
047300030806     c     SortSfl       BegSr
047400030806      *
047500030806     c                   clear                   VC1cd1
047600030806     c                   clear                   VC1cd2
047700030806     c                   clear                   VC1cli
047800030806     c                   clear                   SAVcd1
047900030806     c                   clear                   SAVcd2
048000030806     c                   clear                   SAVcli
048100030806     c                   eval      RcdNbr = 1
048200030806      *
048300030806sel 1c                   select
048400030806      * Sono in ordine di codice: all'F11 passo al cliente
048500030806w   1c                   when      WrkSort  = *blanks
048600030806     c                   eval      WrkSort  = Ord_CodCli
048700030806      * Sono in prdine di cliente: all'F11 passo al codice
048800030806w   1c                   when      WrkSort  = Ord_CodCli
048900030806     c                   eval      WrkSort  = *blanks
049000030806e   1c                   endsl
049100030806      *
049200030806      * Initialize the key fields to sort on. There is one extra field
049300030806      * not in the subfile -- Selected -- that is added to the record.
049400030806      * This field is used to place selected records in front of
049500030806      * nonselected records.
049600030806      *
049700030806     c                   clear                   QLGSCB
049800030806     c                   clear                   QLGSCB00
049900030806      *
050000030806      * Gestione della scelta ordinamento:
050100030806sel 1c                   SELECT
050200030806      *
050300030806      * Ordinamento per: P.O. emissione ORM + Numero Serie
050400030806w   1c                   WHEN      WrkSort  = *blanks
050500030806      *   Numero campi chiave: 2
050600030806     c                   eval      QLGNBRK  = 2
050700030806      *   Dati relativi al 1° campo chiave:
050800030806      *   - Posizone iniziale: 2
050900030806      *   - Lunghezza        : 3
051000030806      *   - Tipo byte        : Zoned
051100030806      *   - Ordinamento      : Ascending
051200030806     c                   eval      QLGSP    = 2
051300030806     c                   eval      QLGSS    = %size(VS1cd1)
051400030806     c                   eval      QLGDT    = Numero
051500030806     c                   eval      QLGSO    = Ascendente
051600030806     c                   eval      QLGKL(1) = QLGSKL
051700030806      *   Dati relativi al 2° campo chiave:
051800030806      *   - Posizone iniziale: 5
051900030806      *   - Lunghezza        : 2
052000030806      *   - Tipo byte        : Zoned
052100030806      *   - Ordinamento      : Ascending
052200030806     c                   eval      QLGSP    = 5
052300030806     c                   eval      QLGSS    = %size(VS1cd2)
052400030806     c                   eval      QLGDT    = Numero
052500030806     c                   eval      QLGSO    = Ascendente
052600030806     c                   eval      QLGKL(2) = QLGSKL
052700030806      *
052800030806      * Ordinamento per Codice Cliente
052900030806w   1c                   WHEN      WrkSort  = Ord_CodCli
053000030806      *   Numero campi chiave: 1
053100030806     c                   eval      QLGNBRK  = 1
053200030806      *   Dati relativi all'unico campo chiave:
053300030806      *   - Posizone iniziale: 7
053400030806      *   - Lunghezza        : 7 byte
053500030806      *   - Tipo byte        : Zoned
053600030806      *   - Ordinamento      : Ascending
053700030806     c                   eval      QLGSP    = 7
053800030806     c                   eval      QLGSS    = %size(VS1cli)
053900030806     c                   eval      QLGDT    = Numero
054000030806     c                   eval      QLGSO    = Ascendente
054100030806     c                   eval      QLGKL(1) = QLGSKL
054200030806      *
054300030806e   1c                   ENDSL
054400030806      *
054500030806      * Load other sort parameters.
054600030806     c                   eval      QLGLB    = 80 + 16 * MaxKey
054700030806     c                   eval      QLGRL    = %size(SflRcd) - 1
054800030806     c                   eval      QLGRT    = 8
054900030806     c                   eval      QLGOKL   = 80
055000030806     c                   eval      QLGLKE   = 16
055100030806     c                   eval      QLGLSS   = 290
055200030806      * Initialize Sort I/O API fields.
055300030806     c                   eval      QLGRL00  = QLGRL
055400030806     c                   eval      QLGRC00  = 1
055500030806     c                   clear                   QUSEI
055600030806     c                   eval      QUSBPRV  = %size(QUSEC)
055700030806      *
055800030806      * First step - Initialize the sort routine.
055900030806     c                   call      'QLGSORT'
056000030806     c                   parm                    QLGSCB
056100030806     c                   parm                    NotUsed
056200030806     c                   parm                    NotUsed
056300030806     c                   parm                    SizeList
056400030806     c                   parm                    ReturnSize
056500030806     c                   parm                    QUSEC
056600030806      *
056700030806      * Next step - Write records to I/O routine.
056800030806     c                   eval      QLGRT00  = Put
056900030806do  1c                   DO        RrnLast       xx
057000030806     c     xx            chain     tb301s1
057100030806      * Solo le righe con Selected = 'Y' sono riordinate,
057200030806      * quindi per fare un ordinamento di tutte le righe
057300030806      * metto 'Y' sempre.
057400030806     c                   eval      Selected = 'Y'
057500030806     c                   clear                   QUSEI
057600030806     c                   eval      QUSBPRV  = %size(QUSEC)
057700030806     c                   call      'QLGSRTIO'
057800030806     c                   parm                    QLGSCB00
057900030806     c                   parm                    SflRcd
058000030806     c                   parm                    NotUsed
058100030806     c                   parm                    SizeList
058200030806     c                   parm                    NotUsed
058300030806     c                   parm                    QUSEC
058400030806e   1c                   ENDDO
058500030806      *
058600030806      * Next step - Signal end of input, clear subfile for reload.
058700030806     c                   eval      QLGRT00  = EndPut
058800030806     c                   clear                   QUSEI
058900030806     c                   eval      QUSBPRV  = %size(QUSEC)
059000030806     c                   call      'QLGSRTIO'
059100030806     c                   parm                    QLGSCB00
059200030806     c                   parm                    SflRcd
059300030806     c                   parm                    NotUsed
059400030806     c                   parm                    SizeList
059500030806     c                   parm                    NotUsed
059600030806     c                   parm                    QUSEC
059700030806     c                   eval      *in40 = *on
059800030806     c                   write     tb301c1
059900030806     c                   eval      *in40 = *off
060000030806      *
060100030806      * Final step - Write the records back to the subfile.
060200030806     c                   eval      QLGRT00 = Get
060300030806do  1c                   DO        RrnLast       xx
060400030806     c                   clear                   QUSEI
060500030806     c                   eval      QUSBPRV = %size(QUSEC)
060600030806     c                   call      'QLGSRTIO'
060700030806     c                   parm                    QLGSCB00
060800030806     c                   parm                    NotUsed
060900030806     c                   parm                    SflRcd
061000030806     c                   parm                    QLGRL00
061100030806     c                   parm                    NotUsed
061200030806     c                   parm                    QUSEC
061300030806     c                   z-add     xx            Nrr
061400030806     c                   write     tb301s1
061500030806e   1c                   ENDDO
061600030806      *
061700030806     c                   EndSr
061800030806      *
061900030806      *---------------------------------------------------------------*
062000030806      * Ctr01                                                         *
062100030806      *---------------------------------------------------------------*
062200030806     c     Ctr01         BegSr
062300030806      *
062400030806     c                   eval      *in41 = *off
062500030806      *
062600030806do  1c                   DO        *hival        xx
062700030806      *
062800030806     c     xx            chain     tb301s1                            01
062900030806if  2c                   if        NOT %found(tntb301d)
063000030806     c                   leave
063100030806e   2c                   endif
063200030806      *
063300030806if  2c                   if        VS1sce = '1'
063400030806     c                   move      VS1cd1        DSKcd1
063500030806     c                   move      VS1cd2        DSKcd2
063600030806     c                   movel(p)  DSkeyOSR      B30ke1
063700030806     c                   clear                   B30ke2
063800030806     c                   leave
063900030806e   2c                   endif
064000090623
064100090623      * se copia emetto window con richiesta nuova chiave
064200090623if  2c                   if        VS1sce = '3'
064300090623     c                   exsr      sr_w01
064400090623     c                   if        *inkf
064500090623     c                   move      w01poen       DSKcd1
064600090623     c                   move      w01nsrn       DSKcd2
064700090623     c                   movel(p)  DSkeyOSR      B30ke1
064800090623     c                   clear                   B30ke2
064900090623     c                   leave
065000090623     c                   endif
065100090623e   2c                   endif
065200030806      *
065300030806      * Controllo se effettuata ricerca
065400030806if  1c                   if           (VC1cd1 <> *zeros
065500030806     c                             and VC1cd1 <> SAVcd1)
065600030806     c                             or (VC1cd2 <> *zeros
065700030806     c                             and VC1cd2 <> SAVcd2)
065800030806     c                             or (VC1cli <> *zeros
065900030806     c                             and VC1cli <> SAVcli)
066000030806     c                   eval      *in41 = *on
066100030806if  2c                   if           (WrkSort = *blanks
066200030806     c                             and VC1cd1 <= VS1cd1
066300030806     c                             and VC1cd2 <= VS1cd2)
066400030806     c                             or (WrkSort = Ord_CodCli
066500030806     c                             and VC1cli <= VS1cli)
066600030806     c                   eval      *in41 = *off
066700030806     c                   z-add     xx            RcdNbr
066800030806sel 3c                   select
066900030806w   3c                   when      WrkSort = *blanks
067000030806     c                   move      VC1cd1        SAVcd1
067100030806     c                   move      VC1cd2        SAVcd2
067200030806w   3c                   when      WrkSort = Ord_CodCli
067300030806     c                   movel     VC1cli        SAVcli
067400030806e   3c                   endsl
067500030806     c                   leave
067600030806     c                   endif
067700030806     c                   endif
067800030806      *
067900030806e   1c                   ENDDO
068000030806      *
068100030806if  1c                   if        not *in41
068200030806     c                   clear                   VC1cd1
068300030806     c                   clear                   VC1cd2
068400030806     c                   clear                   VC1cli
068500030806e   1c                   endif
068600030806      *
068700030806     c                   EndSr
068800030806      *
068900030806      *---------------------------------------------------------------*
069000030806      * DecodCBO                                                      *
069100030806      *---------------------------------------------------------------*
069200030806     c     DecodCBO      BegSr
069300030806      *
069400030807     c                   clear                   ds3A
069500030806if  1c                   if        d§OSRcbo <> *blanks
069600030807     c                   eval      TBLcod = '3A'
069700030806     c                   movel(p)  d§OSRcbo      TBLkey
069800030806     c     K03TAB00      chain     TABEL
069900030806if  2c                   if        %found(TABEL00F)
070000030806     c                             and TBLflg = *blanks
070100030807     c                   movel     TBLuni        ds3A
070200030806e   2c                   endif
070300030806e   1c                   endif
070400030806      *
070500030806     c                   EndSr
070600090623      *
070700090623      *---------------------------------------------------------------*
070800090623      * Gestione window per richiesta copia tabella                   *
070900090623      *---------------------------------------------------------------*
071000090623     c     sr_w01        begsr
071100091204      *
071200090623     c                   eval      w01poeo = vs1cd1
071300090623     c                   eval      w01nsro = vs1cd2
071400090623     c                   clear                   w01poen
071500090623     c                   clear                   w01nsrn
071600090623     c                   eval      *in60 = *off
071700090623     c                   eval      *in61 = *off
071800091204      *
071900091204     c                   DO        *hival
072000091204      *
072100090623     c                   exfmt     tb301w1
072200090623     c                   eval      *in28 = *on
072300090623     c                   clear                   w01msg
072400090623      * F12=Ritorno
072500091204     c   kl              leavesr
072600090623      * Controllo
072700090623     c                   if        w01poen = *zeros
072800090623     c                   eval      w01msg = 'Immettere la filiale'
072900090623     c                   eval      *in28 = *on
073000090623     c                   eval      *in60 = *on
073100090623     c                   iter
073200090623     c                   endif
073300090623     c     w01poen       chain     azorg01l
073400090623     c                   if        not %found(azorg01l) or orgfva <> *blanks
073500090623     c                   eval      w01msg = 'Filiale errata'
073600090623     c                   eval      *in28 = *on
073700090623     c                   eval      *in60 = *on
073800090623     c                   iter
073900090623     c                   endif
074000090623     c                   if        w01nsrn = *blanks
074100090623     c                   eval      w01msg = 'Immettere la serie'
074200090623     c                   eval      *in28 = *on
074300090623     c                   eval      *in61 = *on
074400090623     c                   iter
074500090623     c                   endif
074600090623     c                   if        w01nsrn = '99' or
074700090623     c                             %subst(w01nsrn:1:1) < '0' or
074800090623     c                             %subst(w01nsrn:2:1) < '0'
074900090623     c                   eval      w01msg = 'Serie errata'
075000090623     c                   eval      *in28 = *on
075100090623     c                   eval      *in61 = *on
075200090623     c                   iter
075300090623     c                   endif
075400090623     c                   move      w01nsro       wrknsr
075500090623     c                   if        w01poeo = w01poen and
075600090623     c                             wrknsr  = w01nsrn
075700090623     c                   eval      w01msg = 'Copia su stesso codice'
075800090623     c                   eval      *in28 = *on
075900090623     c                   eval      *in60 = *on
076000090623     c                   iter
076100090623     c                   endif
076200090623     c                   move      w01poen       DSKcd1
076300090623     c                   move      w01nsrn       DSKcd2
076400090623     c                   movel(p)  DSkeyOSR      tbeke1
076500091204     c     K02tbe01      chain     TNTBE000
076600090623     c                   if        %found(tntbe01l)
076700090623     c                   eval      w01msg = 'Codice già presente'
076800090623     c                   eval      *in28 = *on
076900090623     c                   eval      *in60 = *on
077000090623     c                   iter
077100090623     c                   endif
077200091204      * F6=Conferma
077300091204     c                   if        *inKF
077400091204     c                   exsr      sr_CopyOSR
077500090623     c                   leave
077600091204     c                   endif
077700091204      *
077800090623     c                   enddo
077900091204      *
078000090623     c                   endsr
078100091204
078200091204       //--------------------------------------------------------------
078300091204       //?Copia del record-tabella.                                    ?
078400091204       //--------------------------------------------------------------
078500091204     c     sr_CopyOSR    BEGSR
078600091204      *
078700091204     c                   move      w01poeo       DSKcd1
078800091204     c                   move      w01nsro       DSKcd2
078900091204     c                   movel(p)  DSkeyOSR      tbeke1
079000091204     c     K02tbe01      chain     TNTBE000
079100091204      *
079200091204     c                   if        %found(tntbe01l)
079300091204     c                   eval      dosr = tbeuni
079400091204     c                   eval      savtbesif = tbesif
079500091204     c                   eval      savtbeapl = tbeapl
079600091204     c                   eval      savtbeftt = tbeftt
079700091204     c                   endif
079800091204      *
079900091204      /free
080000091204
080100091204         // -?Ciclo di elaborazione per ogni sistema informativo?
080200091204         For  w_SisInf = 1  To  %elem($Libr);
080300091204
080400091204           // -?Apertura (eventuale) del file ed aggancio record?
080500091204           if  w_SisInf > 1;
080600091204             exsr  sr_OpenFileTab;
080700091204           endif;
080800091204
080900091204      /end-free
081000091204      *
081100091204      *
081200091204     c                   move      w01poen       DSKcd1
081300091204     c                   move      w01nsrn       DSKcd2
081400091204     c                   movel(p)  DSkeyOSR      TBEke1
081500091204     c     K02tbe01      chain     TNTBE000
081600091204      *
081700091204     c                   if        not %found(tntbe01l)
081800091204     c                   clear                   tntbe000
081900091204     c                   movel     kcod          tbecod
082000091204     c                   movel(p)  DSkeyOSR      tbeke1
082100091204     c                   eval      tbeuni = dosr
082200091204     c                   eval      tbesif = savtbesif
082300091204     c                   eval      tbeapl = savtbeapl
082400091204     c                   eval      tbeftt = savtbeftt
082500091204      /free
082600091204           // - Aggiornamento o Scrittura in sede
082700091204           if  w_SisInf = 1;
082800091204             TBEftr = 'T';
082900091204           // - Aggiornamento o Scrittura in filiale
083000091204           else;
083100091204             TBEftr = 'R';
083200091204           endif;
083300091204           TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
083400091204                                   : 1 : 8 ) );
083500091204      /end-free
083600091204     c                   write     tntbe000
083700091204     c                   endif
083800091204      /free
083900091204
084000091204         EndFor;
084100091204
084200091204      /end-free
084300091204      *
084400091204     c                   ENDSR
084500091204
084600091204       //--------------------------------------------------------------
084700091204       //?Schiere a tempo di compilazione.                             ?
084800091204       //--------------------------------------------------------------
084900091204
085000091204** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
085100091204SETRAS  GAITRAGRU FILTRAGRU
085200091204AS888   GAITRAGRPSFILTRAGRPF
