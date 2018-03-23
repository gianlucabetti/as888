000100100922      //--------------------------------------------------------------
000200110330      //?TNSB32R - controllo bolle non trasferite da TITASP
000300100922      //--------------------------------------------------------------
000400100922
000500110330     h decedit('0,')  option(*nodebugio)
000600100922     h dftactgrp(*no) actgrp(*caller)
000700100922
000800100922      //---------------------------------------------------------------
000900100922      //?Dichiarazione file.
001000100922      //---------------------------------------------------------------
001100110125     ftabel00f  if   e           k disk
001200110218     ffiar531c  if   e           k disk
001300110125     fTNTBE01L  if   e           k disk
001400110218     fazorg01l  if   e           k disk
001500110330     ftncsb03l  if   e           k disk
001600110330     ftigcp51l  if   e           k disk
001700110331     Ffnarb01L  IF   E           K DISK    usropn extfile(WFNARB)
001800110331     Ffnlbl02l  IF   E           K DISK
001900110330     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002000110330     F                                     FORMLEN(66)
002100110330     F                                     FORMOFL(63)
002200100922
002300100922      //---------------------------------------------------------------
002400100922      //?Definizione costanti.
002500100922      //---------------------------------------------------------------
002600100922      //---------------------------------------------------------------
002700100922      //?Definizione schiere.
002800100922      //---------------------------------------------------------------
002900110330     D TES             S             66    DIM(5) CTDATA PERRCD(1)
003000100922      //---------------------------------------------------------------
003100100922      //?Definizione aree dati.
003200100922      //---------------------------------------------------------------
003300100922
003400100922      // - Dati utente
003500100922     d §AzUte        e ds                  extname(AZUTE00F)
003600100922     d                                     dtaara
003700100922     d §DatiUte      e ds                  extname(dDatiUte)
003800100922     d                                     dtaara
003900100922
004000100922     d titasDS       e ds                  ExtName(titas10f)
004100100922
004200100922      //---------------------------------------------------------------
004300100922      //?Definizione strutture dati.
004400100922      //---------------------------------------------------------------
004500100922
004600100922      // - Parametri ricevuti
004700100922     d KPJBA         e ds
004800100922
004900100922      // - Reperimento dati utente
005000100922     d TIBS34DS      e ds
005100100922
005200110330     D FIDN12DS      E DS
005300110411     d  skKey                 26    305    dim(20)
005400110411     d  skAnn                306    325    dim(20)
005500110411     d  skDca                326    485  0 dim(20)
005600110411     d  skDch                546    705  0 dim(20)
005700110411     d  skCch                706    745    dim(20)
005800110411      *
005900110411     d dsKey           ds
006000110411     d  dsaac                         4  0
006100110411     d  dsfil                         3  0
006200110411     d  dsnca                         7  0
006300110411      *
006400100922      //---------------------------------------------------------------
006500100922      //?Definizione variabili globali.
006600100922      //---------------------------------------------------------------
006700100922
006800100922      // - Flags booleani
006900100922     d $End            s               n   inz(*off)
007000100922     d $Endtab         s               n   inz(*off)
007100100922     d $RcdOk          s               n   inz(*off)
007200100922
007300100922      // - Indici di schiera
007400100922     d xx              s              4  0 inz
007500100922     d yy              s              4  0 inz
007600100922
007700100922       // - Stringa SQL da eseguire
007800100922     d wSQL            s           2048    Varying        inz
007900100922
008000100922      // - Campi di comodo data
008100110125     d Dataeur         s               d   datfmt(*eur)
008200110125     d Dataiso         s               d   datfmt(*iso)
008300110125     d Datasys         s               d   datfmt(*iso) inz(*sys)
008400110330     d Orasys          s               t   timfmt(*iso) inz(*sys)
008500100922
008600100922      // - Parametri di lancio
008700110330     D tnsb32DS        DS
008800110331     D  PARdti                 1      8  0
008900110331     D  PARdtf                 9     16  0
009000110408     D  PARescCA              17     17
009100110411     D  PARanCA               18     21  0
009200100922
009300100922      // - Campi di comodo
009400110125     d datcor          s              8  0
009500110330     d Oracor          s              6  0
009600110330     d kcpfrg          s                   like(gcpfrg)  inz(0)
009700110330     d p1cdti          s              8  0               inz
009800110330     d p1cdtf          s              8  0               inz
009900110330     d p1cdcm          s              8  0               inz
010000110330     d p1cerr          s             80                  inz
010100110330     d Errore          s              3                  inz
010200110411     d stdca           s              8  0               inz
010300110411     d stdch           s              8  0               inz
010400110331     d WFNARB          s             21    inz('FILTRA201/FNARB01L')
010500100922      //---------------------------------------------------------------
010600100922      //?Definizione procedure esterne.
010700100922      //---------------------------------------------------------------
010800100922
010900100922
011000100922      //---------------------------------------------------------------
011100100922      //?prototipi
011200100922      //---------------------------------------------------------------
011300100922
011400100922      /copy gaitrasrc/srcprotopr,tibs34r
011500110330      /copy gaitrasrc/srcprotopr,fidn12r
011600100922
011700100922      //---------------------------------------------------------------
011800100922      //?Definizione key-list.
011900100922      //---------------------------------------------------------------
012000100922
012100100922      //---------------------------------------------------------------
012200100922      //?Riepilogo indicatori.
012300100922      //---------------------------------------------------------------
012400100922
012500100922
012600100922      //---------------------------------------------------------------
012700100922
012800100922      //---------------------------------------------------------------
012900100922      //?M A I N - L I N E
013000100922      //---------------------------------------------------------------
013100100922
013200100922     c     *Entry        plist
013300100922     c                   parm                    KPJBA
013400100922
013500100922      /free
013600100922
013700100922       //?Operazioni iniziali
013800100922       exsr RoutInz;
013900100922
014000100922       //?Elaboro le bolle tassate
014100100922       exsr sr_spedizioni ;
014200100922
014300100922
014400100922       //?Operazioni finali
014500100922       exsr RoutEnd;
014600100922
014700100922       //--------------------------------------------------------------
014800100922       //?Operazioni iniziali.
014900100922       //--------------------------------------------------------------
015000100922       BEGSR RoutInz;
015100100922
015200100922         //?Imposto la ds con i dati della KPJBU
015300110330         tnsb32ds = kpjbu;
015400100922
015500100922         //?Reperimento dati job
015600100922         exsr DatiJob;
015700100922
015800100922         //?Imposto data e ora elaborazione
015900110125         dataiso=datasys  ;
016000110330         dataeur=dataiso  ;
016100110330         datcor=%dec(dataeur) ;
016200110330
016300110330         oracor=%dec(orasys)   ;
016400110330         //?giro le date passate
016500110330         dataiso=%date(pardti)  ;
016600110330         dataeur=dataiso  ;
016700110330         p1cdti=%dec(dataeur)  ;
016800110330
016900110330         dataiso=%date(pardtf) ;
017000110330         dataeur=dataiso ;
017100110330         p1cdtf=%dec(dataeur)  ;
017200100922
017300110330         *inof=*on   ;
017400110331         OPEN(e)   fnarb01l                  ;
017500110331
017600110331        if        not %open(fnarb01l)        ;
017700110331         %subst(WFNARB:7:3)='PRD'           ;
017800110331         OPEN      fnarb01l                  ;
017900110331        endif                                ;
018000110330
018100100922       ENDSR;
018200100922
018300100922       //--------------------------------------------------------------
018400100922       //?Leggo le spedizioni fatturate del periodo richieste
018500100922       //--------------------------------------------------------------
018600100922       BEGSR sr_Spedizioni ;
018700100922
018800100922
018900100922         //?Imposto la stringa per SQL
019000100922         exsr  sr_PrepSQL;
019100100922
019200110221         exsr elaSQL  ;
019300110221
019400110221
019500110221       ENDSR;
019600110221       //--------------------------------------------------------------
019700110221       //?SQL da richiamare due volte: per TITAS10f e TITAS00F
019800110221       //--------------------------------------------------------------
019900110221         BEGSR  elaSQL  ;
020000110221         $End = *off;
020100110221
020200100922         //?Dichiarazione cursore
020300100922         exec sql
020400100922           prepare S1   from :wSQL;
020500100922         exec sql
020600100922           declare c1   cursor for S1;
020700100922
020800100922         //?Apertura del cursore
020900100922         exec sql
021000100922           open c1 ;
021100100922
021200100922         IF sqlcode < 0;
021300100922           $End = *on;
021400100922         ENDIF;
021500100922
021600100922         DOW not $End;
021700100922           exec sql
021800110218             fetch next from C1  into :titasds ;
021900110218
022000100922           IF sqlcod = 100 or sqlcod < 0;
022100100922             $End = *on;
022200100922             leave;
022300100922           ENDIF;
022400100922
022500100922
022600110218           exsr elaBolla    ;
022700100922         ENDDO;
022800110218
022900100922
023000100922         exec sql
023100100922           close c1 ;
023200100922
023300100922       ENDSR;
023400100922
023500100922       //--------------------------------------------------------------
023600110218       //?elaboro bolla di TITAS
023700100922       //--------------------------------------------------------------
023800110218       BEGSR ElaBolla      ;
023900110330
024000110330       clear Errore  ;
024100110411       clear stdca  ;
024200110411       clear stdch  ;
024300110411       clear dskey   ;
024400110330
024500110330       // Controllo se ha C.A.
024600110330       clear fidn12ds  ;
024700110330       i12aas = TASAAS  ;
024800110330       i12lnp = TASLNP  ;
024900110330       i12nrs = TASNRS  ;
025000110330       i12nsp = TASNSP  ;
025100110330       i12fel = 'E'     ;
025200110330       callp FIDN12R (fidn12ds)  ;
025300110330
025400110330       // Se trovata C.A. --> stampo errore
0255001103311      if o12nca>0  ;
025600110411          dskey = skkey(1)   ;
025700110411
025800110411             dataiso=%date(skdca(1)) ;
025900110411             dataeur=dataiso ;
026000110411             stdca=  %dec(dataeur)    ;
026100110411
026200110411             dataiso=%date(skdch(1)) ;
026300110411             dataeur=dataiso ;
026400110411             stdch=  %dec(dataeur)    ;
026500110411
026600110330         Errore='CA '   ;
026700110330         exsr stampa  ;
026800110330       leavesr   ;
0269001103311      endif   ;
027000110330
027100110330       // se ha contrassegno veriifico se non incassato
0272001103311      if tasfca='S'   ;
027300110330
027400110330       chain (tasaas:taslnp:tasnrs:tasnsp)   tncsb03l  ;
027500110330
0276001103312        if %found(tncsb03l) and csbsta<>9 and csbbna=0  ;
027700110330           Errore='CAS'  ;
027800110330           exsr stampa  ;
027900110330         leavesr   ;
0280001103312        endif   ;
0281001103311      endif   ;
028200110330
028300110330       // Se ha giacenza non chiusa
0284001103311      if tasfgc='S'    ;
028500110330       chain (tasaas:taslnp:tasnrs:tasnsp:kcpfrg)   tigcp51l  ;
028600110330
0287001103312        if %found(tigcp51l) and gcpfas<50               ;
028800110330           Errore='GIA'  ;
028900110330           exsr stampa  ;
029000110330         leavesr   ;
0291001103312        endif   ;
0292001103311      endif   ;
029300110330
029400110330       // se non consegnata controllo se si tratta di un rimpallo
0295001103311      if   tasdcm=0  ;
029600110330       chain (tasaas:taslnp:tasnrs:tasnsp)   fnarb01l  ;
029700110330
0298001103312        if %found(fnarb01l) and arbfbc='B'         ;
029900110330           Errore='RIM'  ;
030000110330           exsr stampa  ;
030100110331x2       else   ;
0302001103313          if taslna=101  ;
030300110330           Errore='101'  ;
030400110330           exsr stampa  ;
030500110331x3         else  ;
030600110330           clear lblaan  ;
030700110330           clear lbllpn  ;
030800110330           clear lblnrn  ;
030900110330           clear lblnsn  ;
031000110330           chain (tasaas:taslnp:tasnrs:tasnsp)   fnlbl02l  ;
0311001103314          dow       %found(fnlbl02l) and lbllan<>101  ;
031200110330             chain (tasaas:taslnp:tasnrs:tasnsp)   fnlbl02l  ;
0313001103314          enddo   ;
0314001103314          if        %found(fnlbl02l) and lbllan=101 ;
031500110331           Errore='LE1'  ;
031600110331           exsr stampa  ;
031700110331x4         else  ;
031800110331           chain (lblaan:lbllpn:lblnrn:lblnsn)   fnarb01l  ;
031900110331 5         if        %found(fnarb01l) and arbfbc='B'     ;
032000110331            Errore='LER'  ;
032100110331            exsr stampa  ;
032200110331 5         endif  ;
032300110331 4         endif  ;
032400110331 3         endif  ;
032500110331 2         endif  ;
032600110331 1         endif  ;
032700110330
032800110330         exsr stampa  ;
032900110218
033000110330
033100100922       ENDSR ;
033200110218       //--------------------------------------------------------------
033300110330       //?Stampa degli errori
033400110218       //--------------------------------------------------------------
033500110330       BEGSR Stampa        ;
033600110330
033700110330       clear  p1cerr ;
033800110330
033900110330       if *inof   ;
034000110330       except testa  ;
034100110330       *inof=*off   ;
034200110330       endif  ;
034300110330
034400110330       if tasdcm> 0   ;
034500110330         dataiso=%date(tasdcm) ;
034600110330         dataeur=dataiso  ;
034700110330         p1cdcm=%dec(dataeur)  ;
034800110330       else  ;
034900110330       clear   p1cdcm  ;
035000110330       endif   ;
035100110330
035200110330       select   ;
035300110330       when Errore='CA '   ;
035400110411       p1cerr='Spedizione con C.A. del '+%editw(stdca:'  /  /    ') +
035500110411              ' chiusa il '+%editw(stdch:'  /  /    ')  ;
035600110330
035700110330       when Errore='CAS'  ;
035800110330       p1cerr='C/Asssegno non incassato'    ;
035900110330
036000110330       when Errore='GIA'  ;
036100110330       p1cerr='Presente giacenza ancora aperta (fase < 50)'  ;
036200110330
036300110330       when Errore='RIM'  ;
036400110330       p1cerr='Rimpallo';
036500110330
036600110330       when Errore='101'  ;
036700110330       p1cerr='Bolla con linea di arrivo 101'  ;
036800110330
036900110330       when Errore='LE1'  ;
037000110330       p1cerr='Bolla legata a figlia con linea di arrivo 101'  ;
037100110331
037200110331       when Errore='LER'  ;
037300110331       p1cerr='Bolla legata a figlia con blocco cons."B-Rimpallo"'  ;
037400110330       endsl  ;
037500110330
037600110331       // Le C.A. le stampo solo se richieste
037700110411       if Errore<>'CA '   or   parescCA<>'S'
037800110411          or (parescCA='S' and dsaac<paranca)   ;
037900110331         except    Detta   ;
038000110331       endif  ;
038100110330
038200110218       ENDSR ;
038300100922       //--------------------------------------------------------------
038400100922       //?Reperimento Dati del job (Utente/Operativi).
038500100922       //--------------------------------------------------------------
038600100922       BEGSR DatiJob;
038700100922
038800100922         in(E) §AzUte;
038900100922         if NOT %error;
039000100922           in(E) §DatiUte;
039100100922         endif;
039200100922         if %error or RSut = *blanks;
039300100922           clear TIBS34ds;
039400100922           tibs34r(tibs34ds);
039500100922           in §AzUte;
039600100922           in §DatiUte;
039700100922         endif;
039800100922
039900100922       ENDSR;
040000100922
040100100922       //--------------------------------------------------------------
040200100922       //?Preparazione stringa SQL.
040300100922       //--------------------------------------------------------------
040400100922       BEGSR sr_PrepSQL;
040500100922
040600110330         wSQL = 'select *  from TITASP0F WHERE +
040700110330            ((tasaas*10000)+tasmgs)>=' + %editc(PARDti:'X')+
040800110330           ' and ((tasaas*10000)+tasmgs)<=' + %editc(PARDTF:'X') +
040900110331           ' order by tasaas, taslna for fetch only ' ;
041000100922
041100100922       ENDSR;
041200100922
041300100922       //--------------------------------------------------------------
041400100922       //?Operazioni finali.
041500100922       //--------------------------------------------------------------
041600100922       BEGSR RoutEnd;
041700100922
041800100922         *inLR = *on;
041900100922         return;
042000100922
042100100922       ENDSR;
042200100922
042300100922      /end-free
042400110330     O*------------------------------------------------------------------------*
042500110330     OPRTF198   E            TESTA            02
042600110330     O                       RSUT                20
042700110330     O                                           60 '** Bolle non trasferite da'
042800110330     O                                         +  0 ' TITASP0F '
042900110330     O                                         +  2 'DAL:'
043000110330     O                       p1cDTI            +  2 '  /  /    '
043100110330     O                                         +  3 'AL:'
043200110330     O                       p1cDTF            +  2 '  /  /    '
043300110330     O                                         +  2 '** '
043400110330     O                                         + 15 'TNSB32R '
043500110330     O                                          145 'Data:'
043600110330     O                       datcor            +  1 '  /  /    '
043700110330     O                                          169 'Ora:'
043800110330     O                       oracor            +  1 '  .  .  '
043900110330     O                                          193 'Pagina:'
044000110330     O                       PAGE          Z    198
044100110330     O          E            TESTA       1  1
044200110330     O                       TES(2)              66
044300110330     O                       TES(3)             132
044400110330     O                       TES(4)             198
044500110330     O          E            TESTA          1
044600110330     O                       TES(1)              66
044700110330     O                       TES(1)             132
044800110330     O                       TES(1)             198
044900110330     O*------------------------------------
045000110330     O          E            Detta          1
045100110330     O                       TASlna            +  1
045200110330     O                       TASAAS            +  2
045300110330     O                       TASLNP            +  1
045400110330     O                       TASNRS            +  2
045500110330     O                       TASNSP        2   +  2
045600110330     O                       TASTBL            +  3
045700110330     O                       p1cdcm            +  5 '  /  /    '
045800110330     o                                         +  5 '|'
045900110330     O                       p1cerr            +  3
046000110330**  TES - RIGHE IN STAMPA
046100110330------------------------------------------------------------------  1
046200110330L.Arr Anno LNP  Ser   NumSped  TipBo  Data consegna   |   Descrizi  2
046300110330one ERRORE                                                          3
046400110330                                                                    4
046500110330 xxx  xxxx xxx  xx  X.xxx.xxx   XX     xx/xx/xxxx     |                    5
