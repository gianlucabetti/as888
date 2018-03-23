000100100922      //--------------------------------------------------------------
000200110126      //?TISTF1R - totalefatturato "ALTRI CODICI" CLIENTI di  ROMA
000300110120      //           POST fatturazione prima della statistiche
000400100922      //--------------------------------------------------------------
000500100922
000600100922     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700100922     h dftactgrp(*no) actgrp(*caller)
000800100922
000900100922      //---------------------------------------------------------------
001000100922      //?Dichiarazione file.
001100100922      //---------------------------------------------------------------
001200110125     ftabel00f  if   e           k disk
001300110125     fcnclp00f  if   e           k disk
001400110125     fTNTBE01L  if   e           k disk
001500110125     FTIVGD00F  o    E             DISK
001600100922
001700100922      //---------------------------------------------------------------
001800100922      //?Definizione costanti.
001900100922      //---------------------------------------------------------------
002000100922      //---------------------------------------------------------------
002100100922      //?Definizione schiere.
002200100922      //---------------------------------------------------------------
002300110120      * codici clienti da elaborare
002400110120     D KSC             s              7  0 dim(100)
002500110120     D dKSC            s             35    dim(100)
002600110125      * codici bolla da scartare
002700110125     D tbs             s              2    dim(50)
002800100922      //---------------------------------------------------------------
002900100922      //?Definizione aree dati.
003000100922      //---------------------------------------------------------------
003100100922
003200100922      // - Dati utente
003300100922     d §AzUte        e ds                  extname(AZUTE00F)
003400100922     d                                     dtaara
003500100922     d §DatiUte      e ds                  extname(dDatiUte)
003600100922     d                                     dtaara
003700100922
003800100922     d titasDS       e ds                  ExtName(titas10f)
003900100922
004000100922      //---------------------------------------------------------------
004100100922      //?Definizione strutture dati.
004200100922      //---------------------------------------------------------------
004300100922
004400100922      // - Parametri ricevuti
004500100922     d KPJBA         e ds
004600100922
004700100922      // - Reperimento dati utente
004800100922     d TIBS34DS      e ds
004900100922
005000110125     d dSFR          e ds
005100110126     d dSFR_ALTR     e ds
005200110125     D DVGDFLO       E DS
005300110125     D DSTB          E DS
005400100922
005500100922
005600100922      //---------------------------------------------------------------
005700100922      //?Definizione variabili globali.
005800100922      //---------------------------------------------------------------
005900100922
006000100922      // - Flags booleani
006100100922     d $End            s               n   inz(*off)
006200100922     d $Endtab         s               n   inz(*off)
006300100922     d $RcdOk          s               n   inz(*off)
006400100922
006500100922      // - Indici di schiera
006600100922     d xx              s              4  0 inz
006700100922     d yy              s              4  0 inz
006800100922
006900100922       // - Stringa SQL da eseguire
007000100922     d wSQL            s           2048    Varying        inz
007100100922
007200100922      // - Campi di comodo data
007300110125     d Dataeur         s               d   datfmt(*eur)
007400110125     d Dataiso         s               d   datfmt(*iso)
007500110125     d Datasys         s               d   datfmt(*iso) inz(*sys)
007600100922
007700100922      // - Parametri di lancio
007800100922     D PARAM           DS
007900110125     D  PARdiniz               1      8  0
008000110125     D  PARdfine               9     16  0
008100110125     D  PARtela               17     17
008200100922
008300100922      // - Campi di comodo
008400110125     d datcor          s              8  0
008500110125     d totspe          s             11  0
008600110120     d totimv          s             13  3
008700100922
008800100922      //---------------------------------------------------------------
008900100922      //?Definizione procedure esterne.
009000100922      //---------------------------------------------------------------
009100100922
009200100922
009300100922      //---------------------------------------------------------------
009400100922      //?prototipi
009500100922      //---------------------------------------------------------------
009600100922
009700100922      /copy gaitrasrc/srcprotopr,tibs34r
009800100922
009900100922      //---------------------------------------------------------------
010000100922      //?Definizione key-list.
010100100922      //---------------------------------------------------------------
010200100922
010300100922      //---------------------------------------------------------------
010400100922      //?Riepilogo indicatori.
010500100922      //---------------------------------------------------------------
010600100922
010700100922
010800100922      //---------------------------------------------------------------
010900100922
011000100922      //---------------------------------------------------------------
011100100922      //?M A I N - L I N E
011200100922      //---------------------------------------------------------------
011300100922
011400100922     c     *Entry        plist
011500100922     c                   parm                    KPJBA
011600100922
011700100922      /free
011800100922
011900100922       //?Operazioni iniziali
012000100922       exsr RoutInz;
012100100922
012200100922       //?Elaboro le bolle tassate
012300100922       exsr sr_spedizioni ;
012400100922
012500100922
012600100922       //?Operazioni finali
012700100922       exsr RoutEnd;
012800100922
012900100922       //--------------------------------------------------------------
013000100922       //?Operazioni iniziali.
013100100922       //--------------------------------------------------------------
013200100922       BEGSR RoutInz;
013300100922
013400100922         //?Imposto la ds con i dati della KPJBU
013500100922         Param    = kpjbu;
013600100922
013700100922         //?Reperimento dati job
013800100922         exsr DatiJob;
013900100922
014000100922         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014100100922
014200100922         //?Imposto data e ora elaborazione
014300110125         dataiso=datasys  ;
014400110125         datcor=%dec(dataiso)  ;
014500100922
014600110120         clear ksc ;
014700110120         clear dksc;
014800100922
014900110120         //?Carico in sk i codici clienti da elaborare
015000100922         clear xx;
015100100922         $EndTab = *off;
015200100922
015300100922         exec sql
015400110125          declare sfr cursor for
015500110120          select tbeke2 , tbeuni
015600110125          from tntbe00f where  tbecod = 'SFR' and
015700110125          tbeke2<>'              ' and tbeatb = ' ' and tbeke1='ALTRICODICI'
015800110125           order by tbeke2;
015900100922
016000100922         exec sql
016100110125           open SFR;
016200100922           IF sqlcode < 0;
016300100922             $Endtab = *on;
016400110125           ENDIF;
016500100922
016600100922         DOW not $Endtab;
016700100922           exec sql
016800110125             fetch next from SFR into :tbeke2, :tbeuni;
016900100922             IF sqlcod = 100 or sqlcod < 0;
017000100922               $EndTab = *on;
017100100922               leave;
017200100922             ENDIF;
017300110120             dSFR = Tbeuni;
017400100922               xx += 1;
017500110120               ksc(xx) = §sfrksc           ;
017600110120               dksc(xx) = §sfrdksc           ;
017700100922         ENDDO;
017800100922
017900110125         exec sql close SFR;
018000110125
018100110125         clear tbs ;
018200110125
018300110125         //?Carico in sk le bolle da scartare
018400110125         clear xx;
018500110125         $EndTab = *off;
018600110125
018700110125         exec sql
018800110125          declare tbs cursor for
018900110125          select tblkey , tbluni
019000110125          from tabel00f where tblkut = '1' and tblcod = 'TB' and
019100110125          tblflg = ' ' order by tblkut, tblcod, tblkey;
019200110125
019300110125         exec sql
019400110125           open TBS;
019500110125           IF sqlcode < 0;
019600110125             $Endtab = *on;
019700110125           ENDIF;
019800110125
019900110125         DOW not $Endtab;
020000110125           exec sql
020100110125             fetch next from TBS into :tblkey, :tbluni;
020200110125             IF sqlcod = 100 or sqlcod < 0;
020300110125               $EndTab = *on;
020400110125               leave;
020500110125             ENDIF;
020600110125             dstb = Tbluni;
020700110125             if §tbfcb <> '1'  ;
020800110125               xx += 1;
020900110125               tbs(xx) = %subst(tblkey:1:2);
021000110125             ENDIF;
021100110125         ENDDO;
021200110125
021300110125         exec sql close TBS;
021400110125
021500100922
021600100922       ENDSR;
021700100922
021800100922       //--------------------------------------------------------------
021900100922       //?Leggo le spedizioni fatturate del periodo richieste
022000100922       //--------------------------------------------------------------
022100100922       BEGSR sr_Spedizioni ;
022200100922
022300100922         $End = *off;
022400100922
022500100922         //?Imposto la stringa per SQL
022600100922         exsr  sr_PrepSQL;
022700100922
022800100922         //?Dichiarazione cursore
022900100922         exec sql
023000100922           prepare S1   from :wSQL;
023100100922         exec sql
023200100922           declare c1   cursor for S1;
023300100922
023400100922         //?Apertura del cursore
023500100922         exec sql
023600100922           open c1 ;
023700100922
023800100922         IF sqlcode < 0;
023900100922           $End = *on;
024000100922         ENDIF;
024100100922
024200100922         DOW not $End;
024300100922           exec sql
024400110125             fetch next from C1  into :tasksc, :totimv, :totspe ;
024500100922           IF sqlcod = 100 or sqlcod < 0;
024600100922             $End = *on;
024700100922             leave;
024800100922           ENDIF;
024900100922
025000100922
025100110120           exsr Scrivifile  ;
025200100922         ENDDO;
025300100922
025400100922         exec sql
025500100922           close c1 ;
025600100922
025700100922       ENDSR;
025800100922
025900100922       //--------------------------------------------------------------
026000100922       //?Scrittura record statistica
026100100922       //--------------------------------------------------------------
026200110120       BEGSR Scrivifile    ;
026300100922
026400110125       //  Preparo la scrittura del file TIVGD
026500110125       clear                   tivgd000    ;
026600110202       vgdtip='&1'    ;
026700110131       vgdksu='0BART001' ;
026800110131       vgdtsc='WW'      ;
026900110125
027000110125       vgddaT=datcor    ;
027100110125       vgdpgm='TISTF1R ';
027200110125       clear                   DVGDFLO     ;
027300110125
027400110125        // Imposto i dati in DS
027500110126        clear                   DSFR_ALTR ;
027600100922
027700110125            §sfr_ksc = tasksc ;
027800110125            xx=1  ;
027900110125            xx=%lookup(tasksc:ksc) ;
028000110125            §sfr_dksc=dksc(xx)  ;
028100110125            §sfr_imv = totimv ;
028200110125            §sfr_spf = totspe ;
028300110125
028400110125            // Data Fattura inizio o fine mese
028500110125
028600110125            chain (1:dutkci:tasksc)  cnclp00f  ;
028700110125            if %found(cnclp00f) and clpfun='F'  ;
028800110125             §sfr_dft=pardfine  ;
028900110125            else  ;
029000110125             §sfr_dft=pardiniz  ;
029100110125            endif ;
029200100922
029300110126            VGDDTA =%subst(Dsfr_ALTR:1:%size(DSFR_ALTR))      ;
029400100922
029500110125         write tivgd000 ;
029600100922
029700100922       ENDSR ;
029800100922       //--------------------------------------------------------------
029900100922       //?Reperimento Dati del job (Utente/Operativi).
030000100922       //--------------------------------------------------------------
030100100922       BEGSR DatiJob;
030200100922
030300100922         in(E) §AzUte;
030400100922         if NOT %error;
030500100922           in(E) §DatiUte;
030600100922         endif;
030700100922         if %error or RSut = *blanks;
030800100922           clear TIBS34ds;
030900100922           tibs34r(tibs34ds);
031000100922           in §AzUte;
031100100922           in §DatiUte;
031200100922         endif;
031300100922
031400100922       ENDSR;
031500100922
031600100922       //--------------------------------------------------------------
031700100922       //?Preparazione stringa SQL.
031800100922       //--------------------------------------------------------------
031900100922       BEGSR sr_PrepSQL;
032000100922
032100110125         wSQL = 'select tasksc, sum(tasimv), count(*)  from TITAS00F +
032200110120                where  TASDFT>0 AND TASKSC IN ('''  ;
032300110120
032400100922         yy = 0;
032500100922         xx = 1;
032600110120         FOR xx by 1 to %elem(ksc);
032700110120           IF ksc(xx) > 0     ;
032800100922             IF yy > 0;
032900100923               wSQL += ''', ''';
033000100922             ELSE;
033100100922               yy = 1;
033200100922             ENDIF;
033300110120             wSQL += %editc(ksc(xx):'X');
033400100922           ENDIF;
033500100922         ENDFOR;
033600100922
033700110125       // solo bolle buone
033800110125           wSQL += ''') and tastbl not in ( ''';
033900110125         yy = 0;
034000110125         xx = 1;
034100110125         FOR xx by 1 to %elem(tbs);
034200110125           IF Tbs(xx) <> *blanks;
034300110125             IF yy > 0;
034400110125               wSQL += ''', ''';
034500110125             ELSE;
034600110125               yy = 1;
034700110125             ENDIF;
034800110125             wSQL += Tbs(xx);
034900110125           ENDIF;
035000110125         ENDFOR;
035100110125
035200110125           wSQL += ''')  group by tasksc order by TASKSC for fetch only ' ;
035300100922
035400100922       ENDSR;
035500100922
035600100922       //--------------------------------------------------------------
035700100922       //?Operazioni finali.
035800100922       //--------------------------------------------------------------
035900100922       BEGSR RoutEnd;
036000100922
036100100922         *inLR = *on;
036200100922         return;
036300100922
036400100922       ENDSR;
036500100922
036600100922      /end-free
