000100100922      //--------------------------------------------------------------
000200110126      //?TISTF2R - totale colli oltre il primo cliente Infolinea di Roma
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
005100110126     d dSFR_INFO     e ds
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
008500110126     d totcol5         s             11  0
008600110126     d totcol2         s             11  0
008700110126     d savksc          s                   like(tasksc)
008800100922
008900100922      //---------------------------------------------------------------
009000100922      //?Definizione procedure esterne.
009100100922      //---------------------------------------------------------------
009200100922
009300100922
009400100922      //---------------------------------------------------------------
009500100922      //?prototipi
009600100922      //---------------------------------------------------------------
009700100922
009800100922      /copy gaitrasrc/srcprotopr,tibs34r
009900100922
010000100922      //---------------------------------------------------------------
010100100922      //?Definizione key-list.
010200100922      //---------------------------------------------------------------
010300100922
010400100922      //---------------------------------------------------------------
010500100922      //?Riepilogo indicatori.
010600100922      //---------------------------------------------------------------
010700100922
010800100922
010900100922      //---------------------------------------------------------------
011000100922
011100100922      //---------------------------------------------------------------
011200100922      //?M A I N - L I N E
011300100922      //---------------------------------------------------------------
011400100922
011500100922     c     *Entry        plist
011600100922     c                   parm                    KPJBA
011700100922
011800100922      /free
011900100922
012000100922       //?Operazioni iniziali
012100100922       exsr RoutInz;
012200100922
012300100922       //?Elaboro le bolle tassate
012400100922       exsr sr_spedizioni ;
012500100922
012600100922
012700100922       //?Operazioni finali
012800100922       exsr RoutEnd;
012900100922
013000100922       //--------------------------------------------------------------
013100100922       //?Operazioni iniziali.
013200100922       //--------------------------------------------------------------
013300100922       BEGSR RoutInz;
013400100922
013500100922         //?Imposto la ds con i dati della KPJBU
013600100922         Param    = kpjbu;
013700100922
013800100922         //?Reperimento dati job
013900100922         exsr DatiJob;
014000100922
014100100922         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014200100922
014300100922         //?Imposto data e ora elaborazione
014400110125         dataiso=datasys  ;
014500110125         datcor=%dec(dataiso)  ;
014600100922
014700110120         clear ksc ;
014800110120         clear dksc;
014900100922
015000110120         //?Carico in sk i codici clienti da elaborare
015100100922         clear xx;
015200100922         $EndTab = *off;
015300100922
015400100922         exec sql
015500110125          declare sfr cursor for
015600110120          select tbeke2 , tbeuni
015700110125          from tntbe00f where  tbecod = 'SFR' and
015800110126          tbeke2<>'              ' and tbeatb = ' ' and tbeke1='INFOLINE'
015900110125           order by tbeke2;
016000100922
016100100922         exec sql
016200110125           open SFR;
016300100922           IF sqlcode < 0;
016400100922             $Endtab = *on;
016500110125           ENDIF;
016600100922
016700100922         DOW not $Endtab;
016800100922           exec sql
016900110125             fetch next from SFR into :tbeke2, :tbeuni;
017000100922             IF sqlcod = 100 or sqlcod < 0;
017100100922               $EndTab = *on;
017200100922               leave;
017300100922             ENDIF;
017400110120             dSFR = Tbeuni;
017500100922               xx += 1;
017600110120               ksc(xx) = §sfrksc           ;
017700110120               dksc(xx) = §sfrdksc           ;
017800100922         ENDDO;
017900100922
018000110125         exec sql close SFR;
018100110125
018200110125         clear tbs ;
018300110125
018400110125         //?Carico in sk le bolle da scartare
018500110125         clear xx;
018600110125         $EndTab = *off;
018700110125
018800110125         exec sql
018900110125          declare tbs cursor for
019000110125          select tblkey , tbluni
019100110125          from tabel00f where tblkut = '1' and tblcod = 'TB' and
019200110125          tblflg = ' ' order by tblkut, tblcod, tblkey;
019300110125
019400110125         exec sql
019500110125           open TBS;
019600110125           IF sqlcode < 0;
019700110125             $Endtab = *on;
019800110125           ENDIF;
019900110125
020000110125         DOW not $Endtab;
020100110125           exec sql
020200110125             fetch next from TBS into :tblkey, :tbluni;
020300110125             IF sqlcod = 100 or sqlcod < 0;
020400110125               $EndTab = *on;
020500110125               leave;
020600110125             ENDIF;
020700110125             dstb = Tbluni;
020800110126             if (§tbrbl <> 'R' and §tbfst='1')  or %subst(tblkey:1:2)='F7' ;
020900110125               xx += 1;
021000110125               tbs(xx) = %subst(tblkey:1:2);
021100110125             ENDIF;
021200110125         ENDDO;
021300110125
021400110125         exec sql close TBS;
021500110125
021600100922
021700100922       ENDSR;
021800100922
021900100922       //--------------------------------------------------------------
022000100922       //?Leggo le spedizioni fatturate del periodo richieste
022100100922       //--------------------------------------------------------------
022200100922       BEGSR sr_Spedizioni ;
022300100922
022400100922         $End = *off;
022500110126         clear savksc   ;
022600100922
022700100922         //?Imposto la stringa per SQL
022800100922         exsr  sr_PrepSQL;
022900100922
023000100922         //?Dichiarazione cursore
023100100922         exec sql
023200100922           prepare S1   from :wSQL;
023300100922         exec sql
023400100922           declare c1   cursor for S1;
023500100922
023600100922         //?Apertura del cursore
023700100922         exec sql
023800100922           open c1 ;
023900100922
024000100922         IF sqlcode < 0;
024100100922           $End = *on;
024200100922         ENDIF;
024300100922
024400100922         DOW not $End;
024500100922           exec sql
024600110126             fetch next from C1  into :tasksc, :tasncl, :taspkc, :taspkf ;
024700100922           IF sqlcod = 100 or sqlcod < 0;
024800100922             $End = *on;
024900100922             leave;
025000100922           ENDIF;
025100100922
025200110126          if tasksc<> savksc  ;
025300110126            if savksc>0  ;
025400110126              exsr Scrivifile  ;
025500110126            endif  ;
025600110126
025700110126           savksc=tasksc  ;
025800110126
025900110126           clear  totcol2  ;
026000110126           clear  totcol5  ;
026100110126           endif  ;
026200110126
026300110126           select  ;
026400110126           when taspkf<=2 and taspkc<= 2 ;
026500110126             totcol2=totcol2+tasncl-1 ;
026600110126
026700110126           when taspkf>2 and taspkf<= 5 and taspkc<=5 ;
026800110126             totcol5=totcol5+tasncl-1 ;
026900110126
027000110126           when taspkc>2 and taspkc<= 5 and taspkf<=5 ;
027100110126             totcol5=totcol5+tasncl-1 ;
027200110126             endsl  ;
027300110126
027400100922         ENDDO;
027500100922
027600100922         exec sql
027700100922           close c1 ;
027800110126
027900110126            if savksc>0  ;
028000110126              exsr Scrivifile  ;
028100110126            endif  ;
028200100922
028300100922       ENDSR;
028400100922
028500100922       //--------------------------------------------------------------
028600100922       //?Scrittura record statistica
028700100922       //--------------------------------------------------------------
028800110120       BEGSR Scrivifile    ;
028900100922
029000110125       //  Preparo la scrittura del file TIVGD
029100110125       clear                   tivgd000    ;
029200110202       vgdtip='&2'    ;
029300110131       vgdksu='0BART001' ;
029400110131       vgdtsc='WW'      ;
029500110125
029600110125       vgddaT=datcor    ;
029700110126       vgdpgm='TISTF2R ';
029800110125       clear                   DVGDFLO     ;
029900110125
030000110125        // Imposto i dati in DS
030100110126        clear                   DSFR_INFO ;
030200100922
030300110303            §sfr_ksc = savksc ;
030400110125            xx=1  ;
030500110303            xx=%lookup(savksc:ksc) ;
030600110125            §sfr_dksc=dksc(xx)  ;
030700110126            §sfr_cole2= totcol2;
030800110126            §sfr_cole5= totcol5;
030900110125
031000110125            // Data Fattura inizio o fine mese
031100110125
031200110125            chain (1:dutkci:tasksc)  cnclp00f  ;
031300110125            if %found(cnclp00f) and clpfun='F'  ;
031400110125             §sfr_dft=pardfine  ;
031500110125            else  ;
031600110125             §sfr_dft=pardiniz  ;
031700110125            endif ;
031800100922
031900110126            VGDDTA =%subst(Dsfr_info:1:%size(DSFR_info))      ;
032000100922
032100110125         write tivgd000 ;
032200100922
032300100922       ENDSR ;
032400100922       //--------------------------------------------------------------
032500100922       //?Reperimento Dati del job (Utente/Operativi).
032600100922       //--------------------------------------------------------------
032700100922       BEGSR DatiJob;
032800100922
032900100922         in(E) §AzUte;
033000100922         if NOT %error;
033100100922           in(E) §DatiUte;
033200100922         endif;
033300100922         if %error or RSut = *blanks;
033400100922           clear TIBS34ds;
033500100922           tibs34r(tibs34ds);
033600100922           in §AzUte;
033700100922           in §DatiUte;
033800100922         endif;
033900100922
034000100922       ENDSR;
034100100922
034200100922       //--------------------------------------------------------------
034300100922       //?Preparazione stringa SQL.
034400100922       //--------------------------------------------------------------
034500100922       BEGSR sr_PrepSQL;
034600100922
034700110126         wSQL = 'select tasksc, tasncl, taspkc, taspkf  from TITAS00F +
034800110126                where  TASDFT>0 AND tasncl>1 and   TASKSC IN ('''  ;
034900110120
035000100922         yy = 0;
035100100922         xx = 1;
035200110120         FOR xx by 1 to %elem(ksc);
035300110120           IF ksc(xx) > 0     ;
035400100922             IF yy > 0;
035500100923               wSQL += ''', ''';
035600100922             ELSE;
035700100922               yy = 1;
035800100922             ENDIF;
035900110120             wSQL += %editc(ksc(xx):'X');
036000100922           ENDIF;
036100100922         ENDFOR;
036200100922
036300110125       // solo bolle buone
036400110126           wSQL += ''') and tastbl in ( ''';
036500110125         yy = 0;
036600110125         xx = 1;
036700110125         FOR xx by 1 to %elem(tbs);
036800110125           IF Tbs(xx) <> *blanks;
036900110125             IF yy > 0;
037000110125               wSQL += ''', ''';
037100110125             ELSE;
037200110125               yy = 1;
037300110125             ENDIF;
037400110125             wSQL += Tbs(xx);
037500110125           ENDIF;
037600110125         ENDFOR;
037700110125
037800110126           wSQL += ''')  order by TASKSC for fetch only ' ;
037900100922
038000100922       ENDSR;
038100100922
038200100922       //--------------------------------------------------------------
038300100922       //?Operazioni finali.
038400100922       //--------------------------------------------------------------
038500100922       BEGSR RoutEnd;
038600100922
038700100922         *inLR = *on;
038800100922         return;
038900100922
039000100922       ENDSR;
039100100922
039200100922      /end-free
