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
001400151019     ffiar531c  if   e           k disk
001500110125     fTNTBE01L  if   e           k disk
001600110125     FTIVGD00F  o    E             DISK
001700100922
001800100922      //---------------------------------------------------------------
001900100922      //?Definizione costanti.
002000100922      //---------------------------------------------------------------
002100100922      //---------------------------------------------------------------
002200100922      //?Definizione schiere.
002300100922      //---------------------------------------------------------------
002400110120      * codici clienti da elaborare
002500110120     D KSC             s              7  0 dim(100)
002600110120     D dKSC            s             35    dim(100)
002700110125      * codici bolla da scartare
002800110125     D tbs             s              2    dim(50)
002900100922      //---------------------------------------------------------------
003000100922      //?Definizione aree dati.
003100100922      //---------------------------------------------------------------
003200100922
003300100922      // - Dati utente
003400100922     d §AzUte        e ds                  extname(AZUTE00F)
003500100922     d                                     dtaara
003600100922     d §DatiUte      e ds                  extname(dDatiUte)
003700100922     d                                     dtaara
003800100922
003900100922     d titasDS       e ds                  ExtName(titas10f)
004000100922
004100100922      //---------------------------------------------------------------
004200100922      //?Definizione strutture dati.
004300100922      //---------------------------------------------------------------
004400100922
004500100922      // - Parametri ricevuti
004600100922     d KPJBA         e ds
004700100922
004800100922      // - Reperimento dati utente
004900100922     d TIBS34DS      e ds
005000100922
005100110125     d dSFR          e ds
005200110126     d dSFR_INFO     e ds
005300110125     D DVGDFLO       E DS
005400110125     D DSTB          E DS
005500151019     D Dar5fat       E DS
005600100922
005700100922
005800100922      //---------------------------------------------------------------
005900100922      //?Definizione variabili globali.
006000100922      //---------------------------------------------------------------
006100100922
006200100922      // - Flags booleani
006300100922     d $End            s               n   inz(*off)
006400100922     d $Endtab         s               n   inz(*off)
006500100922     d $RcdOk          s               n   inz(*off)
006600100922
006700100922      // - Indici di schiera
006800100922     d xx              s              4  0 inz
006900100922     d yy              s              4  0 inz
007000100922
007100100922       // - Stringa SQL da eseguire
007200100922     d wSQL            s           2048    Varying        inz
007300100922
007400100922      // - Campi di comodo data
007500110125     d Dataeur         s               d   datfmt(*eur)
007600110125     d Dataiso         s               d   datfmt(*iso)
007700110125     d Datasys         s               d   datfmt(*iso) inz(*sys)
007800100922
007900100922      // - Parametri di lancio
008000100922     D PARAM           DS
008100110125     D  PARdiniz               1      8  0
008200110125     D  PARdfine               9     16  0
008300110125     D  PARtela               17     17
008400100922
008500100922      // - Campi di comodo
008600110125     d datcor          s              8  0
008700110126     d totcol5         s             11  0
008800110126     d totcol2         s             11  0
008900110126     d savksc          s                   like(tasksc)
009000151019     d ktrd            s                   like(ar5trd) inz('FAT')
009100100922
009200100922      //---------------------------------------------------------------
009300100922      //?Definizione procedure esterne.
009400100922      //---------------------------------------------------------------
009500100922
009600100922
009700100922      //---------------------------------------------------------------
009800100922      //?prototipi
009900100922      //---------------------------------------------------------------
010000100922
010100100922      /copy gaitrasrc/srcprotopr,tibs34r
010200100922
010300100922      //---------------------------------------------------------------
010400100922      //?Definizione key-list.
010500100922      //---------------------------------------------------------------
010600100922
010700100922      //---------------------------------------------------------------
010800100922      //?Riepilogo indicatori.
010900100922      //---------------------------------------------------------------
011000100922
011100100922
011200100922      //---------------------------------------------------------------
011300100922
011400100922      //---------------------------------------------------------------
011500100922      //?M A I N - L I N E
011600100922      //---------------------------------------------------------------
011700100922
011800100922     c     *Entry        plist
011900100922     c                   parm                    KPJBA
012000100922
012100100922      /free
012200100922
012300100922       //?Operazioni iniziali
012400100922       exsr RoutInz;
012500100922
012600100922       //?Elaboro le bolle tassate
012700100922       exsr sr_spedizioni ;
012800100922
012900100922
013000100922       //?Operazioni finali
013100100922       exsr RoutEnd;
013200100922
013300100922       //--------------------------------------------------------------
013400100922       //?Operazioni iniziali.
013500100922       //--------------------------------------------------------------
013600100922       BEGSR RoutInz;
013700100922
013800100922         //?Imposto la ds con i dati della KPJBU
013900100922         Param    = kpjbu;
014000100922
014100100922         //?Reperimento dati job
014200100922         exsr DatiJob;
014300100922
014400100922         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014500100922
014600100922         //?Imposto data e ora elaborazione
014700110125         dataiso=datasys  ;
014800110125         datcor=%dec(dataiso)  ;
014900100922
015000110120         clear ksc ;
015100110120         clear dksc;
015200100922
015300110120         //?Carico in sk i codici clienti da elaborare
015400100922         clear xx;
015500100922         $EndTab = *off;
015600100922
015700100922         exec sql
015800110125          declare sfr cursor for
015900110120          select tbeke2 , tbeuni
016000110125          from tntbe00f where  tbecod = 'SFR' and
016100110126          tbeke2<>'              ' and tbeatb = ' ' and tbeke1='INFOLINE'
016200110125           order by tbeke2;
016300100922
016400100922         exec sql
016500110125           open SFR;
016600100922           IF sqlcode < 0;
016700100922             $Endtab = *on;
016800110125           ENDIF;
016900100922
017000100922         DOW not $Endtab;
017100100922           exec sql
017200110125             fetch next from SFR into :tbeke2, :tbeuni;
017300100922             IF sqlcod = 100 or sqlcod < 0;
017400100922               $EndTab = *on;
017500100922               leave;
017600100922             ENDIF;
017700110120             dSFR = Tbeuni;
017800100922               xx += 1;
017900110120               ksc(xx) = §sfrksc           ;
018000110120               dksc(xx) = §sfrdksc           ;
018100100922         ENDDO;
018200100922
018300110125         exec sql close SFR;
018400110125
018500110125         clear tbs ;
018600110125
018700110125         //?Carico in sk le bolle da scartare
018800110125         clear xx;
018900110125         $EndTab = *off;
019000110125
019100110125         exec sql
019200110125          declare tbs cursor for
019300110125          select tblkey , tbluni
019400110125          from tabel00f where tblkut = '1' and tblcod = 'TB' and
019500110125          tblflg = ' ' order by tblkut, tblcod, tblkey;
019600110125
019700110125         exec sql
019800110125           open TBS;
019900110125           IF sqlcode < 0;
020000110125             $Endtab = *on;
020100110125           ENDIF;
020200110125
020300110125         DOW not $Endtab;
020400110125           exec sql
020500110125             fetch next from TBS into :tblkey, :tbluni;
020600110125             IF sqlcod = 100 or sqlcod < 0;
020700110125               $EndTab = *on;
020800110125               leave;
020900110125             ENDIF;
021000110125             dstb = Tbluni;
021100110126             if (§tbrbl <> 'R' and §tbfst='1')  or %subst(tblkey:1:2)='F7' ;
021200110125               xx += 1;
021300110125               tbs(xx) = %subst(tblkey:1:2);
021400110125             ENDIF;
021500110125         ENDDO;
021600110125
021700110125         exec sql close TBS;
021800110125
021900100922
022000100922       ENDSR;
022100100922
022200100922       //--------------------------------------------------------------
022300100922       //?Leggo le spedizioni fatturate del periodo richieste
022400100922       //--------------------------------------------------------------
022500100922       BEGSR sr_Spedizioni ;
022600100922
022700100922         $End = *off;
022800110126         clear savksc   ;
022900100922
023000100922         //?Imposto la stringa per SQL
023100100922         exsr  sr_PrepSQL;
023200100922
023300100922         //?Dichiarazione cursore
023400100922         exec sql
023500100922           prepare S1   from :wSQL;
023600100922         exec sql
023700100922           declare c1   cursor for S1;
023800100922
023900100922         //?Apertura del cursore
024000100922         exec sql
024100100922           open c1 ;
024200100922
024300100922         IF sqlcode < 0;
024400100922           $End = *on;
024500100922         ENDIF;
024600100922
024700100922         DOW not $End;
024800100922           exec sql
024900151019             fetch next from C1  into :tasaas, :taslnp, :tasnrs, :tasnsp,
025000151019                                      :tasksc, :tasncl, :taspkc, :taspkf ;
025100100922           IF sqlcod = 100 or sqlcod < 0;
025200100922             $End = *on;
025300100922             leave;
025400100922           ENDIF;
025500100922
025600110126          if tasksc<> savksc  ;
025700110126            if savksc>0  ;
025800110126              exsr Scrivifile  ;
025900110126            endif  ;
026000110126
026100110126           savksc=tasksc  ;
026200110126
026300110126           clear  totcol2  ;
026400110126           clear  totcol5  ;
026500110126           endif  ;
026600151019           // chaino record FAT
026700151019           chain (tasaas:taslnp:tasnrs:tasnsp:KTRD) fiar531c  ;
026800151019           if not %found(fiar531c)   ;
026900110126
027000151019           exsr   Calcolo  ;
027100151019
027200151019           else  ;
027300151019           dar5fat=ar5uni   ;
027400151019           // se calcolato un peso desunto lo sostiuisco al peso VDL per il calcolo
027500151019           if §AR5FPTAS = 'D' ;
027600151019           taspkc=§AR5PKTAS   ;
027700151019           endif  ;
027800151019           exsr Calcolo       ;
027900151019
028000151019           endif ;
028100110126
028200100922         ENDDO;
028300100922
028400100922         exec sql
028500100922           close c1 ;
028600110126
028700110126            if savksc>0  ;
028800110126              exsr Scrivifile  ;
028900110126            endif  ;
029000100922
029100100922       ENDSR;
029200100922
029300151019       //--------------------------------------------------------------
029400151019       //?Scrittura record statistica
029500151019       //--------------------------------------------------------------
029600151019           BEGSR Calcolo ;
029700151019           select  ;
029800151019           when taspkf<=2 and taspkc<= 2 ;
029900151019             totcol2=totcol2+tasncl-1 ;
030000151019
030100151019           when taspkf>2 and taspkf<= 5 and taspkc<=5 ;
030200151019             totcol5=totcol5+tasncl-1 ;
030300151019
030400151019           when taspkc>2 and taspkc<= 5 and taspkf<=5 ;
030500151019             totcol5=totcol5+tasncl-1 ;
030600151019             endsl  ;
030700151019           ENDSR  ;
030800151019       //--------------------------------------------------------------
030900151019       //?Scrittura record statistica
031000151019       //--------------------------------------------------------------
031100110120       BEGSR Scrivifile    ;
031200100922
031300110125       //  Preparo la scrittura del file TIVGD
031400110125       clear                   tivgd000    ;
031500110202       vgdtip='&2'    ;
031600110131       vgdksu='0BART001' ;
031700110131       vgdtsc='WW'      ;
031800110125
031900110125       vgddaT=datcor    ;
032000110126       vgdpgm='TISTF2R ';
032100110125       clear                   DVGDFLO     ;
032200110125
032300110125        // Imposto i dati in DS
032400110126        clear                   DSFR_INFO ;
032500100922
032600110303            §sfr_ksc = savksc ;
032700110125            xx=1  ;
032800110303            xx=%lookup(savksc:ksc) ;
032900110125            §sfr_dksc=dksc(xx)  ;
033000110126            §sfr_cole2= totcol2;
033100110126            §sfr_cole5= totcol5;
033200110125
033300110125            // Data Fattura inizio o fine mese
033400110125
033500110125            chain (1:dutkci:tasksc)  cnclp00f  ;
033600110125            if %found(cnclp00f) and clpfun='F'  ;
033700110125             §sfr_dft=pardfine  ;
033800110125            else  ;
033900110125             §sfr_dft=pardiniz  ;
034000110125            endif ;
034100100922
034200110126            VGDDTA =%subst(Dsfr_info:1:%size(DSFR_info))      ;
034300100922
034400110125         write tivgd000 ;
034500100922
034600100922       ENDSR ;
034700100922       //--------------------------------------------------------------
034800100922       //?Reperimento Dati del job (Utente/Operativi).
034900100922       //--------------------------------------------------------------
035000100922       BEGSR DatiJob;
035100100922
035200100922         in(E) §AzUte;
035300100922         if NOT %error;
035400100922           in(E) §DatiUte;
035500100922         endif;
035600100922         if %error or RSut = *blanks;
035700100922           clear TIBS34ds;
035800100922           tibs34r(tibs34ds);
035900100922           in §AzUte;
036000100922           in §DatiUte;
036100100922         endif;
036200100922
036300100922       ENDSR;
036400100922
036500100922       //--------------------------------------------------------------
036600100922       //?Preparazione stringa SQL.
036700100922       //--------------------------------------------------------------
036800100922       BEGSR sr_PrepSQL;
036900100922
037000151019         wSQL = 'select tasaas, taslnp, tasnrs, tasnsp,               +
037100151019                        tasksc, tasncl, taspkc, taspkf  from TITAS00F +
037200110126                where  TASDFT>0 AND tasncl>1 and   TASKSC IN ('''  ;
037300110120
037400100922         yy = 0;
037500100922         xx = 1;
037600110120         FOR xx by 1 to %elem(ksc);
037700110120           IF ksc(xx) > 0     ;
037800100922             IF yy > 0;
037900100923               wSQL += ''', ''';
038000100922             ELSE;
038100100922               yy = 1;
038200100922             ENDIF;
038300110120             wSQL += %editc(ksc(xx):'X');
038400100922           ENDIF;
038500100922         ENDFOR;
038600100922
038700110125       // solo bolle buone
038800110126           wSQL += ''') and tastbl in ( ''';
038900110125         yy = 0;
039000110125         xx = 1;
039100110125         FOR xx by 1 to %elem(tbs);
039200110125           IF Tbs(xx) <> *blanks;
039300110125             IF yy > 0;
039400110125               wSQL += ''', ''';
039500110125             ELSE;
039600110125               yy = 1;
039700110125             ENDIF;
039800110125             wSQL += Tbs(xx);
039900110125           ENDIF;
040000110125         ENDFOR;
040100110125
040200110126           wSQL += ''')  order by TASKSC for fetch only ' ;
040300100922
040400100922       ENDSR;
040500100922
040600100922       //--------------------------------------------------------------
040700100922       //?Operazioni finali.
040800100922       //--------------------------------------------------------------
040900100922       BEGSR RoutEnd;
041000100922
041100100922         *inLR = *on;
041200100922         return;
041300100922
041400100922       ENDSR;
041500100922
041600100922      /end-free
