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
001600151019     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
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
008900151019     d wriga           s            132
009000110126     d savksc          s                   like(tasksc)
009100151019     d ktrd            s                   like(ar5trd) inz('FAT')
009200100922
009300100922      //---------------------------------------------------------------
009400100922      //?Definizione procedure esterne.
009500100922      //---------------------------------------------------------------
009600100922
009700100922
009800100922      //---------------------------------------------------------------
009900100922      //?prototipi
010000100922      //---------------------------------------------------------------
010100100922
010200100922      /copy gaitrasrc/srcprotopr,tibs34r
010300100922
010400100922      //---------------------------------------------------------------
010500100922      //?Definizione key-list.
010600100922      //---------------------------------------------------------------
010700100922
010800100922      //---------------------------------------------------------------
010900100922      //?Riepilogo indicatori.
011000100922      //---------------------------------------------------------------
011100100922
011200100922
011300100922      //---------------------------------------------------------------
011400100922
011500100922      //---------------------------------------------------------------
011600100922      //?M A I N - L I N E
011700100922      //---------------------------------------------------------------
011800100922
011900100922     c     *Entry        plist
012000100922     c                   parm                    KPJBA
012100100922
012200100922      /free
012300100922
012400100922       //?Operazioni iniziali
012500100922       exsr RoutInz;
012600100922
012700100922       //?Elaboro le bolle tassate
012800100922       exsr sr_spedizioni ;
012900100922
013000100922
013100100922       //?Operazioni finali
013200100922       exsr RoutEnd;
013300100922
013400100922       //--------------------------------------------------------------
013500100922       //?Operazioni iniziali.
013600100922       //--------------------------------------------------------------
013700100922       BEGSR RoutInz;
013800100922
013900100922         //?Imposto la ds con i dati della KPJBU
014000100922         Param    = kpjbu;
014100100922
014200100922         //?Reperimento dati job
014300100922         exsr DatiJob;
014400100922
014500100922         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014600100922
014700100922         //?Imposto data e ora elaborazione
014800110125         dataiso=datasys  ;
014900110125         datcor=%dec(dataiso)  ;
015000100922
015100110120         clear ksc ;
015200110120         clear dksc;
015300100922
015400110120         //?Carico in sk i codici clienti da elaborare
015500100922         clear xx;
015600100922         $EndTab = *off;
015700100922
015800100922         exec sql
015900110125          declare sfr cursor for
016000110120          select tbeke2 , tbeuni
016100110125          from tntbe00f where  tbecod = 'SFR' and
016200110126          tbeke2<>'              ' and tbeatb = ' ' and tbeke1='INFOLINE'
016300110125           order by tbeke2;
016400100922
016500100922         exec sql
016600110125           open SFR;
016700100922           IF sqlcode < 0;
016800100922             $Endtab = *on;
016900110125           ENDIF;
017000100922
017100100922         DOW not $Endtab;
017200100922           exec sql
017300110125             fetch next from SFR into :tbeke2, :tbeuni;
017400100922             IF sqlcod = 100 or sqlcod < 0;
017500100922               $EndTab = *on;
017600100922               leave;
017700100922             ENDIF;
017800110120             dSFR = Tbeuni;
017900100922               xx += 1;
018000110120               ksc(xx) = §sfrksc           ;
018100110120               dksc(xx) = §sfrdksc           ;
018200100922         ENDDO;
018300100922
018400110125         exec sql close SFR;
018500110125
018600110125         clear tbs ;
018700110125
018800110125         //?Carico in sk le bolle da scartare
018900110125         clear xx;
019000110125         $EndTab = *off;
019100110125
019200110125         exec sql
019300110125          declare tbs cursor for
019400110125          select tblkey , tbluni
019500110125          from tabel00f where tblkut = '1' and tblcod = 'TB' and
019600110125          tblflg = ' ' order by tblkut, tblcod, tblkey;
019700110125
019800110125         exec sql
019900110125           open TBS;
020000110125           IF sqlcode < 0;
020100110125             $Endtab = *on;
020200110125           ENDIF;
020300110125
020400110125         DOW not $Endtab;
020500110125           exec sql
020600110125             fetch next from TBS into :tblkey, :tbluni;
020700110125             IF sqlcod = 100 or sqlcod < 0;
020800110125               $EndTab = *on;
020900110125               leave;
021000110125             ENDIF;
021100110125             dstb = Tbluni;
021200110126             if (§tbrbl <> 'R' and §tbfst='1')  or %subst(tblkey:1:2)='F7' ;
021300110125               xx += 1;
021400110125               tbs(xx) = %subst(tblkey:1:2);
021500110125             ENDIF;
021600110125         ENDDO;
021700110125
021800110125         exec sql close TBS;
021900110125
022000100922
022100100922       ENDSR;
022200100922
022300100922       //--------------------------------------------------------------
022400100922       //?Leggo le spedizioni fatturate del periodo richieste
022500100922       //--------------------------------------------------------------
022600100922       BEGSR sr_Spedizioni ;
022700100922
022800100922         $End = *off;
022900110126         clear savksc   ;
023000100922
023100100922         //?Imposto la stringa per SQL
023200100922         exsr  sr_PrepSQL;
023300100922
023400100922         //?Dichiarazione cursore
023500100922         exec sql
023600100922           prepare S1   from :wSQL;
023700100922         exec sql
023800100922           declare c1   cursor for S1;
023900100922
024000100922         //?Apertura del cursore
024100100922         exec sql
024200100922           open c1 ;
024300100922
024400100922         IF sqlcode < 0;
024500100922           $End = *on;
024600100922         ENDIF;
024700100922
024800100922         DOW not $End;
024900100922           exec sql
025000151019             fetch next from C1  into :tasaas, :taslnp, :tasnrs, :tasnsp,
025100151019                                      :tasksc, :tasncl, :taspkc, :taspkf ;
025200100922           IF sqlcod = 100 or sqlcod < 0;
025300100922             $End = *on;
025400100922             leave;
025500100922           ENDIF;
025600100922
025700110126          if tasksc<> savksc  ;
025800110126            if savksc>0  ;
025900110126              exsr Scrivifile  ;
026000110126            endif  ;
026100110126
026200110126           savksc=tasksc  ;
026300110126
026400110126           clear  totcol2  ;
026500110126           clear  totcol5  ;
026600110126           endif  ;
026700151019           // chaino record FAT
026800151019           chain (tasaas:taslnp:tasnrs:tasnsp:KTRD) fiar531c  ;
026900151019           if not %found(fiar531c)   ;
027000110126
027100151019           exsr   Calcolo  ;
027200151019
027300151019           else  ;
027400151019           dar5fat=ar5uni   ;
027500151019           // se calcolato un peso desunto lo sostiuisco al peso VDL per il calcolo
027600151019           if §AR5FPTAS = 'D' ;
027700151019           taspkc=§AR5PKTAS   ;
027800151019           endif  ;
027900151019           exsr Calcolo       ;
028000151019
028100151019           endif ;
028200110126
028300100922         ENDDO;
028400100922
028500100922         exec sql
028600100922           close c1 ;
028700110126
028800110126            if savksc>0  ;
028900110126              exsr Scrivifile  ;
029000110126            endif  ;
029100100922
029200100922       ENDSR;
029300100922
029400151019       //--------------------------------------------------------------
029500151019       //?Scrittura record statistica
029600151019       //--------------------------------------------------------------
029700151019           BEGSR Calcolo ;
029800151019           select  ;
029900151019           when taspkf<=2 and taspkc<= 2 ;
030000151019             totcol2=totcol2+tasncl-1 ;
030100151019
030200151019           when taspkf>2 and taspkf<= 5 and taspkc<=5 ;
030300151019             totcol5=totcol5+tasncl-1 ;
030400151019
030500151019           when taspkc>2 and taspkc<= 5 and taspkf<=5 ;
030600151019             totcol5=totcol5+tasncl-1 ;
030700151019             endsl  ;
030800151019           ENDSR  ;
030900151019       //--------------------------------------------------------------
031000151019       //?Scrittura record statistica
031100151019       //--------------------------------------------------------------
031200110120       BEGSR Scrivifile    ;
031300100922
031400110125        // Imposto i dati in DS
031500110126        clear                   DSFR_INFO ;
031600100922
031700110303            §sfr_ksc = savksc ;
031800110125            xx=1  ;
031900110303            xx=%lookup(savksc:ksc) ;
032000110125            §sfr_dksc=dksc(xx)  ;
032100110126            §sfr_cole2= totcol2;
032200110126            §sfr_cole5= totcol5;
032300110125
032400110125            // Data Fattura inizio o fine mese
032500110125
032600110125            chain (1:dutkci:tasksc)  cnclp00f  ;
032700110125            if %found(cnclp00f) and clpfun='F'  ;
032800110125             §sfr_dft=pardfine  ;
032900110125            else  ;
033000110125             §sfr_dft=pardiniz  ;
033100110125            endif ;
033200100922
033300151019             wriga=dsfr_info  ;
033400151019         except riga    ;
033500100922
033600100922       ENDSR ;
033700100922       //--------------------------------------------------------------
033800100922       //?Reperimento Dati del job (Utente/Operativi).
033900100922       //--------------------------------------------------------------
034000100922       BEGSR DatiJob;
034100100922
034200100922         in(E) §AzUte;
034300100922         if NOT %error;
034400100922           in(E) §DatiUte;
034500100922         endif;
034600100922         if %error or RSut = *blanks;
034700100922           clear TIBS34ds;
034800100922           tibs34r(tibs34ds);
034900100922           in §AzUte;
035000100922           in §DatiUte;
035100100922         endif;
035200100922
035300100922       ENDSR;
035400100922
035500100922       //--------------------------------------------------------------
035600100922       //?Preparazione stringa SQL.
035700100922       //--------------------------------------------------------------
035800100922       BEGSR sr_PrepSQL;
035900100922
036000151019         wSQL = 'select tasaas, taslnp, tasnrs, tasnsp,               +
036100151019                        tasksc, tasncl, taspkc, taspkf  from TITAS10F +
036200110126                where  TASDFT>0 AND tasncl>1 and   TASKSC IN ('''  ;
036300110120
036400100922         yy = 0;
036500100922         xx = 1;
036600110120         FOR xx by 1 to %elem(ksc);
036700110120           IF ksc(xx) > 0     ;
036800100922             IF yy > 0;
036900100923               wSQL += ''', ''';
037000100922             ELSE;
037100100922               yy = 1;
037200100922             ENDIF;
037300110120             wSQL += %editc(ksc(xx):'X');
037400100922           ENDIF;
037500100922         ENDFOR;
037600100922
037700110125       // solo bolle buone
037800110126           wSQL += ''') and tastbl in ( ''';
037900110125         yy = 0;
038000110125         xx = 1;
038100110125         FOR xx by 1 to %elem(tbs);
038200110125           IF Tbs(xx) <> *blanks;
038300110125             IF yy > 0;
038400110125               wSQL += ''', ''';
038500110125             ELSE;
038600110125               yy = 1;
038700110125             ENDIF;
038800110125             wSQL += Tbs(xx);
038900110125           ENDIF;
039000110125         ENDFOR;
039100110125
039200110126           wSQL += ''')  order by TASKSC for fetch only ' ;
039300100922
039400100922       ENDSR;
039500100922
039600100922       //--------------------------------------------------------------
039700100922       //?Operazioni finali.
039800100922       //--------------------------------------------------------------
039900100922       BEGSR RoutEnd;
040000100922
040100100922         *inLR = *on;
040200100922         return;
040300100922
040400100922       ENDSR;
040500100922
040600100922      /end-free
040700151019     OQSYSPRT   E            riga        2
040800151019     O                       wriga
