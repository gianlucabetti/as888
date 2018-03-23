000100100922      //--------------------------------------------------------------
000200110405      //?TISTB3R - Totale bolle con supermercato
000300100922      //--------------------------------------------------------------
000400100922
000500100922     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600100922     h dftactgrp(*no) actgrp(*caller)
000700100922
000800100922      //---------------------------------------------------------------
000900100922      //?Dichiarazione file.
001000100922      //---------------------------------------------------------------
001100110405     ftitas38c  if   e           k disk
001200110405     ftabel00f  if   e           k disk
001300110405     ffnlbl01l  if   e           k disk
001400110125     fTNTBE01L  if   e           k disk
001500110218     fazorg01l  if   e           k disk
001600110405     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
001700110405     F                                     FORMLEN(66)
001800110405     F                                     FORMOFL(63)
001900100922      //---------------------------------------------------------------
002000100922      //?Definizione costanti.
002100100922      //---------------------------------------------------------------
002200100922      //---------------------------------------------------------------
002300100922      //?Definizione schiere.
002400100922      //---------------------------------------------------------------
002500110405      * testate di stampa
002600110405     D TES             S             66    DIM(7) CTDATA PERRCD(1)
002700110125      * codici bolla da scartare
002800110125     D tbs             s              2    dim(50)
002900110405      * fasce peso
003000110405     D FPR             s              5    dim(20)
003100110405     D KIR             s              6  0 dim(20)
003200110405     D KFR             s              6  0 dim(20)
003300110405     D col             s             11  0 dim(20)
003400110405     D pes             s             11  1 dim(20)
003500110405     D vol             s             11  3 dim(20)
003600110405     D spe             s             11  0 dim(20)
003700100922      //---------------------------------------------------------------
003800100922      //?Definizione aree dati.
003900100922      //---------------------------------------------------------------
004000100922
004100100922      // - Dati utente
004200100922     d §AzUte        e ds                  extname(AZUTE00F)
004300100922     d                                     dtaara
004400100922     d §DatiUte      e ds                  extname(dDatiUte)
004500100922     d                                     dtaara
004600100922
004700100922      //---------------------------------------------------------------
004800100922      //?Definizione strutture dati.
004900100922      //---------------------------------------------------------------
005000100922
005100100922      // - Parametri ricevuti
005200100922     d KPJBA         e ds
005300100922
005400100922      // - Reperimento dati utente
005500100922     d TIBS34DS      e ds
005600100922
005700110405     D og143         E DS
005800110405     D DSTB          E DS
005900110405     D DS2L          E DS
006000100922
006100100922      //---------------------------------------------------------------
006200100922      //?Definizione variabili globali.
006300100922      //---------------------------------------------------------------
006400100922
006500100922      // - Flags booleani
006600100922     d $End            s               n   inz(*off)
006700100922     d $Endtab         s               n   inz(*off)
006800110405     d $RecOk          s               n   inz(*off)
006900100922
007000100922      // - Indici di schiera
007100100922     d xx              s              4  0 inz
007200100922     d yy              s              4  0 inz
007300110405     d Indx            s              4  0 inz
007400100922
007500100922      // - Campi di comodo data
007600110125     d Dataeur         s               d   datfmt(*eur)
007700110125     d Dataiso         s               d   datfmt(*iso)
007800110125     d Datasys         s               d   datfmt(*iso) inz(*sys)
007900110405     d Orasys          s               t   timfmt(*iso) inz(*sys)
008000100922
008100100922      // - Parametri di lancio
008200110405     D TISTB3DS        DS
008300110217     D  PARdiniz               1      8  0
008400110405     D  kaain                  1      4  0
008500110405     D  kmgin                  5      8  0
008600110125     D  PARdfine               9     16  0
008700100922
008800100922      // - Campi di comodo
008900110125     d datcor          s              8  0
009000110405     d Oracor          s              6  0
009100110405     d p1cdti          s              8  0               inz
009200110405     d p1cdtf          s              8  0               inz
009300110218     d savlnp          s                   like(taslnp)  inz
009400110218     d savlna          s                   like(taslna)  inz
009500110405     d P_§ogntw        s                   like(§ogntw)  inz
009600110405     d A_§ogntw        s                   like(§ogntw)  inz
009700110405     d wpes            s                   like(taspkf)  inz
009800110405     d wvol            s                   like(tasvlf)  inz
009900110405     d PesM            s              7  1               inz
010000110405     d volM            s              7  3               inz
010100110405     d colM            s              6  0               inz
010200100922
010300100922      //---------------------------------------------------------------
010400100922      //?Definizione procedure esterne.
010500100922      //---------------------------------------------------------------
010600100922
010700100922
010800100922      //---------------------------------------------------------------
010900100922      //?prototipi
011000100922      //---------------------------------------------------------------
011100100922
011200100922      /copy gaitrasrc/srcprotopr,tibs34r
011300100922
011400100922      //---------------------------------------------------------------
011500100922      //?Definizione key-list.
011600100922      //---------------------------------------------------------------
011700100922
011800100922      //---------------------------------------------------------------
011900100922      //?Riepilogo indicatori.
012000100922      //---------------------------------------------------------------
012100100922
012200100922
012300100922      //---------------------------------------------------------------
012400100922
012500100922      //---------------------------------------------------------------
012600100922      //?M A I N - L I N E
012700100922      //---------------------------------------------------------------
012800100922
012900100922     c     *Entry        plist
013000100922     c                   parm                    KPJBA
013100100922
013200100922      /free
013300100922
013400100922       //?Operazioni iniziali
013500100922       exsr RoutInz;
013600100922
013700100922       //?Elaboro le bolle tassate
013800100922       exsr sr_spedizioni ;
013900100922
014000100922
014100100922       //?Operazioni finali
014200100922       exsr RoutEnd;
014300100922
014400100922       //--------------------------------------------------------------
014500100922       //?Operazioni iniziali.
014600100922       //--------------------------------------------------------------
014700100922       BEGSR RoutInz;
014800100922
014900100922         //?Imposto la ds con i dati della KPJBU
015000110405         tistb3ds = kpjbu;
015100100922
015200100922         //?Reperimento dati job
015300100922         exsr DatiJob;
015400100922
015500100922         //?Imposto data e ora elaborazione
015600110125         dataiso=datasys  ;
015700110405         dataeur=dataiso  ;
015800110405         datcor=%dec(dataeur)  ;
015900110405
016000110405         oracor=%dec(orasys)   ;
016100110405
016200110405         //?giro le date passate
016300110405         dataiso=%date(pardiniz) ;
016400110405         dataeur=dataiso  ;
016500110405         p1cdti=%dec(dataeur)  ;
016600110405
016700110405         dataiso=%date(pardfine) ;
016800110405         dataeur=dataiso ;
016900110405         p1cdtf=%dec(dataeur)  ;
017000100922
017100110125         //?Carico in sk le bolle da scartare
017200110125         clear xx;
017300110125         $EndTab = *off;
017400110125
017500110125         exec sql
017600110125          declare tbs cursor for
017700110125          select tblkey , tbluni
017800110125          from tabel00f where tblkut = '1' and tblcod = 'TB' and
017900110125          tblflg = ' ' order by tblkut, tblcod, tblkey;
018000110125
018100110125         exec sql
018200110125           open TBS;
018300110125           IF sqlcode < 0;
018400110125             $Endtab = *on;
018500110125           ENDIF;
018600110125
018700110125         DOW not $Endtab;
018800110125           exec sql
018900110125             fetch next from TBS into :tblkey, :tbluni;
019000110125             IF sqlcod = 100 or sqlcod < 0;
019100110125               $EndTab = *on;
019200110125               leave;
019300110125             ENDIF;
019400110125             dstb = Tbluni;
019500110405             if (§tbrbl <> 'R' and §tbfst='1')   ;
019600110125               xx += 1;
019700110125               tbs(xx) = %subst(tblkey:1:2);
019800110125             ENDIF;
019900110125         ENDDO;
020000110125
020100110125         exec sql close TBS;
020200110125
020300110405         // Carico le fasce di peso
020400110405         clear xx;
020500110405         $EndTab = *off;
020600110405
020700110405         exec sql
020800110405          declare TPE cursor for
020900110405          select tblkey , tbluni
021000110405          from tabel00f where tblkut = '1' and tblcod = '2L' and
021100110405          tblflg = ' ' order by tblkut, tblcod, tblkey;
021200110405
021300110405         exec sql
021400110405           open TPE;
021500110405           IF sqlcode < 0;
021600110405             $Endtab = *on;
021700110405           ENDIF;
021800110405
021900110405         DOW not $Endtab;
022000110405           exec sql
022100110405             fetch next from TPE into :tblkey, :tbluni;
022200110405             IF sqlcod = 100 or sqlcod < 0;
022300110405               $EndTab = *on;
022400110405               leave;
022500110405             ENDIF;
022600110405
022700110405             ds2L = Tbluni;
022800110405             if %subst(tblkey:1:3)='SDR'         ;
022900110405               xx += 1;
023000110405               FPR(xx) = %subst(tblkey:1:5);
023100110405               kir(xx) = §2lkgi            ;
023200110405               kfr(xx) = §2lkgf            ;
023300110405             ENDIF;
023400110405         ENDDO;
023500110405
023600110405         exec sql close TPE;
023700100922
023800110406       // Alla posizione 20 imposto come da cartello un unico 99999 oltre 100 kg.
023900110406       kir(20)=100  ;
024000110406       kfr(20)=999999 ;
024100110406
024200100922       ENDSR;
024300100922
024400100922       //--------------------------------------------------------------
024500110405       //?Leggo le spedizioni  del periodo richiesto
024600100922       //--------------------------------------------------------------
024700100922       BEGSR sr_Spedizioni ;
024800110405
024900110405       clear savlnp   ;
025000110405       clear savlna   ;
025100110405       setll (kaain:kmgin)   titas38c   ;
025200110405       read                  titas38c   ;
025300110405
025400110405       dow not %eof(titas38c)  ;
025500110405
025600110405       // Finito di elaborare il periodo richiesto, esco dal ciclo
025700110405       if (tasaas*10000)+tasmgs>pardfine   ;
025800110405         leave  ;
025900110405       endif  ;
026000110405
026100110405       exsr Escludi   ;
026200110405
026300110405
026400110405        // Conteggi  colli peso e volume x fasce di peso
026500110405        if $recok = *on ;
026600110405          exsr ElaTAS  ;
026700110405        endif   ;
026800110221
026900110405       read                  titas38c   ;
027000110405       enddo   ;
027100110221
027200110405       // elaborazioni finali e stampa
027300110405       exsr Stampa  ;
027400110405
027500110221       ENDSR;
027600110405       //--------------------------------------------------------------
027700110405       //?Esclusione sulle bolle lette
027800110405       //--------------------------------------------------------------
027900110405       BEGSR  Escludi  ;
028000110405
028100110405        $recok=*on ;
028200110405
028300110405       // Escludo le bolle senza supermercato
028400110405       if tasftc<>'S'  and tastc2<>'S'   ;
028500110405       $recok=*off ;
028600110405        leavesr ;
028700110405       endif  ;
028800110405
028900110405       // Escludo tipi bolla non richiesti   ;
029000110405       Indx=%lookup(tastbl:tbs)  ;
029100110405       if Indx=0   ;
029200110405       $recok=*off ;
029300110405        leavesr ;
029400110405       endif  ;
029500110405
029600110405       // Escludo import ed export   ;
029700110405
029800110405       if taslnp<>savlnp   ;
029900110405       chain  taslnp   azorg01l   ;
030000110405        if %found(azorg01l)   ;
030100110405         og143 =orgde3  ;
030200110405         P_§ogntw =§ogntw  ;
030300110405         savlnp=taslnp   ;
030400110405        endif   ;
030500110405       endif   ;
030600110405
030700110405       if P_§ogntw='DPD'  or P_§ogntw='FED' or P_§ogntw='EEX'  ;
030800110405       $recok=*off ;
030900110405        leavesr ;
031000110405        endif   ;
031100110405
031200110405       if taslna<>savlna   ;
031300110405       chain  taslna   azorg01l   ;
031400110405        if %found(azorg01l)   ;
031500110405         og143 =orgde3  ;
031600110405         A_§ogntw =§ogntw  ;
031700110405         savlna=taslna   ;
031800110405        endif   ;
031900110405       endif   ;
032000110405
032100110405       if A_§ogntw='DPD'  or A_§ogntw='FED' or A_§ogntw='EEX'  ;
032200110405       $recok=*off ;
032300110405        leavesr ;
032400110405        endif   ;
032500110405
032600110405        // Escludo se si tratta di bolla figlia
032700110405        chain (tasaas:taslnp:tasnrs:tasnsp)   fnlbl01l    ;
032800110405        if %found(fnlbl01l ) ;
032900110405       $recok=*off ;
033000110405        leavesr ;
033100110405        endif  ;
033200110405
033300110405        ENDSR  ;
033400110221       //--------------------------------------------------------------
033500110405       //?conteggi dati della bolla letta
033600110221       //--------------------------------------------------------------
033700110405         BEGSR  elaTAS  ;
033800110405
033900110405       // Imposto campo peso
034000110405
034100110405       if  taspkc>0 and tasncp=tasncl ;
034200110405        wpes=taspkc  ;
034300110405       else   ;
034400110405       if taspkc>taspkf ;
034500110405        wpes=taspkc ;
034600110405        else  ;
034700110405        wpes=taspkf  ;
034800110405       endif  ;
034900110405       endif  ;
035000110405
035100110405       // Imposto campo volume
035200110405 1     if tasfvf='T' or tasfvf='Z' ;
035300110405        wvol=tasvlf  ;
035400110405 x1    else  ;
035500110405 2       if  tasvlc>0 and tasncl=tasncr   ;
035600110405           wvol= tasvlc  ;
035700110405 x2      else   ;
035800110405 3        if tasvlc>tasvlf  ;
035900110405           wvol=tasvlc  ;
036000110405          else  ;
036100110405           wvol=tasvlf ;
036200110405 3        endif  ;
036300110405 2       endif  ;
036400110405 1     endif  ;
036500100922
036600110405       XX=1         ;
036700110405
036800110405       if wpes=0  ;
036900110405        wpes=0,1   ;
037000110405       endif ;
037100110405
0372001104051      dow xx<=20   ;
0373001104052      if wpes  > kir(xx) and wpes<=kfr(xx)  ;
037400110405         col(xx)= tasncl + col(xx)  ;
037500110405         pes(xx)=wpes + pes(xx)  ;
037600110405         vol(xx)=wvol + vol(xx)  ;
037700110405         spe(xx) +=1  ;
037800110406
037900110406       // Vedo anche se rientra nello scaglione 20 forzato
0380001104062      if wpes  > kir(20) and wpes<=kfr(20)  ;
038100110406         col(20)= tasncl + col(20)  ;
038200110406         pes(20)=wpes + pes(20)  ;
038300110406         vol(20)=wvol + vol(20)  ;
038400110406         spe(20) +=1  ;
038500110406       endif ;
038600110405         leave  ;
038700110405       endif   ;
038800110405
038900110405         xx +=1  ;
039000110405       enddo  ;
039100110405
039200100922       ENDSR;
039300110405       //--------------------------------------------------------------
039400110405       //?Stampa dati elaborati
039500110405       //--------------------------------------------------------------
039600110405       BEGSR  Stampa  ;
039700110405
039800110405       except testa  ;
039900110405
040000110405       xx= 1 ;
040100110405
0402001104051      dow xx<=20   ;
040300110405
040400110405       if kfr(xx) >0   ;
040500110405
040600110405       // Calcolo peso, volume, colli  medi
040700110405       if pes(xx)>0   ;
040800110405        eval(h)  PesM=pes(xx)/spe(xx)  ;
040900110405       else   ;
041000110405        clear PesM ;
041100110405       endif  ;
041200110405
041300110405       if vol(xx)>0   ;
041400110405        eval(h) VolM=vol(xx)/spe(xx)  ;
041500110405       else   ;
041600110405        clear volM ;
041700110405       endif  ;
041800110405       if col(xx)>0   ;
041900110405        eval(h) ColM=col(xx)/spe(xx)  ;
042000110405       else  ;
042100110405        clear colM ;
042200110405       endif  ;
042300110405
042400110405       except Detta  ;
042500110405       endif  ;
042600110405
042700110405       xx +=1 ;
042800110405       enddo  ;
042900110405
043000110405       except fine   ;
043100110405
043200110405       ENDSR;
043300100922
043400100922       //--------------------------------------------------------------
043500100922       //?Reperimento Dati del job (Utente/Operativi).
043600100922       //--------------------------------------------------------------
043700100922       BEGSR DatiJob;
043800100922
043900100922         in(E) §AzUte;
044000100922         if NOT %error;
044100100922           in(E) §DatiUte;
044200100922         endif;
044300100922         if %error or RSut = *blanks;
044400100922           clear TIBS34ds;
044500100922           tibs34r(tibs34ds);
044600100922           in §AzUte;
044700100922           in §DatiUte;
044800100922         endif;
044900100922
045000100922       ENDSR;
045100100922       //--------------------------------------------------------------
045200100922       //?Operazioni finali.
045300100922       //--------------------------------------------------------------
045400100922       BEGSR RoutEnd;
045500100922
045600100922         *inLR = *on;
045700100922         return;
045800100922
045900100922       ENDSR;
046000100922
046100100922      /end-free
046200110405     O*------------------------------------------------------------------------*
046300110405     OQSYSPRT   E            TESTA            02
046400110405     O                       RSUT                20
046500110405     O                                           50 '** Totale bolle dirette a '
046600110405     O                                         +  0 'GDO per fasce di peso **'
046700110405     O                                         + 08 'TISTB3R '
046800110405     O                                         +  4 'Data:'
046900110405     O                       datcor            +  1 '  /  /    '
047000110405     O                                         +  4 'Ora:'
047100110405     O                       oracor            +  1 '  .  .  '
047200110405     o
047300110405     OQSYSPRT   E            TESTA       2
047400110405     O                                         +  5 'Bolle elaborate per D'
047500110405     o                                         +  0 'ata Spedizione:'
047600110405     O                                         +  2 'DAL:'
047700110405     O                       p1cDTI            +  2 '  /  /    '
047800110405     O                                         +  3 'AL:'
047900110405     O                       p1cDTF            +  2 '  /  /    '
048000110405     O          E            TESTA       2  1
048100110405     O                       TES(1)              66
048200110405     O                       TES(1)             132
048300110405     O          E            TESTA          1
048400110405     O                       TES(2)              66
048500110405     O                       TES(5)             132
048600110405     O          E            TESTA          1
048700110405     O                       TES(3)              66
048800110405     O                       TES(6)             132
048900110405     O          E            TESTA          1
049000110405     O                       TES(1)              66
049100110405     O                       TES(1)             132
049200110405     O*------------------------------------
049300110405     O          E            Detta          1
049400110405     O                       TES(4)              66
049500110405     O                       TES(7)             132
049600110405     o                       kir(xx)       2     16
049700110405     o                       kfr(xx)       2     32
049800110405     o                       colM          2     41
049900110405     o                       pesM          2     53
050000110405     o                       volM          2     64
050100110405     o                       spe(xx)       2     80
050200110405     O          E            FINE           1
050300110405     O                       TES(1)              66
050400110405     O                       TES(1)             132
050500110405**  TES - RIGHE IN STAMPA
050600110405------------------------------------------------------------------  1
050700110405                                 |         Valore MEDIO          |  2
050800110405                                 |  Colli     Peso      Volume   |  3
050900110405Kg: > di xxx.xxx e < = a xxx.xxx |xxx.xxx | xxx.xxx,x |x.xxx,xxx |  4
051000110405      TOTALE   |                                                       5
051100110405    Spedizioni |                                                       6
051200110405xx.xxx.xxx.xxx |                                                       7
