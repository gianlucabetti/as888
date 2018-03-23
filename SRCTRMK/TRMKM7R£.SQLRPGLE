000100031205      *------------------------------------------------------------------------*
000200090202      * Creazione file per mappatura commerciale
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600090203     ftncpo01l  if   e           k Disk
000700090209     fticpi01l  if   e           k Disk
000800090203     fazorg01l  if   e           k Disk
000900071123     fTntbe01l  if   e           k Disk
001000080102     ftabel00f  if   e           k Disk
001100071218     fcnind00f  if   e           k Disk
001200071218     fcnind02l  if   e           k Disk    rename(cnind000:cnind002)
001300071212     fcnind03l  if   e           k Disk    rename(cnind000:cnind003)
001400071218     fcnaco16l  if   e           k Disk    rename(cnaco000:cnind016)
001500071218     fcnaco00f  if   e           k Disk
001600071213     fcnclp00f  if   e           k Disk
001700090205     Fwfdemd3l  iF   E           k DISK
001800090205     Fwfdemi3l  iF   E           k DISK    rename(wadelpc0:wfdemi03)
001900090205     Fwfdeme3l  iF   E           k DISK    rename(wadelpc0:wfdeme03)
002000090203     FWFDEmu0f  uF a E             DISK
002100090203     FWFDEmp0F  uF a E             DISK
002200090202     FWFDEmp1L  iF   E           K DISK    rename(wfdemp00:wfdemp01)
002300090202     FWFDEmp2L  iF   E           K DISK    rename(wfdemp00:wfdemp02)
002400090203     FWFDEmp3L  uF   E           K DISK    rename(wfdemp00:wfdemp03)
002500090203     FWFDEmu1L  uF   E           K DISK    rename(wfdemu00:wfdemu01)
002600090203     F                                     prefix(u1_)
002700090209     fTNcpo05L  if   e           k Disk    rename(tncpo000:tncpo005)
002800090209     fTNcpo06L  if   e           k Disk    rename(tncpo000:tncpo006)
002900031205      *------------------------------------------------------------------------*
003000031205      *   C A M P I   D I   L A V O R O
003100031205      *------------------------------------------------------------------------*
003200080110     d UNI             s              7  0 dim(100)
003300080111     d giapre          s              7  0 dim(10)
003400080110     d COD             s              7  0 dim(100)
003500090209
003600090209     d potsede         s             11  0 dim(90)
003700090209     d potfat          s             11  0 dim(90)
003800090209     d potinfo         s              1    dim(90)
003900090209     d potinfop        s              1    dim(90)
004000090209     D
004100090209     d savfat          s             11  0
004200090209     d RipSpesa        s             11  0
004300090209     d Trov            s              3  0
004400090205     d Usaunific       s              1
004500071212     d Wksc            s              7  0
004600071213     d Wuni            s              7  0
004700071213     d Kcod            s              7
004800071217     d Kcdf            s             16
004900090203     d Kpiv            s             16
005000080110     d wtrov           s              1
005100090203     d Wfil            s              3  0
005200071213     d xx              s              3  0
005300071213     d kut             s              1  0 inz(1)
005400080102     d kkey            s                   like(tblkey)
005500071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
005600090202     d Annocor         s              4  0
005700090204     d maxfac          s             15  3
005800090209     d Indx            s              4  0
005900090209     d yy              s              4  0
006000031205
006100031205      *------------------------------------------------------------------------*
006200031205      *   D S   I N T E R N E / E S T E R N E
006300031205      *------------------------------------------------------------------------*
006400031205     d Kpjba         e ds
006500090209     d TRMK50ds      e ds                  inz
006600090209     d Dcpo01        e ds
006700071122     d fnlv13ds      e ds
006800071122     d tisi95ds      e ds
006900071212     d tibs10ds      e ds
007000090209     d  skc                   21   5520    dim(500)
007100071122     d
007200090203      // - Reperimento dati utente
007300090203     d TIBS34ds      e ds
007400090203      // - Dati utente
007500090203     d §AzUte        e ds                  extname(AZUTE00F)
007600090203     d                                     dtaara
007700090203     d §DatiUte      e ds                  extname(dDatiUte)
007800090203     d                                     dtaara
007900090203
008000090203     D trmkm3ds        DS
008100090203     D  Elabcpo                1      1
008200090203     D  Elabwadel              2      2
008300090204     D  Elabdempc              3      3
008400090204     D  Elabdempd              4      4
008500090209     D  Elabdemuc              5      5
008600090209     D  Elabinfo               6      6
008700090203     D*
008800071122      *------------------------------------------------------------------------*
008900071122      *   P R O T O T I P I
009000071122      *------------------------------------------------------------------------*
009100071122     D FNLV13R         pr                  extpgm('FNLV13R')
009200071122     D  kpjba                              likeds(kpjba)
009300071122     D  FNLV13DS                           likeds(FNLV13DS)
009400071122     D  TISI95DS                           likeds(TISI95DS)
009500071213     d
009600071213     D TIBS10R         pr                  extpgm('TIBS10R')
009700071213     D  TIBS10DS                           likeds(TIBS10DS)
009800071213     d  TIPEXE                        1a
009900071213     d  TIPEXE         s              1a   inz(' ')
010000090203
010100090203      /copy gaitrasrc/srcprotopr,tibs34r
010200090209      /copy gaitrasrc/srcprotopr,trmk50r
010300031205      *------------------------------------------------------------------------*
010400071217     c     *entry        plist
010500090203     c                   parm                    kpjba
010600090203     c                   movel     kpjbu         trmkm3ds
010700090203     c
010800071116      /free
010900090203         // Reperimento dati job
011000090203         exsr DatiJob;
011100090203
011200071122
011300090203       // 1a) Ciclo sui potenziali per scrivere WFDEMP
011400090203
011500090202       if Elabcpo='S'    ;
011600090202
011700090202       exec sql   delete  from  wfdemp0f   ;
011800071217
011900090202        setll  *loval    tncpo01l   ;
012000090202        read    tncpo01l    ;
012100071122
012200090202 1      dow  not  %eof(tncpo01l)  ;
012300090202
012400080108         // Escludo la bartolini
012500090202         if cpoduns<>*blanks and cpoduns<>'431338995'  and
012600090202                                 cpoftaz>0  and cpofsf='S'   ;
012700090202            exsr     ScriviDEMP  ;
012800080108          endif  ;
012900071122
013000090202        read    tncpo01l    ;
013100071122 1      enddo                      ;
013200080108 0     endif   ;
013300090202
013400090202
013500090203       // 1b) Se devo aggiornare  tutto il file pulisco i campi
013600090203       //     degli unificanti
013700090203
013800090206       if  elabCPO=' ' and (elabDEMPC='S' or elabWADEL='S');
013900090203       setll   *start    wfdemp0f ;
014000090203       read   wfdemp00  ;
0141000902031       dow   not %eof(wfdemp0f)  ;
014200090206        exsr cleardaWADEL   ;
014300090203        update wfdemp00 ;
014400090203       read   wfdemp00  ;
014500080110        enddo   ;
014600080110        endif ;
014700080110
014800090205       // 2a) Ciclo sui clienti unificanti  WFDEMD x scrivere WFDEMU
014900090203
015000090202        if  elabWADel= 'S' ;
015100090203            exsr     ScriviDEMU  ;
015200090203        endif  ;
015300090203
015400090203       // 2b) Se non devo elaborare, sflago solo flag utilizzo
015500090203
015600090206        if  elabWADEL=' ' and (elabDEMPC= 'S' or elabCPO='S')  ;
015700090203         setll  *loval wfdemu1l ;
015800090203         read          wfdemu1l  ;
0159000902031        dow   not %eof(wfdemu1l)  ;
016000090203           clear u1_demusa         ;
016100090203          update wfdemu01 ;
016200090203         read    wfdemu1l  ;
016300090203         enddo   ;
016400090203         endif   ;
016500080110
016600090203       // 3 ) Aggiornamento codici unificanti su WFDEMP da wfdemu
016700090203
0168000902040      if elabdempC='S' or elabcpo='S' or elabwadel='S'    ;
016900090203
017000090203       setll   *loval   wfdemu1l   ;
017100090203       read             wfdemu1l    ;
017200080110
017300090203 1     dow not %eof(wfdemu1l)   ;
017400080110
017500090203       kpiv=u1_dempiv    ;
017600090203       kcdf=u1_demcdf    ;
017700080110       clear   wtrov ;
017800090203 2     if kpiv<>*blanks  and %subst(kpiv:1:2)<>'$$'
017900090203                             AND kpiv<>'04507990150'        ;
018000090203
018100090203       chain   kpiv      WFDEmp1l  ;
018200090203 3     if %found(wfdemp1l) ;
018300080110       Wtrov='S' ;
018400090203 3     endif ;
018500080110
018600090203 3     if wtrov<>'S' ;
018700090203       chain   kpiv      WFDEmp2l  ;
018800090203 4     if %found(wfdemp2l) ;
018900080110       Wtrov='S' ;
019000090203 4     endif ;
019100090203 3     endif ;
019200090203 2     endif ;
019300080110
019400090203 2     if  kcdf<>*blanks  and kcdf<>'04507990150';
019500080110
019600090203 3     if wtrov<>'S' ;
019700090203       chain   kcdf    WFdemp1l  ;
019800090203 4     if %found(wfdemp1l ) ;
019900080110       Wtrov='S' ;
020000090203 4     endif ;
020100090203 3     endif ;
020200080110
020300090203 3     if wtrov<>'S' ;
020400090203       chain   kcdf    WFDEmp2l  ;
020500090203 4     if %found(wfdemp2l) ;
020600080110       Wtrov='S' ;
020700090203 4     endif ;
020800090203 3     endif ;
020900080110
021000090203 2     endif ;
021100080110
021200090203 2     if  wtrov='S'  ;
021300090203       chain  demcpo   wfdemp3l ;
021400080110                  select   ;
021500090203                  when demunif1=0       ;
021600090206                  demunif1=u1_demkscu         ;
021700090203                  when demunif2=0       ;
021800090206                  demunif2=u1_demkscu         ;
021900090203                  when demunif3=0       ;
022000090206                  demunif3=u1_demkscu       ;
022100090203                  when demunif4=0       ;
022200090206                  demunif4=u1_demkscu       ;
022300090203                  when demunif5=0       ;
022400090206                  demunif5=u1_demkscu       ;
022500090203                  when demunif6=0       ;
022600090206                  demunif6=u1_demkscu       ;
022700090203                  when demunif7=0       ;
022800090206                  demunif7=u1_demkscu       ;
022900090203                  when demunif8=0       ;
023000090206                  demunif8=u1_demkscu       ;
023100090203                  when demunif9=0       ;
023200090206                  demunif9=u1_demkscu       ;
023300090203                  when demunif0=0       ;
023400090206                  demunif0=u1_demkscu       ;
023500080110                  other          ;
023600090203                  demuniol='S'  ;
023700080110                  endsl  ;
023800080110
023900090203                demcli='S' ;
024000090203                update wfdemp03;
024100090203
024200090203                // updato flagg utilizzo cli unificante x demp
024300090203                u1_demusa='S'       ;
024400090203                update wfdemu01  ;
024500090203 2     endif  ;
024600080110
024700090203       read       wfdemu1l    ;
024800090203 1     enddo ;
024900080110
025000071217
025100090203       // 4 ) Aggiornamento codici unificanti su DEMP non trovati
025200090203       //             da cnaco
025300080110
025400090203       setll   *start    wfdemp0f ;
025500090203       read   wfdemp0f  ;
0256000902031       dow   not %eof(wfdemp0f)  ;
025700080110
025800071213
025900071213        xx=0  ;
026000080111        clear   uni ;
026100090203             uni(1) =   demunif1 ;
026200090203             uni(2) =   demunif2 ;
026300090203             uni(3) =   demunif3 ;
026400090203             uni(4) =   demunif4 ;
026500090203             uni(5) =   demunif5 ;
026600090203             uni(6) =   demunif6 ;
026700090203             uni(7) =   demunif7 ;
026800090203             uni(8) =   demunif8 ;
026900090203             uni(9) =   demunif9 ;
027000090203             uni(10) =  demunif0 ;
027100090203             giapre(1) =   demunif1 ;
027200090203             giapre(2) =   demunif2 ;
027300090203             giapre(3) =   demunif3 ;
027400090203             giapre(4) =   demunif4 ;
027500090203             giapre(5) =   demunif5 ;
027600090203             giapre(6) =   demunif6 ;
027700090203             giapre(7) =   demunif7 ;
027800090203             giapre(8) =   demunif8 ;
027900090203             giapre(9) =   demunif9 ;
028000090203             giapre(10) =  demunif0 ;
028100071212
028200071213         // Cerco con la partita IVA
028300090203         if   dempiv<>*blanks  and %subst(dempiv:1:2)<>'$$'
028400090203                               AND   dempiv<>'04507990150'        ;
028500090203         kcdf=dempiv ;
028600071213         EXSR    CERCAPIV  ;
028700071217         EXSR    CERCACDF  ;
028800071213 3       endif   ;
028900071213
029000071213         // Cerco con il codice fiscale
029100090203 3       if    demcdf<>*blanks   AND   demcdf<>'04507990150'   ;
029200090203         kcdf=demcdf ;
029300071213         EXSR    CERCACDF  ;
029400071217         EXSR    CERCAPIV  ;
029500071213 3       endif   ;
029600080110
029700080110       // Scrivo gli unificanti ed aggiorno se non ancora fatto altri
029800080110       //    menti cliente da eliminare
029900071213
030000080110 3     if   uni(1)>0 ;
030100080110        kcod= %editc(uni(1): 'X' ) ;
030200080110        wksc= cod(1)               ;
030300080110        exsr MEMECOD  ;
030400080110 3     endif   ;
030500080110       if   uni(2)>0 ;
030600080110        kcod= %editc(uni(2): 'X' ) ;
030700080110        wksc= cod(2)               ;
030800080110        exsr MEMECOD  ;
030900080110       endif   ;
031000080110       if   uni(3)>0 ;
031100080110        kcod= %editc(uni(3): 'X' ) ;
031200080110        wksc= cod(3)               ;
031300080110        exsr MEMECOD  ;
031400080110       endif   ;
031500080110       if   uni(4)>0 ;
031600080110        kcod= %editc(uni(4): 'X' ) ;
031700080110        wksc= cod(4)               ;
031800080110        exsr MEMECOD  ;
031900080110       endif   ;
032000080110       if   uni(5)>0 ;
032100080110        kcod= %editc(uni(5): 'X' ) ;
032200080110        wksc= cod(5)               ;
032300080110        exsr MEMECOD  ;
032400080110       endif   ;
032500080110       if   uni(6)>0 ;
032600080110        kcod= %editc(uni(6): 'X' ) ;
032700080110        wksc= cod(6)               ;
032800080110        exsr MEMECOD  ;
032900080110       endif   ;
033000080110       if   uni(7)>0 ;
033100080110        kcod= %editc(uni(7): 'X' ) ;
033200080110        wksc= cod(7)               ;
033300080110        exsr MEMECOD  ;
033400080110       endif   ;
033500080110       if   uni(8)>0 ;
033600080110        kcod= %editc(uni(8): 'X' ) ;
033700080110        wksc= cod(8)               ;
033800080110        exsr MEMECOD  ;
033900080110       endif   ;
034000080110       if   uni(9)>0 ;
034100080110        kcod= %editc(uni(9): 'X' ) ;
034200080110        wksc= cod(9)               ;
034300080110        exsr MEMECOD  ;
034400080110       endif   ;
034500080110 3     if   uni(10)>0 ;
034600080110        kcod= %editc(uni(10): 'X' ) ;
034700080110        wksc= cod(10)               ;
034800080110        exsr MEMECOD  ;
034900080110 3     endif   ;
035000080110
035100080102
035200090203       update wfdemp00 ;
035300080102
035400071212
035500090203       read   wfdemp00  ;
035600071213 1      enddo   ;
035700071218
035800090203 0      endif   ;
035900090204
036000090209       // 5 ) Aggiornamento dati di wadel  su WFDEMP
0361000902040      if elabdempD='S' or elabcpo='S' or elabwadel='S'    ;
036200090204       exsr DatiWADEL    ;
036300090204       endif   ;
036400090204
036500090209
036600090209       // 6 ) Aggiornamento codice potenziale per info su WFDEMU
036700090209       if elabdemuc='S'  and elabwadel=' '  ;
036800090209       setll *loval  wfdemu1l   ;
036900090209       read          wfdemu1l   ;
037000090209       dow   not %eof(wfdemu1l)  ;
037100090209       clear  u1_demcpoifo       ;
037200090209       clear  u1_demcpi          ;
037300090209       clear  u1_demcpip         ;
037400090209       clear  u1_demspt          ;
037500090209       clear  u1_demfac          ;
037600090209       clear  u1_demprf          ;
037700090209       clear  u1_dempro          ;
037800090209       clear  u1_demmes          ;
037900090209       clear  u1_demaer          ;
038000090209       clear  u1_demcae          ;
038100090209       clear  u1_dematr          ;
038200090209       clear  u1_dembar          ;
038300090209       clear  u1_demmez          ;
038400090209       clear  u1_demarco         ;
038500090209       clear  u1_demarto         ;
038600090209       clear  u1_demaws          ;
038700090209       clear  u1_demsusa         ;
038800090209       clear  u1_demtnt          ;
038900090209       clear  u1_demups          ;
039000090209       clear  u1_demcatr         ;
039100090209       clear  u1_dembars         ;
039200090209       clear  u1_demdhl          ;
039300090209       clear  u1_demexec         ;
039400090209       clear  u1_demferc         ;
039500090209       clear  u1_demmail         ;
039600090209       clear  u1_demmtn          ;
039700090209       clear  u1_demsda          ;
039800090209
039900090209       update wfdemu01  ;
040000090209
040100090209       read          wfdemu1l   ;
040200090209       enddo   ;
040300090209       endif  ;
040400090209
040500090209       if elabdemuc='S'  or elabwadel='S'  ;
040600090209       exsr AggioperINFO  ;
040700090209
040800090209       exsr AggiodatiInfo ;
040900090209       else   ;
041000090209
041100090209       if elabinfo='S'   ;
041200090209       exsr AggiodatiInfo ;
041300090209       endif  ;
041400090209       endif  ;
041500090209
041600071121       *inlr=*on         ;
041700071116       //-------------------------------------------------------------
041800090202       //  Scrittura dile mappatura base potenziali
041900090202       //-------------------------------------------------------------
042000090202       BEGSR  ScriviDEMP    ;
042100071122
042200090202       demcpo = cpocpo     ;
042300071122
042400090202       demrag  = cporag    ;
042500090202       demvia  = cpovia    ;
042600090202       demcit  = cpocit    ;
042700090202       demcap  = cpocap         ;
042800090202       demprv  = cpoprv    ;
042900090202       demnaz  = cponaz    ;
043000071121
043100090202       dempiv  = cpopiv    ;
043200090202       demcdf  = cpocdf     ;
043300090202       demtotfat=cpoftaz    ;
043400090202       demexport=cpopexp    ;
043500090202       demcmerc =%editc(cposct: 'X')     ;
043600090202       clear   kkey  ;
043700090203       kkey= demcmerc              ;
043800090202         chain (1: '1L': kkey) tabel00f ;
043900090202         if %found(tabel00f) ;
044000090202         demdmerc=tbluni   ;
044100090202         endif  ;
044200071122
044300071122       exsr   CALLTisi  ;
044400090210
044500090210       // Aggancio info spesa trasporti per prendere se c'e' la spesa reale
044600090210       chain (demcpo:'SPF')  ticpi01l   ;
044700090210 1     if %found(ticpi01l)    ;
044800090210         demsptrel=cpipft    ;
044900090210       endif   ;
045000090210
045100071122
045200090202       write  wfdeMP00       ;
045300071122
045400071116       ENDSR                ;
045500071122       //-------------------------------------------------------------
045600090202       // Chiamata al TISI per avere la filiale da cappario, no messag
045700090202       //-------------------------------------------------------------
045800071122       BEGSR  CALLTisi      ;
045900071122
046000071122       clear   tisi95ds  ;
046100071122       clear   fnlv13ds  ;
046200071122       i95tcn='7'        ;
046300071122       i13la3='S'     ;
046400090202       i95cap=demcap  ;
046500090202       i95loc=demcit  ;
046600090202       i95prv=demprv  ;
046700090202       i95nar=demnaz  ;
046800090202       // data di elaborazione: 1 gennaio anno in corso
046900090202       Annocor=%subdt(datasys:*y) ;
047000090203       i95dat=%int(%editc(annocor:'X') +'0101') ;
047100071122       i13af0='S'     ;
047200071129       i13cnv=' '     ;
047300071129       i13af1=' '     ;
047400071211
047500071122       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
047600071122
047700090202       demfil=o95lna       ;
047800071212       // se trovato imposto area
047900071213
0480000902021      if    demfil>0    ;
048100090203       chain   demfil   AZORG01L   ;
0482000902022      if %found(azorg01l) ;
048300090202         demdfil=orgdes   ;
048400090202         demare=orgcar ;
048500090206         demdiv=orgfl3 ;
0486000902022      endif  ;
048700080102
0488000902022      if demare>0                      ;
048900080102         clear   kkey  ;
049000090203         kkey=%editc(demare: 'X')    ;
049100080102         chain (1: '05': kkey) tabel00f ;
0492000902023        if %found(tabel00f) ;
049300090202          demdare=tbluni  ;
0494000902023        endif  ;
0495000902022        endif  ;
049600090206       if demdiv<>' ' ;
049700090206         clear   kkey  ;
049800090206         kkey=demdiv                 ;
049900090206         chain (1: '17': kkey) tabel00f ;
050000090206         if %found(tabel00f) ;
050100090206          demddiv=tbluni  ;
050200090206         endif  ;
050300090206       endif  ;
050400090206
0505000902021        endif  ;
050600071122
050700071122       ENDSR                ;
050800090202       //-------------------------------------------------------------
050900090202       //  Scrittura file mappatura base u8nificanti da WADEL
051000090202       //-------------------------------------------------------------
051100090202       BEGSR  ScriviDEMU    ;
051200090202
051300090205       setll  *loval  wfdemd3l  ;
051400090205       read    wfdemd3l ;
051500090202
051600090205 1     dow  not %eof(wfdemd3l);
051700090202
051800090202         // Escludo distretto estero   agente unificante
051900090202         wfil=%int(%subst(%editc(wapauc:'X'):1:3))     ;
052000090202         chain   wfil   azorg01l   ;
052100090202
052200090202 2a      if orgfl3<>'7'    ;
052300090202
052400090202         kcod = %editc(wapcli: 'X' ) ;
052500090202         // Escludo codici indirizzo italia ma esteri
052600090202            if kcod<>'0894106' and kcod<>'0894274' ;
052700090202
052800090202         // Escludo 8888  e 9999
052900090202 2         if (%subst(kcod:4:4)<>'9999' and %subst(kcod:4:4)<>'8888') ;
053000090202
053100090202         // Escludo senza fatturato  o senza spedizioni  anno corrente
053200090202 3          if   wapfac>0 and wapsac>0;
053300090202
053400090202             chain (kut:dutkci:wapcli)  cnclp00f ;
053500090202
053600090202 4          if wapblo=' ' ;
053700090202
053800090202             clear  wfdemu00   ;
053900090202
054000090202             demkscu=wapcli ;
054100090202             demrag =wapcld ;
054200090202             demcmmu=wapauc ;
054300090202             demdcmm=wapaud ;
054400090202             demclv =wapclv;
054500090202             select  ;
054600090202             when demclv='T'   ;
054700090202             demord='1'  ;
054800090202             when demclv='A'   ;
054900090202             demord='2'  ;
055000090202             when demclv='B'   ;
055100090202             demord='3'  ;
055200090202             when demclv='C'   ;
055300090202             demord='4'  ;
055400090202             when demclv='D'   ;
055500090202             demord='5'  ;
055600090202             other   ;
055700090203             demord='6'  ;
055800090202             endsl    ;
055900090202
056000090206             demvia =wapind    ;
056100090206             demcit =waploc    ;
056200090202             demcap =wapcap;
056300090202             demprv =wapprv;
056400090202             demnaz =wapnaz;
056500090202             dempiv =wapiva;
056600090202             demcdf =wapcdf;
056700090202             demdmerc=wapitc;
056800090202
056900090202               chain (kut:dutkci:wapcli)  cnaco00f ;
057000090202 5              if %found(cnaco00f) ;
057100090203                 demrag  =%triml(acorag)   ;
057200090203                 demcmerc=%subst(%editc(acoitc:'Z'):3:5)          ;
057300090202
057400090202             kkey=demcmerc               ;
057500090202             chain (1: '1L': kkey) tabel00f ;
057600090202 6           if %found(tabel00f) ;
057700090202              demdmerc=tbluni  ;
057800090202 6           endif  ;
057900090202 5           endif  ;
058000090202
058100090202             chain (kut:dutkci:wapcli)  cnind00f ;
058200090202 5           if %found(cnind00f) ;
058300090202             demcap =indcae;
058400090202             demprv =indprv;
058500090202             demnaz =indsta;
058600090202             dempiv =indiva;
058700090202             demcdf =indcdf;
058800090206             demvia =indvia    ;
058900090206             demcit =indcit   ;
059000090202 5         endif    ;
059100090202
059200090202           write(e) wfdemu00   ;
059300090202
059400090202 4         endif  ;
059500090202 4         endif  ;
059600090202 3         endif  ;
059700090202 2         endif  ;
059800090202 2a        endif  ;
059900090202
060000090205       read    wfdemd3l ;
060100090202 1     enddo ;
060200090202
060300090202       ENDSR   ;
060400071212
060500071212       //-------------------------------------------------------------
060600090203       //   cerco clienti unificanti con la partita iva
060700090203       //-------------------------------------------------------------
060800071213       BEGSR  CercaPIV      ;
060900071213
061000090203       setll  (kut:dutkci:kcdf)   cnind02l   ;
061100090203       reade  (kut:dutkci:kcdf)   cnind02l   ;
0612000712131      dow   not %eof(cnind02l)  ;
061300071213       wksc=indksc   ;
061400071213       exsr CALLTIBS   ;
061500071213
061600090203       reade  (kut:dutkci:kcdf)   cnind02l   ;
061700071213 1     enddo    ;
061800071213
061900071213       ENDSR                ;
062000071213       //-------------------------------------------------------------
062100090203       //   cerco clienti unificanti con la codice fiscale
062200090203       //-------------------------------------------------------------
062300071213       BEGSR  CercaCDF      ;
062400071213
062500090203       setll  (kut:dutkci:kCDF)   cnind03l   ;
062600090203       reade  (kut:dutkci:kcdf)   cnind03l   ;
0627000712131      dow   not %eof(cnind03l)  ;
062800071213       wksc=indksc   ;
062900071213       exsr CALLTIBS   ;
063000071213
063100090203       reade  (kut:dutkci:kcdf)   cnind03l   ;
063200071213 1     enddo    ;
063300071213
063400071213       ENDSR                ;
063500071213       //-------------------------------------------------------------
063600071213       BEGSR   CALLTIBS ;
063700071213
063800071213       clear  tibs10ds ;
063900071213       d10tle='ST' ;
064000071213       d10paf='F' ;
064100071213       d10cod=wksc   ;
064200090206       Annocor=%subdt(datasys:*y) ;
064300090206       d10drf=%int(%editc(annocor:'X') +'0101') ;
064400071213       callp   TIBS10R  (TIBS10ds:tipexe) ;
064500071213
064600071217 1       if d10err<>' ' ;
064700071213       d10err='  ' ;
064800071213       d10paf='P' ;
064900090206       Annocor=%subdt(datasys:*y) ;
065000090206       d10drf=%int(%editc(annocor:'X') +'0101') ;
065100071213       callp   TIBS10R  (TIBS10ds:tipexe) ;
065200071217 1       endif  ;
065300071213
065400071217 1       if d10err<>' '   ;
065500071213           wuni=wksc     ;
065600071213         else  ;
065700071213           wuni=d10cop ;
065800071217 1       endif  ;
065900071213
066000071213         trov=1 ;
066100071213         trov=%lookup (wuni:uni) ;
066200071213 2         if  trov=0   ;
066300071213           xx=xx+1     ;
066400080110 3            if xx<=100 ;
066500071213                   uni(XX)=wuni  ;
066600080110                   cod(XX)=wksc  ;
066700071213 3            endif     ;
066800071213 2         endif     ;
066900071213       ENDSR  ;
067000080110       //-------------------------------------------------------------
067100080110       BEGSR  MEMECOD       ;
067200080111       wuni=%int(kcod)  ;
067300090205       clear UsaUnific   ;
067400080111
067500080111         trov=%lookup (wuni:giapre) ;
067600090205       if trov=0 ;
067700080111
067800090203       chain   (wuni)      wfdemu1l  ;
067900090205
068000090205       // SE NON TROVATO VERIFICO SE c'e' negli unificati da
068100090205       //  wfdemd0f
068200090205       if not %found(wfdemu1l)   ;
068300090205         chain   (wuni)      wfdemd3l  ;
068400090205
068500090205 1       if %found(WFdemd3l) and wapunio='S';
068600090205         // richaino col suo unificato
068700090205           chain   (wapcliuni)  wfdemu1l  ;
068800090205           UsaUnific='S'    ;
068900090205         endif   ;
069000090205       endif   ;
069100080110
069200090203 1     if %found(WFdemu1l) ;
069300090203
069400090203 2     if u1_demusa<>' '    ;
069500080110
069600090203 3     if demerr=*blanks    ;
069700090203       demerr='+ CPO PER STESSO UNIFICANTE '+ %char(wksc) ;
069800080110 3     endif  ;
069900080110
070000080110 x2    ELSE  ;
070100090205       if UsaUnific=' ';
070200090205         u1_demusa='D' ;
070300090205       else   ;
070400090205         u1_demusa='U' ;
070500090205       endif   ;
070600090203       update wfdemu01   ;
070700080110  3               select   ;
070800090203                  when demunif1=0       ;
070900090203                  demunif1=%int(kcod)  ;
071000090203                  when demunif2=0       ;
071100090203                  demunif2=%int(kcod)  ;
071200090203                  when demunif3=0       ;
071300090203                  demunif3=%int(kcod)  ;
071400090203                  when demunif4=0       ;
071500090203                  demunif4=%int(kcod)  ;
071600090203                  when demunif5=0       ;
071700090203                  demunif5=%int(kcod)  ;
071800090203                  when demunif6=0       ;
071900090203                  demunif6=%int(kcod)  ;
072000090203                  when demunif7=0       ;
072100090203                  demunif7=%int(kcod)  ;
072200090203                  when demunif8=0       ;
072300090203                  demunif8=%int(kcod)  ;
072400090203                  when demunif9=0       ;
072500090203                  demunif9=%int(kcod)  ;
072600090203                  when demunif0=0       ;
072700090203                  demunif0=%int(kcod)  ;
072800080110                  other          ;
072900090203                  demuniol='S'  ;
073000080110  3               endsl  ;
073100080110
073200090203                demcli='S' ;
073300080110  2    endif  ;
073400080110  1    endif  ;
073500080111  1    endif  ;
073600080110
073700080110       ENDSR  ;
073800090204       //--------------------------------------------------------------
073900090204       //?Aggiorno file wfdemp con i dati di wadel
074000090204       //--------------------------------------------------------------
074100090204       BEGSR DatiWADEL   ;
074200090204
074300090204        clear   wfdemp00  ;
074400090204        clear maxfac ;
074500090204
074600090204        setll  *start    Wfdemp0f   ;
074700090204        read    wfdemp00    ;
074800090204
074900090204 1      dow  not  %eof(wfdemp0f)  ;
075000090204        uni(1)=demunif1 ;
075100090204        uni(2)=demunif2 ;
075200090204        uni(3)=demunif3 ;
075300090204        uni(4)=demunif4 ;
075400090204        uni(5)=demunif5 ;
075500090204        uni(6)=demunif6 ;
075600090204        uni(7)=demunif7 ;
075700090204        uni(8)=demunif8 ;
075800090204        uni(9)=demunif9 ;
075900090204        uni(10)=demunif0 ;
076000090204        xx=1 ;
076100090204
076200090204 2      dow   uni(xx) > 0 ;
076300090204
076400090204       wuni=uni(xx)   ;
076500090205       chain   wuni    wfdemi3l   ;
076600090204
076700090205 3      if %found(wfdemi3l)   ;
076800090204
076900090204        iwapita='I'  ;
077000090204        iwapsap= wapsap+iwapsap  ;
077100090204        iwapfap= wapfap+iwapfap ;
077200090204        iwapsac= wapsac+iwapsac  ;
077300090204        iwapfac= wapfac+iwapsap  ;
077400090204
077500090204        iwaptotrpp=waprp_01ap+waprp_02ap+waprp_03ap+waprp_04ap+waprp_05ap+
077600090204                   waprp_06ap+waprp_07ap+waprp_08ap+waprp_09ap+waprp_10ap+
077700090204                   waprp_11ap+waprp_12ap + iwaptotrpp  ;
077800090204        iwaptotrpc=waprp_01ac+waprp_02ac+waprp_03ac+waprp_04ac+waprp_05ac+
077900090204                   waprp_06ac+waprp_07ac+waprp_08ac+waprp_09ac+waprp_10ac+
078000090204                   waprp_11ac+waprp_12ac + iwaptotrpc ;
078100090204        iwapns01ac=wapns_01ac + iwapns01ac  ;
078200090204        iwapns02ac=wapns_02ac + iwapns02ac  ;
078300090204        iwapns03ac=wapns_03ac + iwapns03ac  ;
078400090204        iwapns04ac=wapns_04ac + iwapns04ac  ;
078500090204        iwapns05ac=wapns_05ac + iwapns05ac  ;
078600090204        iwapns06ac=wapns_06ac + iwapns06ac  ;
078700090204        iwapns07ac=wapns_07ac + iwapns07ac  ;
078800090204        iwapns08ac=wapns_08ac + iwapns08ac  ;
078900090204        iwapns09ac=wapns_09ac + iwapns09ac  ;
079000090204        iwapns10ac=wapns_10ac + iwapns10ac  ;
079100090204        iwapns11ac=wapns_11ac + iwapns11ac  ;
079200090204        iwapns12ac=wapns_12ac + iwapns12ac  ;
079300090204
079400090204        iwaprr01ac=waprr_01ac + iwaprr01ac  ;
079500090204        iwaprr02ac=waprr_02ac + iwaprr02ac  ;
079600090204        iwaprr03ac=waprr_03ac + iwaprr03ac  ;
079700090204        iwaprr04ac=waprr_04ac + iwaprr04ac  ;
079800090204        iwaprr05ac=waprr_05ac + iwaprr05ac  ;
079900090204        iwaprr06ac=waprr_06ac + iwaprr06ac  ;
080000090204        iwaprr07ac=waprr_07ac + iwaprr07ac  ;
080100090204        iwaprr08ac=waprr_08ac + iwaprr08ac  ;
080200090204        iwaprr09ac=waprr_09ac + iwaprr09ac  ;
080300090204        iwaprr10ac=waprr_10ac + iwaprr10ac  ;
080400090204        iwaprr11ac=waprr_11ac + iwaprr11ac  ;
080500090204        iwaprr12ac=waprr_12ac + iwaprr12ac  ;
080600090204
080700090204        iwaprp01ac=waprp_01ac + iwaprp01ac  ;
080800090204        iwaprp02ac=waprp_02ac + iwaprp02ac  ;
080900090204        iwaprp03ac=waprp_03ac + iwaprp03ac  ;
081000090204        iwaprp04ac=waprp_04ac + iwaprp04ac  ;
081100090204        iwaprp05ac=waprp_05ac + iwaprp05ac  ;
081200090204        iwaprp06ac=waprp_06ac + iwaprp06ac  ;
081300090204        iwaprp07ac=waprp_07ac + iwaprp07ac  ;
081400090204        iwaprp08ac=waprp_08ac + iwaprp08ac  ;
081500090204        iwaprp09ac=waprp_09ac + iwaprp09ac  ;
081600090204        iwaprp10ac=waprp_10ac + iwaprp10ac  ;
081700090204        iwaprp11ac=waprp_11ac + iwaprp11ac  ;
081800090204        iwaprp12ac=waprp_12ac + iwaprp12ac  ;
081900090204 3      endif  ;
082000090204
082100090205       chain   wuni    wfdeme3l   ;
082200090204
082300090205 3      if %found(wfdeme3l)   ;
082400090204        ewapest='E'  ;
082500090204        ewapsap= wapsap+ewapsap  ;
082600090204        ewapfap= wapfap+ewapfap ;
082700090204        ewapsac= wapsac+ewapsac  ;
082800090204        ewapfac= wapfac+ewapsap  ;
082900090204
083000090204        ewaptotrpp=waprp_01ap+waprp_02ap+waprp_03ap+waprp_04ap+waprp_05ap+
083100090204                   waprp_06ap+waprp_07ap+waprp_08ap+waprp_09ap+waprp_10ap+
083200090204                   waprp_11ap+waprp_12ap + ewaptotrpp  ;
083300090204        ewaptotrpc=waprp_01ac+waprp_02ac+waprp_03ac+waprp_04ac+waprp_05ac+
083400090204                   waprp_06ac+waprp_07ac+waprp_08ac+waprp_09ac+waprp_10ac+
083500090204                   waprp_11ac+waprp_12ac + ewaptotrpc ;
083600090204        ewapns01ac=wapns_01ac + ewapns01ac  ;
083700090204        ewapns02ac=wapns_02ac + ewapns02ac  ;
083800090204        ewapns03ac=wapns_03ac + ewapns03ac  ;
083900090204        ewapns04ac=wapns_04ac + ewapns04ac  ;
084000090204        ewapns05ac=wapns_05ac + ewapns05ac  ;
084100090204        ewapns06ac=wapns_06ac + ewapns06ac  ;
084200090204        ewapns07ac=wapns_07ac + ewapns07ac  ;
084300090204        ewapns08ac=wapns_08ac + ewapns08ac  ;
084400090204        ewapns09ac=wapns_09ac + ewapns09ac  ;
084500090204        ewapns10ac=wapns_10ac + ewapns10ac  ;
084600090204        ewapns11ac=wapns_11ac + ewapns11ac  ;
084700090204        ewapns12ac=wapns_12ac + ewapns12ac  ;
084800090204
084900090204        ewaprr01ac=waprr_01ac + ewaprr01ac  ;
085000090204        ewaprr02ac=waprr_02ac + ewaprr02ac  ;
085100090204        ewaprr03ac=waprr_03ac + ewaprr03ac  ;
085200090204        ewaprr04ac=waprr_04ac + ewaprr04ac  ;
085300090204        ewaprr05ac=waprr_05ac + ewaprr05ac  ;
085400090204        ewaprr06ac=waprr_06ac + ewaprr06ac  ;
085500090204        ewaprr07ac=waprr_07ac + ewaprr07ac  ;
085600090204        ewaprr08ac=waprr_08ac + ewaprr08ac  ;
085700090204        ewaprr09ac=waprr_09ac + ewaprr09ac  ;
085800090204        ewaprr10ac=waprr_10ac + ewaprr10ac  ;
085900090204        ewaprr11ac=waprr_11ac + ewaprr11ac  ;
086000090204        ewaprr12ac=waprr_12ac + ewaprr12ac  ;
086100090204
086200090204        ewaprp01ac=waprp_01ac + ewaprp01ac  ;
086300090204        ewaprp02ac=waprp_02ac + ewaprp02ac  ;
086400090204        ewaprp03ac=waprp_03ac + ewaprp03ac  ;
086500090204        ewaprp04ac=waprp_04ac + ewaprp04ac  ;
086600090204        ewaprp05ac=waprp_05ac + ewaprp05ac  ;
086700090204        ewaprp06ac=waprp_06ac + ewaprp06ac  ;
086800090204        ewaprp07ac=waprp_07ac + ewaprp07ac  ;
086900090204        ewaprp08ac=waprp_08ac + ewaprp08ac  ;
087000090204        ewaprp09ac=waprp_09ac + ewaprp09ac  ;
087100090204        ewaprp10ac=waprp_10ac + ewaprp10ac  ;
087200090204        ewaprp11ac=waprp_11ac + ewaprp11ac  ;
087300090204        ewaprp12ac=waprp_12ac + ewaprp12ac  ;
087400090204 3      endif  ;
087500090204
087600090204        // Dati generici del cliente con maggior fatturato se + di uno  ;
0877000902043       if  iwapfac>maxfac ;
087800090204        maxfac=iwapfac     ;
087900090204        cwapfil=wapfil        ;
088000090204        cwapfid=wapfid        ;
088100090204        cwapage=wapage        ;
088200090204        cwapagd=wapagd        ;
088300090204        cwapitc=wapitc        ;
088400090204        cwapclv=wapclv        ;
088500090204        cwapblo=wapblo        ;
088600090204        clear cwapare ;
088700090204        clear cwapard ;
088800090204
088900090204       chain  cwapfil   AZORG01L   ;
0890000902044      if %found(azorg01l) ;
089100090204         cwapare=orgcar ;
089200090204
089300090204         clear   kkey  ;
089400090204         kkey=%editc(cwapare: 'X')    ;
089500090204         chain (1: '05': kkey) tabel00f ;
0896000902045        if %found(tabel00f) ;
089700090204          cwapard=tbluni  ;
0898000902045        endif  ;
0899000902044        endif  ;
0900000902043        endif  ;
090100090204
090200090204        xx=xx+1  ;
090300090204 2      enddo   ;
090400090204
090500090204        update  wfdemp00  ;
090600090204        clear   wfdemp00  ;
090700090204        clear maxfac ;
090800090204
090900090204       read   wfdemp00  ;
091000090204 1     enddo   ;
091100090204
091200090204       ENDSR  ;
091300090203
091400090203       //--------------------------------------------------------------
091500090203       //?Reperimento Dati del job (Utente/Operativi).
091600090203       //--------------------------------------------------------------
091700090203       BEGSR DatiJob;
091800090203
091900090203         in(E) §AzUte;
092000090203         if NOT %error;
092100090203           in(E) §DatiUte;
092200090203         endif;
092300090203         if %error or RSut = *blanks;
092400090203           clear TIBS34ds;
092500090203           tibs34r(tibs34ds);
092600090203           in §AzUte;
092700090203           in §DatiUte;
092800090203         endif;
092900090203
093000090203       ENDSR;
093100090206       //--------------------------------------------------------------
093200090206       //?pulizia campi aggioranti da WADEL
093300090206       //--------------------------------------------------------------
093400090206        BEGSR cleardaWADEL   ;
093500090206        clear demunif1       ;
093600090206        clear demunif2       ;
093700090206        clear demunif3       ;
093800090206        clear demunif4       ;
093900090206        clear demunif5       ;
094000090206        clear demunif6       ;
094100090206        clear demunif7       ;
094200090206        clear demunif8       ;
094300090206        clear demunif9       ;
094400090206        clear demunif0       ;
094500090206        clear demuniol       ;
094600090206        clear demcli         ;
094700090206        clear demerr         ;
094800090206        clear iwapita ;
094900090206        clear iwapsap ;
095000090206        clear iwapfap ;
095100090206        clear iwaptotrpp ;
095200090206        clear iwapsac ;
095300090206        clear iwapfac ;
095400090206        clear iwaptotrpc ;
095500090206        clear iwapns01ac ;
095600090206        clear iwaprr01ac ;
095700090206        clear iwaprp01ac ;
095800090206        clear iwapns02ac ;
095900090206        clear iwaprr02ac ;
096000090206        clear iwaprp02ac ;
096100090206        clear iwapns03ac ;
096200090206        clear iwaprr03ac ;
096300090206        clear iwaprp03ac ;
096400090206        clear iwapns04ac ;
096500090206        clear iwaprr04ac ;
096600090206        clear iwaprp04ac ;
096700090206        clear iwapns05ac ;
096800090206        clear iwaprr05ac ;
096900090206        clear iwaprp05ac ;
097000090206        clear iwapns06ac ;
097100090206        clear iwaprr06ac ;
097200090206        clear iwaprp06ac ;
097300090206        clear iwapns07ac ;
097400090206        clear iwaprr07ac ;
097500090206        clear iwaprp07ac ;
097600090206        clear iwapns08ac ;
097700090206        clear iwaprr08ac ;
097800090206        clear iwaprp08ac ;
097900090206        clear iwapns09ac ;
098000090206        clear iwaprr09ac ;
098100090206        clear iwaprp09ac ;
098200090206        clear iwapns10ac ;
098300090206        clear iwaprr10ac ;
098400090206        clear iwaprp10ac ;
098500090206        clear iwapns11ac ;
098600090206        clear iwaprr11ac ;
098700090206        clear iwaprp11ac ;
098800090206        clear iwapns12ac ;
098900090206        clear iwaprr12ac ;
099000090206        clear iwaprp12ac ;
099100090206
099200090206        clear ewapest ;
099300090206        clear ewapsap ;
099400090206        clear ewapfap ;
099500090206        clear ewaptotrpp ;
099600090206        clear ewapsac ;
099700090206        clear ewapfac ;
099800090206        clear ewaptotrpc ;
099900090206        clear ewapns01ac ;
100000090206        clear ewaprr01ac ;
100100090206        clear ewaprp01ac ;
100200090206        clear ewapns02ac ;
100300090206        clear ewaprr02ac ;
100400090206        clear ewaprp02ac ;
100500090206        clear ewapns03ac ;
100600090206        clear ewaprr03ac ;
100700090206        clear ewaprp03ac ;
100800090206        clear ewapns04ac ;
100900090206        clear ewaprr04ac ;
101000090206        clear ewaprp04ac ;
101100090206        clear ewapns05ac ;
101200090206        clear ewaprr05ac ;
101300090206        clear ewaprp05ac ;
101400090206        clear ewapns06ac ;
101500090206        clear ewaprr06ac ;
101600090206        clear ewaprp06ac ;
101700090206        clear ewapns07ac ;
101800090206        clear ewaprr07ac ;
101900090206        clear ewaprp07ac ;
102000090206        clear ewapns08ac ;
102100090206        clear ewaprr08ac ;
102200090206        clear ewaprp08ac ;
102300090206        clear ewapns09ac ;
102400090206        clear ewaprr09ac ;
102500090206        clear ewaprp09ac ;
102600090206        clear ewapns10ac ;
102700090206        clear ewaprr10ac ;
102800090206        clear ewaprp10ac ;
102900090206        clear ewapns11ac ;
103000090206        clear ewaprr11ac ;
103100090206        clear ewaprp11ac ;
103200090206        clear ewapns12ac ;
103300090206        clear ewaprr12ac ;
103400090206        clear ewaprp12ac ;
103500090206
103600090206        clear cwapare    ;
103700090206        clear cwapard    ;
103800090206        clear cwapfil    ;
103900090206        clear cwapfid    ;
104000090206        clear cwapage    ;
104100090206        clear cwapagd    ;
104200090206        clear cwapblo    ;
104300090206        clear cwapitc    ;
104400090206        clear cwapclv    ;
104500090206        ENDSR   ;
104600090209       //-------------------------------------------------------------
104700090209       //  Aggiorno cod potenziale su WFDEMU0FF per le info
104800090209       //-------------------------------------------------------------
104900090209       BEGSR AggioperInfo   ;
105000090209       setll *loval   wfdemu1l  ;
105100090209       read           wfdemu1l  ;
105200090209       dow   not %eof(wfdemu1l)  ;
105300090209
105400090209           EXSR CercaPotSede   ;
105500090209
105600090209           xx=1   ;
105700090209           clear savfat   ;
105800090209
105900090209           dow potsede(xx)>0  ;
106000090209           // di quelli letti, memorizzo quello che ha le info
106100090209           //  col fatturato D&B più alto
106200090209
106300090209           if potinfo(XX)='S' or potinfop(xx)='S'  ;
106400090209             if  potfat(XX) >=savfat   ;
106500090209              u1_demcpoifo=potsede(XX) ;
106600090209              u1_demcpi   =potinfo(XX) ;
106700090209              u1_demcpip  =potinfop(XX) ;
106800090209              savfat=potfat(xx)   ;
106900090209             endif  ;
107000090209           endif  ;
107100090209
107200090209           xx=xx+1  ;
107300090209           enddo    ;
107400090209
107500090209       update wfdemu01  ;
107600090209
107700090209       read           wfdemu1l  ;
107800090209       enddo   ;
107900090209       ENDSR   ;
108000090209
108100090209       //-------------------------------------------------------------
108200090209       //?Cerca i potenziali sede associati  all'unificante
108300090209       //--------------------------------------------------------------
108400090209       BEGSR  CercaPotSede ;
108500090209
108600090209       // Prendo codice unificante e carico cod collegati
108700090209           clear tibs10ds ;
108800090209           d10tle='ST'   ;
108900090209           d10paf='F'    ;
109000090209           d10cod=u1_demkscu;
109100090209           callp TIBS10R (tibs10ds:tipexe)  ;
109200090209
109300090209  1        if d10err<>*blanks   ;
109400090209           // Se errore verifico se è figlio
109500090209           clear tibs10ds ;
109600090209           d10tle='ST'   ;
109700090209           d10paf='P'    ;
109800090209           d10cod=u1_demkscu;
109900090209           callp TIBS10R (tibs10ds:tipexe) ;
110000090209
110100090209  2        if d10err<>*blanks   ;
110200090209           // Imposto nella skiera e come padre solo se stesso
110300090209           skc(1)='0000'+%editc(u1_demkscu:'X');
110400090209           d10cop=u1_demkscu                  ;
110500090209  2        endif                ;
110600090209  1        endif ;
110700090209
110800090209       clear potsede  ;
110900090209       clear potfat   ;
111000090209       clear potinfo  ;
111100090209       clear potinfop ;
111200090209       clear xx       ;
111300090209       yy=1    ;
111400090209
111500090209 1     dow skc(yy)>*Zeros  ;
111600090209
111700090209       // Verifico se cnaco ha un codice SEDE attaccato ;
111800090209       wksc=%int(%subst(skc(yy):5:7)) ;
111900090209       chain   (1:dutkci:wksc)  cnaco00f  ;
112000090209 2     if %found(cnaco00f)   ;
112100090209
112200090209         chain  acolib   tncpo01l   ;
112300090209
112400090209 3       if %found(tncpo01l) and cpofsf='S'  ;
112500090209         exsr MemPotSede     ;
112600090209 3       endif     ;
112700090209 2     endif     ;
112800090209
112900090209       yy=yy+1   ;
113000090209 1     enddo     ;
113100090209
113200090209       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
113300090209       //  codice unificante, altri potenziali  SEDE
113400090209       wksc=d10cop;
113500090209       chain   (1:dutkci:wksc)  cnclp00f  ;
113600090209 1     if %found(cnclp00f)   ;
113700090209
113800090209       // col sottoconti int fattura
113900090209       chain   (1:dutkci:clpscf)  cnind00f  ;
114000090209 2     if %found(cnind00f) and indsta=*blanks  ;
114100090209         // con la partita iva
114200090209 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
114300090209                             AND INDIVA<>'04507990150'        ;
114400090209         setll indiva   tncpo06l  ;
114500090209         reade indiva   tncpo06l  ;
114600090209
114700090209 3         dow not %eof(tncpo06l)   ;
114800090209 4         if cpofsf='S'          ;
114900090209              exsr MempotSede       ;
115000090209 4         endif                  ;
115100090209
115200090209           reade indiva   tncpo06l  ;
115300090209 3       enddo                    ;
115400090209 2a      endif    ;
115500090209
115600090209         // con il codice fiscale
115700090209 2a      if indcdf<>*blanks       ;
115800090209         setll indcdf   tncpo05l  ;
115900090209         reade indcdf   tncpo05l  ;
116000090209
116100090209 3         dow not %eof(tncpo05l)   ;
116200090209 4         if cpofsf='S'          ;
116300090209              exsr MempotSede      ;
116400090209 4         endif                  ;
116500090209
116600090209           reade indcdf   tncpo05l  ;
116700090209 3       enddo                    ;
116800090209 2a    endif                  ;
116900090209 2     endif                  ;
117000090209 1     endif                  ;
117100090209
117200090209       ENDSR  ;
117300090209       //--------------------------------------------------------------
117400090209       //?Memorizza potenziali sede per ogni unificante
117500090209       //--------------------------------------------------------------
117600090209       BEGSR MemPotSede;
117700090209
117800090209         // Memorizzo i codici visualizzati per evitare ripetizioni
117900090209         //           solo se ha le info complete o parziali
118000090209
118100090209         Indx=%lookup(cpocpo:potsede)   ;
118200090209 1         if Indx=0    ;
118300090209           dcpo01=cporst  ;
118400090209
118500090209           clear trmk50ds   ;
118600090209           // Memorizzo solo se ha le info complete o parziali
118700090209 2           if §cpoifotot <>'S'    ;
118800090209                    i50cpo=cpocpo     ;
118900090209                    i50mod='C'        ;
119000090209                    i50obl='S'        ;
119100090209                    callp TRMK50R (kpjba:trmk50ds)  ;
119200090209  2          endif   ;
119300090209
119400090209  2          if §cpoifotot = 'S'    or o50ifomin='S' ;
119500090209           xx=xx+1  ;
119600090209           potsede(xx)=cpocpo   ;
119700090209           potfat (xx)=cpoftaz  ;
119800090209           potinfo(xx)=§cpoifotot ;
119900090209           potinfop(xx)=o50ifomin  ;
120000090209
120100090209 2         endif;
120200090209 1         endif;
120300090209           ENDSR  ;
120400090209       //--------------------------------------------------------------
120500090209       //?Aggiorno dati delle info trovati
120600090209       //--------------------------------------------------------------
120700090209       BEGSR AggioDatiInfo  ;
120800090209       setll *loval  wfdemu1l   ;
120900090209       read          wfdemu1l   ;
121000090209       dow   not %eof(wfdemu1l)  ;
121100090209
121200090209       if u1_demcpoifo>0  ;
121300090209       Exsr LeggiCPI  ;
121400090209       endif   ;
121500090209
121600090209       read          wfdemu1l   ;
121700090209       enddo   ;
121800090209       ENDSR  ;
121900090209       //--------------------------------------------------------------
122000090209       //?lettura info commerciali
122100090209       //--------------------------------------------------------------
122200090209       BEGSR LeggiCPI      ;
122300090209       // spesa trasporti
122400090209       chain (u1_demcpoifo:'SPF')  ticpi01l   ;
122500090209 1     if %found(ticpi01l)    ;
122600090209       demspt=cpipft    ;
122700090209
122800090209       //  se trovata spesa faccio la ripartizione
122900090209       setll u1_demcpoifo  ticpi01l   ;
123000090209       reade u1_demcpoifo  ticpi01l   ;
123100090209
123200090209 2     dow  not %eof(ticpi01l)  ;
123300090209
123400090209 3     if  cpival>0 and cpitva='%'   ;
123500090209       ripSpesa=(demspt*cpival)/100   ;
123600090209
123700090209 4     if cpitpf='10 '  ;
123800090209 5     select   ;
123900090209       when cpispf='_BAR'   ;
124000090209       dembar=ripspesa ;
124100090209
124200090209       when cpispf='_MEZ'   ;
124300090209       demmez=ripspesa ;
124400090209
124500090209       when cpispf='ARCO'   ;
124600090209       demARCO=ripspesa ;
124700090209
124800090209       when cpispf='ARTO'   ;
124900090209       demARTO=ripspesa ;
125000090209
125100090209       when cpispf='AWS'   ;
125200090209       demAWS=ripspesa ;
125300090209
125400090209       when cpispf='BARS'   ;
125500090209       dembarS=ripspesa ;
125600090209
125700090209       when cpispf='DHL'   ;
125800090209       demDHL=ripspesa ;
125900090209
126000090209       when cpispf='EXEC'   ;
126100090209       demEXEC=ripspesa ;
126200090209
126300090209       when cpispf='FERC'   ;
126400090209       demFERC=ripspesa ;
126500090209
126600090209       when cpispf='MAIL'   ;
126700090209       demMAIL=ripspesa ;
126800090209
126900090209       when cpispf='MTN'   ;
127000090209       demMTN=ripspesa ;
127100090209
127200090209       when cpispf='SDA'   ;
127300090209       demSDA=ripspesa ;
127400090209
127500090209       when cpispf='SUSA'   ;
127600090209       demSUSA=ripspesa ;
127700090209
127800090209       when cpispf='TNT'   ;
127900090209       demTNT=ripspesa ;
128000090209
128100090209       when cpispf='UPS'   ;
128200090209       demUPS=ripspesa ;
128300090209
128400090209       when cpispf='9ATR'   ;
128500090209       demCATR=ripspesa ;
128600090209 5     ENDSL  ;
128700090209
128800090209 x4    else  ;
128900090209 5     select   ;
129000090209       when cpitpf='PRF'   ;
129100090209       demprf=ripspesa ;
129200090209
129300090209       when cpitpf='PRO'   ;
129400090209       dempro=ripspesa ;
129500090209
129600090209       when cpitpf='MES'   ;
129700090209       demMES=ripspesa ;
129800090209
129900090209       when cpitpf='AER'   ;
130000090209       demAER=ripspesa ;
130100090209
130200090209       when cpitpf='CAE'   ;
130300090209       demCAE=ripspesa ;
130400090209
130500090209       when cpitpf='ATR'   ;
130600090209       demATR=ripspesa ;
130700090209 5     ENDSL  ;
130800090209
130900090209 4     ENDif  ;
131000090209 3     ENDif  ;
131100090209
131200090209       reade u1_demcpoifo  ticpi01l   ;
131300090209 2     enddo    ;
131400090209
131500090209       UPDATE wfdemu01  ;
131600090209 1     endif  ;
131700090209
131800090209       ENDSR  ;
131900090203
132000090203
132100071116      /end-free
