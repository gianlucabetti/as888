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
001900090409     Fwfdeme3l  iF   E           k DISK    rename(wadelpc0:wfdeme03)
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
012400090319         // Escludo la bartolini, gli indirizzi esteri
012500090202         if cpoduns<>*blanks and cpoduns<>'431338995'  and
012600090319                                 cpoftaz>0  and cpofsf='S' and cponaz='   '  ;
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
015100090210            exec sql   delete  from  wfdemU0f   ;
015200090210
015300090203            exsr     ScriviDEMU  ;
015400090203        endif  ;
015500090203
015600090203       // 2b) Se non devo elaborare, sflago solo flag utilizzo
015700090203
015800090206        if  elabWADEL=' ' and (elabDEMPC= 'S' or elabCPO='S')  ;
015900090203         setll  *loval wfdemu1l ;
016000090203         read          wfdemu1l  ;
0161000902031        dow   not %eof(wfdemu1l)  ;
016200090203           clear u1_demusa         ;
016300090203          update wfdemu01 ;
016400090203         read    wfdemu1l  ;
016500090203         enddo   ;
016600090203         endif   ;
016700080110
016800090203       // 3 ) Aggiornamento codici unificanti su WFDEMP da wfdemu
016900090203
0170000902040      if elabdempC='S' or elabcpo='S' or elabwadel='S'    ;
017100090203
017200090203       setll   *loval   wfdemu1l   ;
017300090203       read             wfdemu1l    ;
017400080110
017500090203 1     dow not %eof(wfdemu1l)   ;
017600080110
017700090203       kpiv=u1_dempiv    ;
017800090203       kcdf=u1_demcdf    ;
017900080110       clear   wtrov ;
018000090203 2     if kpiv<>*blanks  and %subst(kpiv:1:2)<>'$$'
018100090203                             AND kpiv<>'04507990150'        ;
018200090203
018300090203       chain   kpiv      WFDEmp1l  ;
018400090203 3     if %found(wfdemp1l) ;
018500080110       Wtrov='S' ;
018600090203 3     endif ;
018700080110
018800090203 3     if wtrov<>'S' ;
018900090203       chain   kpiv      WFDEmp2l  ;
019000090203 4     if %found(wfdemp2l) ;
019100080110       Wtrov='S' ;
019200090203 4     endif ;
019300090203 3     endif ;
019400090203 2     endif ;
019500080110
019600090203 2     if  kcdf<>*blanks  and kcdf<>'04507990150';
019700080110
019800090203 3     if wtrov<>'S' ;
019900090203       chain   kcdf    WFdemp1l  ;
020000090203 4     if %found(wfdemp1l ) ;
020100080110       Wtrov='S' ;
020200090203 4     endif ;
020300090203 3     endif ;
020400080110
020500090203 3     if wtrov<>'S' ;
020600090203       chain   kcdf    WFDEmp2l  ;
020700090203 4     if %found(wfdemp2l) ;
020800080110       Wtrov='S' ;
020900090203 4     endif ;
021000090203 3     endif ;
021100080110
021200090203 2     endif ;
021300080110
021400090203 2     if  wtrov='S'  ;
021500090203       chain  demcpo   wfdemp3l ;
021600080110                  select   ;
021700090203                  when demunif1=0       ;
021800090206                  demunif1=u1_demkscu         ;
021900090203                  when demunif2=0       ;
022000090206                  demunif2=u1_demkscu         ;
022100090203                  when demunif3=0       ;
022200090206                  demunif3=u1_demkscu       ;
022300090203                  when demunif4=0       ;
022400090206                  demunif4=u1_demkscu       ;
022500090203                  when demunif5=0       ;
022600090206                  demunif5=u1_demkscu       ;
022700090203                  when demunif6=0       ;
022800090206                  demunif6=u1_demkscu       ;
022900090203                  when demunif7=0       ;
023000090206                  demunif7=u1_demkscu       ;
023100090203                  when demunif8=0       ;
023200090206                  demunif8=u1_demkscu       ;
023300090203                  when demunif9=0       ;
023400090206                  demunif9=u1_demkscu       ;
023500090203                  when demunif0=0       ;
023600090206                  demunif0=u1_demkscu       ;
023700080110                  other          ;
023800090203                  demuniol='S'  ;
023900080110                  endsl  ;
024000080110
024100090203                demcli='S' ;
024200090203                update wfdemp03;
024300090203
024400090203                // updato flagg utilizzo cli unificante x demp
024500090203                u1_demusa='S'       ;
024600090203                update wfdemu01  ;
024700090203 2     endif  ;
024800080110
024900090203       read       wfdemu1l    ;
025000090203 1     enddo ;
025100080110
025200071217
025300090203       // 4 ) Aggiornamento codici unificanti su DEMP non trovati
025400090203       //             da cnaco
025500080110
025600090203       setll   *start    wfdemp0f ;
025700090203       read   wfdemp0f  ;
0258000902031       dow   not %eof(wfdemp0f)  ;
025900080110
026000071213
026100071213        xx=0  ;
026200080111        clear   uni ;
026300090203             uni(1) =   demunif1 ;
026400090203             uni(2) =   demunif2 ;
026500090203             uni(3) =   demunif3 ;
026600090203             uni(4) =   demunif4 ;
026700090203             uni(5) =   demunif5 ;
026800090203             uni(6) =   demunif6 ;
026900090203             uni(7) =   demunif7 ;
027000090203             uni(8) =   demunif8 ;
027100090203             uni(9) =   demunif9 ;
027200090203             uni(10) =  demunif0 ;
027300090203             giapre(1) =   demunif1 ;
027400090203             giapre(2) =   demunif2 ;
027500090203             giapre(3) =   demunif3 ;
027600090203             giapre(4) =   demunif4 ;
027700090203             giapre(5) =   demunif5 ;
027800090203             giapre(6) =   demunif6 ;
027900090203             giapre(7) =   demunif7 ;
028000090203             giapre(8) =   demunif8 ;
028100090203             giapre(9) =   demunif9 ;
028200090203             giapre(10) =  demunif0 ;
028300071212
028400071213         // Cerco con la partita IVA
028500090203         if   dempiv<>*blanks  and %subst(dempiv:1:2)<>'$$'
028600090203                               AND   dempiv<>'04507990150'        ;
028700090203         kcdf=dempiv ;
028800071213         EXSR    CERCAPIV  ;
028900071217         EXSR    CERCACDF  ;
029000071213 3       endif   ;
029100071213
029200071213         // Cerco con il codice fiscale
029300090203 3       if    demcdf<>*blanks   AND   demcdf<>'04507990150'   ;
029400090203         kcdf=demcdf ;
029500071213         EXSR    CERCACDF  ;
029600071217         EXSR    CERCAPIV  ;
029700071213 3       endif   ;
029800080110
029900080110       // Scrivo gli unificanti ed aggiorno se non ancora fatto altri
030000080110       //    menti cliente da eliminare
030100071213
030200080110 3     if   uni(1)>0 ;
030300080110        kcod= %editc(uni(1): 'X' ) ;
030400080110        wksc= cod(1)               ;
030500080110        exsr MEMECOD  ;
030600080110 3     endif   ;
030700080110       if   uni(2)>0 ;
030800080110        kcod= %editc(uni(2): 'X' ) ;
030900080110        wksc= cod(2)               ;
031000080110        exsr MEMECOD  ;
031100080110       endif   ;
031200080110       if   uni(3)>0 ;
031300080110        kcod= %editc(uni(3): 'X' ) ;
031400080110        wksc= cod(3)               ;
031500080110        exsr MEMECOD  ;
031600080110       endif   ;
031700080110       if   uni(4)>0 ;
031800080110        kcod= %editc(uni(4): 'X' ) ;
031900080110        wksc= cod(4)               ;
032000080110        exsr MEMECOD  ;
032100080110       endif   ;
032200080110       if   uni(5)>0 ;
032300080110        kcod= %editc(uni(5): 'X' ) ;
032400080110        wksc= cod(5)               ;
032500080110        exsr MEMECOD  ;
032600080110       endif   ;
032700080110       if   uni(6)>0 ;
032800080110        kcod= %editc(uni(6): 'X' ) ;
032900080110        wksc= cod(6)               ;
033000080110        exsr MEMECOD  ;
033100080110       endif   ;
033200080110       if   uni(7)>0 ;
033300080110        kcod= %editc(uni(7): 'X' ) ;
033400080110        wksc= cod(7)               ;
033500080110        exsr MEMECOD  ;
033600080110       endif   ;
033700080110       if   uni(8)>0 ;
033800080110        kcod= %editc(uni(8): 'X' ) ;
033900080110        wksc= cod(8)               ;
034000080110        exsr MEMECOD  ;
034100080110       endif   ;
034200080110       if   uni(9)>0 ;
034300080110        kcod= %editc(uni(9): 'X' ) ;
034400080110        wksc= cod(9)               ;
034500080110        exsr MEMECOD  ;
034600080110       endif   ;
034700080110 3     if   uni(10)>0 ;
034800080110        kcod= %editc(uni(10): 'X' ) ;
034900080110        wksc= cod(10)               ;
035000080110        exsr MEMECOD  ;
035100080110 3     endif   ;
035200080110
035300080102
035400090203       update wfdemp00 ;
035500080102
035600071212
035700090203       read   wfdemp00  ;
035800071213 1      enddo   ;
035900071218
036000090203 0      endif   ;
036100090204
036200090209       // 5 ) Aggiornamento dati di wadel  su WFDEMP
0363000902040      if elabdempD='S' or elabcpo='S' or elabwadel='S'    ;
036400090204       exsr DatiWADEL    ;
036500090204       endif   ;
036600090204
036700090209
036800090209       // 6 ) Aggiornamento codice potenziale per info su WFDEMU
036900090210       if (elabdemuc='S' or elabinfo='S')  and elabwadel=' '  ;
037000090209       setll *loval  wfdemu1l   ;
037100090209       read          wfdemu1l   ;
037200090209       dow   not %eof(wfdemu1l)  ;
037300090210       if elabdemuc='S'   ;
037400090209       clear  u1_demcpoifo       ;
037500090210       clear  u1_demcpi          ;
037600090210       clear  u1_demcpip         ;
037700090210       endif   ;
037800090310       clear  u1_dem100b         ;
037900090209       clear  u1_demspt          ;
038000090209       clear  u1_demfac          ;
038100090209       clear  u1_demprf          ;
038200090209       clear  u1_dempro          ;
038300090209       clear  u1_demmes          ;
038400090209       clear  u1_demaer          ;
038500090209       clear  u1_demcae          ;
038600090209       clear  u1_dematr          ;
038700090209       clear  u1_dembar          ;
038800090209       clear  u1_demmez          ;
038900090209       clear  u1_demarco         ;
039000090209       clear  u1_demarto         ;
039100090209       clear  u1_demaws          ;
039200090209       clear  u1_demsusa         ;
039300090209       clear  u1_demtnt          ;
039400090209       clear  u1_demups          ;
039500090209       clear  u1_demcatr         ;
039600090209       clear  u1_dembars         ;
039700090209       clear  u1_demdhl          ;
039800090209       clear  u1_demexec         ;
039900090209       clear  u1_demferc         ;
040000090209       clear  u1_demmail         ;
040100090209       clear  u1_demmtn          ;
040200090209       clear  u1_demsda          ;
040300090310       clear  u1_demit0001       ;
040400090310       clear  u1_demit0002       ;
040500090310       clear  u1_demit0003       ;
040600090310       clear  u1_demit0004       ;
040700090310       clear  u1_demit0005       ;
040800090310       clear  u1_demes1001       ;
040900090310       clear  u1_demes1002       ;
041000090310       clear  u1_demes1003       ;
041100090310       clear  u1_demes1004       ;
041200090310       clear  u1_demes1005       ;
041300090310       clear  u1_dem__9999       ;
041400090209
041500090209       update wfdemu01  ;
041600090209
041700090209       read          wfdemu1l   ;
041800090209       enddo   ;
041900090209       endif  ;
042000090209
042100090209       if elabdemuc='S'  or elabwadel='S'  ;
042200090209       exsr AggioperINFO  ;
042300090209
042400090209       exsr AggiodatiInfo ;
042500090209       else   ;
042600090209
042700090209       if elabinfo='S'   ;
042800090209       exsr AggiodatiInfo ;
042900090209       endif  ;
043000090209       endif  ;
043100090209
043200071121       *inlr=*on         ;
043300071116       //-------------------------------------------------------------
043400090202       //  Scrittura dile mappatura base potenziali
043500090202       //-------------------------------------------------------------
043600090202       BEGSR  ScriviDEMP    ;
043700071122
043800090212       clear wfdemp00      ;
043900090202       demcpo = cpocpo     ;
044000071122
044100090202       demrag  = cporag    ;
044200090202       demvia  = cpovia    ;
044300090202       demcit  = cpocit    ;
044400090202       demcap  = cpocap         ;
044500090202       demprv  = cpoprv    ;
044600090202       demnaz  = cponaz    ;
044700071121
044800090202       dempiv  = cpopiv    ;
044900090202       demcdf  = cpocdf     ;
045000090212       demtotfat=cpoftaz*1000    ;
045100090202       demexport=cpopexp    ;
045200090202       demcmerc =%editc(cposct: 'X')     ;
045300090202       clear   kkey  ;
045400090203       kkey= demcmerc              ;
045500090202         chain (1: '1L': kkey) tabel00f ;
045600090202         if %found(tabel00f) ;
045700090202         demdmerc=tbluni   ;
045800090202         endif  ;
045900071122
046000090319       clear   demfil   ;
046100071122       exsr   CALLTisi  ;
046200090210
046300090210       // Aggancio info spesa trasporti per prendere se c'e' la spesa reale
046400090210       chain (demcpo:'SPT')  ticpi01l   ;
046500090212 1     if %found(ticpi01l) and cpipft>250   ;
046600090210         demsptrel=cpipft    ;
046700090210       endif   ;
046800090210
046900071122
047000090202       write  wfdeMP00       ;
047100071122
047200071116       ENDSR                ;
047300071122       //-------------------------------------------------------------
047400090202       // Chiamata al TISI per avere la filiale da cappario, no messag
047500090202       //-------------------------------------------------------------
047600071122       BEGSR  CALLTisi      ;
047700071122
047800090211       clear demdfil   ;
047900090211       clear demare   ;
048000090211       clear demdare  ;
048100090211       clear demdiv   ;
048200090211       clear demddiv  ;
048300090211
048400090319       // se presente la nazione, imposto la filiale del codice
048500090319       //  e non da cappario
048600090319       if demnaz= '  '   ;
048700071122       clear   tisi95ds  ;
048800071122       clear   fnlv13ds  ;
048900071122       i95tcn='7'        ;
049000071122       i13la3='S'     ;
049100090202       i95cap=demcap  ;
049200090202       i95loc=demcit  ;
049300090202       i95prv=demprv  ;
049400090202       i95nar=demnaz  ;
049500090202       // data di elaborazione: 1 gennaio anno in corso
049600090202       Annocor=%subdt(datasys:*y) ;
049700090203       i95dat=%int(%editc(annocor:'X') +'0101') ;
049800071122       i13af0='S'     ;
049900071129       i13cnv=' '     ;
050000071129       i13af1=' '     ;
050100090211       if %subst(demcap:1:2)='47 'and  demprv='  ' ;
050200090211            demprv='RN'  ;
050300090211            I95PRV='RN'  ;
0504000902111        endif  ;
050500071211
050600071122       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
050700071122
050800090202       demfil=o95lna       ;
050900090319       endif   ;
051000090319
051100071212       // se trovato imposto area
051200071213
0513000902021      if    demfil>0    ;
051400090203       chain   demfil   AZORG01L   ;
0515000902022      if %found(azorg01l) ;
051600090202         demdfil=orgdes   ;
051700090202         demare=orgcar ;
051800090206         demdiv=orgfl3 ;
0519000902022      endif  ;
052000080102
0521000902022      if demare>0                      ;
052200080102         clear   kkey  ;
052300090203         kkey=%editc(demare: 'X')    ;
052400080102         chain (1: '05': kkey) tabel00f ;
0525000902023        if %found(tabel00f) ;
052600090202          demdare=tbluni  ;
0527000902023        endif  ;
0528000902022        endif  ;
052900090206       if demdiv<>' ' ;
053000090206         clear   kkey  ;
053100090206         kkey=demdiv                 ;
053200090206         chain (1: '17': kkey) tabel00f ;
053300090206         if %found(tabel00f) ;
053400090206          demddiv=tbluni  ;
053500090206         endif  ;
053600090206       endif  ;
053700090206
0538000902021        endif  ;
053900071122
054000071122       ENDSR                ;
054100090202       //-------------------------------------------------------------
054200090202       //  Scrittura file mappatura base u8nificanti da WADEL
054300090202       //-------------------------------------------------------------
054400090202       BEGSR  ScriviDEMU    ;
054500090202
054600090205       setll  *loval  wfdemd3l  ;
054700090205       read    wfdemd3l ;
054800090202
054900090205 1     dow  not %eof(wfdemd3l);
055000090202
055100090202         // Escludo distretto estero   agente unificante
055200090202         wfil=%int(%subst(%editc(wapauc:'X'):1:3))     ;
055300090202         chain   wfil   azorg01l   ;
055400090202
055500090202 2a      if orgfl3<>'7'    ;
055600090202
055700090202         kcod = %editc(wapcli: 'X' ) ;
055800090202         // Escludo codici indirizzo italia ma esteri
055900090202            if kcod<>'0894106' and kcod<>'0894274' ;
056000090202
056100090202         // Escludo 8888  e 9999
056200090202 2         if (%subst(kcod:4:4)<>'9999' and %subst(kcod:4:4)<>'8888') ;
056300090202
056400090202         // Escludo senza fatturato  o senza spedizioni  anno corrente
056500090202 3          if   wapfac>0 and wapsac>0;
056600090202
056700090202             chain (kut:dutkci:wapcli)  cnclp00f ;
056800090202
056900090202 4          if wapblo=' ' ;
057000090202
057100090202             clear  wfdemu00   ;
057200090202
057300090202             demkscu=wapcli ;
057400090202             demrag =wapcld ;
057500090202             demcmmu=wapauc ;
057600090202             demdcmm=wapaud ;
057700090202             demclv =wapclv;
057800090202             select  ;
057900090202             when demclv='T'   ;
058000090202             demord='1'  ;
058100090202             when demclv='A'   ;
058200090202             demord='2'  ;
058300090202             when demclv='B'   ;
058400090202             demord='3'  ;
058500090202             when demclv='C'   ;
058600090202             demord='4'  ;
058700090202             when demclv='D'   ;
058800090202             demord='5'  ;
058900090202             other   ;
059000090203             demord='6'  ;
059100090202             endsl    ;
059200090202
059300090206             demvia =wapind    ;
059400090206             demcit =waploc    ;
059500090202             demcap =wapcap;
059600090202             demprv =wapprv;
059700090202             demnaz =wapnaz;
059800090202             dempiv =wapiva;
059900090202             demcdf =wapcdf;
060000090202             demdmerc=wapitc;
060100090202
060200090202               chain (kut:dutkci:wapcli)  cnaco00f ;
060300090202 5              if %found(cnaco00f) ;
060400090203                 demrag  =%triml(acorag)   ;
060500090309                 demcmerc=%subst(%editc(acoitc:'X'):3:5)          ;
060600090202
060700090202             kkey=demcmerc               ;
060800090202             chain (1: '1L': kkey) tabel00f ;
060900090202 6           if %found(tabel00f) ;
061000090202              demdmerc=tbluni  ;
061100090202 6           endif  ;
061200090202 5           endif  ;
061300090202
061400090202             chain (kut:dutkci:wapcli)  cnind00f ;
061500090202 5           if %found(cnind00f) ;
061600090202             demcap =indcae;
061700090202             demprv =indprv;
061800090202             demnaz =indsta;
061900090202             dempiv =indiva;
062000090202             demcdf =indcdf;
062100090206             demvia =indvia    ;
062200090206             demcit =indcit   ;
062300090202 5         endif    ;
062400090319
062500090319           clear demfil   ;
062600090319           if demnaz<>'  ' ;
062700090319           demfil=wapfil   ;
062800090319           endif    ;
062900090202
063000090210           exsr CallTISI   ;
063100090210
063200090202           write(e) wfdemu00   ;
063300090202
063400090202 4         endif  ;
063500090202 4         endif  ;
063600090202 3         endif  ;
063700090202 2         endif  ;
063800090202 2a        endif  ;
063900090202
064000090205       read    wfdemd3l ;
064100090202 1     enddo ;
064200090202
064300090202       ENDSR   ;
064400071212
064500071212       //-------------------------------------------------------------
064600090203       //   cerco clienti unificanti con la partita iva
064700090203       //-------------------------------------------------------------
064800071213       BEGSR  CercaPIV      ;
064900071213
065000090203       setll  (kut:dutkci:kcdf)   cnind02l   ;
065100090203       reade  (kut:dutkci:kcdf)   cnind02l   ;
0652000712131      dow   not %eof(cnind02l)  ;
065300071213       wksc=indksc   ;
065400071213       exsr CALLTIBS   ;
065500071213
065600090203       reade  (kut:dutkci:kcdf)   cnind02l   ;
065700071213 1     enddo    ;
065800071213
065900071213       ENDSR                ;
066000071213       //-------------------------------------------------------------
066100090203       //   cerco clienti unificanti con la codice fiscale
066200090203       //-------------------------------------------------------------
066300071213       BEGSR  CercaCDF      ;
066400071213
066500090203       setll  (kut:dutkci:kCDF)   cnind03l   ;
066600090203       reade  (kut:dutkci:kcdf)   cnind03l   ;
0667000712131      dow   not %eof(cnind03l)  ;
066800071213       wksc=indksc   ;
066900071213       exsr CALLTIBS   ;
067000071213
067100090203       reade  (kut:dutkci:kcdf)   cnind03l   ;
067200071213 1     enddo    ;
067300071213
067400071213       ENDSR                ;
067500071213       //-------------------------------------------------------------
067600071213       BEGSR   CALLTIBS ;
067700071213
067800071213       clear  tibs10ds ;
067900071213       d10tle='ST' ;
068000071213       d10paf='F' ;
068100071213       d10cod=wksc   ;
068200090206       Annocor=%subdt(datasys:*y) ;
068300090206       d10drf=%int(%editc(annocor:'X') +'0101') ;
068400071213       callp   TIBS10R  (TIBS10ds:tipexe) ;
068500071213
068600071217 1       if d10err<>' ' ;
068700071213       d10err='  ' ;
068800071213       d10paf='P' ;
068900090206       Annocor=%subdt(datasys:*y) ;
069000090206       d10drf=%int(%editc(annocor:'X') +'0101') ;
069100071213       callp   TIBS10R  (TIBS10ds:tipexe) ;
069200071217 1       endif  ;
069300071213
069400071217 1       if d10err<>' '   ;
069500071213           wuni=wksc     ;
069600071213         else  ;
069700071213           wuni=d10cop ;
069800071217 1       endif  ;
069900071213
070000071213         trov=1 ;
070100071213         trov=%lookup (wuni:uni) ;
070200071213 2         if  trov=0   ;
070300071213           xx=xx+1     ;
070400080110 3            if xx<=100 ;
070500071213                   uni(XX)=wuni  ;
070600080110                   cod(XX)=wksc  ;
070700071213 3            endif     ;
070800071213 2         endif     ;
070900071213       ENDSR  ;
071000080110       //-------------------------------------------------------------
071100080110       BEGSR  MEMECOD       ;
071200080111       wuni=%int(kcod)  ;
071300090205       clear UsaUnific   ;
071400080111
071500080111         trov=%lookup (wuni:giapre) ;
071600090205       if trov=0 ;
071700080111
071800090203       chain   (wuni)      wfdemu1l  ;
071900090205
072000090205       // SE NON TROVATO VERIFICO SE c'e' negli unificati da
072100090205       //  wfdemd0f
072200090205       if not %found(wfdemu1l)   ;
072300090205         chain   (wuni)      wfdemd3l  ;
072400090205
072500090205 1       if %found(WFdemd3l) and wapunio='S';
072600090205         // richaino col suo unificato
072700090205           chain   (wapcliuni)  wfdemu1l  ;
072800090205           UsaUnific='S'    ;
072900090205         endif   ;
073000090205       endif   ;
073100080110
073200090203 1     if %found(WFdemu1l) ;
073300090203
073400090203 2     if u1_demusa<>' '    ;
073500080110
073600090203 3     if demerr=*blanks    ;
073700090203       demerr='+ CPO PER STESSO UNIFICANTE '+ %char(wksc) ;
073800080110 3     endif  ;
073900080110
074000080110 x2    ELSE  ;
074100090205       if UsaUnific=' ';
074200090205         u1_demusa='D' ;
074300090205       else   ;
074400090205         u1_demusa='U' ;
074500090205       endif   ;
074600090203       update wfdemu01   ;
074700080110  3               select   ;
074800090203                  when demunif1=0       ;
074900090203                  demunif1=%int(kcod)  ;
075000090203                  when demunif2=0       ;
075100090203                  demunif2=%int(kcod)  ;
075200090203                  when demunif3=0       ;
075300090203                  demunif3=%int(kcod)  ;
075400090203                  when demunif4=0       ;
075500090203                  demunif4=%int(kcod)  ;
075600090203                  when demunif5=0       ;
075700090203                  demunif5=%int(kcod)  ;
075800090203                  when demunif6=0       ;
075900090203                  demunif6=%int(kcod)  ;
076000090203                  when demunif7=0       ;
076100090203                  demunif7=%int(kcod)  ;
076200090203                  when demunif8=0       ;
076300090203                  demunif8=%int(kcod)  ;
076400090203                  when demunif9=0       ;
076500090203                  demunif9=%int(kcod)  ;
076600090203                  when demunif0=0       ;
076700090203                  demunif0=%int(kcod)  ;
076800080110                  other          ;
076900090203                  demuniol='S'  ;
077000080110  3               endsl  ;
077100080110
077200090203                demcli='S' ;
077300080110  2    endif  ;
077400080110  1    endif  ;
077500080111  1    endif  ;
077600080110
077700080110       ENDSR  ;
077800090204       //--------------------------------------------------------------
077900090204       //?Aggiorno file wfdemp con i dati di wadel
078000090204       //--------------------------------------------------------------
078100090204       BEGSR DatiWADEL   ;
078200090204
078300090204        clear   wfdemp00  ;
078400090204        clear maxfac ;
078500090204
078600090204        setll  *start    Wfdemp0f   ;
078700090204        read    wfdemp00    ;
078800090204
078900090204 1      dow  not  %eof(wfdemp0f)  ;
079000090204        uni(1)=demunif1 ;
079100090204        uni(2)=demunif2 ;
079200090204        uni(3)=demunif3 ;
079300090204        uni(4)=demunif4 ;
079400090204        uni(5)=demunif5 ;
079500090204        uni(6)=demunif6 ;
079600090204        uni(7)=demunif7 ;
079700090204        uni(8)=demunif8 ;
079800090204        uni(9)=demunif9 ;
079900090204        uni(10)=demunif0 ;
080000090204        xx=1 ;
080100090204
080200090204 2      dow   uni(xx) > 0 ;
080300090204
080400090204       wuni=uni(xx)   ;
080500090205       chain   wuni    wfdemi3l   ;
080600090204
080700090205 3      if %found(wfdemi3l)   ;
080800090204
080900090204        iwapita='I'  ;
081000090204        iwapsap= wapsap+iwapsap  ;
081100090204        iwapfap= wapfap+iwapfap ;
081200090204        iwapsac= wapsac+iwapsac  ;
081300090410        iwapfac= wapfac+iwapfac  ;
081400090204
081500090204        iwaptotrpp=waprp_01ap+waprp_02ap+waprp_03ap+waprp_04ap+waprp_05ap+
081600090204                   waprp_06ap+waprp_07ap+waprp_08ap+waprp_09ap+waprp_10ap+
081700090204                   waprp_11ap+waprp_12ap + iwaptotrpp  ;
081800090204        iwaptotrpc=waprp_01ac+waprp_02ac+waprp_03ac+waprp_04ac+waprp_05ac+
081900090204                   waprp_06ac+waprp_07ac+waprp_08ac+waprp_09ac+waprp_10ac+
082000090204                   waprp_11ac+waprp_12ac + iwaptotrpc ;
082100090204        iwapns01ac=wapns_01ac + iwapns01ac  ;
082200090204        iwapns02ac=wapns_02ac + iwapns02ac  ;
082300090204        iwapns03ac=wapns_03ac + iwapns03ac  ;
082400090204        iwapns04ac=wapns_04ac + iwapns04ac  ;
082500090204        iwapns05ac=wapns_05ac + iwapns05ac  ;
082600090204        iwapns06ac=wapns_06ac + iwapns06ac  ;
082700090204        iwapns07ac=wapns_07ac + iwapns07ac  ;
082800090204        iwapns08ac=wapns_08ac + iwapns08ac  ;
082900090204        iwapns09ac=wapns_09ac + iwapns09ac  ;
083000090204        iwapns10ac=wapns_10ac + iwapns10ac  ;
083100090204        iwapns11ac=wapns_11ac + iwapns11ac  ;
083200090204        iwapns12ac=wapns_12ac + iwapns12ac  ;
083300090204
083400090204        iwaprr01ac=waprr_01ac + iwaprr01ac  ;
083500090204        iwaprr02ac=waprr_02ac + iwaprr02ac  ;
083600090204        iwaprr03ac=waprr_03ac + iwaprr03ac  ;
083700090204        iwaprr04ac=waprr_04ac + iwaprr04ac  ;
083800090204        iwaprr05ac=waprr_05ac + iwaprr05ac  ;
083900090204        iwaprr06ac=waprr_06ac + iwaprr06ac  ;
084000090204        iwaprr07ac=waprr_07ac + iwaprr07ac  ;
084100090204        iwaprr08ac=waprr_08ac + iwaprr08ac  ;
084200090204        iwaprr09ac=waprr_09ac + iwaprr09ac  ;
084300090204        iwaprr10ac=waprr_10ac + iwaprr10ac  ;
084400090204        iwaprr11ac=waprr_11ac + iwaprr11ac  ;
084500090204        iwaprr12ac=waprr_12ac + iwaprr12ac  ;
084600090204
084700090204        iwaprp01ac=waprp_01ac + iwaprp01ac  ;
084800090204        iwaprp02ac=waprp_02ac + iwaprp02ac  ;
084900090204        iwaprp03ac=waprp_03ac + iwaprp03ac  ;
085000090204        iwaprp04ac=waprp_04ac + iwaprp04ac  ;
085100090204        iwaprp05ac=waprp_05ac + iwaprp05ac  ;
085200090204        iwaprp06ac=waprp_06ac + iwaprp06ac  ;
085300090204        iwaprp07ac=waprp_07ac + iwaprp07ac  ;
085400090204        iwaprp08ac=waprp_08ac + iwaprp08ac  ;
085500090204        iwaprp09ac=waprp_09ac + iwaprp09ac  ;
085600090204        iwaprp10ac=waprp_10ac + iwaprp10ac  ;
085700090204        iwaprp11ac=waprp_11ac + iwaprp11ac  ;
085800090204        iwaprp12ac=waprp_12ac + iwaprp12ac  ;
085900090204 3      endif  ;
086000090204
086100090205       chain   wuni    wfdeme3l   ;
086200090204
086300090205 3      if %found(wfdeme3l)   ;
086400090204        ewapest='E'  ;
086500090204        ewapsap= wapsap+ewapsap  ;
086600090204        ewapfap= wapfap+ewapfap ;
086700090204        ewapsac= wapsac+ewapsac  ;
086800090410        ewapfac= wapfac+ewapfac  ;
086900090204
087000090204        ewaptotrpp=waprp_01ap+waprp_02ap+waprp_03ap+waprp_04ap+waprp_05ap+
087100090204                   waprp_06ap+waprp_07ap+waprp_08ap+waprp_09ap+waprp_10ap+
087200090204                   waprp_11ap+waprp_12ap + ewaptotrpp  ;
087300090204        ewaptotrpc=waprp_01ac+waprp_02ac+waprp_03ac+waprp_04ac+waprp_05ac+
087400090204                   waprp_06ac+waprp_07ac+waprp_08ac+waprp_09ac+waprp_10ac+
087500090204                   waprp_11ac+waprp_12ac + ewaptotrpc ;
087600090204        ewapns01ac=wapns_01ac + ewapns01ac  ;
087700090204        ewapns02ac=wapns_02ac + ewapns02ac  ;
087800090204        ewapns03ac=wapns_03ac + ewapns03ac  ;
087900090204        ewapns04ac=wapns_04ac + ewapns04ac  ;
088000090204        ewapns05ac=wapns_05ac + ewapns05ac  ;
088100090204        ewapns06ac=wapns_06ac + ewapns06ac  ;
088200090204        ewapns07ac=wapns_07ac + ewapns07ac  ;
088300090204        ewapns08ac=wapns_08ac + ewapns08ac  ;
088400090204        ewapns09ac=wapns_09ac + ewapns09ac  ;
088500090204        ewapns10ac=wapns_10ac + ewapns10ac  ;
088600090204        ewapns11ac=wapns_11ac + ewapns11ac  ;
088700090204        ewapns12ac=wapns_12ac + ewapns12ac  ;
088800090204
088900090204        ewaprr01ac=waprr_01ac + ewaprr01ac  ;
089000090204        ewaprr02ac=waprr_02ac + ewaprr02ac  ;
089100090204        ewaprr03ac=waprr_03ac + ewaprr03ac  ;
089200090204        ewaprr04ac=waprr_04ac + ewaprr04ac  ;
089300090204        ewaprr05ac=waprr_05ac + ewaprr05ac  ;
089400090204        ewaprr06ac=waprr_06ac + ewaprr06ac  ;
089500090204        ewaprr07ac=waprr_07ac + ewaprr07ac  ;
089600090204        ewaprr08ac=waprr_08ac + ewaprr08ac  ;
089700090204        ewaprr09ac=waprr_09ac + ewaprr09ac  ;
089800090204        ewaprr10ac=waprr_10ac + ewaprr10ac  ;
089900090204        ewaprr11ac=waprr_11ac + ewaprr11ac  ;
090000090204        ewaprr12ac=waprr_12ac + ewaprr12ac  ;
090100090204
090200090204        ewaprp01ac=waprp_01ac + ewaprp01ac  ;
090300090204        ewaprp02ac=waprp_02ac + ewaprp02ac  ;
090400090204        ewaprp03ac=waprp_03ac + ewaprp03ac  ;
090500090204        ewaprp04ac=waprp_04ac + ewaprp04ac  ;
090600090204        ewaprp05ac=waprp_05ac + ewaprp05ac  ;
090700090204        ewaprp06ac=waprp_06ac + ewaprp06ac  ;
090800090204        ewaprp07ac=waprp_07ac + ewaprp07ac  ;
090900090204        ewaprp08ac=waprp_08ac + ewaprp08ac  ;
091000090204        ewaprp09ac=waprp_09ac + ewaprp09ac  ;
091100090204        ewaprp10ac=waprp_10ac + ewaprp10ac  ;
091200090204        ewaprp11ac=waprp_11ac + ewaprp11ac  ;
091300090204        ewaprp12ac=waprp_12ac + ewaprp12ac  ;
091400090204 3      endif  ;
091500090204
091600090204        // Dati generici del cliente con maggior fatturato se + di uno  ;
091700090409        chain   wuni    wfdemd3l   ;
0918000904093       if %found(wfdemd3l)   ;
0919000904094       if  wapfac>maxfac ;
092000090409        maxfac=wapfac     ;
092100090204        cwapfil=wapfil        ;
092200090204        cwapfid=wapfid        ;
092300090204        cwapage=wapage        ;
092400090204        cwapagd=wapagd        ;
092500090204        cwapitc=wapitc        ;
092600090204        cwapclv=wapclv        ;
092700090204        cwapblo=wapblo        ;
092800090204        clear cwapare ;
092900090204        clear cwapard ;
093000090409
093100090204       chain  cwapfil   AZORG01L   ;
0932000904095      if %found(azorg01l) ;
093300090204         cwapare=orgcar ;
093400090204
093500090204         clear   kkey  ;
093600090204         kkey=%editc(cwapare: 'X')    ;
093700090204         chain (1: '05': kkey) tabel00f ;
0938000904096        if %found(tabel00f) ;
093900090204          cwapard=tbluni  ;
0940000904096        endif  ;
0941000904095        endif  ;
0942000904094        endif  ;
0943000904093        endif  ;
094400090204
094500090204        xx=xx+1  ;
094600090204 2      enddo   ;
094700090204
094800090204        update  wfdemp00  ;
094900090204        clear   wfdemp00  ;
095000090204        clear maxfac ;
095100090204
095200090204       read   wfdemp00  ;
095300090204 1     enddo   ;
095400090204
095500090204       ENDSR  ;
095600090203
095700090203       //--------------------------------------------------------------
095800090203       //?Reperimento Dati del job (Utente/Operativi).
095900090203       //--------------------------------------------------------------
096000090203       BEGSR DatiJob;
096100090203
096200090203         in(E) §AzUte;
096300090203         if NOT %error;
096400090203           in(E) §DatiUte;
096500090203         endif;
096600090203         if %error or RSut = *blanks;
096700090203           clear TIBS34ds;
096800090203           tibs34r(tibs34ds);
096900090203           in §AzUte;
097000090203           in §DatiUte;
097100090203         endif;
097200090203
097300090203       ENDSR;
097400090206       //--------------------------------------------------------------
097500090206       //?pulizia campi aggioranti da WADEL
097600090206       //--------------------------------------------------------------
097700090206        BEGSR cleardaWADEL   ;
097800090206        clear demunif1       ;
097900090206        clear demunif2       ;
098000090206        clear demunif3       ;
098100090206        clear demunif4       ;
098200090206        clear demunif5       ;
098300090206        clear demunif6       ;
098400090206        clear demunif7       ;
098500090206        clear demunif8       ;
098600090206        clear demunif9       ;
098700090206        clear demunif0       ;
098800090206        clear demuniol       ;
098900090206        clear demcli         ;
099000090206        clear demerr         ;
099100090206        clear iwapita ;
099200090206        clear iwapsap ;
099300090206        clear iwapfap ;
099400090206        clear iwaptotrpp ;
099500090206        clear iwapsac ;
099600090206        clear iwapfac ;
099700090206        clear iwaptotrpc ;
099800090206        clear iwapns01ac ;
099900090206        clear iwaprr01ac ;
100000090206        clear iwaprp01ac ;
100100090206        clear iwapns02ac ;
100200090206        clear iwaprr02ac ;
100300090206        clear iwaprp02ac ;
100400090206        clear iwapns03ac ;
100500090206        clear iwaprr03ac ;
100600090206        clear iwaprp03ac ;
100700090206        clear iwapns04ac ;
100800090206        clear iwaprr04ac ;
100900090206        clear iwaprp04ac ;
101000090206        clear iwapns05ac ;
101100090206        clear iwaprr05ac ;
101200090206        clear iwaprp05ac ;
101300090206        clear iwapns06ac ;
101400090206        clear iwaprr06ac ;
101500090206        clear iwaprp06ac ;
101600090206        clear iwapns07ac ;
101700090206        clear iwaprr07ac ;
101800090206        clear iwaprp07ac ;
101900090206        clear iwapns08ac ;
102000090206        clear iwaprr08ac ;
102100090206        clear iwaprp08ac ;
102200090206        clear iwapns09ac ;
102300090206        clear iwaprr09ac ;
102400090206        clear iwaprp09ac ;
102500090206        clear iwapns10ac ;
102600090206        clear iwaprr10ac ;
102700090206        clear iwaprp10ac ;
102800090206        clear iwapns11ac ;
102900090206        clear iwaprr11ac ;
103000090206        clear iwaprp11ac ;
103100090206        clear iwapns12ac ;
103200090206        clear iwaprr12ac ;
103300090206        clear iwaprp12ac ;
103400090206
103500090206        clear ewapest ;
103600090206        clear ewapsap ;
103700090206        clear ewapfap ;
103800090206        clear ewaptotrpp ;
103900090206        clear ewapsac ;
104000090206        clear ewapfac ;
104100090206        clear ewaptotrpc ;
104200090206        clear ewapns01ac ;
104300090206        clear ewaprr01ac ;
104400090206        clear ewaprp01ac ;
104500090206        clear ewapns02ac ;
104600090206        clear ewaprr02ac ;
104700090206        clear ewaprp02ac ;
104800090206        clear ewapns03ac ;
104900090206        clear ewaprr03ac ;
105000090206        clear ewaprp03ac ;
105100090206        clear ewapns04ac ;
105200090206        clear ewaprr04ac ;
105300090206        clear ewaprp04ac ;
105400090206        clear ewapns05ac ;
105500090206        clear ewaprr05ac ;
105600090206        clear ewaprp05ac ;
105700090206        clear ewapns06ac ;
105800090206        clear ewaprr06ac ;
105900090206        clear ewaprp06ac ;
106000090206        clear ewapns07ac ;
106100090206        clear ewaprr07ac ;
106200090206        clear ewaprp07ac ;
106300090206        clear ewapns08ac ;
106400090206        clear ewaprr08ac ;
106500090206        clear ewaprp08ac ;
106600090206        clear ewapns09ac ;
106700090206        clear ewaprr09ac ;
106800090206        clear ewaprp09ac ;
106900090206        clear ewapns10ac ;
107000090206        clear ewaprr10ac ;
107100090206        clear ewaprp10ac ;
107200090206        clear ewapns11ac ;
107300090206        clear ewaprr11ac ;
107400090206        clear ewaprp11ac ;
107500090206        clear ewapns12ac ;
107600090206        clear ewaprr12ac ;
107700090206        clear ewaprp12ac ;
107800090206
107900090206        clear cwapare    ;
108000090206        clear cwapard    ;
108100090206        clear cwapfil    ;
108200090206        clear cwapfid    ;
108300090206        clear cwapage    ;
108400090206        clear cwapagd    ;
108500090206        clear cwapblo    ;
108600090206        clear cwapitc    ;
108700090206        clear cwapclv    ;
108800090206        ENDSR   ;
108900090209       //-------------------------------------------------------------
109000090209       //  Aggiorno cod potenziale su WFDEMU0FF per le info
109100090209       //-------------------------------------------------------------
109200090209       BEGSR AggioperInfo   ;
109300090209       setll *loval   wfdemu1l  ;
109400090209       read           wfdemu1l  ;
109500090209       dow   not %eof(wfdemu1l)  ;
109600090209
109700090209           EXSR CercaPotSede   ;
109800090209
109900090209           xx=1   ;
110000090209           clear savfat   ;
110100090209
110200090209           dow potsede(xx)>0  ;
110300090209           // di quelli letti, memorizzo quello che ha le info
110400090209           //  col fatturato D&B più alto
110500090209
110600090209           if potinfo(XX)='S' or potinfop(xx)='S'  ;
110700090209             if  potfat(XX) >=savfat   ;
110800090209              u1_demcpoifo=potsede(XX) ;
110900090209              u1_demcpi   =potinfo(XX) ;
111000090209              u1_demcpip  =potinfop(XX) ;
111100090209              savfat=potfat(xx)   ;
111200090209             endif  ;
111300090209           endif  ;
111400090209
111500090209           xx=xx+1  ;
111600090209           enddo    ;
111700090209
111800090209       update wfdemu01  ;
111900090209
112000090209       read           wfdemu1l  ;
112100090209       enddo   ;
112200090209       ENDSR   ;
112300090209
112400090209       //-------------------------------------------------------------
112500090209       //?Cerca i potenziali sede associati  all'unificante
112600090209       //--------------------------------------------------------------
112700090209       BEGSR  CercaPotSede ;
112800090209
112900090209       // Prendo codice unificante e carico cod collegati
113000090209           clear tibs10ds ;
113100090209           d10tle='ST'   ;
113200090209           d10paf='F'    ;
113300090209           d10cod=u1_demkscu;
113400090209           callp TIBS10R (tibs10ds:tipexe)  ;
113500090209
113600090209  1        if d10err<>*blanks   ;
113700090209           // Se errore verifico se è figlio
113800090209           clear tibs10ds ;
113900090209           d10tle='ST'   ;
114000090209           d10paf='P'    ;
114100090209           d10cod=u1_demkscu;
114200090209           callp TIBS10R (tibs10ds:tipexe) ;
114300090209
114400090209  2        if d10err<>*blanks   ;
114500090209           // Imposto nella skiera e come padre solo se stesso
114600090209           skc(1)='0000'+%editc(u1_demkscu:'X');
114700090209           d10cop=u1_demkscu                  ;
114800090209  2        endif                ;
114900090209  1        endif ;
115000090209
115100090209       clear potsede  ;
115200090209       clear potfat   ;
115300090209       clear potinfo  ;
115400090209       clear potinfop ;
115500090209       clear xx       ;
115600090209       yy=1    ;
115700090209
115800090209 1     dow skc(yy)>*Zeros  ;
115900090209
116000090209       // Verifico se cnaco ha un codice SEDE attaccato ;
116100090209       wksc=%int(%subst(skc(yy):5:7)) ;
116200090209       chain   (1:dutkci:wksc)  cnaco00f  ;
116300090209 2     if %found(cnaco00f)   ;
116400090209
116500090209         chain  acolib   tncpo01l   ;
116600090209
116700090209 3       if %found(tncpo01l) and cpofsf='S'  ;
116800090209         exsr MemPotSede     ;
116900090209 3       endif     ;
117000090209 2     endif     ;
117100090209
117200090209       yy=yy+1   ;
117300090209 1     enddo     ;
117400090209
117500090209       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
117600090209       //  codice unificante, altri potenziali  SEDE
117700090209       wksc=d10cop;
117800090209       chain   (1:dutkci:wksc)  cnclp00f  ;
117900090209 1     if %found(cnclp00f)   ;
118000090209
118100090209       // col sottoconti int fattura
118200090209       chain   (1:dutkci:clpscf)  cnind00f  ;
118300090209 2     if %found(cnind00f) and indsta=*blanks  ;
118400090209         // con la partita iva
118500090209 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
118600090209                             AND INDIVA<>'04507990150'        ;
118700090209         setll indiva   tncpo06l  ;
118800090209         reade indiva   tncpo06l  ;
118900090209
119000090209 3         dow not %eof(tncpo06l)   ;
119100090209 4         if cpofsf='S'          ;
119200090209              exsr MempotSede       ;
119300090209 4         endif                  ;
119400090209
119500090209           reade indiva   tncpo06l  ;
119600090209 3       enddo                    ;
119700090209 2a      endif    ;
119800090209
119900090209         // con il codice fiscale
120000090209 2a      if indcdf<>*blanks       ;
120100090209         setll indcdf   tncpo05l  ;
120200090209         reade indcdf   tncpo05l  ;
120300090209
120400090209 3         dow not %eof(tncpo05l)   ;
120500090209 4         if cpofsf='S'          ;
120600090209              exsr MempotSede      ;
120700090209 4         endif                  ;
120800090209
120900090209           reade indcdf   tncpo05l  ;
121000090209 3       enddo                    ;
121100090209 2a    endif                  ;
121200090209 2     endif                  ;
121300090209 1     endif                  ;
121400090209
121500090209       ENDSR  ;
121600090209       //--------------------------------------------------------------
121700090209       //?Memorizza potenziali sede per ogni unificante
121800090209       //--------------------------------------------------------------
121900090209       BEGSR MemPotSede;
122000090209
122100090209         // Memorizzo i codici visualizzati per evitare ripetizioni
122200090209         //           solo se ha le info complete o parziali
122300090209
122400090209         Indx=%lookup(cpocpo:potsede)   ;
122500090209 1         if Indx=0    ;
122600090209           dcpo01=cporst  ;
122700090209
122800090209           clear trmk50ds   ;
122900090209           // Memorizzo solo se ha le info complete o parziali
123000090209 2           if §cpoifotot <>'S'    ;
123100090209                    i50cpo=cpocpo     ;
123200090209                    i50mod='C'        ;
123300090209                    i50obl='S'        ;
123400090209                    callp TRMK50R (kpjba:trmk50ds)  ;
123500090209  2          endif   ;
123600090209
123700090209  2          if §cpoifotot = 'S'    or o50ifomin='S' ;
123800090209           xx=xx+1  ;
123900090209           potsede(xx)=cpocpo   ;
124000090209           potfat (xx)=cpoftaz  ;
124100090209           potinfo(xx)=§cpoifotot ;
124200090209           potinfop(xx)=o50ifomin  ;
124300090209
124400090209 2         endif;
124500090209 1         endif;
124600090209           ENDSR  ;
124700090209       //--------------------------------------------------------------
124800090209       //?Aggiorno dati delle info trovati
124900090209       //--------------------------------------------------------------
125000090209       BEGSR AggioDatiInfo  ;
125100090209       setll *loval  wfdemu1l   ;
125200090209       read          wfdemu1l   ;
125300090209       dow   not %eof(wfdemu1l)  ;
125400090209
125500090209       if u1_demcpoifo>0  ;
125600090209       Exsr LeggiCPI  ;
125700090209       endif   ;
125800090209
125900090209       read          wfdemu1l   ;
126000090209       enddo   ;
126100090209       ENDSR  ;
126200090209       //--------------------------------------------------------------
126300090209       //?lettura info commerciali
126400090209       //--------------------------------------------------------------
126500090209       BEGSR LeggiCPI      ;
126600090210
126700090209       // spesa trasporti
126800090210       chain (u1_demcpoifo:'SPT')  ticpi01l   ;
126900090212 1     if %found(ticpi01l) and cpipft>250   ;
127000090210       u1_demspt=cpipft    ;
127100090310
127200090310       //  se trovata spesa
127300090310       // chaino wfdemd per avere fatturato reale
127400090310         chain   (u1_demkscu)      wfdemd3l  ;
127500090310        if %found(wfdemd3l)  ;
127600090310          u1_demfac=WAPFAC  ;
127700090310        endif   ;
127800090310 1      endif   ;
127900090209
128000090209       //  se trovata spesa faccio la ripartizione
128100090209       setll u1_demcpoifo  ticpi01l   ;
128200090209       reade u1_demcpoifo  ticpi01l   ;
128300090209
128400090310 1     dow  not %eof(ticpi01l)  ;
128500090209
128600090310 2     if u1_demspt>0   ;
128700090310
128800090209 3     if  cpival>0 and cpitva='%'   ;
128900090210       ripSpesa=(u1_demspt*cpival)/100   ;
129000090209
129100090209 4     if cpitpf='10 '  ;
129200090209 5     select   ;
129300090209       when cpispf='_BAR'   ;
129400090210       u1_dembar=ripspesa ;
129500090209
129600090311       when cpispf='_MEP'   ;
129700090210       u1_demmez=ripspesa ;
129800090209
129900090209       when cpispf='ARCO'   ;
130000090210       u1_demARCO=ripspesa ;
130100090209
130200090209       when cpispf='ARTO'   ;
130300090210       u1_demARTO=ripspesa ;
130400090209
130500090209       when cpispf='AWS'   ;
130600090210       u1_demAWS=ripspesa ;
130700090209
130800090209       when cpispf='BARS'   ;
130900090210       u1_dembarS=ripspesa ;
131000090209
131100090209       when cpispf='DHL'   ;
131200090210       u1_demDHL=ripspesa ;
131300090209
131400090209       when cpispf='EXEC'   ;
131500090210       u1_demEXEC=ripspesa ;
131600090209
131700090209       when cpispf='FERC'   ;
131800090210       u1_demFERC=ripspesa ;
131900090209
132000090209       when cpispf='MAIL'   ;
132100090210       u1_demMAIL=ripspesa ;
132200090209
132300090209       when cpispf='MTN'   ;
132400090210       u1_demMTN=ripspesa ;
132500090209
132600090209       when cpispf='SDA'   ;
132700090210       u1_demSDA=ripspesa ;
132800090209
132900090209       when cpispf='SUSA'   ;
133000090210       u1_demSUSA=ripspesa ;
133100090209
133200090209       when cpispf='TNT'   ;
133300090210       u1_demTNT=ripspesa ;
133400090209
133500090209       when cpispf='UPS'   ;
133600090210       u1_demUPS=ripspesa ;
133700090209
133800090209       when cpispf='9ATR'   ;
133900090210       u1_demCATR=ripspesa ;
134000090209 5     ENDSL  ;
134100090209
134200090209 x4    else  ;
134300090209 5     select   ;
134400090209       when cpitpf='PRF'   ;
134500090210       u1_demprf=ripspesa ;
134600090209
134700090209       when cpitpf='PRO'   ;
134800090210       u1_dempro=ripspesa ;
134900090209
135000090209       when cpitpf='MES'   ;
135100090210       u1_demMES=ripspesa ;
135200090209
135300090209       when cpitpf='AER'   ;
135400090210       u1_demAER=ripspesa ;
135500090209
135600090209       when cpitpf='CAE'   ;
135700090210       u1_demCAE=ripspesa ;
135800090209
135900090209       when cpitpf='ATR'   ;
136000090210       u1_demATR=ripspesa ;
136100090209 5     ENDSL  ;
136200090209
136300090209 4     ENDif  ;
136400090209 3     ENDif  ;
136500090310 2     ENDif  ;
136600090310
136700090310         // se 100% memorizzo flag
136800090310 4     if cpitpf='10 '
136900090310          and cpispf='_BAR'
137000090310 6            and  cpival=100   ;
137100090310           u1_dem100b='S'   ;
137200090310 6       endif    ;
137300090310
137400090209
137500090310       // Memorizzo il motivo non affidamento
137600090310 2     if cpitpf='MOI'   ;
137700090310 3     select   ;
137800090310       when cpispf='0001'  ;
137900090310       u1_demit0001='S'   ;
138000090310
138100090310       when cpispf='0002'  ;
138200090310       u1_demit0002='S'   ;
138300090310
138400090310       when cpispf='0003'  ;
138500090310       u1_demit0003='S'   ;
138600090310
138700090310       when cpispf='0004'  ;
138800090310       u1_demit0004='S'   ;
138900090310
139000090310       when cpispf='0005'  ;
139100090310       u1_demit0005='S'   ;
139200090310
139300090310       when cpispf='1001'  ;
139400090310       u1_demes1001='S'   ;
139500090310
139600090310       when cpispf='1002'  ;
139700090310       u1_demes1002='S'   ;
139800090310
139900090310       when cpispf='1003'  ;
140000090310       u1_demes1003='S'   ;
140100090310
140200090310       when cpispf='1004'  ;
140300090310       u1_demes1004='S'   ;
140400090310
140500090310       when cpispf='1005'  ;
140600090310       u1_demes1005='S'   ;
140700090310
140800090310       when cpispf='9999'  ;
140900090310       u1_dem__9999='S'   ;
141000090310 3     endsl   ;
141100090310 2     endif   ;
141200090310
141300090209       reade u1_demcpoifo  ticpi01l   ;
141400090310 1     enddo    ;
141500090209
141600090209       UPDATE wfdemu01  ;
141700090209
141800090209       ENDSR  ;
141900090203
142000090203
142100071116      /end-free
