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
001400071218     fcnaco00f  if   e           k Disk
001500071213     fcnclp00f  if   e           k Disk
001600090205     Fwfdemd3l  iF   E           k DISK
001700090205     Fwfdemi3l  iF   E           k DISK    rename(wadelpc0:wfdemi03)
001800090409     Fwfdeme3l  iF   E           k DISK    rename(wadelpc0:wfdeme03)
001900090203     FWFDEmu0f  uF a E             DISK
002000090203     FWFDEmp0F  uF a E             DISK
002100090202     FWFDEmp1L  iF   E           K DISK    rename(wfdemp00:wfdemp01)
002200090202     FWFDEmp2L  iF   E           K DISK    rename(wfdemp00:wfdemp02)
002300090203     FWFDEmp3L  uF   E           K DISK    rename(wfdemp00:wfdemp03)
002400090203     FWFDEmu1L  uF   E           K DISK    rename(wfdemu00:wfdemu01)
002500090203     F                                     prefix(u1_)
002600090209     fTNcpo05L  if   e           k Disk    rename(tncpo000:tncpo005)
002700090209     fTNcpo06L  if   e           k Disk    rename(tncpo000:tncpo006)
002800031205      *------------------------------------------------------------------------*
002900031205      *   C A M P I   D I   L A V O R O
003000031205      *------------------------------------------------------------------------*
003100080110     d UNI             s              7  0 dim(100)
003200080111     d giapre          s              7  0 dim(10)
003300080110     d COD             s              7  0 dim(100)
003400090209
003500090209     d potsede         s             11  0 dim(90)
003600090209     d potfat          s             11  0 dim(90)
003700090209     d potinfo         s              1    dim(90)
003800090209     d potinfop        s              1    dim(90)
003900090209     D
004000090209     d savfat          s             11  0
004100090209     d RipSpesa        s             11  0
004200090209     d Trov            s              3  0
004300090205     d Usaunific       s              1
004400071212     d Wksc            s              7  0
004500071213     d Wuni            s              7  0
004600071213     d Kcod            s              7
004700071217     d Kcdf            s             16
004800090203     d Kpiv            s             16
004900080110     d wtrov           s              1
005000090203     d Wfil            s              3  0
005100071213     d xx              s              3  0
005200071213     d kut             s              1  0 inz(1)
005300080102     d kkey            s                   like(tblkey)
005400071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
005500090202     d Annocor         s              4  0
005600090204     d maxfac          s             15  3
005700090209     d Indx            s              4  0
005800090209     d yy              s              4  0
005900031205
006000031205      *------------------------------------------------------------------------*
006100031205      *   D S   I N T E R N E / E S T E R N E
006200031205      *------------------------------------------------------------------------*
006300031205     d Kpjba         e ds
006400090209     d TRMK50ds      e ds                  inz
006500090209     d Dcpo01        e ds
006600071122     d fnlv13ds      e ds
006700071122     d tisi95ds      e ds
006800071212     d tibs10ds      e ds
006900090209     d  skc                   21   5520    dim(500)
007000071122     d
007100090203      // - Reperimento dati utente
007200090203     d TIBS34ds      e ds
007300090203      // - Dati utente
007400090203     d §AzUte        e ds                  extname(AZUTE00F)
007500090203     d                                     dtaara
007600090203     d §DatiUte      e ds                  extname(dDatiUte)
007700090203     d                                     dtaara
007800090203
007900090203     D trmkm3ds        DS
008000090203     D  Elabcpo                1      1
008100090203     D  Elabwadel              2      2
008200090204     D  Elabdempc              3      3
008300090204     D  Elabdempd              4      4
008400090209     D  Elabdemuc              5      5
008500090209     D  Elabinfo               6      6
008600101020     D  ElabWFMKU              7      7
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
011500101020       if Elabcpo='S'  or ElabWFMKU='S'  ;
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
015000101020        if  elabWADel= 'S' or  ElabWFMKU='S'  ;
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
0170001010200      if elabdempC='S' or elabcpo='S' or elabwadel='S'
017100101020          or ElabWFMKU='S'    ;
017200090203
017300090203       setll   *loval   wfdemu1l   ;
017400090203       read             wfdemu1l    ;
017500080110
017600090203 1     dow not %eof(wfdemu1l)   ;
017700080110
017800090203       kpiv=u1_dempiv    ;
017900090203       kcdf=u1_demcdf    ;
018000080110       clear   wtrov ;
018100090203 2     if kpiv<>*blanks  and %subst(kpiv:1:2)<>'$$'
018200101020                             AND kpiv<>'04507990150' and kpiv<>'09999999999' ;
018300090203
018400090203       chain   kpiv      WFDEmp1l  ;
018500090203 3     if %found(wfdemp1l) ;
018600080110       Wtrov='S' ;
018700090203 3     endif ;
018800080110
018900101019 3     if wtrov<>'S' ;
019000090203       chain   kpiv      WFDEmp2l  ;
019100090203 4     if %found(wfdemp2l) ;
019200080110       Wtrov='S' ;
019300090203 4     endif ;
019400090203 3     endif ;
019500090203 2     endif ;
019600080110
019700101020 2     if  kcdf<>*blanks  and kcdf<>'04507990150' and kcdf<>'09999999999'   ;
019800080110
019900090203 3     if wtrov<>'S' ;
020000090203       chain   kcdf    WFdemp1l  ;
020100090203 4     if %found(wfdemp1l ) ;
020200080110       Wtrov='S' ;
020300090203 4     endif ;
020400090203 3     endif ;
020500080110
020600090203 3     if wtrov<>'S' ;
020700090203       chain   kcdf    WFDEmp2l  ;
020800090203 4     if %found(wfdemp2l) ;
020900080110       Wtrov='S' ;
021000090203 4     endif ;
021100090203 3     endif ;
021200080110
021300090203 2     endif ;
021400080110
021500090203 2     if  wtrov='S'  ;
021600090203       chain  demcpo   wfdemp3l ;
021700080110                  select   ;
021800090203                  when demunif1=0       ;
021900090206                  demunif1=u1_demkscu         ;
022000090203                  when demunif2=0       ;
022100090206                  demunif2=u1_demkscu         ;
022200090203                  when demunif3=0       ;
022300090206                  demunif3=u1_demkscu       ;
022400090203                  when demunif4=0       ;
022500090206                  demunif4=u1_demkscu       ;
022600090203                  when demunif5=0       ;
022700090206                  demunif5=u1_demkscu       ;
022800090203                  when demunif6=0       ;
022900090206                  demunif6=u1_demkscu       ;
023000090203                  when demunif7=0       ;
023100090206                  demunif7=u1_demkscu       ;
023200090203                  when demunif8=0       ;
023300090206                  demunif8=u1_demkscu       ;
023400090203                  when demunif9=0       ;
023500090206                  demunif9=u1_demkscu       ;
023600090203                  when demunif0=0       ;
023700090206                  demunif0=u1_demkscu       ;
023800080110                  other          ;
023900090203                  demuniol='S'  ;
024000080110                  endsl  ;
024100080110
024200090203                demcli='S' ;
024300090203                update wfdemp03;
024400090203
024500090203                // updato flagg utilizzo cli unificante x demp
024600090203                u1_demusa='S'       ;
024700090203                update wfdemu01  ;
024800090203 2     endif  ;
024900080110
025000090203       read       wfdemu1l    ;
025100090203 1     enddo ;
025200080110
025300071217
025400090203       // 4 ) Aggiornamento codici unificanti su DEMP non trovati
025500090203       //             da cnaco
025600080110
025700090203       setll   *start    wfdemp0f ;
025800090203       read   wfdemp0f  ;
0259000902031       dow   not %eof(wfdemp0f)  ;
026000080110
026100071213
026200071213        xx=0  ;
026300080111        clear   uni ;
026400090203             uni(1) =   demunif1 ;
026500090203             uni(2) =   demunif2 ;
026600090203             uni(3) =   demunif3 ;
026700090203             uni(4) =   demunif4 ;
026800090203             uni(5) =   demunif5 ;
026900090203             uni(6) =   demunif6 ;
027000090203             uni(7) =   demunif7 ;
027100090203             uni(8) =   demunif8 ;
027200090203             uni(9) =   demunif9 ;
027300090203             uni(10) =  demunif0 ;
027400090203             giapre(1) =   demunif1 ;
027500090203             giapre(2) =   demunif2 ;
027600090203             giapre(3) =   demunif3 ;
027700090203             giapre(4) =   demunif4 ;
027800090203             giapre(5) =   demunif5 ;
027900090203             giapre(6) =   demunif6 ;
028000090203             giapre(7) =   demunif7 ;
028100090203             giapre(8) =   demunif8 ;
028200090203             giapre(9) =   demunif9 ;
028300090203             giapre(10) =  demunif0 ;
028400071212
028500071213         // Cerco con la partita IVA
028600090203         if   dempiv<>*blanks  and %subst(dempiv:1:2)<>'$$'
028700101020                        AND   dempiv<>'04507990150'  and dempiv<>'09999999999' ;
028800090203         kcdf=dempiv ;
028900071213         EXSR    CERCAPIV  ;
029000071217         EXSR    CERCACDF  ;
029100071213 3       endif   ;
029200071213
029300071213         // Cerco con il codice fiscale
029400101020 3       if demcdf<>*blanks AND demcdf<>'04507990150' and demcdf<>'09999999999';
029500090203         kcdf=demcdf ;
029600071213         EXSR    CERCACDF  ;
029700071217         EXSR    CERCAPIV  ;
029800071213 3       endif   ;
029900080110
030000080110       // Scrivo gli unificanti ed aggiorno se non ancora fatto altri
030100080110       //    menti cliente da eliminare
030200071213
030300080110 3     if   uni(1)>0 ;
030400080110        kcod= %editc(uni(1): 'X' ) ;
030500080110        wksc= cod(1)               ;
030600080110        exsr MEMECOD  ;
030700080110 3     endif   ;
030800080110       if   uni(2)>0 ;
030900080110        kcod= %editc(uni(2): 'X' ) ;
031000080110        wksc= cod(2)               ;
031100080110        exsr MEMECOD  ;
031200080110       endif   ;
031300080110       if   uni(3)>0 ;
031400080110        kcod= %editc(uni(3): 'X' ) ;
031500080110        wksc= cod(3)               ;
031600080110        exsr MEMECOD  ;
031700080110       endif   ;
031800080110       if   uni(4)>0 ;
031900080110        kcod= %editc(uni(4): 'X' ) ;
032000080110        wksc= cod(4)               ;
032100080110        exsr MEMECOD  ;
032200080110       endif   ;
032300080110       if   uni(5)>0 ;
032400080110        kcod= %editc(uni(5): 'X' ) ;
032500080110        wksc= cod(5)               ;
032600080110        exsr MEMECOD  ;
032700080110       endif   ;
032800080110       if   uni(6)>0 ;
032900080110        kcod= %editc(uni(6): 'X' ) ;
033000080110        wksc= cod(6)               ;
033100080110        exsr MEMECOD  ;
033200080110       endif   ;
033300080110       if   uni(7)>0 ;
033400080110        kcod= %editc(uni(7): 'X' ) ;
033500080110        wksc= cod(7)               ;
033600080110        exsr MEMECOD  ;
033700080110       endif   ;
033800080110       if   uni(8)>0 ;
033900080110        kcod= %editc(uni(8): 'X' ) ;
034000080110        wksc= cod(8)               ;
034100080110        exsr MEMECOD  ;
034200080110       endif   ;
034300080110       if   uni(9)>0 ;
034400080110        kcod= %editc(uni(9): 'X' ) ;
034500080110        wksc= cod(9)               ;
034600080110        exsr MEMECOD  ;
034700080110       endif   ;
034800080110 3     if   uni(10)>0 ;
034900080110        kcod= %editc(uni(10): 'X' ) ;
035000080110        wksc= cod(10)               ;
035100080110        exsr MEMECOD  ;
035200080110 3     endif   ;
035300080110
035400080102
035500090203       update wfdemp00 ;
035600080102
035700071212
035800090203       read   wfdemp00  ;
035900071213 1      enddo   ;
036000071218
036100090203 0      endif   ;
036200090204
036300090209       // 5 ) Aggiornamento dati di wadel  su WFDEMP
0364000902040      if elabdempD='S' or elabcpo='S' or elabwadel='S'    ;
036500090204       exsr DatiWADEL    ;
036600090204       endif   ;
036700090204
036800090209
036900090209       // 6 ) Aggiornamento codice potenziale per info su WFDEMU
037000090210       if (elabdemuc='S' or elabinfo='S')  and elabwadel=' '  ;
037100090209       setll *loval  wfdemu1l   ;
037200090209       read          wfdemu1l   ;
037300090209       dow   not %eof(wfdemu1l)  ;
037400090210       if elabdemuc='S'   ;
037500090209       clear  u1_demcpoifo       ;
037600090210       clear  u1_demcpi          ;
037700090210       clear  u1_demcpip         ;
037800090210       endif   ;
037900090310       clear  u1_dem100b         ;
038000090209       clear  u1_demspt          ;
038100090209       clear  u1_demfac          ;
038200090209       clear  u1_demprf          ;
038300090209       clear  u1_dempro          ;
038400090209       clear  u1_demmes          ;
038500090209       clear  u1_demaer          ;
038600090209       clear  u1_demcae          ;
038700090209       clear  u1_dematr          ;
038800090209       clear  u1_dembar          ;
038900090209       clear  u1_demmez          ;
039000090209       clear  u1_demarco         ;
039100090209       clear  u1_demarto         ;
039200090209       clear  u1_demaws          ;
039300090209       clear  u1_demsusa         ;
039400090209       clear  u1_demtnt          ;
039500090209       clear  u1_demups          ;
039600090209       clear  u1_demcatr         ;
039700090209       clear  u1_dembars         ;
039800090209       clear  u1_demdhl          ;
039900090209       clear  u1_demexec         ;
040000090209       clear  u1_demferc         ;
040100090209       clear  u1_demmail         ;
040200090209       clear  u1_demmtn          ;
040300090209       clear  u1_demsda          ;
040400090310       clear  u1_demit0001       ;
040500090310       clear  u1_demit0002       ;
040600090310       clear  u1_demit0003       ;
040700090310       clear  u1_demit0004       ;
040800090310       clear  u1_demit0005       ;
040900090310       clear  u1_demes1001       ;
041000090310       clear  u1_demes1002       ;
041100090310       clear  u1_demes1003       ;
041200090310       clear  u1_demes1004       ;
041300090310       clear  u1_demes1005       ;
041400090310       clear  u1_dem__9999       ;
041500090209
041600090209       update wfdemu01  ;
041700090209
041800090209       read          wfdemu1l   ;
041900090209       enddo   ;
042000090209       endif  ;
042100090209
042200090209       if elabdemuc='S'  or elabwadel='S'  ;
042300090209       exsr AggioperINFO  ;
042400090209
042500090209       exsr AggiodatiInfo ;
042600090209       else   ;
042700090209
042800090209       if elabinfo='S'   ;
042900090209       exsr AggiodatiInfo ;
043000090209       endif  ;
043100090209       endif  ;
043200090209
043300071121       *inlr=*on         ;
043400071116       //-------------------------------------------------------------
043500090202       //  Scrittura dile mappatura base potenziali
043600090202       //-------------------------------------------------------------
043700090202       BEGSR  ScriviDEMP    ;
043800071122
043900090212       clear wfdemp00      ;
044000090202       demcpo = cpocpo     ;
044100071122
044200090202       demrag  = cporag    ;
044300090202       demvia  = cpovia    ;
044400090202       demcit  = cpocit    ;
044500090202       demcap  = cpocap         ;
044600090202       demprv  = cpoprv    ;
044700090202       demnaz  = cponaz    ;
044800071121
044900090202       dempiv  = cpopiv    ;
045000090202       demcdf  = cpocdf     ;
045100090212       demtotfat=cpoftaz*1000    ;
045200090202       demexport=cpopexp    ;
045300090202       demcmerc =%editc(cposct: 'X')     ;
045400090202       clear   kkey  ;
045500090203       kkey= demcmerc              ;
045600090202         chain (1: '1L': kkey) tabel00f ;
045700090202         if %found(tabel00f) ;
045800090202         demdmerc=tbluni   ;
045900090202         endif  ;
046000071122
046100090319       clear   demfil   ;
046200071122       exsr   CALLTisi  ;
046300090210
046400090210       // Aggancio info spesa trasporti per prendere se c'e' la spesa reale
046500090210       chain (demcpo:'SPT')  ticpi01l   ;
046600090212 1     if %found(ticpi01l) and cpipft>250   ;
046700090210         demsptrel=cpipft    ;
046800090210       endif   ;
046900090210
047000071122
047100090202       write  wfdeMP00       ;
047200071122
047300071116       ENDSR                ;
047400071122       //-------------------------------------------------------------
047500090202       // Chiamata al TISI per avere la filiale da cappario, no messag
047600090202       //-------------------------------------------------------------
047700071122       BEGSR  CALLTisi      ;
047800071122
047900090211       clear demdfil   ;
048000090211       clear demare   ;
048100090211       clear demdare  ;
048200090211       clear demdiv   ;
048300090211       clear demddiv  ;
048400090211
048500090319       // se presente la nazione, imposto la filiale del codice
048600090319       //  e non da cappario
048700090319       if demnaz= '  '   ;
048800071122       clear   tisi95ds  ;
048900071122       clear   fnlv13ds  ;
049000071122       i95tcn='7'        ;
049100071122       i13la3='S'     ;
049200090202       i95cap=demcap  ;
049300090202       i95loc=demcit  ;
049400090202       i95prv=demprv  ;
049500090202       i95nar=demnaz  ;
049600090202       // data di elaborazione: 1 gennaio anno in corso
049700090202       Annocor=%subdt(datasys:*y) ;
049800090203       i95dat=%int(%editc(annocor:'X') +'0101') ;
049900071122       i13af0='S'     ;
050000071129       i13cnv=' '     ;
050100071129       i13af1=' '     ;
050200090211       if %subst(demcap:1:2)='47 'and  demprv='  ' ;
050300090211            demprv='RN'  ;
050400090211            I95PRV='RN'  ;
0505000902111        endif  ;
050600071211
050700071122       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
050800071122
050900090202       demfil=o95lna       ;
051000090319       endif   ;
051100090319
051200071212       // se trovato imposto area
051300071213
0514000902021      if    demfil>0    ;
051500090203       chain   demfil   AZORG01L   ;
0516000902022      if %found(azorg01l) ;
051700090202         demdfil=orgdes   ;
051800090202         demare=orgcar ;
051900090206         demdiv=orgfl3 ;
0520000902022      endif  ;
052100080102
0522000902022      if demare>0                      ;
052300080102         clear   kkey  ;
052400090203         kkey=%editc(demare: 'X')    ;
052500080102         chain (1: '05': kkey) tabel00f ;
0526000902023        if %found(tabel00f) ;
052700090202          demdare=tbluni  ;
0528000902023        endif  ;
0529000902022        endif  ;
053000090206       if demdiv<>' ' ;
053100090206         clear   kkey  ;
053200090206         kkey=demdiv                 ;
053300090206         chain (1: '17': kkey) tabel00f ;
053400090206         if %found(tabel00f) ;
053500090206          demddiv=tbluni  ;
053600090206         endif  ;
053700090206       endif  ;
053800090206
0539000902021        endif  ;
054000071122
054100071122       ENDSR                ;
054200090202       //-------------------------------------------------------------
054300090202       //  Scrittura file mappatura base u8nificanti da WADEL
054400090202       //-------------------------------------------------------------
054500090202       BEGSR  ScriviDEMU    ;
054600090202
054700090205       setll  *loval  wfdemd3l  ;
054800090205       read    wfdemd3l ;
054900090202
055000090205 1     dow  not %eof(wfdemd3l);
055100090202
055200090202         // Escludo distretto estero   agente unificante
055300090202         wfil=%int(%subst(%editc(wapauc:'X'):1:3))     ;
055400090202         chain   wfil   azorg01l   ;
055500090202
055600090202 2a      if orgfl3<>'7'    ;
055700090202
055800090202         kcod = %editc(wapcli: 'X' ) ;
055900090202         // Escludo codici indirizzo italia ma esteri
056000090202            if kcod<>'0894106' and kcod<>'0894274' ;
056100090202
056200090202         // Escludo 8888  e 9999
056300090202 2         if (%subst(kcod:4:4)<>'9999' and %subst(kcod:4:4)<>'8888') ;
056400090202
056500090202         // Escludo senza fatturato  o senza spedizioni  anno corrente
056600090202 3          if   wapfac>0 and wapsac>0;
056700090202
056800090202             chain (kut:dutkci:wapcli)  cnclp00f ;
056900090202
057000090202 4          if wapblo=' ' ;
057100090202
057200090202             clear  wfdemu00   ;
057300090202
057400090202             demkscu=wapcli ;
057500090202             demrag =wapcld ;
057600090202             demcmmu=wapauc ;
057700090202             demdcmm=wapaud ;
057800090202             demclv =wapclv;
057900090202             select  ;
058000090202             when demclv='T'   ;
058100090202             demord='1'  ;
058200090202             when demclv='A'   ;
058300090202             demord='2'  ;
058400090202             when demclv='B'   ;
058500090202             demord='3'  ;
058600090202             when demclv='C'   ;
058700090202             demord='4'  ;
058800090202             when demclv='D'   ;
058900090202             demord='5'  ;
059000090202             other   ;
059100090203             demord='6'  ;
059200090202             endsl    ;
059300090202
059400090206             demvia =wapind    ;
059500090206             demcit =waploc    ;
059600090202             demcap =wapcap;
059700090202             demprv =wapprv;
059800090202             demnaz =wapnaz;
059900090202             dempiv =wapiva;
060000090202             demcdf =wapcdf;
060100090202             demdmerc=wapitc;
060200090202
060300090202               chain (kut:dutkci:wapcli)  cnaco00f ;
060400090202 5              if %found(cnaco00f) ;
060500090203                 demrag  =%triml(acorag)   ;
060600090309                 demcmerc=%subst(%editc(acoitc:'X'):3:5)          ;
060700090202
060800090202             kkey=demcmerc               ;
060900090202             chain (1: '1L': kkey) tabel00f ;
061000090202 6           if %found(tabel00f) ;
061100090202              demdmerc=tbluni  ;
061200090202 6           endif  ;
061300090202 5           endif  ;
061400090202
061500090202             chain (kut:dutkci:wapcli)  cnind00f ;
061600090202 5           if %found(cnind00f) ;
061700090202             demcap =indcae;
061800090202             demprv =indprv;
061900090202             demnaz =indsta;
062000090202             dempiv =indiva;
062100090202             demcdf =indcdf;
062200090206             demvia =indvia    ;
062300090206             demcit =indcit   ;
062400090202 5         endif    ;
062500090319
062600090319           clear demfil   ;
062700090319           if demnaz<>'  ' ;
062800090319           demfil=wapfil   ;
062900090319           endif    ;
063000090202
063100090210           exsr CallTISI   ;
063200090210
063300090202           write(e) wfdemu00   ;
063400090202
063500090202 4         endif  ;
063600090202 4         endif  ;
063700090202 3         endif  ;
063800090202 2         endif  ;
063900090202 2a        endif  ;
064000090202
064100090205       read    wfdemd3l ;
064200090202 1     enddo ;
064300090202
064400090202       ENDSR   ;
064500071212
064600071212       //-------------------------------------------------------------
064700090203       //   cerco clienti unificanti con la partita iva
064800090203       //-------------------------------------------------------------
064900071213       BEGSR  CercaPIV      ;
065000071213
065100090203       setll  (kut:dutkci:kcdf)   cnind02l   ;
065200090203       reade  (kut:dutkci:kcdf)   cnind02l   ;
0653000712131      dow   not %eof(cnind02l)  ;
065400071213       wksc=indksc   ;
065500071213       exsr CALLTIBS   ;
065600071213
065700090203       reade  (kut:dutkci:kcdf)   cnind02l   ;
065800071213 1     enddo    ;
065900071213
066000071213       ENDSR                ;
066100071213       //-------------------------------------------------------------
066200090203       //   cerco clienti unificanti con la codice fiscale
066300090203       //-------------------------------------------------------------
066400071213       BEGSR  CercaCDF      ;
066500071213
066600090203       setll  (kut:dutkci:kCDF)   cnind03l   ;
066700090203       reade  (kut:dutkci:kcdf)   cnind03l   ;
0668000712131      dow   not %eof(cnind03l)  ;
066900071213       wksc=indksc   ;
067000071213       exsr CALLTIBS   ;
067100071213
067200090203       reade  (kut:dutkci:kcdf)   cnind03l   ;
067300071213 1     enddo    ;
067400071213
067500071213       ENDSR                ;
067600071213       //-------------------------------------------------------------
067700071213       BEGSR   CALLTIBS ;
067800071213
067900071213       clear  tibs10ds ;
068000071213       d10tle='ST' ;
068100071213       d10paf='F' ;
068200071213       d10cod=wksc   ;
068300090206       Annocor=%subdt(datasys:*y) ;
068400090206       d10drf=%int(%editc(annocor:'X') +'0101') ;
068500071213       callp   TIBS10R  (TIBS10ds:tipexe) ;
068600071213
068700071217 1       if d10err<>' ' ;
068800071213       d10err='  ' ;
068900071213       d10paf='P' ;
069000090206       Annocor=%subdt(datasys:*y) ;
069100090206       d10drf=%int(%editc(annocor:'X') +'0101') ;
069200071213       callp   TIBS10R  (TIBS10ds:tipexe) ;
069300071217 1       endif  ;
069400071213
069500071217 1       if d10err<>' '   ;
069600071213           wuni=wksc     ;
069700071213         else  ;
069800071213           wuni=d10cop ;
069900071217 1       endif  ;
070000071213
070100071213         trov=1 ;
070200071213         trov=%lookup (wuni:uni) ;
070300071213 2         if  trov=0   ;
070400071213           xx=xx+1     ;
070500080110 3            if xx<=100 ;
070600071213                   uni(XX)=wuni  ;
070700080110                   cod(XX)=wksc  ;
070800071213 3            endif     ;
070900071213 2         endif     ;
071000071213       ENDSR  ;
071100080110       //-------------------------------------------------------------
071200080110       BEGSR  MEMECOD       ;
071300080111       wuni=%int(kcod)  ;
071400090205       clear UsaUnific   ;
071500080111
071600080111         trov=%lookup (wuni:giapre) ;
071700090205       if trov=0 ;
071800080111
071900090203       chain   (wuni)      wfdemu1l  ;
072000090205
072100090205       // SE NON TROVATO VERIFICO SE c'e' negli unificati da
072200090205       //  wfdemd0f
072300090205       if not %found(wfdemu1l)   ;
072400090205         chain   (wuni)      wfdemd3l  ;
072500090205
072600090205 1       if %found(WFdemd3l) and wapunio='S';
072700090205         // richaino col suo unificato
072800090205           chain   (wapcliuni)  wfdemu1l  ;
072900090205           UsaUnific='S'    ;
073000090205         endif   ;
073100090205       endif   ;
073200080110
073300090203 1     if %found(WFdemu1l) ;
073400090203
073500090203 2     if u1_demusa<>' '    ;
073600080110
073700090203 3     if demerr=*blanks    ;
073800090203       demerr='+ CPO PER STESSO UNIFICANTE '+ %char(wksc) ;
073900080110 3     endif  ;
074000080110
074100080110 x2    ELSE  ;
074200090205       if UsaUnific=' ';
074300090205         u1_demusa='D' ;
074400090205       else   ;
074500090205         u1_demusa='U' ;
074600090205       endif   ;
074700090203       update wfdemu01   ;
074800080110  3               select   ;
074900090203                  when demunif1=0       ;
075000090203                  demunif1=%int(kcod)  ;
075100090203                  when demunif2=0       ;
075200090203                  demunif2=%int(kcod)  ;
075300090203                  when demunif3=0       ;
075400090203                  demunif3=%int(kcod)  ;
075500090203                  when demunif4=0       ;
075600090203                  demunif4=%int(kcod)  ;
075700090203                  when demunif5=0       ;
075800090203                  demunif5=%int(kcod)  ;
075900090203                  when demunif6=0       ;
076000090203                  demunif6=%int(kcod)  ;
076100090203                  when demunif7=0       ;
076200090203                  demunif7=%int(kcod)  ;
076300090203                  when demunif8=0       ;
076400090203                  demunif8=%int(kcod)  ;
076500090203                  when demunif9=0       ;
076600090203                  demunif9=%int(kcod)  ;
076700090203                  when demunif0=0       ;
076800090203                  demunif0=%int(kcod)  ;
076900080110                  other          ;
077000090203                  demuniol='S'  ;
077100080110  3               endsl  ;
077200080110
077300090203                demcli='S' ;
077400080110  2    endif  ;
077500080110  1    endif  ;
077600080111  1    endif  ;
077700080110
077800080110       ENDSR  ;
077900090204       //--------------------------------------------------------------
078000090204       //?Aggiorno file wfdemp con i dati di wadel
078100090204       //--------------------------------------------------------------
078200090204       BEGSR DatiWADEL   ;
078300090204
078400090204        clear   wfdemp00  ;
078500090204        clear maxfac ;
078600090204
078700090204        setll  *start    Wfdemp0f   ;
078800090204        read    wfdemp00    ;
078900090204
079000090204 1      dow  not  %eof(wfdemp0f)  ;
079100090204        uni(1)=demunif1 ;
079200090204        uni(2)=demunif2 ;
079300090204        uni(3)=demunif3 ;
079400090204        uni(4)=demunif4 ;
079500090204        uni(5)=demunif5 ;
079600090204        uni(6)=demunif6 ;
079700090204        uni(7)=demunif7 ;
079800090204        uni(8)=demunif8 ;
079900090204        uni(9)=demunif9 ;
080000090204        uni(10)=demunif0 ;
080100090204        xx=1 ;
080200090204
080300090204 2      dow   uni(xx) > 0 ;
080400090204
080500090204       wuni=uni(xx)   ;
080600090205       chain   wuni    wfdemi3l   ;
080700090204
080800090205 3      if %found(wfdemi3l)   ;
080900090204
081000090204        iwapita='I'  ;
081100090204        iwapsap= wapsap+iwapsap  ;
081200090204        iwapfap= wapfap+iwapfap ;
081300090204        iwapsac= wapsac+iwapsac  ;
081400090410        iwapfac= wapfac+iwapfac  ;
081500090204
081600090204        iwaptotrpp=waprp_01ap+waprp_02ap+waprp_03ap+waprp_04ap+waprp_05ap+
081700090204                   waprp_06ap+waprp_07ap+waprp_08ap+waprp_09ap+waprp_10ap+
081800090204                   waprp_11ap+waprp_12ap + iwaptotrpp  ;
081900090204        iwaptotrpc=waprp_01ac+waprp_02ac+waprp_03ac+waprp_04ac+waprp_05ac+
082000090204                   waprp_06ac+waprp_07ac+waprp_08ac+waprp_09ac+waprp_10ac+
082100090204                   waprp_11ac+waprp_12ac + iwaptotrpc ;
082200090204        iwapns01ac=wapns_01ac + iwapns01ac  ;
082300090204        iwapns02ac=wapns_02ac + iwapns02ac  ;
082400090204        iwapns03ac=wapns_03ac + iwapns03ac  ;
082500090204        iwapns04ac=wapns_04ac + iwapns04ac  ;
082600090204        iwapns05ac=wapns_05ac + iwapns05ac  ;
082700090204        iwapns06ac=wapns_06ac + iwapns06ac  ;
082800090204        iwapns07ac=wapns_07ac + iwapns07ac  ;
082900090204        iwapns08ac=wapns_08ac + iwapns08ac  ;
083000090204        iwapns09ac=wapns_09ac + iwapns09ac  ;
083100090204        iwapns10ac=wapns_10ac + iwapns10ac  ;
083200090204        iwapns11ac=wapns_11ac + iwapns11ac  ;
083300090204        iwapns12ac=wapns_12ac + iwapns12ac  ;
083400090204
083500090204        iwaprr01ac=waprr_01ac + iwaprr01ac  ;
083600090204        iwaprr02ac=waprr_02ac + iwaprr02ac  ;
083700090204        iwaprr03ac=waprr_03ac + iwaprr03ac  ;
083800090204        iwaprr04ac=waprr_04ac + iwaprr04ac  ;
083900090204        iwaprr05ac=waprr_05ac + iwaprr05ac  ;
084000090204        iwaprr06ac=waprr_06ac + iwaprr06ac  ;
084100090204        iwaprr07ac=waprr_07ac + iwaprr07ac  ;
084200090204        iwaprr08ac=waprr_08ac + iwaprr08ac  ;
084300090204        iwaprr09ac=waprr_09ac + iwaprr09ac  ;
084400090204        iwaprr10ac=waprr_10ac + iwaprr10ac  ;
084500090204        iwaprr11ac=waprr_11ac + iwaprr11ac  ;
084600090204        iwaprr12ac=waprr_12ac + iwaprr12ac  ;
084700090204
084800090204        iwaprp01ac=waprp_01ac + iwaprp01ac  ;
084900090204        iwaprp02ac=waprp_02ac + iwaprp02ac  ;
085000090204        iwaprp03ac=waprp_03ac + iwaprp03ac  ;
085100090204        iwaprp04ac=waprp_04ac + iwaprp04ac  ;
085200090204        iwaprp05ac=waprp_05ac + iwaprp05ac  ;
085300090204        iwaprp06ac=waprp_06ac + iwaprp06ac  ;
085400090204        iwaprp07ac=waprp_07ac + iwaprp07ac  ;
085500090204        iwaprp08ac=waprp_08ac + iwaprp08ac  ;
085600090204        iwaprp09ac=waprp_09ac + iwaprp09ac  ;
085700090204        iwaprp10ac=waprp_10ac + iwaprp10ac  ;
085800090204        iwaprp11ac=waprp_11ac + iwaprp11ac  ;
085900090204        iwaprp12ac=waprp_12ac + iwaprp12ac  ;
086000090204 3      endif  ;
086100090204
086200090205       chain   wuni    wfdeme3l   ;
086300090204
086400090205 3      if %found(wfdeme3l)   ;
086500090204        ewapest='E'  ;
086600090204        ewapsap= wapsap+ewapsap  ;
086700090204        ewapfap= wapfap+ewapfap ;
086800090204        ewapsac= wapsac+ewapsac  ;
086900090410        ewapfac= wapfac+ewapfac  ;
087000090204
087100090204        ewaptotrpp=waprp_01ap+waprp_02ap+waprp_03ap+waprp_04ap+waprp_05ap+
087200090204                   waprp_06ap+waprp_07ap+waprp_08ap+waprp_09ap+waprp_10ap+
087300090204                   waprp_11ap+waprp_12ap + ewaptotrpp  ;
087400090204        ewaptotrpc=waprp_01ac+waprp_02ac+waprp_03ac+waprp_04ac+waprp_05ac+
087500090204                   waprp_06ac+waprp_07ac+waprp_08ac+waprp_09ac+waprp_10ac+
087600090204                   waprp_11ac+waprp_12ac + ewaptotrpc ;
087700090204        ewapns01ac=wapns_01ac + ewapns01ac  ;
087800090204        ewapns02ac=wapns_02ac + ewapns02ac  ;
087900090204        ewapns03ac=wapns_03ac + ewapns03ac  ;
088000090204        ewapns04ac=wapns_04ac + ewapns04ac  ;
088100090204        ewapns05ac=wapns_05ac + ewapns05ac  ;
088200090204        ewapns06ac=wapns_06ac + ewapns06ac  ;
088300090204        ewapns07ac=wapns_07ac + ewapns07ac  ;
088400090204        ewapns08ac=wapns_08ac + ewapns08ac  ;
088500090204        ewapns09ac=wapns_09ac + ewapns09ac  ;
088600090204        ewapns10ac=wapns_10ac + ewapns10ac  ;
088700090204        ewapns11ac=wapns_11ac + ewapns11ac  ;
088800090204        ewapns12ac=wapns_12ac + ewapns12ac  ;
088900090204
089000090204        ewaprr01ac=waprr_01ac + ewaprr01ac  ;
089100090204        ewaprr02ac=waprr_02ac + ewaprr02ac  ;
089200090204        ewaprr03ac=waprr_03ac + ewaprr03ac  ;
089300090204        ewaprr04ac=waprr_04ac + ewaprr04ac  ;
089400090204        ewaprr05ac=waprr_05ac + ewaprr05ac  ;
089500090204        ewaprr06ac=waprr_06ac + ewaprr06ac  ;
089600090204        ewaprr07ac=waprr_07ac + ewaprr07ac  ;
089700090204        ewaprr08ac=waprr_08ac + ewaprr08ac  ;
089800090204        ewaprr09ac=waprr_09ac + ewaprr09ac  ;
089900090204        ewaprr10ac=waprr_10ac + ewaprr10ac  ;
090000090204        ewaprr11ac=waprr_11ac + ewaprr11ac  ;
090100090204        ewaprr12ac=waprr_12ac + ewaprr12ac  ;
090200090204
090300090204        ewaprp01ac=waprp_01ac + ewaprp01ac  ;
090400090204        ewaprp02ac=waprp_02ac + ewaprp02ac  ;
090500090204        ewaprp03ac=waprp_03ac + ewaprp03ac  ;
090600090204        ewaprp04ac=waprp_04ac + ewaprp04ac  ;
090700090204        ewaprp05ac=waprp_05ac + ewaprp05ac  ;
090800090204        ewaprp06ac=waprp_06ac + ewaprp06ac  ;
090900090204        ewaprp07ac=waprp_07ac + ewaprp07ac  ;
091000090204        ewaprp08ac=waprp_08ac + ewaprp08ac  ;
091100090204        ewaprp09ac=waprp_09ac + ewaprp09ac  ;
091200090204        ewaprp10ac=waprp_10ac + ewaprp10ac  ;
091300090204        ewaprp11ac=waprp_11ac + ewaprp11ac  ;
091400090204        ewaprp12ac=waprp_12ac + ewaprp12ac  ;
091500090204 3      endif  ;
091600090204
091700090204        // Dati generici del cliente con maggior fatturato se + di uno  ;
091800090409        chain   wuni    wfdemd3l   ;
0919000904093       if %found(wfdemd3l)   ;
0920000904094       if  wapfac>maxfac ;
092100090409        maxfac=wapfac     ;
092200090204        cwapfil=wapfil        ;
092300090204        cwapfid=wapfid        ;
092400090204        cwapage=wapage        ;
092500090204        cwapagd=wapagd        ;
092600090204        cwapitc=wapitc        ;
092700090204        cwapclv=wapclv        ;
092800090204        cwapblo=wapblo        ;
092900090204        clear cwapare ;
093000090204        clear cwapard ;
093100090409
093200090204       chain  cwapfil   AZORG01L   ;
0933000904095      if %found(azorg01l) ;
093400090204         cwapare=orgcar ;
093500090204
093600090204         clear   kkey  ;
093700090204         kkey=%editc(cwapare: 'X')    ;
093800090204         chain (1: '05': kkey) tabel00f ;
0939000904096        if %found(tabel00f) ;
094000090204          cwapard=tbluni  ;
0941000904096        endif  ;
0942000904095        endif  ;
0943000904094        endif  ;
0944000904093        endif  ;
094500090204
094600090204        xx=xx+1  ;
094700090204 2      enddo   ;
094800090204
094900090204        update  wfdemp00  ;
095000090204        clear   wfdemp00  ;
095100090204        clear maxfac ;
095200090204
095300090204       read   wfdemp00  ;
095400090204 1     enddo   ;
095500090204
095600090204       ENDSR  ;
095700090203
095800090203       //--------------------------------------------------------------
095900090203       //?Reperimento Dati del job (Utente/Operativi).
096000090203       //--------------------------------------------------------------
096100090203       BEGSR DatiJob;
096200090203
096300090203         in(E) §AzUte;
096400090203         if NOT %error;
096500090203           in(E) §DatiUte;
096600090203         endif;
096700090203         if %error or RSut = *blanks;
096800090203           clear TIBS34ds;
096900090203           tibs34r(tibs34ds);
097000090203           in §AzUte;
097100090203           in §DatiUte;
097200090203         endif;
097300090203
097400090203       ENDSR;
097500090206       //--------------------------------------------------------------
097600090206       //?pulizia campi aggioranti da WADEL
097700090206       //--------------------------------------------------------------
097800090206        BEGSR cleardaWADEL   ;
097900090206        clear demunif1       ;
098000090206        clear demunif2       ;
098100090206        clear demunif3       ;
098200090206        clear demunif4       ;
098300090206        clear demunif5       ;
098400090206        clear demunif6       ;
098500090206        clear demunif7       ;
098600090206        clear demunif8       ;
098700090206        clear demunif9       ;
098800090206        clear demunif0       ;
098900090206        clear demuniol       ;
099000090206        clear demcli         ;
099100090206        clear demerr         ;
099200090206        clear iwapita ;
099300090206        clear iwapsap ;
099400090206        clear iwapfap ;
099500090206        clear iwaptotrpp ;
099600090206        clear iwapsac ;
099700090206        clear iwapfac ;
099800090206        clear iwaptotrpc ;
099900090206        clear iwapns01ac ;
100000090206        clear iwaprr01ac ;
100100090206        clear iwaprp01ac ;
100200090206        clear iwapns02ac ;
100300090206        clear iwaprr02ac ;
100400090206        clear iwaprp02ac ;
100500090206        clear iwapns03ac ;
100600090206        clear iwaprr03ac ;
100700090206        clear iwaprp03ac ;
100800090206        clear iwapns04ac ;
100900090206        clear iwaprr04ac ;
101000090206        clear iwaprp04ac ;
101100090206        clear iwapns05ac ;
101200090206        clear iwaprr05ac ;
101300090206        clear iwaprp05ac ;
101400090206        clear iwapns06ac ;
101500090206        clear iwaprr06ac ;
101600090206        clear iwaprp06ac ;
101700090206        clear iwapns07ac ;
101800090206        clear iwaprr07ac ;
101900090206        clear iwaprp07ac ;
102000090206        clear iwapns08ac ;
102100090206        clear iwaprr08ac ;
102200090206        clear iwaprp08ac ;
102300090206        clear iwapns09ac ;
102400090206        clear iwaprr09ac ;
102500090206        clear iwaprp09ac ;
102600090206        clear iwapns10ac ;
102700090206        clear iwaprr10ac ;
102800090206        clear iwaprp10ac ;
102900090206        clear iwapns11ac ;
103000090206        clear iwaprr11ac ;
103100090206        clear iwaprp11ac ;
103200090206        clear iwapns12ac ;
103300090206        clear iwaprr12ac ;
103400090206        clear iwaprp12ac ;
103500090206
103600090206        clear ewapest ;
103700090206        clear ewapsap ;
103800090206        clear ewapfap ;
103900090206        clear ewaptotrpp ;
104000090206        clear ewapsac ;
104100090206        clear ewapfac ;
104200090206        clear ewaptotrpc ;
104300090206        clear ewapns01ac ;
104400090206        clear ewaprr01ac ;
104500090206        clear ewaprp01ac ;
104600090206        clear ewapns02ac ;
104700090206        clear ewaprr02ac ;
104800090206        clear ewaprp02ac ;
104900090206        clear ewapns03ac ;
105000090206        clear ewaprr03ac ;
105100090206        clear ewaprp03ac ;
105200090206        clear ewapns04ac ;
105300090206        clear ewaprr04ac ;
105400090206        clear ewaprp04ac ;
105500090206        clear ewapns05ac ;
105600090206        clear ewaprr05ac ;
105700090206        clear ewaprp05ac ;
105800090206        clear ewapns06ac ;
105900090206        clear ewaprr06ac ;
106000090206        clear ewaprp06ac ;
106100090206        clear ewapns07ac ;
106200090206        clear ewaprr07ac ;
106300090206        clear ewaprp07ac ;
106400090206        clear ewapns08ac ;
106500090206        clear ewaprr08ac ;
106600090206        clear ewaprp08ac ;
106700090206        clear ewapns09ac ;
106800090206        clear ewaprr09ac ;
106900090206        clear ewaprp09ac ;
107000090206        clear ewapns10ac ;
107100090206        clear ewaprr10ac ;
107200090206        clear ewaprp10ac ;
107300090206        clear ewapns11ac ;
107400090206        clear ewaprr11ac ;
107500090206        clear ewaprp11ac ;
107600090206        clear ewapns12ac ;
107700090206        clear ewaprr12ac ;
107800090206        clear ewaprp12ac ;
107900090206
108000090206        clear cwapare    ;
108100090206        clear cwapard    ;
108200090206        clear cwapfil    ;
108300090206        clear cwapfid    ;
108400090206        clear cwapage    ;
108500090206        clear cwapagd    ;
108600090206        clear cwapblo    ;
108700090206        clear cwapitc    ;
108800090206        clear cwapclv    ;
108900090206        ENDSR   ;
109000090209       //-------------------------------------------------------------
109100090209       //  Aggiorno cod potenziale su WFDEMU0FF per le info
109200090209       //-------------------------------------------------------------
109300090209       BEGSR AggioperInfo   ;
109400090209       setll *loval   wfdemu1l  ;
109500090209       read           wfdemu1l  ;
109600090209       dow   not %eof(wfdemu1l)  ;
109700090209
109800090209           EXSR CercaPotSede   ;
109900090209
110000090209           xx=1   ;
110100090209           clear savfat   ;
110200090209
110300090209           dow potsede(xx)>0  ;
110400090209           // di quelli letti, memorizzo quello che ha le info
110500090209           //  col fatturato D&B più alto
110600090209
110700090209           if potinfo(XX)='S' or potinfop(xx)='S'  ;
110800090209             if  potfat(XX) >=savfat   ;
110900090209              u1_demcpoifo=potsede(XX) ;
111000090209              u1_demcpi   =potinfo(XX) ;
111100090209              u1_demcpip  =potinfop(XX) ;
111200090209              savfat=potfat(xx)   ;
111300090209             endif  ;
111400090209           endif  ;
111500090209
111600090209           xx=xx+1  ;
111700090209           enddo    ;
111800090209
111900090209       update wfdemu01  ;
112000090209
112100090209       read           wfdemu1l  ;
112200090209       enddo   ;
112300090209       ENDSR   ;
112400090209
112500090209       //-------------------------------------------------------------
112600090209       //?Cerca i potenziali sede associati  all'unificante
112700090209       //--------------------------------------------------------------
112800090209       BEGSR  CercaPotSede ;
112900090209
113000090209       // Prendo codice unificante e carico cod collegati
113100090209           clear tibs10ds ;
113200090209           d10tle='ST'   ;
113300090209           d10paf='F'    ;
113400090209           d10cod=u1_demkscu;
113500090209           callp TIBS10R (tibs10ds:tipexe)  ;
113600090209
113700090209  1        if d10err<>*blanks   ;
113800090209           // Se errore verifico se è figlio
113900090209           clear tibs10ds ;
114000090209           d10tle='ST'   ;
114100090209           d10paf='P'    ;
114200090209           d10cod=u1_demkscu;
114300090209           callp TIBS10R (tibs10ds:tipexe) ;
114400090209
114500090209  2        if d10err<>*blanks   ;
114600090209           // Imposto nella skiera e come padre solo se stesso
114700090209           skc(1)='0000'+%editc(u1_demkscu:'X');
114800090209           d10cop=u1_demkscu                  ;
114900090209  2        endif                ;
115000090209  1        endif ;
115100090209
115200090209       clear potsede  ;
115300090209       clear potfat   ;
115400090209       clear potinfo  ;
115500090209       clear potinfop ;
115600090209       clear xx       ;
115700090209       yy=1    ;
115800090209
115900090209 1     dow skc(yy)>*Zeros  ;
116000090209
116100090209       // Verifico se cnaco ha un codice SEDE attaccato ;
116200090209       wksc=%int(%subst(skc(yy):5:7)) ;
116300090209       chain   (1:dutkci:wksc)  cnaco00f  ;
116400090209 2     if %found(cnaco00f)   ;
116500090209
116600090209         chain  acolib   tncpo01l   ;
116700090209
116800090209 3       if %found(tncpo01l) and cpofsf='S'  ;
116900090209         exsr MemPotSede     ;
117000090209 3       endif     ;
117100090209 2     endif     ;
117200090209
117300090209       yy=yy+1   ;
117400090209 1     enddo     ;
117500090209
117600090209       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
117700090209       //  codice unificante, altri potenziali  SEDE
117800090209       wksc=d10cop;
117900090209       chain   (1:dutkci:wksc)  cnclp00f  ;
118000090209 1     if %found(cnclp00f)   ;
118100090209
118200090209       // col sottoconti int fattura
118300090209       chain   (1:dutkci:clpscf)  cnind00f  ;
118400090209 2     if %found(cnind00f) and indsta=*blanks  ;
118500090209         // con la partita iva
118600090209 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
118700101020                          AND INDIVA<>'04507990150'  and INDIVA<>'09999999999';
118800090209         setll indiva   tncpo06l  ;
118900090209         reade indiva   tncpo06l  ;
119000090209
119100090209 3         dow not %eof(tncpo06l)   ;
119200090209 4         if cpofsf='S'          ;
119300090209              exsr MempotSede       ;
119400090209 4         endif                  ;
119500090209
119600090209           reade indiva   tncpo06l  ;
119700090209 3       enddo                    ;
119800090209 2a      endif    ;
119900090209
120000090209         // con il codice fiscale
120100101020 2a      if indcdf<>*blanks and INDcdf<>'04507990150' and INDcdf<>'09999999999';
120200090209         setll indcdf   tncpo05l  ;
120300090209         reade indcdf   tncpo05l  ;
120400090209
120500090209 3         dow not %eof(tncpo05l)   ;
120600090209 4         if cpofsf='S'          ;
120700090209              exsr MempotSede      ;
120800090209 4         endif                  ;
120900090209
121000090209           reade indcdf   tncpo05l  ;
121100090209 3       enddo                    ;
121200090209 2a    endif                  ;
121300090209 2     endif                  ;
121400090209 1     endif                  ;
121500090209
121600090209       ENDSR  ;
121700090209       //--------------------------------------------------------------
121800090209       //?Memorizza potenziali sede per ogni unificante
121900090209       //--------------------------------------------------------------
122000090209       BEGSR MemPotSede;
122100090209
122200090209         // Memorizzo i codici visualizzati per evitare ripetizioni
122300090209         //           solo se ha le info complete o parziali
122400090209
122500090209         Indx=%lookup(cpocpo:potsede)   ;
122600090209 1         if Indx=0    ;
122700090209           dcpo01=cporst  ;
122800090209
122900090209           clear trmk50ds   ;
123000090209           // Memorizzo solo se ha le info complete o parziali
123100090209 2           if §cpoifotot <>'S'    ;
123200090209                    i50cpo=cpocpo     ;
123300090209                    i50mod='C'        ;
123400090209                    i50obl='S'        ;
123500090209                    callp TRMK50R (kpjba:trmk50ds)  ;
123600090209  2          endif   ;
123700090209
123800090209  2          if §cpoifotot = 'S'    or o50ifomin='S' ;
123900090209           xx=xx+1  ;
124000090209           potsede(xx)=cpocpo   ;
124100090209           potfat (xx)=cpoftaz  ;
124200090209           potinfo(xx)=§cpoifotot ;
124300090209           potinfop(xx)=o50ifomin  ;
124400090209
124500090209 2         endif;
124600090209 1         endif;
124700090209           ENDSR  ;
124800090209       //--------------------------------------------------------------
124900090209       //?Aggiorno dati delle info trovati
125000090209       //--------------------------------------------------------------
125100090209       BEGSR AggioDatiInfo  ;
125200090209       setll *loval  wfdemu1l   ;
125300090209       read          wfdemu1l   ;
125400090209       dow   not %eof(wfdemu1l)  ;
125500090209
125600090209       if u1_demcpoifo>0  ;
125700090209       Exsr LeggiCPI  ;
125800090209       endif   ;
125900090209
126000090209       read          wfdemu1l   ;
126100090209       enddo   ;
126200090209       ENDSR  ;
126300090209       //--------------------------------------------------------------
126400090209       //?lettura info commerciali
126500090209       //--------------------------------------------------------------
126600090209       BEGSR LeggiCPI      ;
126700090210
126800090209       // spesa trasporti
126900090210       chain (u1_demcpoifo:'SPT')  ticpi01l   ;
127000090212 1     if %found(ticpi01l) and cpipft>250   ;
127100090210       u1_demspt=cpipft    ;
127200090310
127300090310       //  se trovata spesa
127400090310       // chaino wfdemd per avere fatturato reale
127500090310         chain   (u1_demkscu)      wfdemd3l  ;
127600090310        if %found(wfdemd3l)  ;
127700090310          u1_demfac=WAPFAC  ;
127800090310        endif   ;
127900090310 1      endif   ;
128000090209
128100090209       //  se trovata spesa faccio la ripartizione
128200090209       setll u1_demcpoifo  ticpi01l   ;
128300090209       reade u1_demcpoifo  ticpi01l   ;
128400090209
128500090310 1     dow  not %eof(ticpi01l)  ;
128600090209
128700090310 2     if u1_demspt>0   ;
128800090310
128900090209 3     if  cpival>0 and cpitva='%'   ;
129000090210       ripSpesa=(u1_demspt*cpival)/100   ;
129100090209
129200090209 4     if cpitpf='10 '  ;
129300090209 5     select   ;
129400090209       when cpispf='_BAR'   ;
129500090210       u1_dembar=ripspesa ;
129600090209
129700090311       when cpispf='_MEP'   ;
129800090210       u1_demmez=ripspesa ;
129900090209
130000090209       when cpispf='ARCO'   ;
130100090210       u1_demARCO=ripspesa ;
130200090209
130300090209       when cpispf='ARTO'   ;
130400090210       u1_demARTO=ripspesa ;
130500090209
130600090209       when cpispf='AWS'   ;
130700090210       u1_demAWS=ripspesa ;
130800090209
130900090209       when cpispf='BARS'   ;
131000090210       u1_dembarS=ripspesa ;
131100090209
131200090209       when cpispf='DHL'   ;
131300090210       u1_demDHL=ripspesa ;
131400090209
131500090209       when cpispf='EXEC'   ;
131600090210       u1_demEXEC=ripspesa ;
131700090209
131800090209       when cpispf='FERC'   ;
131900090210       u1_demFERC=ripspesa ;
132000090209
132100090209       when cpispf='MAIL'   ;
132200090210       u1_demMAIL=ripspesa ;
132300090209
132400090209       when cpispf='MTN'   ;
132500090210       u1_demMTN=ripspesa ;
132600090209
132700090209       when cpispf='SDA'   ;
132800090210       u1_demSDA=ripspesa ;
132900090209
133000090209       when cpispf='SUSA'   ;
133100090210       u1_demSUSA=ripspesa ;
133200090209
133300090209       when cpispf='TNT'   ;
133400090210       u1_demTNT=ripspesa ;
133500090209
133600090209       when cpispf='UPS'   ;
133700090210       u1_demUPS=ripspesa ;
133800090209
133900090209       when cpispf='9ATR'   ;
134000090210       u1_demCATR=ripspesa ;
134100090209 5     ENDSL  ;
134200090209
134300090209 x4    else  ;
134400090209 5     select   ;
134500090209       when cpitpf='PRF'   ;
134600090210       u1_demprf=ripspesa ;
134700090209
134800090209       when cpitpf='PRO'   ;
134900090210       u1_dempro=ripspesa ;
135000090209
135100090209       when cpitpf='MES'   ;
135200090210       u1_demMES=ripspesa ;
135300090209
135400090209       when cpitpf='AER'   ;
135500090210       u1_demAER=ripspesa ;
135600090209
135700090209       when cpitpf='CAE'   ;
135800090210       u1_demCAE=ripspesa ;
135900090209
136000090209       when cpitpf='ATR'   ;
136100090210       u1_demATR=ripspesa ;
136200090209 5     ENDSL  ;
136300090209
136400090209 4     ENDif  ;
136500090209 3     ENDif  ;
136600090310 2     ENDif  ;
136700090310
136800090310         // se 100% memorizzo flag
136900090310 4     if cpitpf='10 '
137000090310          and cpispf='_BAR'
137100090310 6            and  cpival=100   ;
137200090310           u1_dem100b='S'   ;
137300090310 6       endif    ;
137400090310
137500090209
137600090310       // Memorizzo il motivo non affidamento
137700090310 2     if cpitpf='MOI'   ;
137800090310 3     select   ;
137900090310       when cpispf='0001'  ;
138000090310       u1_demit0001='S'   ;
138100090310
138200090310       when cpispf='0002'  ;
138300090310       u1_demit0002='S'   ;
138400090310
138500090310       when cpispf='0003'  ;
138600090310       u1_demit0003='S'   ;
138700090310
138800090310       when cpispf='0004'  ;
138900090310       u1_demit0004='S'   ;
139000090310
139100090310       when cpispf='0005'  ;
139200090310       u1_demit0005='S'   ;
139300090310
139400090310       when cpispf='1001'  ;
139500090310       u1_demes1001='S'   ;
139600090310
139700090310       when cpispf='1002'  ;
139800090310       u1_demes1002='S'   ;
139900090310
140000090310       when cpispf='1003'  ;
140100090310       u1_demes1003='S'   ;
140200090310
140300090310       when cpispf='1004'  ;
140400090310       u1_demes1004='S'   ;
140500090310
140600090310       when cpispf='1005'  ;
140700090310       u1_demes1005='S'   ;
140800090310
140900090310       when cpispf='9999'  ;
141000090310       u1_dem__9999='S'   ;
141100090310 3     endsl   ;
141200090310 2     endif   ;
141300090310
141400090209       reade u1_demcpoifo  ticpi01l   ;
141500090310 1     enddo    ;
141600090209
141700090209       UPDATE wfdemu01  ;
141800090209
141900090209       ENDSR  ;
142000090203
142100090203
142200071116      /end-free
