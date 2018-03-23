000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300100312      //?TNSDUOR - Gestione/interrogazione inVII GIORNALIERI UNITÀ EDP
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200100312     fTNSDUOD   cf   e             workstn indds(IndDspF)
001300080206     f                                     infds(InfDspF)
001400100315     f                                     sfile(SDUOS02 : S02nrr)
001500100827     FUNgio00F  uF a E             DISK    RENAME(UNgio000:UNgioFIS)
001600121024     FUNgio01l  iF   E           K DISK    infds(gio01)
001700100826     FUNgio04L  IF   E           K DISK    RENAME(UNgio000:UNgio4) infds(gio04)
001800100312     FUNANA01L  IF   E           K DISK
001900100930     FUNANA02L  uF   E           K DISK    RENAME(UNANA000:UNANA2)
002000100312     funana07l  if   e           k disk    rename(unana000:unana7)
002100100312     FAZORG21l  IF   E           K DISK
002200100312     fazorg01l  if   e           k disk
002300100312     fkpprf01l  if   e           k disk
002400100826     fcnaco00f  if   e           k disk
002500100922     fundot01l  uf a e           k disk
002600100920     FFNLV22P   O    F  132        PRINTER usropn
002700100312
002800080206      //---------------------------------------------------------------
002900100824      // - Numero di record in ciascuna videata di subfile
003000100827     d c_SflPag        c                   const(16)
003100100827     d W_SflPag        s              3  0 inz
003200100824     d PAgric          s              3  0 inz
003300100824     d totrig          s              5  0 inz
003400100824
003500080207      // - Tasti funzionali a video
003600080207     d c_F01           c                   const(x'31')
003700080207     d c_F02           c                   const(x'32')
003800080207     d c_F03           c                   const(x'33')
003900080207     d c_F05           c                   const(x'35')
004000080207     d c_F06           c                   const(x'36')
004100080207     d c_F07           c                   const(x'37')
004200080207     d c_F08           c                   const(x'38')
004300080207     d c_F09           c                   const(x'39')
004400080207     d c_F10           c                   const(x'3A')
004500101018     d c_F11           c                   const(x'3B')
004600080207     d c_F12           c                   const(x'3C')
004700080207     d c_F13           c                   const(x'B1')
004800080207     d c_F14           c                   const(x'B2')
004900080207     d c_F15           c                   const(x'B3')
005000080207     d c_F16           c                   const(x'B4')
005100080207     d c_F17           c                   const(x'B5')
005200080207     d c_F18           c                   const(x'B6')
005300080207     d c_F19           c                   const(x'B7')
005400080207     d c_F20           c                   const(x'B8')
005500080207     d c_F21           c                   const(x'B9')
005600080207     d c_F22           c                   const(x'BA')
005700080207     d c_F23           c                   const(x'BB')
005800080207     d c_F24           c                   const(x'BC')
005900080207     d c_Enter         c                   const(x'F1')
006000080207     d c_RollDown      c                   const(x'F4')
006100080207     d c_RollUp        c                   const(x'F5')
006200080214
006300080214      // - Attributi di visualizzazione
006400080215      //  -  DspAtr() - Normale
006500080214     d C_dspatr_Norm   c                   const(x'20')
006600080215      //  -  DspAtr(RI)
006700080214     d C_dspatr_RI     c                   const(x'21')
006800080215      //  -  DspAtr(ND)
006900080214     d C_dspatr_ND     c                   const(x'27')
007000080215      //  -  DspAtr(BL) / Color(Red)
007100080214     d C_dspatr_BL     c                   const(x'28')
007200080206
007300080206      //---------------------------------------------------------------
007400080206      //?Definizione schiere.
007500080206      //---------------------------------------------------------------
007600080206
007700080206      // - Messaggi di errore
007800140207     d MSG             s             78    dim(69) ctdata perrcd(1)
007900100920     d OVRP            s             91    dim(1) ctdata perrcd(1)
008000100920     d
008100100928     d TIPMAT          s             17    dim(1999)
008200101012     d SiCoPe          s             10  0 dim(1999)
008300100830     d WOPZ            s              1    dim(15)
008400080206      //---------------------------------------------------------------
008500080206      //?Definizione aree dati.
008600080206      //---------------------------------------------------------------
008700080206      // - Dati utente
008800080206     d §AzUte        e ds                  extname(AZUTE00F)
008900080206     d                                     dtaara
009000080206     d §DatiUte      e ds                  extname(dDatiUte)
009100080206     d                                     dtaara
009200080206
009300100903     d TNSDUO        e ds                  extname(TNSDUODS)
009400100903     d                                     dtaara
009500080206      //---------------------------------------------------------------
009600080206      //?Definizione strutture dati.
009700080206      //---------------------------------------------------------------
009800121024     D gio01           DS
009900100827     D  gio1NRR              397    400B 0
010000100826     D gio04           DS
010100100827     D  gio4NRR              397    400B 0
010200100826
010300080206
010400080206      // - Status
010500080206     d Psds           sds
010600080206     d   SDSpgm          *proc
010700080206
010800080206      // - InfDS
010900080206     d InfDspF         ds
011000080207     d  dsp_aid              369    369a                                        AID byte
011100100923     d  cursor               370    371a                                        AID byte
011200080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
011300080207     d  min_nrr              378    379i 0                                      Subfile min rrn
011400080207     d  num_rcds             380    381i 0                                      Subfile num rcds
011500080206
011600080206      // - Indicatori su DspF
011700080208     d IndDspF         ds
011800080206        // - Indicatori di gestione del subfile
011900080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
012000080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
012100080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
012200080206     d  SflEnd                        1n   overlay(IndDspF : 33)
012300080206        // - Indicatori di errore
012400080206     d  errMessage                    1n   overlay(IndDspF : 28)
012500080606     d  errGenerico                   1n   overlay(IndDspF : 99)
012600080627        // - Indicatori vari
012700100830     d  ModINT                        1n   overlay(IndDspF : 01)
012800100830     d  EmisMod1                      1n   overlay(IndDspF : 02)
012900100831     d  ImmMat                        1n   overlay(IndDspF : 03)
013000081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
013100100908     d  ModReso                       1n   overlay(IndDspF : 05)
013200100831     d  ImmCMOV                       1n   overlay(IndDspF : 06)
013300100831     d  ImmGES                        1n   overlay(IndDspF : 07)
013400100831     d  EmisCONF                      1n   overlay(IndDspF : 10)
013500100826     d  ProtperRIE                    1n   overlay(IndDspF : 11)
013600100826     d  ProtperINV                    1n   overlay(IndDspF : 12)
013700100830     d  protOPZ                       1n   overlay(IndDspF : 13)
013800100826     d  ProtINVR                      1n   overlay(IndDspF : 14)
013900080627     d  protriga                      1n   overlay(IndDspF : 16)
014000100830     d  recANN                        1n   overlay(IndDspF : 17)
014100100831     d  ProtMAT                       1n   overlay(IndDspF : 18)
014200101006     d  RevMAG                        1n   overlay(IndDspF : 19)
014300110404     d  ProtperFOR                    1n   overlay(IndDspF : 20)
014400120524     d  NostampaBO                    1n   overlay(IndDspF : 21)
014500100315     d
014600100315     d  ErrMAG                        1n   overlay(IndDspF : 40)
014700100315     d  ErrMGC                        1n   overlay(IndDspF : 41)
014800100315     d  ErrTIP                        1n   overlay(IndDspF : 42)
014900100315     d  ErrMAT                        1n   overlay(IndDspF : 43)
015000100315     d  ErrUTE                        1n   overlay(IndDspF : 44)
015100100315     d  ErrDIM                        1n   overlay(IndDspF : 45)
015200100826     d  ErrCUF                        1n   overlay(IndDspF : 46)
015300100826     d  ErrQTA                        1n   overlay(IndDspF : 47)
015400100826     d  ErrTRU                        1n   overlay(IndDspF : 48)
015500100826     d  ErrTIR                        1n   overlay(IndDspF : 49)
015600100826     d  ErrDES                        1n   overlay(IndDspF : 50)
015700100826     d  ErrKSC                        1n   overlay(IndDspF : 51)
015800100826     d  ErrDTM                        1n   overlay(IndDspF : 52)
015900100831     d  ErrIET                        1n   overlay(IndDspF : 53)
016000100903     d  ErrINVR                       1n   overlay(IndDspF : 54)
016100100908     d  ErrDTMA                       1n   overlay(IndDspF : 55)
016200100909     d  ErrDDO                        1n   overlay(IndDspF : 56)
016300100923     d  ErrMATR                       1n   overlay(IndDspF : 57)
016400101001     d  ErrINSE                       1n   overlay(IndDspF : 58)
016500101012     d  ErrNCL                        1n   overlay(IndDspF : 59)
016600110412     d  ErrNOT                        1n   overlay(IndDspF : 60)
016700080206
016800080206      // - Parametri ricevuti
016900080206     d KPJBA         e ds
017000100923     d og143         e ds
017100080206
017200080206      // - Reperimento dati utente
017300080206     d TIBS34ds      e ds
017400080206     d dUte01        e ds
017500080702     d DLAT          e ds
017600080702
017700100826     d DUNT          e ds
017800100901     d DSUNIT        e ds
017900100827     d TNSDV0DS      e ds
018000100920       // DS richeista stampanti
018100100920     d trul90ds      e ds
018200100920       // Ds stampa etichette
018300100920     d dsetiunit     e ds
018400100930       // Ds controllo magazzino
018500100930     d tnsdmagds     e ds
018600110401       // Ds del campo ungflo
018700110404     d dgio01        e ds
018800080206
018900080206      // - Ricerca/Controllo tabelle
019000080206     d TIBS02ds      e ds                  inz
019100100907
019200100907     d dsungio       e ds                  extname(ungio00f)
019300100924     d dsunana       e ds                  extname(unana00f) prefix(UIM:3)
019400100316
019500100316     d wlbdat          ds                  inz
019600100316     d  g02dat                 1      8  0
019700100316     d  g02inv                 9     16  0
019800100316     d  g02err                17     17
019900100316     d  g02tgi                18     22  0
020000121130     d
020100121130     d tnsduods_1      ds
020200121203     d   o05gma_dtm            1      6s 0
020300121203     d   o05amg_dtm            7     14s 0
020400121203     d   o05F06               15     15
020500121203     d   o05F12               16     16
020600121203     d   datapiu200           17     24s 0
020700121203     d   I05sys               25     32s 0
020800121130
020900100826
021000100827      //  ricerca alfabetica
021100100827     d  xpardut        s             30
021200100827     d  xparkut        s              1  0
021300100827     d  xparrag        s             48
021400100827     d  xparkcc        s              4  0
021500100827     d  xparsta        s              1  0
021600100827     d  xparflr        s             90
021700100827     d  xpardit        s              3
021800100827     d  xparnum        s              2  0
021900100827     d  xparkcm        s             80
022000100827     d  xparksm        s            140
022100100827     d  xparkdm        s             60
022200100827     d  xparesci       s              1
022300100827     d  xparerr        s              2
022400100827     d  xpariva        s             16
022500100827     d  xparcdf        s             16
022600100826
022700100827      // ricerca filiale su organigramma
022800100827     d   pInFIL        S              3
022900100827     d   pInFAG        S              1
023000100827     d   pInDES        S             25
023100100827     d   pInDIT        S              3
023200100316
023300080206      //---------------------------------------------------------------
023400080206      //?Definizione variabili globali.
023500080206      //---------------------------------------------------------------
023600080206
023700080206      // - Flags booleani
023800080208     d $Fine           s               n   inz(*off)
023900100312     d $InzS02         s               n   inz(*on)
024000100312     d $Inzd01         s               n   inz(*on)
024100080206
024200100312     d $Video          s              2    inz('D1')
024300100315     d S02nrr          s              4  0 inz
024400081006     d XX              s              3  0 inz
024500080627     d Indx            s              3  0 inz
024600080703     d Primavolta      s              1    inz
024700081126     d yy              s              3  0 inz
024800121203     d wdiffe          s              8  0 inz
024900121203     d w1cdim          s              8  0 inz
025000100831     d w1cdtm          s              8  0 inz
025100100908     d w1cdtmd         s              8  0 inz
025200100908     d w1cdtma         s              8  0 inz
025300121203     d w1ctgid         s                   like(g02tgi)
025400121203     d w1ctgia         s                   like(g02tgi)
025500100831     d Dataoggi        s              8  0 inz
025600100903     d savDatBlo       s              8  0 inz
025700121203     d w05dtm          s              8  0 inz
025800121203     d w05amg_dtm      s              8  0 inz
025900100908     d wazorg          s              3    Inz('   ')
026000100316     d wmag            s              3
026100100316     d wdmag           s             30
026200100909     d desmag          s             12
026300100708     d wtip            s              2
026400100824     d wOK             s              1
026500100824     d wtMAG           s              1
026600100830     d wSceltaOPZ      s              1
026700100708     d wdtip           s             30
026800100708     d qtatem          s              4  0
026900100823     d w0060           s              6  0
027000100903     d w006A           s              6
027100100823     d wpag            s              6  0
027200100823     d wresto          s              3  0
027300100827     d wTipMAT         s             17
027400101012     d wSIcope         s             10  0
027500100827     d vscslr          s              1  0
027600101012     d SiColliPeso     s              1
027700101001     d savnrr          s                   like(s02nrr)
027800101001     d ktip            s                   like(ungtip)
027900100825     d kinvr           s                   like(unginvr)
028000100823     d kmag            s                   like(ungmag)
028100100823     d kmgc            s                   like(ungmgc)
028200110412     d kmat            s                   like(ungmat)
028300100831     d SavMat          s                   like(ungmat)
028400101005     d SavMatr         s                   like(ungmat)
028500101001     d Primovuoto      s                   like(s02nrr)
028600101012     d sqDTM           s                   like(ungdtm)
028700101012     d sqNCL           s                   like(ungncl)
028800101012     d sqpkg           s                   like(ungpkg)
028900110124     d sqnrr           s              9  0
029000110421     d sqcont          s              7  0
029100100920     d comman          s            100
029200100920     d lung            s             15  5
029300100920     d wrichtrul90     s              1    inz(' ')
029400100921     d Recvuoto        s              1    inz(' ')
029500110421     d Wtrov           s              1    inz(' ')
029600101001     d Lib             s              1    inz('0')
029700121130     d savinkf         s              1    inz
029800101012     d wrkgetlista     s           4096    varying
029900101012     d invdtm          s              8  0 inz
030000101001     d
030100101001     d v4cmag          s                   like(vscmag)
030200101001     d v4cqta          s                   like(vscqta)
030300101001     d v4ctip          s                   like(vsctip)
030400101001     d v4ctir          s                   like(vsctir)
030500101001     d v4ctru          s                   like(vsctru)
030600101001     d v4cksc          s                   like(vscksc)
030700101001     d v4dksc          s                   like(vsdksc)
030800101001     d v4cddo          s                   like(vscddo)
030900101001     d v4cdoc          s                   like(vscdoc)
031000101001     d v4cdtm          s                   like(vscdtm)
031100080414
031200100315     d Dataeur         s               d   datfmt(*eur)
031300100315     d Dataiso         s               d   datfmt(*iso)
031400080605     d Dataymd         s               d   datfmt(*ymd)
031500080605     d Datadmy         s               d   datfmt(*dmy)
031600100901     d Datasys         s               d   inz(*sys)
031700100901     d timeiso         s               t   timfmt(*iso)
031800080604     d
031900080605     d                 DS
032000080605     d  AA                     1      4  0
032100080605     d  MM                     5      6  0
032200080605     d  GG                     7      8  0
032300080605     d DATA                    1      8  0
032400081009     d
032500120524     d Cf15BLO         C                   const('F15=Blocca la stampa della -
032600120524     d                                     bolla')
032700120524     d Cf15SBLO        C                   const('F15=SBLOCCA la stampa della -
032800120524     d                                     bolla')
032900080206      //---------------------------------------------------------------
033000080206      //?Definizione procedure usate.
033100080206      //---------------------------------------------------------------
033200080414      /copy gaitrasrc/srcprotopr,tibs02r
033300080414      /copy gaitrasrc/srcprotopr,tibs34r
033400100316      /copy gaitrasrc/srcprotopr,xsrda8
033500100826      /copy gaitrasrc/srcprotopr,xalfa3br
033600100909      /copy gaitrasrc/srcprotopr,tnsd20r
033700100901      /copy gaitrasrc/srcprotopr,tnsdU1r
033800100909      /copy gaitrasrc/srcprotopr,tnsdU4r
033900100909      /copy gaitrasrc/srcprotopr,tnsdU5r
034000100930      /copy gaitrasrc/srcprotopr,tnsdMAGr
034100100901      /copy gaitrasrc/srcprotopr,trtb47r
034200100920      /copy gaitrasrc/srcprotopr,qcmdexc
034300100920      /copy gaitrasrc/srcprotopr,fnlvuezs
034400100920      /copy gaitrasrc/srcprotopr,trul90r
034500121130      /copy gaitrasrc/srcprotopr,tnsduor1
034600080206
034700080206      //---------------------------------------------------------------
034800080206      //?Riepilogo indicatori.
034900080206      //---------------------------------------------------------------
035000080207      // - Comuni
035100080207      // 28    : Emissione messaggio di errore a video
035200080207      // - C01/S01
035300080207      // 30    : Condiziona SFLDSP    (*not)
035400080207      // 31    : Condiziona SFLDSPCTL (*not)
035500080207      // 30+31 : Condiziona SFLCLR
035600100824      // 32    : Condiziona SFLNXTCHG
035700080207      // 50    : Errore: Opzione errata
035800080207      // - D01
035900080207      // - Comuni
036000080207      // 99    : Generico di Errore
036100080206      //---------------------------------------------------------------
036200080206
036300080206      //---------------------------------------------------------------
036400080206      //?M A I N - L I N E
036500080206      //---------------------------------------------------------------
036600080206
036700080627     c     *Entry        plist
036800080206     c                   parm                    KPJBA
036900100827     c                   movel     kpjbu         tnsdv0ds
037000080702
037100080206      /free
037200080206
037300080206       // Operazioni iniziali
037400080206       exsr RoutInz;
037500081215
037600080206       // Gestione video
037700081215 3     DOW $Fine = *off;
037800081215 4       select;
037900080530
038000080530         // Videata di selezioni
038100100312           when $Video = 'D1';
038200100312             exsr GesD01;
038300080530
038400100830         // Videata del sfl per inserimento/visualizzazione
038500100312           when $Video = 'S2';
038600100312             exsr GesS02;
038700100830
038800080702           other   ;
038900080206             $Fine = *on;
039000081215 4       endsl;
039100081215 3     ENDDO;
039200080206
039300080206       // Operazioni finali
039400080206       exsr RoutEnd;
039500080206
039600080206       //--------------------------------------------------------------
039700080206       //?Operazioni iniziali.
039800080206       //--------------------------------------------------------------
039900080206       BEGSR RoutInz;
040000100312       $inzs02=*on;
040100100312       $inzd01=*on;
040200100831       ImmMAT=*off  ;
040300100908       ModReso=*off  ;
040400100831       ImmCMOV=*off  ;
040500100831       ImmGES =*off  ;
040600080206
040700080703       // Solo la prima volta
040800080703 1     if primavolta=' '   ;
040900080703
041000080206         // Reperimento dati job
041100080206         exsr DatiJob;
041200100824         vscpgm='TNSDUOR'         ;
041300081003
041400081003         // Se sono EDP accendo indicatore
041500081003         if %subst(knmus:1:3)<>'EDP'    ;
041600081003         UTENonEDP=*on   ;
041700100831         endif      ;
041800100826
041900100826       // Udate + 10
042000100826       dataiso=datasys ;
042100100831       dataeur=dataiso ;
042200100903       Dataoggi=%dec(dataeur) ;
042300100924       dataiso=dataiso+ %days(200);
042400100924       datapiu200=%dec(dataiso);
042500100831 1     endif      ;
042600100827
042700100830       clear vsdopz  ;
042800121130       clear vsdopz2 ;
042900100830       clear wopz   ;
043000100830       emisMod1=*on   ;
043100100831
043200100831 1     SELECT  ;
043300100830       // SE INTERROGAZIONE
043400100831 1     when §sdv0iNT='S'   ;
043500100830        modINT=*on   ;
043600100830        protriga=*on   ;
043700100830        v1dmod='    INTERROGAZIONE     '         ;
043800120525        vsdopz='5=visualizza   8=Movimenti' ;
043900100830        wopz(1)='5' ;
044000100909        wopz(2)='8' ;
044100100830
044200100831       // Se  IMMISSIONE/MANUTENZIONE
044300100831 1     WHEN §sdv0imm='S'   ;
044400100831        ImmGES =*on   ;
044500100827        protriga=*on   ;
044600100827        v1dmod='IMMISSIONE/MANUTENZIONE'     ;
044700120525        vsdopz='2=Manut 4=Annulla 5=Visual 9=Etich P=Prepara B=BlocSTA' ;
044800121130        vsdopz2='D=ModDataConf'  ;
044900100830        wopz(1)='2' ;
045000100830        wopz(2)='4' ;
045100100830        wopz(3)='5' ;
045200100920        wopz(4)='9' ;
045300101006        wopz(5)='P' ;
045400120525        wopz(6)='B' ;
045500121130        wopz(7)='D' ;
045600100831
045700100831       // Se  IMMISSIONE/MANUTENZIONE  MATRICOLE
045800100831 1     WHEN §sdv0MMAT='S'   ;
045900100831        protriga=*on   ;
046000100831        ImmMAT  =*on   ;
046100100831        v1dmod='Immissione Cd.MATRICOLE'    ;
046200120525        vsdopz='2=Manutenz. 4=Annulla  5=Visualizza  9=Etich'  ;
046300100831        wopz(1)='2' ;
046400100831        wopz(2)='4' ;
046500100831        wopz(3)='5' ;
046600101006        wopz(4)='9' ;
046700100903        // Scrivo data area per pgm in uso
046800100903        clear savdatblo ;
046900100903         in *lock   tnsduo ;
047000100903         if §sduodat=0 ;
047100100903         §sduodat=%dec(datasys) ;
047200100903         §sduoute=knmus         ;
047300100907         §sduopgm='IMMISSIONE Matricole' ;
047400100903         else ;
047500100903         savdatblo=§sduodat  ;
047600100903         endif;
047700100903         out tnsduo  ;
047800100831
047900100831       // Se  gestione RESI
048000100831 1     WHEN §sdv0RESi='S'   ;
048100100831        protriga=*on   ;
048200100908        ModReso =*on   ;
048300100831        v1dmod='GESTIONE UNITÀ DI RESO '    ;
048400120525        vsdopz='2=Manutenz.  5=Visualizza'  ;
048500100831        wopz(1)='2' ;
048600100831        wopz(2)='5' ;
048700100831
048800100831       // Se  conferma Movimenti
048900100831 1     WHEN §sdv0CONF='S'   ;
049000100831        protriga=*on   ;
049100100831        ImmCMOV =*on   ;
049200100831        v1dmod='  CONFERMA  MOVIMENTI  '    ;
049300120525        vsdopz='E=ESCLUDIdaCONFERMA  2=Manut.  5=Visual.'  ;
049400100831        wopz(1)='2' ;
049500100831        wopz(2)='5' ;
049600100831        wopz(2)='E' ;
049700100907        // Scrivo data area per pgm in uso
049800100907        clear savdatblo ;
049900100907         in *lock   tnsduo ;
050000100907         if §sduodat=0 ;
050100100907         §sduodat=%dec(datasys) ;
050200100907         §sduoute=knmus         ;
050300100907         §sduopgm='CONFERMA   Movimenti' ;
050400100907         else ;
050500100907         savdatblo=§sduodat  ;
050600100907         endif;
050700100907         out tnsduo  ;
050800100831 1     endsl   ;
050900080703
051000080627         ENDSR;
051100080206
051200080206       //--------------------------------------------------------------
051300080206       //?Reperimento Dati del job (Utente/Operativi).
051400080206       //--------------------------------------------------------------
051500080206       BEGSR DatiJob;
051600080206
051700080206         in(E) §AzUte;
051800080206         if NOT %error;
051900080206           in(E) §DatiUte;
052000080206         endif;
052100080206         if %error or RSut = *blanks;
052200080206           clear TIBS34ds;
052300080206           tibs34r(tibs34ds);
052400080206           in §AzUte;
052500080206           in §DatiUte;
052600080206         endif;
052700080206
052800080206       ENDSR;
052900100312       //--------------------------------------------------------------
053000100312       //?Gestione viodeata 01
053100100312       //--------------------------------------------------------------
053200100312       BEGSR Gesd01;
053300100312
053400100312         // Inizializzazione videata
053500100312         if  $Inzd01 = *on;
053600100312            exsr Inzd01;
053700100312            $Inzd01  = *off;
053800100312         endif;
053900100312
054000100312
054100100312         // Emissione Testata e Piede con tasti funzionali abilitati
054200100824         if  errMessage  = *off;
054300100316           write  SDUOT01;
054400100312         endif;
054500100312
054600100312         // Emissione videata
054700100316         exfmt  SDUOD01;
054800100312
054900100312         reset errMessage;
055000100312         reset errGenerico;
055100100824         clear V1cmsg    ;
055200100315         errMAG=*off     ;
055300100315         errMGC=*off     ;
055400100315         errTIP=*off     ;
055500100315         errMAT=*off     ;
055600100315         errUTE=*off     ;
055700100315         errDIM=*off     ;
055800100826         errCUF=*off     ;
055900100909         errDTM=*off     ;
056000100909         errDTMA=*off     ;
056100100909         errIET=*off     ;
056200100923         errINVR=*off     ;
056300100312
056400100312 1       SELECT;
056500100312
056600100312         // - F3=Fine
056700100312 1         WHEN  dsp_aid = c_F03;
056800100312            $Fine = *on;
056900100312
057000100312
057100100824 x1      // Invio
057200100312           OTHER;
057300100316
057400100316           exsr CTRD01  ;
057500100316
057600100316           if ErrGenerico=*ON    ;
057700100316                 leavesr;
057800100316 2             endif;
057900100316
058000100315           $video='S2' ;
058100100825           $inzs02=*on;
058200100824
058300100312 1       ENDSL;
058400100312
058500100312       ENDSR;
058600100312
058700100316       //--------------------------------------------------------------
058800100316       //?controlli video 1
058900100316       //--------------------------------------------------------------
059000100316       BEGSR CTRD01  ;
059100100316
059200100907        in tnsduo   ;
059300100907       // Pgm non utilizzabile al momento
059400100907 1     if §sdv0resi='S' or
059500100907         (§sdv0conf='S' and savdatblo>0) or
059600100907         (§sdv0mmat='S' and savdatblo>0) or
059700100907         (§sdv0imm ='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='C')  or
059800100907         (§sdv0conf='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='I')  or
059900100907         (§sdv0mmat='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='C') ;
060000100903 2      if §sduodat>0   ;
060100100907        exsr Msg37    ;
060200100903         leavesr  ;
060300100903 2     endif   ;
060400100903 1     endif   ;
060500100907
060600140207       // la conferma può essere fatta solo da un utente EDP
060700140207       if ImmCMOV=*on and %subst(knmus:1:3)<>'EDP  '  ;
060800140207           errUTE=*on ;
060900140207           v1cmsg = Msg(69)  ;
061000140207           errGenerico=*on  ;
061100140207           errMessage=*on  ;
061200140207           leavesr  ;
061300140207       endif  ;
061400140207
061500100316       //   Profilo utente
061600100708 1     if v1cute<>*blanks   ;
061700100316       chain   v1cute  kpprf01l   ;
061800100316
061900100708 2     if not %found(kpprf01l)  ;
062000100316           errUTE=*on ;
062100100316           v1cmsg = Msg(1);
062200100316           errGenerico=*on  ;
062300100316           errMessage=*on  ;
062400100316           leavesr  ;
062500100824 2       endif   ;
062600100708 1     endif   ;
062700100316
062800100316       // Data Immissione
062900100831       clear w1cdim   ;
063000100708 1     if v1cdim>0   ;
063100100316             clear wlbdat;
063200100824             g02dat = v1cdim ;
063300100316             xsrda8(wlbdat);
063400100824 2           if g02err = '1';
063500100316               errMessage  = *on;
063600100316               errGenerico = *on;
063700100708               ErrDIM    = *on;
063800100824               V1cmsg = Msg(02);
063900100316               leavesr;
064000100824 2           endif;
064100100316
064200100316             v1cdim = g02dat;
064300100316             w1cdim = g02inv;
064400100708 1     endif   ;
064500100316
064600100826       // Ufficio obbligatorio
064700100826       if v1ccuf=*blanks   ;
064800100826               errMessage  = *on;
064900100826               errGenerico = *on;
065000100826               ErrCUF    = *on;
065100100826               V1cmsg = Msg(10);
065200100826               leavesr;
065300100826       endif  ;
065400100826
065500100923       // tipo invio obbligatorio per FOR solo in immissione
065600100923       if v1ccuf='FOR' and v1cinvr= ' ' and §sdv0imm='S' ;
065700100923               errMessage  = *on;
065800100923               errGenerico = *on;
065900100923               ErrINVR   = *on;
066000100923               V1cmsg = Msg(46);
066100100923               leavesr;
066200100923       endif  ;
066300100923       if v1ccuf=*blanks   ;
066400100923               errMessage  = *on;
066500100923               errGenerico = *on;
066600100923               ErrCUF    = *on;
066700100923               V1cmsg = Msg(10);
066800100923               leavesr;
066900100923       endif  ;
067000100826       // Per MAG possibile solo I=invio
067100140123       if v1ccuf<>'FOR' and v1cinvr<>'I' and v1cinvr<>' ' ;
067200100826               errMessage  = *on;
067300100826               errGenerico = *on;
067400100826               ErrCUF    = *on;
067500100826               V1cmsg = Msg(11);
067600100826               leavesr;
067700100827       endif  ;
067800100826
067900100826       // Immettere o la data immissione o solo i DA confermare
068000100831       //  o solo unità di reso
068100121203       if v1cdim=0 and v1creso =' '
068200121203          and v1cdtm=0 and v1cdtmd = 0 and (v1cconf=' ' or v1cconf='C');
068300100826               errMessage  = *on;
068400100826               errGenerico = *on;
068500100826               ErrDIM    = *on;
068600100826               V1cmsg = Msg(12);
068700100826               leavesr;
068800100827       endif  ;
068900100831
069000100831       // Data conferma movimento: obbligatoria in conferma
069100100831       clear w1cdtm  ;
069200100831 1     if v1cdtm=0  and §sdv0conf='S' ;
069300100831               errMessage  = *on;
069400100831               errGenerico = *on;
069500100831               ErrDTM    = *on;
069600100831               V1cmsg = Msg(30);
069700100831               leavesr;
069800100831 2     endif;
069900100908
070000100831       if v1cdtm>0  ;
070100100831             clear wlbdat;
070200100831             g02dat = v1cdtm ;
070300100831             xsrda8(wlbdat);
070400100831 2           if g02err = '1';
070500100831               errMessage  = *on;
070600100831               errGenerico = *on;
070700100831               ErrDTM    = *on;
070800100831               V1cmsg = Msg(02);
070900100831               leavesr;
071000100831 2           endif;
071100100831
071200100831       // non inferiore a oggi
071300100831       if g02inv<%dec(datasys)  ;
071400100831               errMessage  = *on;
071500100831               errGenerico = *on;
071600100831               ErrDTM    = *on;
071700100831               V1cmsg = Msg(31);
071800100831               leavesr;
071900100831       endif   ;
072000100831             v1cdtm = g02dat;
072100100831             w1cdtm = g02inv;
072200100831 1     endif   ;
072300100908
072400100908       // data conferma movimenti dal / al
072500100908       clear w1cdtmd ;
072600100908       clear w1cdtma ;
072700121203       clear w1ctgid ;
072800121203       clear w1ctgia ;
072900101112       // Incongruente se richisti  movimenti DA confermare
073000110914       if (v1cdtmd>0 or v1cdtma>0) and v1cconf='S'  and ModInt=*on  ;
073100100908               errMessage  = *on;
073200100908               errGenerico = *on;
073300100908               ErrDTM    = *on;
073400100908               V1cmsg = Msg(41);
073500100908               leavesr;
073600100908       endif   ;
073700100908
073800100908 1     if v1cdtmd >0  ;
073900100908             clear wlbdat;
074000100908             g02dat = v1cdtmd;
074100100908             xsrda8(wlbdat);
074200100908 2           if g02err = '1';
074300100908               errMessage  = *on;
074400100908               errGenerico = *on;
074500100908               ErrDTM    = *on;
074600100908               V1cmsg = Msg(02);
074700100908               leavesr;
074800100908 2           endif;
074900100908             v1cdtmd= g02dat;
075000100908             w1cdtmd= g02inv;
075100121203             w1cTGId= g02TGI;
075200100908       endif   ;
075300100831
075400100908 1     if v1cdtma >0  ;
075500100908             clear wlbdat;
075600100908             g02dat = v1cdtma;
075700100908             xsrda8(wlbdat);
075800100908 2           if g02err = '1';
075900100908               errMessage  = *on;
076000100908               errGenerico = *on;
076100100908               ErrDTMA   = *on;
076200100908               V1cmsg = Msg(02);
076300100908               leavesr;
076400100908 2           endif;
076500100908             v1cdtma= g02dat;
076600100908             w1cdtma= g02inv;
076700121203             w1cTGIA= g02TGI;
076800100908
076900100908       // Data A non minore data DAL
077000100908 2     if w1cdtma<w1cdtmd  ;
077100100908               errMessage  = *on;
077200100908               errGenerico = *on;
077300100908               ErrDTMA   = *on;
077400100908               V1cmsg = Msg(40);
077500100908               leavesr;
077600100908 2     endif  ;
077700100908 1     endif   ;
077800121203
077900121203       // Se richIESTi i confermati max i mese posso richiedere
078000121203        if v1cconf<>'S' and v1cdtmd>0  and v1cdtma>0  ;
078100121203          WDIFFE=w1ctgia-w1ctgid  ;
078200121203          if wdiffe>32  ;
078300121203               errMessage  = *on;
078400121203               errGenerico = *on;
078500121203               ErrDTMA   = *on;
078600121203               V1cmsg = Msg(65)  ;
078700121203               leavesr;
078800121203          endif  ;
078900121203        endif  ;
079000100908
079100100316       // Magazzino destinazione
079200100708 1     if v1cmag<>*blanks and v1cmag<>'000' ;
079300100316
079400100909       wtmag='D'      ;
079500100908       wazorg='E  '   ;
079600100708       wmag=v1cmag    ;
079700100824       exsr  chkmag   ;
079800100824       v1cmag=wmag    ;
079900100316
080000100708       if  ErrGenerico=*ON  ;
080100100708         ErrMag=*on  ;
080200100316         leavesr  ;
080300100708       endif    ;
080400100708       else  ;
080500100708       clear v1cmag  ;
080600100708 1     endif    ;
080700100708
080800100708       // Magazzino correlato
080900100708 1     if v1cmgc<>*blanks  and v1cmgc<>'000'   ;
081000100708
081100100909       wtmag='C'      ;
081200100908       wazorg='E  '   ;
081300100708       wmag=v1cmgc    ;
081400100708       exsr  Chkmag   ;
081500100824       v1cmgc=wmag  ;
081600100708
081700100826       if  ErrGenerico=*ON  ;
081800100708         ErrMgc=*on  ;
081900100708         leavesr  ;
082000100708       endif    ;
082100100708       else  ;
082200100708       clear v1cmgc  ;
082300100708       endif    ;
082400100316
082500100708       // Tipo unita
082600100824       clear v1dtip  ;
082700100831 1     if v1ctip<>*blanks ;
082800100708         wtip=v1ctip   ;
082900100708         exsr chktip   ;
083000100824
083100100824         v1ctip=wtip  ;
083200100824         v1dtip=wdtip  ;
083300100827 2       if Errgenerico=*on  ;
083400100826          errTIP=*on       ;
083500100827 3       if ErrMessage =*on  ;
083600100826          v1cmsg=msg(05)  ;
083700100827 3       endif    ;
083800100826          leavesr  ;
083900100827 2       endif    ;
084000100831
084100100831       // Immettere anche Includi o Escludi
084200100831 2     if v1ciet=' '  ;
084300100831          errIET=*on       ;
084400100927          ErrGenerico=*on  ;
084500100927 3        ErrMessage =*on  ;
084600100831          v1cmsg=msg(28)  ;
084700100831          leavesr  ;
084800100831 2     endif   ;
084900100831
085000100827 x1    else  ;
085100100708       clear v1ctip  ;
085200100831       clear v1ciet  ;
085300100708 1     endif    ;
085400100927       // Se esclusione di un tipo, impossibile indciare la matricola
085500100927       if v1ciet='E' and v1ctip>*zeros  and v1cmat<>*blanks  ;
085600100927         Errgenerico=*on  ;
085700100927         Errmessage =*on  ;
085800100927         Errmat=*on  ;
085900100927         v1cmsg=msg(47)  ;
086000100927         leavesr  ;
086100100927 3     endif    ;
086200100708
086300100708       // Matricola
086400100708 1     if v1cmat<>*blanks  ;
086500100708
086600100824 2     if v1ctip>*zeros   ;
086700100824       ktip=%int(v1ctip)  ;
086800100930       setll (v1cmat:ktip)  unana07l    ;
086900100930 3     if not %equal(unana07l)  ;
087000100708         Errgenerico=*on  ;
087100100708         Errmessage =*on  ;
087200100708         Errmat=*on  ;
087300100708         v1cmsg=msg(06)  ;
087400100708         leavesr  ;
087500100708 3     endif    ;
087600100708
087700100708 x2    else   ;
087800100708       setll (v1cmat)  unana07l    ;
087900100708 3     if not %equal(unana07l)  ;
088000100708         Errgenerico=*on  ;
088100100708         Errmessage =*on  ;
088200100708         Errmat=*on  ;
088300100708         v1cmsg=msg(07)  ;
088400100708         leavesr  ;
088500100927 x3    //else   ;
088600100708
088700100708       // controllo se univoca
088800100927       //clear  qtatem   ;
088900100927       //reade v1cmat   unana07l   ;
089000100927 4     //dow  not %eof(unana07l) ;
089100100927 5     //if qtatem=0   ;
089200100927       //qtatem=qtatem+1  ;
089300100927       //v1ctip=%editc(unatip:'X') ;
089400100927 x5    //else   ;
089500100927       //  Errgenerico=*on  ;
089600100927       //  Errmessage =*on  ;
089700100927        // Errmat=*on  ;
089800100927        // v1cmsg=msg(08)  ;
089900100927       //leave  ;
090000100927 5     //endif   ;
090100100927       //reade v1cmat   unana07l   ;
090200100927 4     //enddo   ;
090300100708 3     endif    ;
090400100708 2     endif    ;
090500100708 1     endif    ;
090600100708
090700100316       ENDSR  ;
090800080206       //--------------------------------------------------------------
090900100312       //?Gestione SFL 02
091000080206       //--------------------------------------------------------------
091100100312       BEGSR GesS02;
091200080207
091300080207         // Inizializzazione videata
091400100312         if  $InzS02 = *on;
091500100312            exsr InzS02;
091600100824            $InzS02  = *off;
091700100824         endif;
091800100827
091900100831         // INTERRROGAZIONE: se non trovati record errore in prima videata
092000100831         if s02nrr=0   ;
092100100831         v1cmsg=msg(27)  ;
092200100831         errGenerico=*on  ;
092300100831         ErrMessage=*on  ;
092400100831         $Video='D1'  ;
092500100831         leavesr   ;
092600100831         endif  ;
092700100831
092800100831         // Posizionamento cursore
092900101005 1       if  C02csr  > *zero ;
093000101005
093100100924         // se non ci sono errori e il record +1 è vuoto, mi posiziono qui
093200101005 2       if ErrGenerico =*off ;
093300101005
093400101005 2a      if §sdv0imm='S'    ;
093500100924           s02nrr=c02csr    ;
093600100924           chain s02nrr sduos02   ;
093700101005 3         if %found ;
093800100927            exsr Aggiosfl  ;
093900101027            else  ;
094000101027            // Se non trovato riporto indietro di uno il campo per visulizzare il sfl
094100101027            C02csr=c02csr-1  ;
094200101005 3         endif  ;
094300101005 3         if %found and vsctip>=*zeros ;
094400100927
094500100924            s02nrr=c02csr+1  ;
094600100924            chain s02nrr sduos02   ;
094700101005 4          if %found and vshimm='S'  and vsctip<=*zeros ;
094800100924             c02csr=s02nrr ;
094900101005 4          endif ;
095000101005 4          if %found ;
095100100927             exsr Aggiosfl  ;
095200101005 4         endif  ;
095300101005 3         endif  ;
095400101005 2a      endif  ;
095500101005
095600101005         // Per immissione matricole mi posiziono sul record senza matricola
095700101005 2a      if §sdv0MMAT='S'    ;
095800101005           s02nrr=c02csr    ;
095900101005           chain s02nrr sduos02   ;
096000101005 3         if %found ;
096100101005            exsr Aggiosfl  ;
096200101005 3         endif  ;
096300101005 3         if %found and vscmat<>*blanks  ;
096400101005
096500101005            s02nrr=c02csr+1  ;
096600101005            chain s02nrr sduos02   ;
096700101005 4          if %found and vscmat=*blanks  ;
096800101005             c02csr=s02nrr ;
096900101005 4          endif ;
097000101005 4          if %found ;
097100101005             exsr Aggiosfl  ;
097200101005 4         endif  ;
097300101005 3         endif  ;
097400101005 2a      endif  ;
097500101005
097600101005 2       endif  ;
097700100924
097800100827           C02rcd = C02csr;
097900101005 1       endif  ;
098000080207
098100080207         // Emissione Testata e Piede con tasti funzionali abilitati
098200100824         if  errMessage  = *off;
098300100824           write  SDUOT01;
098400100824           write  SDUOZ02;
098500080207         endif;
098600080703
098700080207         // Emissione videata
098800100316         exfmt  SDUOC02;
098900080410
099000080207         reset errMessage;
099100080207         reset errGenerico;
099200100824         clear V1cmsg;
099300100830         errMAG=*off     ;
099400100830         errMGC=*off     ;
099500100830         errTIP=*off     ;
099600100830         errMAT=*off     ;
099700100830         errQTA=*off     ;
099800100830         errTRU=*off     ;
099900100830         errTIR=*off     ;
100000100830         errDES=*off     ;
100100110412         errNOT=*off     ;
100200100830         errKSC=*off     ;
100300100830         errDTM=*off     ;
100400100909         errDDO=*off     ;
100500101012         errNCL=*off     ;
100600100923         errMATR=*off     ;
100700100909         errINVR=*off     ;
100800100927         s02nrr=1  ;
100900080207
101000080523 1       SELECT;
101100080207
101200080207         // - F3=Fine
101300080523 1         WHEN  dsp_aid = c_F03;
101400080409            $Fine = *on;
101500080207
101600080207         // - F12=Ritorno
101700080523 1         WHEN  dsp_aid = c_F12;
101800100315           $video='D1'   ;
101900100824
102000100824         // - Roll-Up
102100100824           WHEN  dsp_aid = c_RollUp;
102200100831         if §sdv0imm= 'S'  ;
102300100824           // - Verifica se raggiunto il limite di records registrabili
102400100824           //   nel subfile (9999)
102500100824             if  S02nrr = *hival;
102600100824               errMessage  = *on;
102700100824               errGenerico = *on;
102800100824               V1cmsg = Msg(09);
102900100824             else;
103000100824               exsr RollUpS02;
103100100824             endif;
103200100830             endif;
103300100901
103400100901         // - F2=Gestione Unità EDP
103500100901 1         WHEN  dsp_aid = c_F02;
103600100924           clear dsunana ;
103700100924           chain c02csr sduos02 ;
103800100927           if %found ;
103900100927           exsr Aggiosfl  ;
104000100927           endif   ;
104100100928           if %found  and vscmat<>*blanks  and
104200100928 2          (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
104300100924              UIMmat=vscmat  ;
104400100927             endif  ;
104500100928           if %found  and vscmatr<>*blanks  and
104600140123 2          (rnmfl= 'VSCMATR ' and v1ccuf<>'FOR' )  ;
104700100928              UIMmat=vscmatr ;
104800100928             endif  ;
104900100901           exsr CallGESUNIT     ;
105000100824
105100100901         // - F7=Ricerca Matricola
105200100901 1         WHEN  dsp_aid = c_F07;
105300100901           chain c02csr sduos02 ;
105400100901           if %found   ;
105500100901           exsr CallRicMat      ;
105600100901           endif  ;
105700100920
105800100901
105900100901         // - F14=Gestione tabella scheda clienti x scambio dati
106000100901 1         WHEN  dsp_aid = c_F14;
106100100901           exsr Callpgm3R       ;
106200100909
106300101001         // - F15=Immissione multipla
106400101001 1         WHEN  dsp_aid = c_F15;
106500101001            $VIDEO='D4'   ;
106600101001            exsr Gesd04  ;
106700101001
106800110404         // - F21=Abilita campo opzione
106900100909 1         WHEN  dsp_aid = c_F21;
107000100909           chain c02csr sduos02 ;
107100100909           if %found   ;
107200100909           exsr Protcampi  ;
107300100909           exsr Aggiosfl    ;
107400100909           endif  ;
107500100909
107600100901
107700080530 x1      // Invio / F6
107800080207           OTHER;
107900081006
108000100315               exsr ContrS02 ;
108100100930              if %open(fnlv22p) ;
108200100930               close    fnlv22p  ;
108300100930              endif  ;
108400100930
108500100830 2             if  errGenerico = *on or wSceltaOPZ='S';
108600120524               if wsceltaOPZ='S'  ;
108700120524                 Errmessage=*off   ;
108800120524               endif  ;
108900081006                 leavesr;
109000100824 2             endif;
109100080530
109200121130         // se non ci sono errori--> pulisco la data modifica conferma mov
109300121130               clear w05dtm  ;
109400121203               clear w05amg_dtm  ;
109500121130
109600100907         // F6=conferma Aggiornamento
109700100920 1         if   dsp_aid = c_F06 ;
109800100907
109900100907         // Non posso confermare immissione se pgm CONFERMA in uso
110000100907           if   §sdv0imm='S' ;
110100100907            in tnsduo   ;
110200100907            if §sduodat>0 and %subst(§sduopgm:1:1)='C'   ;
110300100907            exsr Msg37  ;
110400100907            leavesr     ;
110500100907           endif  ;
110600100907           endif  ;
110700100907
110800080702           exsr   ConfAggio ;
110900081023
111000100315           $video='D1'   ;
111100080530           endif  ;
111200080207
111300080523 1       ENDSL;
111400080207
111500080207       ENDSR;
111600100901       //--------------------------------------------------------------
111700100901       //?chiamata pgm gestione unità EDP
111800100901       //--------------------------------------------------------------
111900100901       BEGSR CallGesUNIT ;
112000100901
112100100901       clear dsunit  ;
112200100901       dsupro='A'  ;
112300100901       if §sdv0int='S'   ;
112400100901       dsuop0=' 5'  ;
112500100901       else  ;
112600100901       dsuop0=' 2'  ;
112700100901       endif  ;
112800100901
112900100901       clear kpjbu  ;
113000100901       kpjbu=dsunit  ;
113100100924       callp TNSDU1R  (kpjba:dsunana)  ;
113200100901
113300100901       ENDSR;
113400080207
113500100901       //--------------------------------------------------------------
113600100901       //?chiamata pgm ricerca matricola unità EDP
113700100901       //--------------------------------------------------------------
113800100901       BEGSR CallricMat  ;
113900100901
114000100901       clear kpjbu  ;
114100100901       clear dsunit  ;
114200100927       clear dsunana;
114300100930
114400100930       // Nella gestione RESI si può fare F7 solo sulla mmatricola di reso
114500100927 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
114600100930 3        If ModReso=*on  ;
114700100930          Errgenerico=*on  ;
114800100930          ErrMessage=*on  ;
114900100930          v1cmsg=msg(48)  ;
115000100930          exsr Aggiosfl  ;
115100100930          leavesr;
115200100930  3       endif  ;
115300100930
115400100927       uimmat=vscmat  ;
115500100927       else   ;
115600100927       uimmat=vscmatr ;
115700100927       endif  ;
115800100901       dsupro='A'  ;
115900100901       dsuop0=' 1'  ;
116000100901
116100100901       kpjbu=dsunit  ;
116200100924       callp TNSDU1R  (kpjba:dsunana)  ;
116300100901       dsunit=kpjbu    ;
116400100901
116500100927 1     if uimtip>0 and uimcod>0  ;
116600100930
116700100901       // In manutenzione se selezionato tipo unità diverso --> errore
116800100923 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
116900100923
117000100930 3     if vsctip<=*zeros  ;
117100100901       vsctip='00' ;
117200100930 x3    else   ;
117300100901
117400100930 4     if  uimtip<>%int(vsctip)  ;
117500100901        ErrGenerico=*on  ;
117600100901        ErrMessage=*on  ;
117700100930        if vshimm=' '  ;
117800100901        v1cmsg=msg(35)  ;
117900100930        else  ;
118000100930        v1cmsg=msg(49)  ;
118100100930        endif  ;
118200100901        ErrMat=*on  ;
118300100901           write  SDUOT01;
118400100901           write  SDUOZ02;
118500100901           exsr Aggiosfl  ;
118600100901           leavesr   ;
118700100930 4     endif ;
118800100930 3     endif ;
118900100930 x2    else  ;
119000100930 3     if vsctir<=*zeros  ;
119100100930       vsctir='00' ;
119200100930 x3    else   ;
119300100930
119400100930 4     if  uimtip<>%int(vsctir)  ;
119500100930        ErrGenerico=*on  ;
119600100930        ErrMessage=*on  ;
119700100930        v1cmsg=msg(49)  ;
119800100930        ErrMatr=*on  ;
119900100930           write  SDUOT01;
120000100930           write  SDUOZ02;
120100100930           exsr Aggiosfl  ;
120200100930           leavesr   ;
120300100930 4     endif ;
120400100930 3     endif ;
120500100923 2     endif ;
120600100923
120700100927         chain (uimtip:uimcod) unana01l  ;
120800100923 2       if %found(unana01l)  ;
120900100927           wtip=%editc(uimtip:'X') ;
121000100901           exsr chktip  ;
121100100923
121200100923       // se sono posizionato su MATR imposto matricola di reso
121300100923       //  in ogni altro caso quella normale
121400100923 3     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
121500100923           vsctip=%editc(unatip:'X')   ;
121600100901           vsdtip=wdtip  ;
121700100901           vscmat=unamat  ;
121800100923 x3    else  ;
121900100923           vsctir=%editc(unatip:'X')   ;
122000100923           vsdtir=wdtip  ;
122100100923           vscmatr=unamat  ;
122200100923 3     endif ;
122300100923 2     endif   ;
122400100901 1     endif   ;
122500100923
122600100901       exsr Aggiosfl  ;
122700100901
122800100901       ENDSR;
122900100901
123000100901       //--------------------------------------------------------------
123100100901       //?chiamata pgm gesitone tabella scheda clienti per scambio dati
123200100901       //--------------------------------------------------------------
123300100901       BEGSR Callpgm3R   ;
123400100901       clear kpjbu  ;
123500100901       callp TRTB47R  (kpjba)  ;
123600100901       ENDSR;
123700100901
123800100920       //--------------------------------------------------------------
123900100920       //?Stampa etichette
124000100920       //--------------------------------------------------------------
124100100920       BEGSR staEti      ;
124200100920
124300100920       // La prima volta devo proporre la richeista stampati
124400100920       if wrichtrul90=' '  ;
124500100920       exsr RichStampan  ;
124600100920       if d90f3<>'1' and not *in91   ;
124700100920       wrichtrul90='S'  ;
124800100920       endif  ;
124900100920       endif  ;
125000100920
125100100920
125200100920       ENDSR  ;
125300100824       //--------------------------------------------------------------
125400100824       //?Caricamento pagina vuota
125500100824       //--------------------------------------------------------------
125600100824       BEGSR RollUpS02;
125700100824         S02nrr   = totrig ;
125800100824
125900100824         // Scrittura righe vuote
126000100824         exsr Righevuote  ;
126100100824
126200100824       ENDSR;
126300080526       //--------------------------------------------------------------
126400100312       //?controlli SFL02
126500080409       //--------------------------------------------------------------
126600100312       BEGSR ContrS02;
126700100315
126800100920         // - F10=Stampa etichette chiedo la stampante
126900100920 1       if      dsp_aid = c_F10;
127000100920           exsr StaEti          ;
127100100920         endif  ;
127200100920
127300100830       clear  WsceltaOPZ   ;
127400081126       yy=1           ;
127500100315       s02nrr=1  ;
127600100824       readc       sduos02    ;
127700080703
127800100824 0     dow not %eof ;
127900100830
128000100831       // RECORD GIÀ INSERITO: CONTROLLO L'OPZIONE
128100100831 1     IF vshimm=' '  and vscopz<>' '
128200100831          and  vscopz<>'E'   ;
128300100830
128400100830       // controllo opzione
128500100830       Indx=%lookup(vscopz:wopz)  ;
128600100831 2     if Indx=0  ;
128700100830         v1cmsg=msg(26)  ;
128800100830         ErrGenerico=*on  ;
128900100830         ErrMessage =*on  ;
129000100830         EXSR AggioSFL   ;
129100100830         leavesr  ;
129200100831 2     endif   ;
129300100920
129400101006       select   ;
129500101006
129600101006       // Per F08 imposto la 'P' di materiale preparato e metto il R.I. in magazzio destinaz.
129700101006 2     when      dsp_aid = c_F08 ;
129800101012       // Se no INVIO tasto funzionale non ammesso
129900120525       //if vscinvr<>'I'    ;
130000120525       //  v1cmsg=msg(52)  ;
130100120525       //  ErrGenerico=*on  ;
130200120525       //  ErrMessage =*on  ;
130300120525       //  EXSR AggioSFL   ;
130400120525       //  leavesr  ;
130500120525       //endif   ;
130600101006
130700101006 3     if vscopz='P'  ;
130800101006           chain(e) vshnrr   ungio00f   ;
130900101006 4          if %error or not %found ;
131000101006             v1cmsg=msg(25)  ;
131100101006             ErrGenerico=*on  ;
131200101006             ErrMessage =*on  ;
131300101006             EXSR AggioSFL   ;
131400101006             leavesr  ;
131500101006 x4          else  ;
131600101006             vshfpr='P'  ;
131700101006             ungfpr='P'  ;
131800101006             update ungioFIS  ;
131900101006 4          endif  ;
132000120525          CLEAR VSCOPZ  ;
132100120525 3      endif  ;
132200120525
132300120525 3     if vscopz='B'  ;
132400120525           chain(e) vshnrr   ungio00f   ;
132500120525 4          if %error or not %found ;
132600120525             v1cmsg=msg(25)  ;
132700120525             ErrGenerico=*on  ;
132800120525             ErrMessage =*on  ;
132900120525             EXSR AggioSFL   ;
133000120525             leavesr  ;
133100120525 x4          else  ;
133200120525             if ungfst='S' ;
133300120525             v1cmsg=msg(62)  ;
133400120525             ErrGenerico=*on  ;
133500120525             ErrMessage =*on  ;
133600120525             EXSR AggioSFL   ;
133700120525             leavesr  ;
133800120525 x4          else  ;
133900120525             vshfst='N'  ;
134000120525             ungfst='N'  ;
134100120525             update ungioFIS  ;
134200120525 4          endif  ;
134300120525 4          endif  ;
134400120525          CLEAR VSCOPZ  ;
134500120525 3      endif  ;
134600101006
134700101006        exsr Aggiosfl    ;
134800121130
134900101006
135000100920       // Per F10 elaboro solo opzioni di stampa  le opzioni diverse da 9 le lascio
135100101006 2     when      dsp_aid = c_F10 and d90f3<>'1';
135200100920 3     if vscopz='9'  ;
135300100930         if not %open(fnlv22p)  ;
135400100930         open fnlv22p ;
135500100930         endif        ;
135600100930
135700100920         clear dsetiunit   ;
135800100920         §eunitmag=%editc(vscmag:'X') ;
135900100920         // Decodifico magazzino
136000100920          clear wazorg  ;
136100100920          wtmag ='D  '   ;
136200100920          wmag=%editc(vscmag:'X')  ;
136300100920          exsr  chkmag   ;
136400100920         §eunitdmag=wdmag              ;
136500100920         §eunitksc =%editc(vscksc:'X') ;
136600100920          chain   (1:dutkci:vscksc) cnaco00f   ;
136700100920         §eunitdksc=acorag             ;
136800101028
136900101028         select  ;
137000101028         // Imposto il campo NOTE
137100101028         when vscnot<>*blanks  ;
137200101028          §eunitnot =vscnot             ;
137300101028         //   se vuoto e tipo 99 -->quantità + descrizione
137400101028         when vsctip='99'  ;
137500101028          §eunitnot =%editc(vscqta:'X') + ' ' + vscdes  ;
137600101028         //   se vuoto e tipo <> 99 -->solo descrizione
137700101028         when vscdes<>*blanks  ;
137800101028          §eunitnot =vscdes             ;
137900101028         other   ;
138000101028         //   se vuota anche descrizione tipo e decodifica
138100101028          §eunitnot=vsctip + '-' + vsdtip   ;
138200101028         endsl   ;
138300100920         callp FNLVUEZS  (dsetiunit) ;
138400100920
138500100920         CLEAR VSCOPZ  ;
138600100920  3    endif  ;
138700100920       exsr Aggiosfl    ;
138800100920
138900121130
139000121130         // opz "D" --> visualizzo Window per richiedere la data
139100121130         //             solo la prima volta
139200121130
139300121130 2       when  vscopz='D'   ;
139400121203
139500121203  3      if vshfmv='S'   ;
139600121203         v1cmsg=msg(66)  ;
139700121203         ErrGenerico=*on  ;
139800121203         ErrMessage =*on  ;
139900121203         EXSR AggioSFL   ;
140000121203         leavesr  ;
140100121203         endif  ;
140200121130
140300121130 4       if w05dtm= 0 ;
140400121130
140500121130         I05sys=%dec(datasys)  ;
140600121130         kpjbu=tnsduods_1 ;
140700121130
140800121130         TNSDUOR1  (kpjba)  ;
140900121130
141000121130         tnsduods_1 = kpjbu   ;
141100121130
141200121203          if   O05F06='1' and o05gma_dtm>0  ;
141300121203           w05dtm=o05gma_dtm  ;
141400121203           w05amg_dtm=o05amg_dtm  ;
141500121130          endif  ;
141600121203
141700121203          if   w05dtm=0   ;
141800121203             ErrGenerico=*on  ;
141900121203             CLEAR VSCOPZ  ;
142000121203             EXSR AggioSFL   ;
142100121203             leavesr  ;
142200121203             endif  ;
142300121203
142400121130
142500121130 4       endif   ;
142600121130
142700121130 4       if w05dtm>0  ;
142800121130           chain(e) vshnrr   ungio00f   ;
142900121130 5          if %error or not %found ;
143000121130             v1cmsg=msg(25)  ;
143100121130             ErrGenerico=*on  ;
143200121130             ErrMessage =*on  ;
143300121130             EXSR AggioSFL   ;
143400121130             leavesr  ;
143500121130 x5          else  ;
143600121130             vscdtm=w05dtm ;
143700121203             ungdtm=w05amg_dtm ;
143800121130             update ungioFIS  ;
143900121130
144000121130 5          endif  ;
144100121130 4          endif  ;
144200121130
144300121130          CLEAR VSCOPZ  ;
144400121130         EXSR AggioSFL   ;
144500121130       dsp_aid= savinkf  ;
144600121130  x2   other    ;
144700100831
144800100831       // non posso annullare record confermato
144900100920  3    if vshimm=' '  and vshfmv='S' and vscopz='4'  ;
145000100831         v1cmsg=msg(29)  ;
145100100831         ErrGenerico=*on  ;
145200100831         ErrMessage =*on  ;
145300100831         EXSR AggioSFL   ;
145400100831         leavesr  ;
145500100920  3    endif  ;
145600101006       // Per opzione '9' premere tasto funzionale F9
145700100920  3    if vshimm=' '  and   vscopz='9'  ;
145800100920         v1cmsg=msg(43)  ;
145900100920         ErrGenerico=*on  ;
146000100920         ErrMessage =*on  ;
146100100920         EXSR AggioSFL   ;
146200100920         leavesr  ;
146300121130        endif  ;
146400121130
146500101006       // Per opzione 'P' premere tasto funzionale F8
146600101006  3    if vshimm=' '  and   vscopz='P'  ;
146700101012       if vscinvr='I'   ;
146800101006         v1cmsg=msg(53)  ;
146900101006         else  ;
147000101006         v1cmsg=msg(52)  ;
147100101006         endif  ;
147200101006         ErrGenerico=*on  ;
147300101006         ErrMessage =*on  ;
147400101006         EXSR AggioSFL   ;
147500101006         leavesr  ;
147600101006  3    endif  ;
147700120525       // Per opzione 'B' premere tasto funzionale F8
147800120525  3    if vshimm=' '  and   vscopz='B'  ;
147900120525         v1cmsg=msg(53)  ;
148000120525         ErrGenerico=*on  ;
148100120525         ErrMessage =*on  ;
148200120525         EXSR AggioSFL   ;
148300120525         leavesr  ;
148400120525  3    endif  ;
148500100903
148600100921       // non posso Manutenzionare  o annullare se blocco data area
148700100921  3    if vshimm=' '  and  (vscopz='2' or vscopz='4') ;
148800100903        in tnsduo   ;
148900100909        if §sduodat>0 and (§sdv0resi='S' or §sdv0imm='S' )  ;
149000100907        exsr Msg37    ;
149100100909        clear vscopz  ;
149200100907
149300100903         EXSR AggioSFL   ;
149400100903         leavesr  ;
149500100920       endif   ;
149600100920 3     endif  ;
149700100830
149800100909       //  Alloco il record non per opz "5"  e "8"
149900100920 3       if vscopz='5' or vscopz='8'  ;
150000100830         chain(n) vshnrr   ungio00f   ;
150100100830         else  ;
150200100830         chain(e) vshnrr   ungio00f   ;
150300100920 3       endif  ;
150400100920 3       if %error or not %found ;
150500100830         v1cmsg=msg(25)  ;
150600100830         ErrGenerico=*on  ;
150700100830         ErrMessage =*on  ;
150800100830         EXSR AggioSFL   ;
150900100830         leavesr  ;
151000100920 3       endif  ;
151100100909
151200100909         // opz "8" --> interroga movimenti
151300100920 3       if vscopz='8'   ;
151400100909         exsr IntMovim  ;
151500100909         clear vscopz   ;
151600100909         EXSR AggioSFL   ;
151700100909         leavesr  ;
151800100920 3       endif  ;
151900121130
152000100830
152100100830        $VIDEO='D3'   ;
152200100830        exsr Gesd03  ;
152300100830
152400100830        wSceltaOPZ='S'  ;
152500100830        clear vscopz ;
152600100830         EXSR AggioSFL   ;
152700100830
152800100920 3     If ErrGenerico=*on  ;
152900100830         leavesr  ;
153000100920 3     endif  ;
153100101006 2     endsl ;
153200100830
153300100830 x1    else ;
153400100831
153500100924       // I record esclusi dalla conferma e annuillati non vengono controllati
153600100924  2    if vscopz<>'E'  and vshann<>'S' ;
153700100831
153800100830       // RECORD DI NUOVA IMMISSIONE: CONTROLLO
153900100830       exsr ContrRIGA  ;
154000100907       if  ErrGenerico=*on  ;
154100100907       leavesr ;
154200100907       endif  ;
154300100830
154400100830         // Per record inserito imposto utente e data ins
154500100831  3      if ErrGenerico=*off and vshimm='S' and vsctip<>*blanks ;
154600100830         vscute=KNMUS  ;
154700100830         datadmy=datasys ;
154800100830         dataeur=datasys ;
154900100830          vscdim=%dec(dataeur) ;
155000100830          w0060 =%dec(datadmy) ;
155100100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
155200100830                  %subst(%editc(w0060:'X'):3:2)    ;
155300100830
155400100831  3      endif    ;
155500100831  2      endif    ;
155600100831
155700100831   2    if ErrGenerico=*off ;
155800100831         EXSR AggioSFL   ;
155900100831   2    endif   ;
156000100831  1    endif   ;
156100100830
156200100830       readc    sduos02    ;
156300100830 0     enddo         ;
156400100830
156500100830       ENDSR  ;
156600081126
156700100907       //--------------------------------------------------------------
156800100907       //?scrivo msg 37 di blocco manutenzione
156900100907       //--------------------------------------------------------------
157000100907        BEGSR Msg37  ;
157100100907         v1cmsg=msg(37)  ;
157200100907         dataiso=%date(§sduodat) ;
157300100907         datadmy=dataiso      ;
157400100907         W006a=%editc(%dec(datadmy):'X') ;
157500100907         %subst(v1cmsg:74:2)=%subst(w006a:1:2) ;
157600100907         %subst(v1cmsg:77:2)=%subst(w006a:3:2) ;
157700100907         %subst(v1cmsg:60:10)=§sduoute          ;
157800100907         %subst(v1cmsg:27:20)=§sduopgm          ;
157900100907         ErrGenerico=*on  ;
158000100907         ErrMessage =*on  ;
158100100907        ENDSR  ;
158200100909       //--------------------------------------------------------------
158300100909       //?Interroga movimenti della unit/matricola
158400100909       //--------------------------------------------------------------
158500100909        BEGSR IntMovim;
158600100909
158700100924        // Se manca la matricola non sono interrogabili
158800100909        if vscmat=*blanks  ;
158900100909         v1cmsg=msg(34)  ;
159000100909         ErrGenerico=*on  ;
159100100909         ErrMessage =*on  ;
159200100909        else  ;
159300100909
159400100909        clear dsunit ;
159500100909        dsuop0=' 5'  ;
159600100909        dsutip=%int(vsctip)  ;
159700100930        chain (vscmat:dsutip)  unana07l    ;
159800100930        if %found(unana07l)  ;
159900110412         dsucod=unacod   ;
160000110412        else  ;
160100110412        // se non trovato provo con nuova matricola per FOR "R"
160200110412           if ProtperFOR=*on and vscnot<>*blanks   ;
160300110412           kmat=vscnot  ;
160400110412           chain (kmat:dsutip)  unana07l    ;
160500110412           if %found(unana07l)  ;
160600110412            dsucod=unacod   ;
160700110412           endif  ;
160800110412           endif  ;
160900110412
161000100909        endif  ;
161100100910        // solo se confermato, passo anche la data movimento
161200100910        if vshfmv='S'   ;
161300100910         datadmy=%date(vscdtm:*dmy)   ;
161400100910         dataiso=datadmy  ;
161500100910         dsudtm=%dec(dataiso) ;
161600100910        endif  ;
161700100909        kpjbu=dsunit   ;
161800100909        callp tnsdu4r   (kpjba)   ;
161900100909        ErrGenerico=*on  ;
162000100909        endif   ;
162100100909        ENDSR  ;
162200100830       //--------------------------------------------------------------
162300100830       //?controllo campi della riga di UNGIO00F
162400100830       //--------------------------------------------------------------
162500100830       BEGSR contrRIGA  ;
162600100830
162700100921       // controllo se inserito almeno un dato
162800101001 0     if vscmag<>0  or vscksc>0  or vscdes<>*blanks or vscnot<>*blanks
162900101001       or (vsctip<>*blanks  and vsctip<>*zeros )  or vsdksc<>*blanks or
163000101001       (vscmag3<>*blanks  and vscmag3<>*zeros and $Video<>'S2') or
163100101001       vscmat<>*blanks  ;
163200100921
163300100903       // Invii/Rientri: campo obbligatorio
163400100923 1     if vscinvr=' ' ;
163500100903           errMessage  = *on;
163600100903           errGenerico = *on;
163700100903           errINVR=*on ;
163800100903           V1cmsg = Msg(39);
163900100903           EXSR AggioSFL   ;
164000100903           leavesr  ;
164100100923 1     endif  ;
164200101001
164300101001       // Per inserimeno multiplo, inserire il numero delle ricorrenza
164400101001       if   $video='D4' and v4cinse<=0  ;
164500101001           errMessage  = *on;
164600101001           errGenerico = *on;
164700101001           errINSE=*on ;
164800101001           V1cmsg = Msg(50);
164900101001           EXSR AggioSFL   ;
165000101001           leavesr  ;
165100101001       endif        ;
165200100826
165300100824       // tipo unita
165400100927       // controllo tipo unità e matricola
165500100927       exsr CtrTipMat    ;
165600100927       if Errgenerico =*on  ;
165700100927       leavesr  ;
165800100927       endif    ;
165900120209
166000120209       // Magazzino di destinazione obbligatorio
166100120209       //  non obbligatorio per inserimento multiplo
166200120209 0     if $video<>'D4'  or vscmag3<>*blanks  ;
166300120209
166400120209       clear vsdmag  ;
166500120209       clear wazorg  ;
166600120209       wtmag ='D  '   ;
166700120209 1     if $Video='S2'   ;
166800120209       wmag=%editc(vscmag:'X')  ;
166900120209       else  ;
167000120209       wmag=vscmag3 ;
167100120209 1     endif ;
167200120209
167300120209       exsr  chkmag   ;
167400130118
167500120209 1     if $Video='S2'   ;
167600120209       vscmag=%int(wmag)    ;
167700130118
167800130118       if Errgenerico=*off ;
167900130118       if §omagfag='Y'  and v1ccuf='FOR'  and vscinvr='F'
168000130118         and vscmag<>unadms  and vscmag>*zeros  ;
168100130118           errMessage  = *on;
168200130118           errGenerico = *on;
168300130118           V1cmsg = Msg(67);
168400130118       endif  ;
168500130118       endif  ;
168600130118
168700120209       else   ;
168800120209       vscmag3=wmag  ;
168900120209 1     endif  ;
169000120209       vsdmag=wdmag  ;
169100120209 1     if Errgenerico=*on  ;
169200120209         errMAG=*on ;
169300120209         EXSR AggioSFL   ;
169400120209         leavesr  ;
169500120209 1     endif    ;
169600120209 0     endif    ;
169700100927
169800100927
169900100927         // Quantità: solo per 99 <> da 0 o 1
170000100927 1       if vscqta>1 and vsctip<> '99'  ;
170100100927           errMessage  = *on;
170200100927           errGenerico = *on;
170300100927           V1cmsg = Msg(15);
170400100927           errQTA=*on ;
170500100927           EXSR AggioSFL   ;
170600100927           leavesr  ;
170700100927 1       endif  ;
170800100831
170900100907       // Controlli sul Magazzino Correlato
171000100831 1     if  vscmgc>0   ;
171100100903       // Non può essere uguale al magazzino di destinazione
171200100923 2     if vscmgc=vscmag  ;
171300100924           errMessage  = *on;
171400100924           errGenerico = *on;
171500100903           errMGC=*on ;
171600100903           V1cmsg = Msg(38);
171700100903           EXSR AggioSFL   ;
171800100903           leavesr  ;
171900100923 2     endif  ;
172000100908
172100100908       clear wazorg ;
172200100908       wtmag='C' ;
172300100909       wmag=%editc(vscmgc:'X')  ;
172400100831       exsr  chkmag   ;
172500100909       vsdmgc=wdmag  ;
172600100831 2     if Errgenerico=*on  ;
172700100831         errMGC=*on ;
172800100831         EXSR AggioSFL   ;
172900100831         leavesr  ;
173000100831 2     endif    ;
173100100831
173200100831 1     endif    ;
173300100923
173400100923
173500100923       // Tipo unità di RESO solo se c'e' la sostituzione
173600110401       // ignoro per FOR F o R
173700110401         if protperRIE=*off  ;
173800110610
173900110610         // Aumento/Sostituz: non per 99
174000110610 1       if vsctru<>' ' and vsctip=  '99'  ;
174100110610           errMessage  = *on;
174200110610           errGenerico = *on;
174300110610           V1cmsg = Msg(16);
174400110610           errTRU=*on ;
174500110610           EXSR AggioSFL   ;
174600110610           leavesr  ;
174700110610 1       endif  ;
174800110610
174900100923 1       if (vsctru<>'S' and vsctir<>*blanks and vsctir<>'00') or
175000100923            (vsctru= 'S' and (vsctir= *blanks or vsctir=*zeros))  ;
175100100923           errMessage  = *on;
175200100923           errGenerico = *on;
175300100923           V1cmsg = Msg(17);
175400100923           errTir=*on ;
175500100923           EXSR AggioSFL   ;
175600100923           leavesr  ;
175700100923 1       endif   ;
175800110404         else  ;
175900110404         // Per FOR F o FOR  R contiene  guasto fisico. Accetto solo la "S"
176000110404         if vsctru<>'S' and vsctru<>' '   ;
176100110404           errMessage  = *on;
176200110404           errGenerico = *on;
176300110404           V1cmsg = Msg(56);
176400110404           errTru=*on ;
176500110404           EXSR AggioSFL   ;
176600110404           leavesr  ;
176700110404         endif  ;
176800110610         // Note obbligatorie
176900110610         if vscnot=*blanks and vsctru='S'   ;
177000110610           errMessage  = *on;
177100110610           errGenerico = *on;
177200110610           V1cmsg = Msg(59);
177300110610           errNot=*on ;
177400110610           EXSR AggioSFL   ;
177500110610           leavesr  ;
177600110610         endif ;
177700110610
177800110401 1       endif   ;
177900100923
178000100923         clear vsdtir  ;
178100101001 1       if vsctir<>*blanks  and vsctir<>*zeros  ;
178200100923         wtip=vsctir   ;
178300100923         exsr chktip   ;
178400100923
178500100923         vsctir=wtip  ;
178600100923         vsdtir=wdtip  ;
178700100923 2       if Errgenerico=*on  ;
178800100923           errTIR=*on ;
178900100923 3       if Errmessage =*on  ;
179000100923           V1cmsg = Msg(18);
179100100923 3       endif  ;
179200100923           EXSR AggioSFL   ;
179300100923           leavesr  ;
179400100923 2       endif    ;
179500100923 1       endif    ;
179600100923
179700100923         // Matricola unità di reso
179800100923 1       if vscmatr<>*blanks and vsctir=*blanks  ;
179900100923           errMessage  = *on;
180000100923           errGenerico = *on;
180100100923           V1cmsg = Msg(44);
180200100923           errMATr=*on ;
180300100930           *inki=*on ;
180400100923           EXSR AggioSFL   ;
180500100923           leavesr  ;
180600100923 1       endif  ;
180700100923
180800100923          clear vsdmodr  ;
180900101005          clear vscmgcr  ;
181000100923 1        if vscmatr<>*blanks ;
181100100923          ktip=%int(vsctir)  ;
181200100923          chain (vscmatr:ktip)  unana07l    ;
181300100923 2        if not %found(unana07l)  ;
181400100923           Errgenerico=*on  ;
181500100923           Errmessage =*on  ;
181600100923           ErrMATR=*on  ;
181700100923           v1cmsg=msg(06)  ;
181800100923           EXSR AggioSFL   ;
181900100923           leavesr  ;
182000100923 2        endif  ;
182100100923          vsdmodr =unamod;
182200101005          vscmgcr =unamag;
182300100923
182400100923          // Il magazzino correlato deve essere uguale a quello di destinazione
182500101005 2        if unamag<>vscmag  and unaksc=0 and vshfmat<>vscmatr  ;
182600100923           errMessage  = *on;
182700100923           errGenerico = *on;
182800100923           V1cmsg = Msg(45);
182900100923           errMATr=*on ;
183000101005           vshfmat=vscmatr ;
183100100923           EXSR AggioSFL   ;
183200100923           leavesr  ;
183300100923 2        endif  ;
183400100923 1        endif  ;
183500101005
183600101005          vshfmat=vscmatr ;
183700100826
183800100827         // Descrizione obbligatoria per Invii per d§UNTfl1='S'
183900101001 1       if $Video<>'D4'   ;
184000101001 2       if vscinvr='I' and d§untfl1='S' and vscdes=*blanks  ;
184100101112           errMessage  = *on;
184200101112           errGenerico = *on;
184300100826           errDES=*on ;
184400100826           V1cmsg = Msg(19);
184500100826           EXSR AggioSFL   ;
184600100826           leavesr  ;
184700101001 2       endif ;
184800101001 1       endif ;
184900110412
185000110412         // per FOR "F" note diventa --> nuova matricola inviata in sostiituizione
185100110412 1       if $Video<>'D4'  and vscnot<>*blanks and ProtperFOR=*on ;
185200110412         // non deve essere più lunga di 15 caratteri
185300110412
185400110412         if %subst(vscnot:16:5)<>*blanks  ;
185500110412           Errgenerico=*on  ;
185600110412           Errmessage =*on  ;
185700110412           Errnot=*on  ;
185800110412           v1cmsg=msg(58)  ;
185900110412           EXSR AggioSFL   ;
186000110412           leavesr  ;
186100110412         endif  ;
186200110412
186300110412         // non deve già esistere
186400110412         ktip=%int(vsctip)  ;
186500110412         eval kmat=vscnot    ;
186600110412          setll (kmat:ktip)  unana07l    ;
186700110412 3        if %equal(unana07l)  ;
186800110412           Errgenerico=*on  ;
186900110412           Errmessage =*on  ;
187000110412           Errnot=*on  ;
187100110412           v1cmsg=msg(57)  ;
187200110412           EXSR AggioSFL   ;
187300110412           leavesr  ;
187400110412 3        endif    ;
187500110412         endif  ;
187600100826
187700100826         // codice cliente
187800100830 1       if ($video='S2' and vsdksc <>*blanks and vscksc=0) or
187900101001            ($video<>'S2' and vsdksc3<>*blanks and vscksc=0)  ;
188000100826           clear xparsta  ;
188100100826           xparkcc=dutkci ;
188200100826           xparkut= 3     ;
188300100826           xpardut=rsut   ;
188400100830           if $video='S2' ;
188500100826           xparrag=vsdksc ;
188600100830           else  ;
188700100830           xparrag=vsdksc3;
188800100830           endif  ;
188900100826           clear xparflr ;
189000100826           clear xparkcm  ;
189100100826           clear xparksm  ;
189200100826           clear xparkdm  ;
189300100826           xparnum=1      ;
189400100826           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
189500100826                           :xparflr:xpardit:xparnum:xparkcm:xparksm
189600100826                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
189700100827 2          if xparsta<>-1  ;
189800100826            vscksc=%int(%subst(xparksm:1:7))   ;
189900100826            vsdksc=xparrag   ;
190000100830            vsdksc3=xparrag   ;
190100100827 2          endif            ;
190200100826             errGenerico = *on;
190300100826             errksc      = *on;
190400100826           EXSR AggioSFL   ;
190500100826           leavesr  ;
190600100827  1      endif ;
190700100826
190800100826         // codice cliente : obbligatorio per tipo 48/49 in invio
190900100920         //  tranne se non c'e' la parola "SCORTA" nelle note
191000100920         Indx=%scan('SCORTA':vscnot)  ;
191100100920         if Indx=0   ;
191200100920         Indx=%scan('scorta':vscnot)  ;
191300100920         endif  ;
191400100920         if Indx=0   ;
191500100920         Indx=%scan('Scorta':vscnot)  ;
191600100920         endif  ;
191700100920
191800100924  2      If Indx=0 ;
191900100924  3      if vscinvr='I' and vscksc=0 and (vsctip='48' or vsctip='49');
192000100924           errMessage  = *on;
192100100924           errGenerico = *on;
192200100826           errKSC=*on ;
192300100826           V1cmsg = Msg(20);
192400100826           EXSR AggioSFL   ;
192500100826           leavesr  ;
192600100924  3      endif ;
192700100920  1      endif ;
192800100826
192900100827  1      if vscksc>0  ;
193000100826           chain   (1:dutkci:vscksc) cnaco00f   ;
193100100827  2        if not %found(cnaco00f)   ;
193200100826             errMessage  = *on;
193300100826             errGenerico = *on;
193400100826             ErrKSC   = *on;
193500100826             V1cmsg = Msg(21);
193600100826            EXSR AggioSFL   ;
193700100826             leavesr;
193800100826             else  ;
193900100826             vsdksc=acorag  ;
194000100830             vsdksc3=acorag  ;
194100100827 2       endif  ;
194200100827 1       endif  ;
194300100825
194400100826         // Data conferma movimento: se > udate non troppo...  ;
194500100826 1       If vscdtm>0  ;
194600100826             clear wlbdat;
194700100826             g02dat = vscdtm ;
194800100826             xsrda8(wlbdat);
194900100826 2           if g02err = '1';
195000100826               errMessage  = *on;
195100100826               errGenerico = *on;
195200100826               ErrDTM    = *on;
195300100826               V1cmsg = Msg(02);
195400100826              EXSR AggioSFL   ;
195500100826               leavesr;
195600100826 2           endif;
195700100826
195800100924 2           if g02inv>datapiu200;
195900100826               errMessage  = *on;
196000100826               errGenerico = *on;
196100100826               ErrDTM    = *on;
196200100826               V1cmsg = Msg(22);
196300100826              EXSR AggioSFL   ;
196400100826               leavesr;
196500100826 2           endif   ;
196600100909 2           if vshfmv=' ' and g02inv<%dec(datasys)  ;
196700100831               errMessage  = *on;
196800100831               errGenerico = *on;
196900100831               ErrDTM    = *on;
197000100831               V1cmsg = Msg(31);
197100100831              EXSR AggioSFL   ;
197200100831               leavesr;
197300100831 2           endif   ;
197400100826 1       endif   ;
197500101012
197600101012       // Se non c'e' un altra riga per lo stesso magazzino, chiedo obbligatori peso e vol
197700101012 1     if vscinvr='I'  and $Video<>'D4'  and vscmag>0  and vshfmv=' '  ;
197800101012        exsr selectSQL   ;
197900101012
198000101012 2        if   vscncl=0 or vscpkg=0   ;
198100101012 3      if SiColliPeso=' ' ;
198200101012               errMessage  = *on;
198300101012               errGenerico = *on;
198400101012               ErrNCL    = *on;
198500101012               V1cmsg = Msg(54);
198600101012              EXSR AggioSFL   ;
198700101012               leavesr;
198800101012 3      endif  ;
198900101012
199000101012 2      endif  ;
199100101012        endif  ;
199200080703
199300101001         // Dcumento obbligatorio  se rientro da fornitore
199400101001 1       if $Video<>'D4'  ;
199500120525 2       if (vscdoc=0 and vscddo=0) and vscinvr='R' ;
199600100909               errMessage  = *on;
199700100909               errGenerico = *on;
199800100909               ErrDDO    = *on;
199900100909               V1cmsg = Msg(33);
200000100909              EXSR AggioSFL   ;
200100100909               leavesr;
200200101001 2        endif   ;
200300101001 1        endif   ;
200400100909
200500100909       // controllo la data
200600100909       if vscddo>0   ;
200700100909             clear wlbdat;
200800100909             g02dat = vscddo ;
200900100909             xsrda8(wlbdat);
201000100909 2           if g02err = '1';
201100100909               errMessage  = *on;
201200100909               errGenerico = *on;
201300100909               ErrDDO    = *on;
201400100909               V1cmsg = Msg(02);
201500100909               leavesr;
201600100909              endif  ;
201700100909       endif  ;
201800100921 x0    else  ;
201900101001       // Per inserimento multipli immettere almeno un dato da duplicare
202000101001       if $Video='D4'  ;
202100101001               errMessage  = *on;
202200101001               errGenerico = *on;
202300101001               ErrMAG    = *on;
202400101001               V1cmsg = Msg(51);
202500101001               leavesr;
202600101001       endif  ;
202700100921       clear vsdmag   ;
202800100921       clear vsdtip  ;
202900100921       clear vsdmod1 ;
203000100921       clear vsdmod2 ;
203100100921       clear vscmgc  ;
203200100921       clear vsdmgc  ;
203300100921       Recvuoto='S'   ;
203400100930       // pulisco eventuale posizione della matricola
203500100930           if s02nrr<2000 ;
203600100930            clear tipmat(s02nrr)          ;
203700101012            clear SiCoPe(s02nrr)          ;
203800100930           endif  ;
2039001009210      endif  ;
204000100909
204100080409       ENDSR;
204200101012
204300101012       //--------------------------------------------------------------
204400101012       //?Select con SQL su ungio00F
204500101012       //--------------------------------------------------------------
204600101012       BEGSR  SelectSQL ;
204700101012
204800101012       clear SiColliPeso  ;
204900101012 1     if vscdtm=0  ;
205000101012         invdtm=%dec(datasys)  ;
205100101012       else  ;
205200101012         dataeur=%date(vscdtm:*dmy)    ;
205300101012         dataiso=dataeur  ;
205400101012         invdtm=%dec(dataiso) ;
205500101012 1     endif  ;
205600101012
205700101012       // Prima verifico se già inserito in questa pagina di lavoro
205800101012
205900101012       // Cerco con: MAGAZZINO  e data conferma
206000101012          // se la data <= oggi cerco con data =0
206100101012
206200101012 1     if invdtm<=%dec(datasys)  ;
206300101012       wSiCoPe=(vscmag+100000000)            ;
206400101012       else  ;
206500101012       wSiCoPe=(vscmag+100000000) +  invdtm  ;
206600101012 1     endif  ;
206700101012
206800101012 1     if   vscncl=0 or vscpkg=0   ;
206900101012
207000101012       clear sicope(s02nrr)   ;
207100101012       Indx=%lookup(wsicope:Sicope)  ;
207200101012 2      if Indx>0  ;
207300101012         SiColliPeso='S'  ;
207400101012 2      endif  ;
207500101012
207600101012 x1     else  ;
207700101012
207800101012         if s02nrr<2000 ;
207900101012           sicope(s02nrr)=wsicope    ;
208000101012         endif  ;
208100101012         SiColliPeso='S'  ;
208200101012 1      endif  ;
208300101012
208400101012
208500101012       // Poi se non trovato controllo se già inserito in UNGIO
208600101012         if SiColliPeso =' '  ;
208700101012
208800101012         wrkgetlista='SELECT ungdtm, ungncl, ungpkg, rrn(UNGIO00f) +
208900101012                               from UNGIO00F WHERE ungcuf=''' +v1ccuf  + ''' +
209000101012         and unginvr=''I'' and ungfmv='' '' and ungmag= ' + %editc(vscmag:'X') ;
209100101012
209200101012         exec sql   PREPARE s1 FROM :wrkgetlista   ;
209300101012
209400101012         exec sql   DECLARE a1 cursor FOR  s1   ;
209500101012
209600101012         exec sql OPEN a1   ;
209700101012
209800101012 0       dow sqlcod=0   ;
209900101012
210000101012         exec sql FETCH NEXT  FROM a1 INTO: sqdtm, :sqncl, :sqpkg, :sqnrr ;
210100101012
210200101012 1       select ;
210300101012 1       when sqlcod=100 ;
210400101012
210500101012 1       when sqlcod<0   ;
210600101012         *inh1=*on   ;
210700101012
210800101012 x1      other   ;
210900101012         // escludo il record se la data conferma è <> da quello del record ins/variato
211000101012 2a       if  vshnrr<>sqnrr ;
211100101012 2        if  vscdtm=0 or invdtm<=%dec(datasys)  ;
211200101012 3         if sqdtm<=invdtm  ;
211300101012 4           if sqncl>0 and sqpkg>0  ;
211400101012               SiColliPeso='S'  ;
211500101012 4           endif  ;
211600101012 3         endif  ;
211700101012
211800101012 x2       else  ;
211900101012 3         if  sqdtm=invdtm  ;
212000101012 4           if sqncl>0 and sqpkg>0  ;
212100101012               SiColliPeso='S'  ;
212200101012 4           endif  ;
212300101012 3         endif  ;
212400101012 2a       endif  ;
212500101012 2        endif  ;
212600101012
212700101012 1       endsl  ;
212800101012
212900101012 0       enddo   ;
213000101012
213100101012         exec sql CLOSE a1   ;
213200101012         endif  ;
213300101012
213400101012       ENDSR;
213500080409
213600100927       //--------------------------------------------------------------
213700100927       //?Controllo solo tipo unità e matricola
213800100927       //--------------------------------------------------------------
213900100927       BEGSR  CtrTipMat  ;
214000100927
214100100927       // Obbligatorio se non immessa la matricola
214200101001       //  non obbligatorio per inser. multiplo
214300101001 1     if (vsctip=*blanks or vsctip=*zeros) and vscmat=*blanks and $Video<>'D4';
214400100927           errMessage  = *on;
214500100927           errGenerico = *on;
214600100927           errTIP=*on ;
214700100927           v1cmsg=msg(05)  ;
214800100927           EXSR AggioSFL   ;
214900100927           leavesr  ;
215000100927  1      endif   ;
215100100927
215200100927  1     if vsctip<>*blanks and vsctip<>*zeros  ;
215300100927         clear vsdtip  ;
215400100927         clear vsdtip3 ;
215500100927         wtip=vsctip   ;
215600100927         exsr chktip   ;
215700100927
215800100927         vsctip=wtip  ;
215900100927         vsdtip =wdtip  ;
216000100927         vsdtip3=wdtip  ;
216100100927 2       if Errgenerico=*on  ;
216200100927           errTIP=*on ;
216300100927 3       if ErrMessage =*on  ;
216400100927          v1cmsg=msg(05)  ;
216500100927 3        endif  ;
216600100927           EXSR AggioSFL   ;
216700100927           leavesr  ;
216800100927 x2      else  ;
216900100927         dunt=t02uni ;
217000100927         vsdtip =d§untdes ;
217100100927         vsdtip3=d§untdes ;
217200100927  2      endif    ;
217300100927
217400140207         // "99" accettato solo per MAG/TLC o FOR-F
217500140207  2      if vsctip='99' and v1ccuf='FOR'
217600140207           and (v1cinvr='R' or v1cinvr='S') ;
217700100927           errMessage  = *on;
217800100927           errGenerico = *on;
217900100927           errTIP=*on ;
218000100927           V1cmsg = Msg(14);
218100100927           EXSR AggioSFL   ;
218200100927           leavesr  ;
218300100927  2      endif   ;
218400100927  1      endif   ;
218500100927
218600100927         // Matricola non ci vuole per 99
218700100927 1       if v1ctip='99' and v1cmat<>*blanks  ;
218800100927           errMessage  = *on;
218900100927           errGenerico = *on;
219000100927           V1cmsg = Msg(13);
219100100927           errMAT=*on ;
219200100927           EXSR AggioSFL   ;
219300100927           leavesr  ;
219400100927 1       endif    ;
219500100927
219600100927
219700100927         // Matricola: obbligatoria in conferma
219800100927         if §sdv0conf='S' and vsctip<>'99' and vscmat=*blanks  ;
219900100927           errMessage  = *on;
220000100927           errGenerico = *on;
220100100927           V1cmsg = Msg(32);
220200100927           errMAT=*on ;
220300100927           EXSR AggioSFL   ;
220400100927           leavesr  ;
220500100927         endif   ;
220600100927
220700100927         // Matricola: se immessa controllo
220800100927         clear vsdmod1 ;
220900100927         clear vsdmod2 ;
221000100927         clear vsdmod  ;
221100100927          if vshfmv=' ' ;
221200100927          clear vscmgc  ;
221300100927          clear vsdmgc  ;
221400100927         endif  ;
221500100927 1       if vscmat<>*blanks   ;
221600100927 2       if vsctip>*zeros   ;
221700100927          ktip=%int(vsctip)  ;
221800100927          chain (vscmat:ktip)  unana07l    ;
221900100927 x2       else  ;
222000100927          chain (vscmat)       unana07l    ;
222100100927 2        endif  ;
222200100927
222300100927 2        if not %found(unana07l)  ;
222400100927          Errgenerico=*on  ;
222500100927          Errmessage =*on  ;
222600100927          ErrMAT=*on  ;
222700100927          if vsctip>*zeros  ;
222800100927          v1cmsg=msg(06)  ;
222900100927          else  ;
223000100927          v1cmsg=msg(07)  ;
223100100927          endif  ;
223200100927
223300100927           EXSR AggioSFL   ;
223400100927           leavesr  ;
223500100927 2        endif  ;
223600100927
223700100927          // se trovata verifico se univoca altrimenti ci vuole il tipo
223800100927 2        if     vsctip<=*zeros and %found(unana07l)  ;
223900100927           clear  qtatem   ;
224000100927           setll (vscmat)  unana07l    ;
224100100927           reade (vscmat)  unana07l    ;
224200100927 3         dow  not %eof(unana07l) ;
224300100927 4          if qtatem=0   ;
224400100927             qtatem=qtatem+1  ;
224500100927 x4         else   ;
224600100927             Errgenerico=*on  ;
224700100927             Errmessage =*on  ;
224800100927             Errmat=*on  ;
224900100927             v1cmsg=msg(08)  ;
225000100927             EXSR AggioSFL   ;
225100100927             leavesr;
225200100927 4          endif   ;
225300100927           reade (vscmat)  unana07l    ;
225400100927 3         enddo   ;
225500100927 2        endif   ;
225600100927
225700100927 2        if vsctip<=*zeros   ;
225800100927             vsctip=%editc(unatip:'X') ;
225900100928            ktip=unatip   ;
226000100928            wtip=vsctip   ;
226100100927            exsr chktip   ;
226200100927            dunt=t02uni ;
226300100927            vsctip=wtip  ;
226400100927            vsdtip =wdtip  ;
226500100927            vsdtip3=wdtip  ;
226600100927 2        endif ;
226700100927          vsdmod =unamod;
226800100927          vsdmod1=unamod;
226900100927          vsdmod2=%subst(unamod:7:9) ;
227000110404          // Se unità in proprietà alla 923 --> non posso accettare MAG 990
227100110404          if unasis='923' and ((vscmag = 990  and $Video='S2') or
227200111024                               (vscmag3='990' and $Video<>'S2')  or
227300111024                               (vscmag3='980' and $Video<>'S2')  or
227400111024                               (vscmag = 980  and $Video='S2')) ;
227500110401             Errgenerico=*on  ;
227600110401             Errmessage =*on  ;
227700110401             Errmag=*on  ;
227800110401             v1cmsg=msg(55)  ;
227900110401             EXSR AggioSFL   ;
228000110401             leavesr;
228100110401          endif  ;
228200130626          // Se unità in proprietà non è 923 --> non posso accettare MAG 923
228300130626          if unasis<>'923' and ((vscmag = 923  and $Video='S2') or
228400130626                               (vscmag3='923' and $Video<>'S2')) ;
228500130626             Errgenerico=*on  ;
228600130626             Errmessage =*on  ;
228700130626             Errmag=*on  ;
228800130626             v1cmsg=msg(68)  ;
228900130626             EXSR AggioSFL   ;
229000130626             leavesr;
229100130626          endif  ;
229200120209
229300120209          // Per FOR F propongo magazzino destinazione se si tratta di un fornitore
229400130116          if v1ccuf='FOR'  and vscinvr='F' ;
229500130116          chain unadms azorg21l ;
229600130116          if  vscmag<=*zeros  and
229700130116                 %found(azorg21l) and  orgfag = 'Y' ;
229800120209              vscmag=unadms  ;
229900120209              Errgenerico=*on  ;
230000120209              EXSR AggioSFL   ;
230100120209              leavesr  ;
230200120209          endif  ;
230300130116
230400130116             endif  ;
230500120209
230600100927          // Solo se record non confermato
230700100927          if vscmgc=0 or vshfmv=' ' ;
230800100927           vscmgc =unamag;
230900100927          endif  ;
231000100927
231100100928          //  controllo l'univocità tra i record non  confermati
231200130321            setll (Ktip:vscmat)  ungio01l    ;
231300130321            reade (Ktip:vscmat)  ungio01l    ;
231400130321            dow not %eof(ungio01l) ;
231500130321             if ungfmv=' '  ;
231600130321              if vshimm='S' or (vshnrr<>gio1nrr)   ;
231700130321               Errgenerico=*on  ;
231800130321               Errmessage =*on  ;
231900130321               ErrMAT=*on  ;
232000130321               v1cmsg=msg(23)  ;
232100130321               EXSR AggioSFL   ;
232200130321               leavesr  ;
232300130321              endif   ;
232400130321             endif   ;
232500130321             reade (Ktip:vscmat)  ungio01l    ;
232600130321           enddo   ;
232700100927
232800100927          // controllo che non sia già stata inserita in questa pagina di lavoro
232900100927          wtipmat=vsctip ;
233000100927          %subst(wtipmat:3:15)=vscmat   ;
233100100927          Indx=%lookup(wtipmat:tipmat)  ;
233200100928 2        if Indx>0 and Indx<>s02nrr ;
233300100927          Errgenerico=*on  ;
233400100927          Errmessage =*on  ;
233500100927          ErrMAT=*on  ;
233600100927          v1cmsg=msg(24)  ;
233700100927           EXSR AggioSFL   ;
233800100927           leavesr  ;
233900100928           else  ;
234000100928           // la memorizzo
234100100928           // solo se maggiore di 1999 non memorizzo
234200100928           if s02nrr<2000 ;
234300100928            tipmat(s02nrr)=wtipmat  ;
234400100928  2        endif    ;
234500100928 2        endif    ;
234600100927
234700101022 2        if unaksc>0  and vscksc=0 and vscinvr='F';
234800100927          vscksc =unaksc;
234900100927           chain   (1:dutkci:unaksc) cnaco00f   ;
235000100927 3         if %found(cnaco00f)   ;
235100100927             vsdksc=acorag  ;
235200100927             vsdksc3=acorag  ;
235300100927           else   ;
235400100927             vsdksc=*all'?'  ;
235500100927             vsdksc3=*all'?'  ;
235600100927 3         endif  ;
235700100927 2        endif  ;
235800100927 2        if vscdes=*blanks ;
235900100927          vscdes=unades ;
236000100927 2        endif ;
236100100927 1       endif    ;
236200100927         ENDSR    ;
236300080703       //--------------------------------------------------------------
236400080703       //?Aggiornamento sfl
236500080703       //--------------------------------------------------------------
236600080703       BEGSR AGGIOSFL  ;
236700100830
236800100830       IF $VIDEO='S2'       ;
236900100830
237000100830       exsr ProtCampi   ;
237100100830
237200100824       if v1cmsg<>*blanks   ;
237300080703       errMessage  = *on;
237400080703       errGenerico = *on;
237500100315       c02csr=s02nrr    ;
237600080703       endif    ;
237700080703
237800100921       if Recvuoto<>'S';
237900100830        Sflnxtchg=*on  ;
238000100921        endif  ;
238100100921        clear recvuoto  ;
238200100824
238300100315       update  sduos02  ;
238400100830       ENDIF   ;
238500100830
238600080703       ENDSR   ;
238700100312       //--------------------------------------------------------------
238800100312       //?Inizializzazione videata 01
238900100312       //--------------------------------------------------------------
239000100312       BEGSR Inzd01  ;
239100100312
239200100315         clear V1cmsg;
239300100825         clear V1dcuf;
239400100830         EmisMOD1    = *on ;
239500100826         errMessage  = *off;
239600100312         errGenerico = *off;
239700140207         // Per utente EDP propongo MAG  ;
239800140207         // altrimenti  TLC
239900140207         if %subst(knmus:1:3)='EDP'  ;
240000100825         v1ccuf ='MAG'   ;
240100140207         else  ;
240200140207         v1ccuf ='TLC'   ;
240300140207         endif  ;
240400100908         if modInt=*off  and modReso=*off ;
240500100826         v1cconf='S'  ;
240600100908         else  ;
240700100908         clear v1cconf ;
240800100908         endif  ;
240900100825         clear v1cinvr ;
241000100824         clear  v1cute  ;
241100100831         clear v1ciet  ;
241200100826         clear v1ctip  ;
241300100831         clear v1ctip  ;
241400100824         clear v1dtip  ;
241500100315         clear v1cmat  ;
241600100315         clear v1cmgc  ;
241700100315         clear v1cmag  ;
241800100831         clear v1creso ;
241900100831         clear v1cdtm  ;
242000101025         clear v1cnot  ;
242100121024         clear v1cdes  ;
242200100312
242300100831         // Per le MATRICOLE escludo il tipo 99
242400100831         if ImmMAT=*on   ;
242500100831          v1ciet='E'  ;
242600100831          v1ctip='99'  ;
242700100831         wtip=v1ctip   ;
242800100831         exsr chktip   ;
242900100831         v1dtip=wdtip  ;
243000100831         endif  ;
243100100831
243200101125         // Per gestine RESI imposto al "S"  e solo i confermati
243300100908         if ModReso=*on   ;
243400100831         v1creso='S'   ;
243500101125         v1cconf='C'  ;
243600100831         endif  ;
243700100831
243800100924         // Per conferma movimenti e immissione imposto data conferma = udate
243900110914         // anche per immissione matricole
244000110914         if ImmGES=*on  or ImmMAT=*on  ;
244100101112         v1cdtma=Dataoggi      ;
244200101112         endif   ;
244300101112         if ImmCMOV=*on   ;
244400100903         v1cdtm=Dataoggi      ;
244500100831         endif  ;
244600100831
244700100312         ENDSR    ;
244800080207       //--------------------------------------------------------------
244900100312       //?Inizializzazione SFL02
245000080207       //--------------------------------------------------------------
245100100312       BEGSR InzS02;
245200080207
245300080207       // Pulizia subfile
245400080207         SflDsp_N    = *on;
245500080207         SflDspCtl_N = *on;
245600100315         write  SDUOC02;
245700080207         SflDspCtl_N = *off;
245800080207         SflEnd      = *off;
245900100824         SflDsp_N    = *off;
246000100824         Sflnxtchg   = *off;
246100080530
246200121130         clear w05dtm  ;
246300100826         clear tipmat;
246400101012         clear SiCoPe  ;
246500100826         clear C02rcd;
246600100315         clear C02csr;
246700100315         S02nrr=0 ;
246800100824         clear V1cmsg;
246900100824         clear totrig;
247000080207         errMessage  = *off;
247100080207         errGenerico = *off;
247200100708
247300100826         // Decodifico ufficio scelto
247400140123         select ;
247500140123         when v1ccuf='MAG'           ;
247600100827         v1dcuf=' MAG=Magazzino  C.E.D. ' ;
247700140123         when v1ccuf='TLC'           ;
247800140123         v1dcuf=' MAG=Magazzino  T.L.C. ' ;
247900140123         other ;
248000100827         v1dcuf='FOR=Magazzino  ACQUISTI' ;
248100140123         ENDsl  ;
248200100826
248300100823         // Predispongo le letture e carico SFL
248400100823         // se movimenti MAG imposto SEDE nel tipo magazzino correlato
248500100825         kinvr=v1cinvr ;
248600140123         if v1ccuf<>'FOR' and v1cinvr=' '  ;
248700100825         eval kinvr='I' ;
248800100823         endif  ;
248900100823
249000100823         clear kmag  ;
249100100823         if v1cmag>*zeros  ;
249200100823         kmag=%int(v1cmag)  ;
249300100823         endif  ;
249400100823
249500100823         clear kmgc  ;
249600100823         if v1cmgc>*zeros  ;
249700100823         kmgc=%int(v1cmgc)  ;
249800100823         endif  ;
249900100824
250000100824         clear ktip  ;
250100100824         if v1ctip>*zeros  ;
250200100824         ktip=%int(v1ctip)  ;
250300100824         endif  ;
250400100823
250500100823         // Se presente tipo e matricola leggo per questo
250600121024         //if ktip>*zeros and v1ciet ='I'   ;
250700100823
250800121024         //select   ;
250900121024         //when v1cmat=*blanks  ;
251000121024         //  setll (ktip)                      ungio01l    ;
251100121024         //  reade (ktip)                      ungio01l    ;
251200121024         //when v1cdim>0   ;
251300121024         //  setll (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
251400121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
251500121024         //other;
251600121024         //  setll (ktip:v1cmat:v1ccuf)        ungio01l    ;
251700121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
251800121024         //endsl   ;
251900100823
252000121024         //dow not %eof(ungio02l)  ;
252100100823         // controllo parzializzazioni
252200121024         // vshnrr =gio1nrr ;
252300121024         //exsr  CtrPARZ   ;
252400121024         //if Wok='S'   ;
252500121024         //exsr CarSF02  ;
252600121024         //endif   ;
252700100823
252800121024         //select   ;
252900121024         //when v1cmat=*blanks  ;
253000121024         //  reade (ktip)                        ungio01l    ;
253100121024         //when v1cdim>0   ;
253200121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
253300121024         //other;
253400121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
253500121024         //endsl   ;
253600121024         //enddo   ;
253700100823
253800121024         //else  ;
253900100823
254000100823         select  ;
254100100831         when kinvr  =*blanks ;
254200100831           setll (v1ccuf)                      ungio04l    ;
254300100831           reade (v1ccuf)                      ungio04l    ;
254400100823         other ;
254500101012           setll (v1ccuf:kinvr:kmag)           ungio04l    ;
254600100831           reade (v1ccuf:kinvr)                ungio04l    ;
254700100824         endsl  ;
254800100823
254900100823         dow not %eof(ungio04l)  ;
255000100823         // Controllo Parzializzazioni
255100100827          vshnrr =gio4nrr ;
255200100823         exsr  CtrPARZ   ;
255300100824         if Wok='S'   ;
255400100823         exsr CarSF02  ;
255500100823         endif   ;
255600100823
255700100823         select  ;
255800100831         when kinvr=*blanks ;
255900100825           reade (v1ccuf)                      ungio04l    ;
256000100823         other ;
256100100831           reade (v1ccuf:kinvr)                ungio04l    ;
256200100823         endsl  ;
256300100823         enddo   ;
256400100823
256500121024         //endif   ;
256600100823
256700100831         select ;
256800100831         // In interrogazione  resi e conferma:
256900100831         //                 mi posiziono sul primio record  e non scrivo righe vuote
257000100831         when §sdv0int='S'  or §sdv0resi='S'  or §sdv0conf='S' ;
257100100830         c02csr=1 ;
257200100830         c02rcd=1  ;
257300100831
257400100831         // In immissione matricole mi posiziono sulla matricola ssul primo record
257500100831         when §sdv0MMat='S'  ;
257600100831         if c02csr=0 ;
257700100831         c02csr=1 ;
257800100831         endif  ;
257900100831
258000100831         other  ;
258100100831         // Solo in Gestione: Scrittura righe vuote
258200101001         // salvo il primo record vuoto che scrivo
258300101001         PrimoVuoto=s02nrr+1  ;
258400100824         exsr Righevuote  ;
258500100831         endsl   ;
258600100823
258700100824         ENDSR  ;
258800100824       //--------------------------------------------------------------
258900100824       //?Scrittura righe vuote
259000100824       //--------------------------------------------------------------
259100100824         BEGSR  Righevuote   ;
259200100824
259300100824         // Finito di leggere carico tutta la pagina con record vuoti
259400100824         if s02nrr>=c_sflPag ;
259500100824         wpag=%div(s02nrr:c_sflPag) ;
259600100824         wresto=%rem(s02nrr:c_sflPag)  ;
259700100824         if wresto>0    ;
259800100924         c02csr=s02nrr  ;
259900100824         else   ;
260000100924         c02csr=(wpag*c_sflpag)+1 ;
260100100824         endif  ;
260200100824
260300100824         else  ;
260400100824         clear wpag  ;
260500100824         wresto=s02nrr  ;
260600100824         // posizionamento sulla prtima pagina e 1° record vuoto
260700100924         c02csr=s02nrr  ;
260800100927          if c02csr=0 ;
260900100927           eval c02csr=1  ;
261000100927          endif ;
261100100824         endif ;
261200100824
261300100824         dow wresto<c_sflpag  ;
261400100823         wresto=wresto+1 ;
261500100826
261600100824         vshimm='S'   ;
261700100830         clear VSCOPZ ;
261800100830         clear VShann ;
261900100909         clear VShfmv ;
262000101006         clear VShfpr ;
262100120524         clear VShfst ;
262200100909         clear VShnrr ;
262300140123         if v1ccuf <>'FOR'  ;
262400100825         vscinvr='I'  ;
262500100824         else   ;
262600100825         vscinvr=v1cinvr;
262700100824         endif   ;
262800100823         clear vscmag ;
262900100830         clear vscmag3;
263000100827         clear vshnrr ;
263100100827         clear vscqta ;
263200100823         clear vsctip ;
263300100830         clear vsdtip ;
263400100830         clear vsdtip3;
263500100827         clear vscmgc ;
263600100827         clear vscmat ;
263700100827         clear vsdmod1 ;
263800100827         clear vsdmod2 ;
263900100830         clear vsdmod  ;
264000100823         clear vscdes ;
264100100826         clear vscnot ;
264200100823         clear vsctru ;
264300100823         clear vsctir ;
264400100923         clear vscmatr;
264500100923         clear vsdmodr;
264600101005         clear vscmgcr;
264700101005         clear vshfmat;
264800100826         clear vscksc ;
264900100826         clear vsdksc ;
265000100830         clear vsdksc3;
265100100823         clear vscdtm ;
265200100823         clear vscddo ;
265300100823         clear vscdoc ;
265400100825         clear vscute ;
265500100830         clear vscdim4;
265600100830         clear vscdim ;
265700100827         clear vscreso  ;
265800100827         clear vscslr   ;
265900101008         clear vscncl   ;
266000101008         clear vscpkg   ;
266100100825         vshdim=%dec(datasys)  ;
266200100824
266300100824          exsr Protcampi  ;
266400100824
266500100823          s02nrr=s02nrr+1 ;
266600100823          write  SDUOS02;
266700100823          enddo  ;
266800080409
266900100824         // Salvo numero righe scritte
267000100824         totrig=s02nrr  ;
267100100824
267200080702         ENDSR    ;
267300080627
267400100823       //--------------------------------------------------------------
267500100823       //?Controllo selezioni impostate in prima videata
267600100823       //--------------------------------------------------------------
267700100823          BEGSR   CtrPARZ    ;
267800100823          Wok='S'    ;
267900100908          if w1cdtmd>0  and w1cdtma=0 ;
268000100908          w1cdtma=w1cdtmd ;
268100100908          endif  ;
268200100826
268300100823          // Utente
268400100823          if v1cute<>*blanks   and ungute<>v1cute ;
268500100823            clear wok  ;
268600100823            leavesr    ;
268700100823          endif      ;
268800100823          // Data Immissione
268900100824          if v1cdim>*zeros    and ungdim<w1cdim ;
269000100823            clear wok  ;
269100100823            leavesr    ;
269200100823          endif      ;
269300100823          // Tipo movimentazione
269400100825          if ungcuf <>v1ccuf ;
269500100823            clear wok  ;
269600100823            leavesr    ;
269700100823          endif      ;
269800100823          // Tipo magazzino correlato
269900100825          if kinvr<>*blanks and unginvr<>kinvr  ;
270000100823            clear wok  ;
270100100823            leavesr    ;
270200100823          endif      ;
270300100823          // Magazzino destinazione
270400100823          if kmag>*zeros  and ungmag<>kmag   ;
270500100823            clear wok  ;
270600100823            leavesr    ;
270700100823          endif      ;
270800100823          // Magazzino correlato
270900100823          if kmgc>*zeros  and ungmgc<>kmag   ;
271000100823            clear wok  ;
271100100823            leavesr    ;
271200100823          endif      ;
271300100826          // solo i record da confermare
271400100826          if v1cconf='S' and ungfmv='S' ;
271500100826            clear wok  ;
271600100826            leavesr    ;
271700100826          endif      ;
271800100908          // solo i record confermati
271900100908          if v1cconf='C' and ungfmv=' ' ;
272000100908            clear wok  ;
272100100908            leavesr    ;
272200100908          endif      ;
272300100908          // data conferma movimenti AL
272400100831          if v1cdtm>0   and ungdtm>w1cdtm  ;
272500100831            clear wok  ;
272600100831            leavesr    ;
272700100831          endif      ;
272800100908          // data conferma movimenti DAL/AL
272900100908          if v1cdtmd>0 or v1cdtma>0 ;
273000100908          if  ungdtm<w1cdtmd or ungdtm>w1cdtma ;
273100100908            clear wok  ;
273200100908            leavesr    ;
273300100908          endif      ;
273400100908          endif      ;
273500101025
273600101025          // SCAN sulle note
273700101025          if v1cnot<>*blanks   ;
273800101025           xx=%checkr(' ':v1cnot)  ;
273900101025           Indx=%scan(%subst(v1cnot:1:xx):ungnot)  ;
274000101025           if Indx=0   ;
274100101025            clear wok  ;
274200101025            leavesr    ;
274300101025           endif      ;
274400101025          endif      ;
274500121024          // SCAN sulla descrizione
274600121024          if v1cdes<>*blanks   ;
274700121024           xx=%checkr(' ':v1cdes)  ;
274800121024           Indx=%scan(%subst(v1cdes:1:xx):ungdes)  ;
274900121024           if Indx=0   ;
275000121024            clear wok  ;
275100121024            leavesr    ;
275200121024           endif      ;
275300121024          endif      ;
275400100823
275500121024          // Tipo Unità  da ESCLUDERE
275600100831          if v1ctip<>*blanks and v1ciet='E' and ktip= ungtip ;
275700100831            clear wok  ;
275800100831            leavesr    ;
275900100831          endif      ;
276000121024          // Tipo Unità  da INCLUDERE
276100121024          if v1ctip<>*blanks and v1ciet='I' and ktip<> ungtip ;
276200121024            clear wok  ;
276300121024            leavesr    ;
276400121024          endif      ;
276500100927
276600121029          // Matricola inseribile solo per "I"-inclusione
276700100927          if v1cmat<>*blanks and  v1cmat<>ungmat ;
276800100831            clear wok  ;
276900100831            leavesr    ;
277000100831          endif      ;
277100100831
277200100831          // unità da rendere
277300100831          if v1creso='S'  and ungreso='S'   ;
277400100831            clear wok  ;
277500100831            leavesr    ;
277600100831          endif      ;
277700100831          if v1creso='S'  and ungtir=0      ;
277800100831            clear wok  ;
277900100831            leavesr    ;
278000100831          endif      ;
278100100823          ENDSR  ;
278200100823       //--------------------------------------------------------------
278300100823       //?eseguo caricamento SFL
278400100823       //--------------------------------------------------------------
278500100823          BEGSR   carSF02    ;
278600100830          clear vscopz  ;
278700100830          clear vshANN  ;
278800100830          protOPZ=*off  ;
278900100830          clear vshimm  ;
279000100830
279100100830          exsr CarRECORD  ;
279200100830
279300100830          s02nrr=s02nrr+1   ;
279400100831
279500100831          // Per l'immissione Matricole mi posizione sul 1° record senza il codice
279600100831          if §sdv0mmat='S' and c02csr=0 and  vscmat=*blanks ;
279700100831          c02csr=s02nrr  ;
279800100831          ErrMat =*on    ;
279900100831          Sflnxtchg=*on  ;
280000100831          endif  ;
280100100831
280200100831          write  SDUOS02;
280300100831          ErrMat =*off   ;
280400100831          Sflnxtchg=*off ;
280500100831
280600100830          ENDSR         ;
280700100830       //--------------------------------------------------------------
280800100830       //?Carica record
280900100830       //--------------------------------------------------------------
281000100830          BEGSR   carRECORD ;
281100100830
281200100831          vshfmv =ungfmv ;
281300101006          vshfpr =ungfpr ;
281400120524          vshfst =ungfst ;
281500100831          vscinvr=unginvr;
281600100830  1       if $video='S2' ;
281700100823          vscmag=ungmag  ;
281800100830          clear vsdmod1  ;
281900100830          clear vsdmod2  ;
282000100830          clear vsdksc   ;
282100100830          clear vscdim4  ;
282200100830          clear vsdtip   ;
282300100830          else ;
282400100830          vscmag3=%editc(ungmag:'X')    ;
282500100909          wtmag='D'      ;
282600100908          wazorg='E  '   ;
282700100830          wmag=vscmag3   ;
282800100830          exsr  chkmag   ;
282900100830          vsdmag=wdmag    ;
283000100830
283100100830          clear vsdmod   ;
283200100830          clear vsdksc3  ;
283300100830          clear vscdim   ;
283400100830          clear vsdtip3  ;
283500100830  1       endif ;
283600100830
283700100826          vscqta=ungqta  ;
283800101008          vscncl=ungncl  ;
283900101008          vscpkg=ungpkg  ;
284000100826
284100100924          clear vsctip  ;
284200100830 1        if ungtip>0 ;
284300100830          vsctip=%editc(ungtip:'X') ;
284400100830          wtip=vsctip   ;
284500100830          exsr chktip   ;
284600100830 3          if $Video='S2' ;
284700100830            vsdtip=wdtip  ;
284800100830            else  ;
284900100830            vsdtip3=wdtip  ;
285000100830            endif ;
285100100830  1       endif ;
285200100826          vscmat=ungmat  ;
285300100826          clear vscksc   ;
285400100826          // Se c'e' tipo e matricola prendo il modello e lo visualizzo
285500100826 1        if ungtip>0 and ungmat<>*blanks ;
285600100930          chain (ungmat:ungtip)  unana07l    ;
285700100930 2        if  %found(unana07l)  ;
285800100830
285900100830 3        if $Video='S2' ;
286000100830          vsdmod1=unamod;
286100100827          vsdmod2=%subst(unamod:7:9) ;
286200100830          else  ;
286300100830          vsdmod =unamod;
286400100830 3        endif  ;
286500100826
286600100830          // se non è invio visualizzo cliente da anagrafica
286700100830 3        if  vscinvr<>'I'  and ungksc=0;
286800100826          vscksc =unaksc;
286900100830 4        if unaksc>0  ;
287000100826           chain   (1:dutkci:unaksc) cnaco00f   ;
287100100830 5         if not %found(cnaco00f)   ;
287200100830             acorag=*all'?'  ;
287300100830 5         endif  ;
287400100830 5         if $Video='S2' ;
287500100830             vsdksc=acorag  ;
287600100830           else  ;
287700100830             vsdksc3=acorag  ;
287800100830 5         endif  ;
287900100830
288000100830 4        endif ;
288100100830 3        endif ;
288200100826 2        endif ;
288300100826 1        endif ;
288400100826
288500100827          vscslr =ungslr  ;
288600100827          vscreso=ungreso ;
288700110404          dgio01=ungflo  ;
288800110404          if unginvr='R' or unginvr='F'  ;
288900110404           vsctru=§giogsf  ;
289000110404          else  ;
289100110404           vsctru=ungtru  ;
289200110404          endif  ;
289300100923
289400100927          clear vsdtir ;
289500100827  1       if ungtir>0 ;
289600100830          vsctir=%editc(ungtir:'X')   ;
289700100927          wtip=vsctir   ;
289800100927          exsr chktip   ;
289900100927          vsdtir=wdtip  ;
290000100923          vscmatr=ungmatr ;
290100101005          vshfmat=ungmatr ;
290200101005          clear vscmgcr;
290300101005          clear vsdmodr;
290400100923 2        if ungtir>0 and ungmatr<>*blanks ;
290500100930          chain (ungmatr:ungtir)  unana07l    ;
290600100930 3        if  %found(unana07l)  ;
290700100923          vsdmodr=unamod  ;
290800101005          vscmgcr=unamag  ;
290900100923 3        endif  ;
291000100923 2        endif  ;
291100100923 xx1      else ;
291200100826          clear vsctir ;
291300100923          clear vscmatr;
291400101005          clear vshfmat;
291500100923          clear vsdmodr;
291600101005          clear vscmgcr;
291700100827  1       endif ;
291800100826          vscmgc=ungmgc  ;
291900100923          clear vsdmgc  ;
292000100909          if ungmgc>0   ;
292100100909          wmag=%editc(ungmgc:'X');
292200100909          wtmag='C' ;
292300100909          wazorg='E  '   ;
292400100909          exsr  chkmag   ;
292500100909          vsdmgc=wdmag    ;
292600100909          endif   ;
292700100826          vscdes=ungdes  ;
292800100826          vscnot=ungnot  ;
292900100830
293000100830          // se pieno CLIENTE decodifico
293100100827 1        if ungksc>0   ;
293200100830           vscksc=ungksc  ;
293300100826           chain   (1:dutkci:ungksc) cnaco00f   ;
293400100830 2         if not %found(cnaco00f)   ;
293500100830             vsdksc=*all'?' ;
293600100830 2         endif  ;
293700100830 2         if $Video='S2' ;
293800100830             vsdksc=acorag  ;
293900100830           else  ;
294000100830             vsdksc3=acorag  ;
294100100830 2         endif  ;
294200100830
294300100827 1         endif  ;
294400100823
294500100823          clear vscdtm   ;
294600100823          if ungdtm>0    ;
294700100823          dataiso=%date(ungdtm)   ;
294800100823          datadmy=dataiso ;
294900100823          vscdtm=%dec(datadmy)  ;
295000100823          endif   ;
295100100823
295200100823          clear vscddo   ;
295300100823          if ungddo>0    ;
295400100823          dataiso=%date(ungddo)   ;
295500100823          datadmy=dataiso ;
295600100823          vscddo=%dec(datadmy)  ;
295700100823          endif   ;
295800100823          vscdoc=ungdoc  ;
295900100823
296000100826          vscute=ungute  ;
296100100823          if ungdim>0    ;
296200100823          dataiso=%date(ungdim)   ;
296300100823          datadmy=dataiso ;
296400100830          dataeur=dataiso ;
296500100823          w0060 =%dec(datadmy)  ;
296600100830          if $Video='D3'  ;
296700100830          vscdim=%dec(dataeur)  ;
296800100920          vschim=unghim         ;
296900100830          else  ;
297000100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
297100100827                  %subst(%editc(w0060:'X'):3:2)    ;
297200100830          endif  ;
297300100824          vshdim=ungdim  ;
297400100823          endif   ;
297500100823
297600100909          ErrGenerico=*off ;
297700100909          ErrMessage =*off ;
297800100826          exsr ProtCampi  ;
297900100823          ENDSR  ;
298000080702       //--------------------------------------------------------------
298100080702       //?Gestione protezione campi del sfl
298200080702       //--------------------------------------------------------------
298300080702          BEGSR   ProtCampi   ;
298400100830           protOPZ=*off ;
298500100826           ProtperINV=*off ;
298600100826           ProtperRIE=*off ;
298700110404           ProtperFOR=*off ;
298800100826           ProtINVR  =*off ;
298900100831           ProtMAT   =*off ;
299000100831           EmisCONF  =*off ;
299100100827           protrIGA=*off   ;
299200100830           RecANN  =*off   ;
299300100909           EmisCONF  =*off ;
299400101006           RevMAG    =*off ;
299500120524           NoSTAMPABO=*off ;
299600100824
299700100830          // Record di nuova immissione
299800100824          if vshimm='S'  ;
299900100830          protOPZ=*on  ;
300000100827          else  ;
300100100827           protrIGA=*on    ;
300200100830          endif    ;
300300100831
300400100903          // Per Int  e resi proteggo matricola
300500100903          if §sdv0resi='S'  or §sdv0int='S';
300600100831           protMAT =*on    ;
300700100831          endif   ;
300800100903          // Per immissione, solo per i record già inseriti
300900100903          if §sdv0imm='S' and vshimm=' ' ;
301000100903           protMAT =*on    ;
301100100903          endif  ;
301200100909
301300100909          // Se immissione matricole proteggo opzione  tranne che per F21
301400100909          if §sdv0mmat='S'  and dsp_aid <> c_F21    ;
301500100909           protOPZ =*on    ;
301600100909          endif   ;
301700100831
301800100831          // Se già confermato visualizzo
301900100831          if vshfmv='S'   ;
302000100831           EmisCONF  =*on  ;
302100100831          endif ;
302200101006          // Se già preparato materiale
302300101006          if vshfpr='P'   ;
302400101006           RevMAG   =*on  ;
302500101006          endif ;
302600100831
302700100831          // In fase di conferma accendo sflnexc per controllare tutti i record
302800100831          if §sdv0conf='S'   ;
302900100831           Sflnxtchg=*on  ;
303000100831          endif   ;
303100100830
303200100830          // Record annullato
303300100830          if vshANN='S'  ;
303400100830          protOPZ=*on  ;
303500100830          protrIGA=*on    ;
303600100830          recANN  =*on    ;
303700100830          endif  ;
303800100824
303900100826          if v1ccuf ='MAG'  or vscinvr='I'  ;
304000100826          protperINV=*on  ;
304100100824          endif    ;
304200100826
304300120523          if vscinvr='R' or vscinvr='F' or vscinvr='O';
304400110404           protperRIE=*on  ;
304500110404
304600120523          if vscinvr='R' or vscinvr='O';
304700110404           protperFOR=*on  ;
304800110404          endif  ;
304900100826          endif   ;
305000100826
305100100830          // Se immesso in proma videata il tipo invio/rientro lo proteggo
305200100826          if v1cinvr<>' ' ;
305300100826          protINVR=*on ;
305400100826          endif  ;
305500120524          // se non voglio la stampa della bolla visualizzo
305600120524          if vshfst='N'  ;
305700120524           NOstampaBO=*on  ;
305800120524          endif  ;
305900080703
306000080702          ENDSR    ;
306100100316       //--------------------------------------------------------------
306200100708       //?controllo Magazzino
306300100316       //--------------------------------------------------------------
306400100316        BEGSR ChkMag                 ;
306500100930        clear v1cmsg   ;
306600100930
306700100930        clear tnsdmagds  ;
306800100930        §magmag=wmag                            ;
306900100930        §imagtmag=wtmag                         ;
307000100930        §imagtorg=wazorg                        ;
307100100930        §imagcuf =v1ccuf                        ;
307200100930        §imaginvr=vscinvr                       ;
307300100930
307400100930        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
307500100930
307600100930        // Eseguita ricerca
307700100930        if §omagerr='R'   ;
307800100930        wmag=§magmag  ;
307900100930        wdmag = §omagdes  ;
308000100930        ErrGenerico=*on  ;
308100100930        leavesr   ;
308200100930        endif  ;
308300100930
308400100930        // Errore
308500100930        if §omagerr='E'   ;
308600100930        v1cmsg=§omagmsg  ;
308700100930        Errgenerico=*on  ;
308800100930        else   ;
308900100930        wdmag=§omagdes;
309000100930        endif  ;
309100100316
309200100316         if ErrGenerico =*on ;
309300100316         ErrMessage=*on  ;
309400100316         wdmag=*all'?'   ;
309500100316         endif  ;
309600100316
309700100316          ENDSR    ;
309800100708       //--------------------------------------------------------------
309900100708       //?controllo Tipo Unità
310000100708       //--------------------------------------------------------------
310100100708        BEGSR ChkTip                 ;
310200100708        clear WDTIP    ;
310300100824        indx=%scan('?':wtip )   ;
310400100827
310500100824        if Indx>0             ;
310600100708         clear tibs02ds  ;
310700100708         t02tla=' '      ;
310800100708         t02mod='R'      ;
310900100708         t02sif=knsif    ;
311000100708         t02cod='UNT'    ;
311100100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
311200100708         if t02ke1<>*blanks;
311300100824         wtip =t02ke1    ;
311400100708         wdtip=t02uni    ;
311500100708         else   ;
311600100708         clear  wtip ;
311700100708         endif  ;
311800100708
311900100708        ErrGenerico=*on   ;
312000100708        leavesr  ;
312100100708        endif   ;
312200100708
312300100708         clear tibs02ds  ;
312400100708         t02tla=' '      ;
312500100708         t02mod='C'      ;
312600100708         t02sif=knsif    ;
312700100708         t02cod='UNT'    ;
312800100708         t02ke1=wtip     ;
312900100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
313000100708
313100100708         if  t02err<>*blanks ;
313200100824         ErrGenerico=*on  ;
313300100824         ErrMessage =*on  ;
313400100824         wdtip=*all'?'   ;
313500100708         else  ;
313600100824         wdtip=t02uni    ;
313700100708         endif    ;
313800100708
313900100708          ENDSR    ;
314000080702       //--------------------------------------------------------------
314100100830       //?Aggiornamento record nuova immissione da SFL
314200080702       //--------------------------------------------------------------
314300080702        BEGSR ConfAGGIO              ;
314400081003
314500100315       s02nrr=1  ;
314600100824       readc    SDUOs02    ;
314700080704
314800100824 1     dow not %eof  ;
314900100831       // Per Gestione, se manca opzione, non c'e' nulla da aggiornare
315000100924       // Per immisssione, devono esserci tipo e mag destinazione
315100100924 2     if (§sdv0imm='S' and vshimm=' ' and vscopz=' ') or
315200100924 2        (§sdv0imm='S' and vshimm='S' and (vsctip<=*zeros or vscmag=0));
315300100831 x2    else   ;
315400100831
315500100831         // chain record
315600100831 3       if vshimm=' '    ;
315700100831           chain(e) vshnrr   ungio00f   ;
315800100831 4          if %error or not %found ;
315900100831             v1cmsg=msg(25)  ;
316000100831             ErrGenerico=*on  ;
316100100831             ErrMessage =*on  ;
316200100831             EXSR AggioSFL   ;
316300100831             leavesr  ;
316400100831 4          endif  ;
316500100831 3       endif  ;
316600100831
316700100907          exsr ScriviUNGIO  ;
316800100907 2     endif  ;
316900100830
317000100830       readc    SDUOs02    ;
317100100831 0      enddo           ;
317200100830       ENDSR            ;
317300100830       //--------------------------------------------------------------
317400100830       //?Aggiornamento file UNGIO
317500100830       //--------------------------------------------------------------
317600100830        BEGSR SCriviUNGIO            ;
317700100830
317800100830 1     if    vshimm=' '  and vscopz='4'  ;
317900100830       delete ungioFIS  ;
318000100830
318100100830 x1    else ;
318200100830
318300100830 2     if    vshimm='S'   ;
318400100827        clear ungiofis  ;
318500100901        timeiso=%time        ;
318600100901        unghim =%dec(timeiso);
318700100930        else   ;
318800100930        // richaino il record per non avere problemi  di "sporco" nei campi
318900100930         chain(e) vshnrr   ungio00f   ;
319000100830 2      endif   ;
319100100830
319200100824       ungdim  =vshdim  ;
319300100824       ungute  =vscute  ;
319400100826       ungcuf  =v1ccuf  ;
319500100825       if vsctip>*zeros  ;
319600100824       ungtip  =%int(vsctip) ;
319700100825       else  ;
319800100825       clear ungtip ;
319900100825       endif  ;
320000100824       ungmat  =vscmat  ;
320100100827       ungqta  =vscqta  ;
320200100827       if ungqta=0 ;
320300100827       ungqta=1 ;
320400100827       endif ;
320500100830       if $Video='S2' ;
320600100825       ungmag  =vscmag  ;
320700100830       else  ;
320800100830       ungmag  =%int(vscmag3)  ;
320900100830       endif  ;
321000100827       ungmgc  =vscmgc  ;
321100100825       unginvr =vscinvr ;
321200100824       ungdes  =vscdes  ;
321300100827       ungnot  =vscnot  ;
321400110404       // Per mag memorizzo vsctru un ungtru  come unità in aumento/sostituz
321500110404       if ProtperRIE=*off  ;
321600110404         ungtru  =vsctru  ;
321700110404         clear §giogsf  ;
321800110404       else  ;
321900110404       // Per "FOR" memorizzo nella DS DGIO01 come guasto fisico
322000110404        §giogsf=vsctru  ;
322100110404        clear ungtru  ;
322200120524       // per FOR "O" aggiorno "N" nella stampa bolla
322300120524       if ImmCMOV=*on  and  vscinvr='O'  ;
322400120524        ungfst='N'  ;
322500120524       endif  ;
322600110404       endif  ;
322700110404
322800110404       ungflo=dgio01  ;
322900110404
323000100827       clear ungksc   ;
323100100827       if vscksc>0 and vscinvr='I'  ;
323200100827       ungksc  =vscksc  ;
323300100827       endif  ;
323400100825       if vsctir>*zeros  ;
323500100824       ungtir  =%int(vsctir) ;
323600100923       ungmatr=vscmatr ;
323700100825       else  ;
323800100825       clear ungtir ;
323900100923       clear ungmatr;
324000100825       endif  ;
324100100827       if vscdtm>0 ;
324200100827       dataeur=%date(vscdtm:*dmy)  ;
324300100827       dataiso=dataeur  ;
324400100827       ungdtm=%dec(dataiso)  ;
324500100922       else  ;
324600100922       clear ungdtm  ;
324700100827       endif  ;
324800100922
324900100827       if vscddo>0 ;
325000100827       dataeur=%date(vscddo:*dmy)  ;
325100100827       dataiso=dataeur  ;
325200100827       ungddo=%dec(dataiso)  ;
325300100922       else  ;
325400100922       clear ungddo ;
325500100827       endif  ;
325600100827       ungdoc=vscdoc  ;
325700100827       ungreso=vscreso ;
325800100827       ungslr =vscslr  ;
325900101008       ungncl= vscncl  ;
326000101008       ungpkg= vscpkg  ;
326100100824
326200100908       // Richiamo pgm che crea i movimenti
326300100930       //  se non è escluso e se sono in conferma
326400100922 2     if §sdv0conf='S'  and vscopz= ' '  ;
326500100908
326600100908       // il tipo "99" lo flaggo senza richiamo
326700100908 3     if ungtip<> 99  ;
326800110412
326900110412       // Se FOR"F" e c'e' nuova matricola la scrivo in UNANA prima di RENDERE
327000110412
327100110412 4     if ProtperFOR=*on and ungnot<>*blanks ;
327200110412       chain (ungtip:ungmat)  unana02l    ;
327300110412 5     if %found(unana02l) ;
327400110412        unamat=ungnot;
327500110412        unadtr=%dec(datasys)    ;
327600110412        update unana2   ;
327700110412 5     endif  ;
327800110412 4     endif  ;
327900100922
328000100907       clear dsunit  ;
328100100907       dsuop1='F6N'  ;
328200100907       dsutip=ungtip  ;
328300100907       kpjbu = dsunit  ;
328400100907       callp TNSDU5R (kpjba: dsungio)  ;
328500100922
328600100922       // Aggiorno file dotazioni se previsto e se tipo unità inviato e di reso diversi
328700101026 4     if unginvr='I'    and ungtru<>'  ' and ungtip<>ungtir and ungksc=0 ;
328800100922       chain (ungtip:ungmag)  undot01l   ;
328900100922 5     if %found(undot01l)  ;
329000100922        dotqta=dotqta+1  ;
329100100922        update undot000 ;
329200100922 x5     else    ;
329300100922        clear undot000;
329400100922        dottip=ungtip  ;
329500100922        dotqta=1 ;
329600100922        dotfil= ungmag ;
329700100922        write undot000 ;
329800100922 5      endif  ;
329900100922
3300001009225      if ungtru='S' and ungtir>0   ;
330100100922       chain (ungtir:ungmag)  undot01l   ;
3302001009226      if %found(undot01l)  ;
330300100922        dotqta=dotqta-1  ;
330400100922        update undot000 ;
3305001009226      endif  ;
3306001010285      endif  ;
3307001010284      endif  ;
330800100922
330900100930       // Se presente anche la matricola aggiorno UNANA con la "S" di RENDERE
331000101028 4     if ungmatr<>*blanks  ;
331100110421
331200110421       //  non imposto al "S" di rientro se già rientrata in data di oggi con FOR "F"
331300110421       exsr SelectSQLFORF   ;
331400110421
331500100930       chain (ungtir:ungmatr)  unana02l    ;
331600101028 5     if %found(unana02l) ;
331700110421
331800110421 4a    if wtrov=' ' ;
331900130321       // se però il magazzino è sede --> non imposto lo stesso la "S"
332000130321          clear tnsdmagds  ;
332100130321          §magmag=%editc(unamag:'X')  ;
332200130321          §imagtmag='C' ;
332300130321          §imagtorg='E  '   ;
332400130321
332500130321        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
332600130321
332700130321        if §omagerr<>' ' or (§omagfag<>'W' and §omagfag<>'Y')  ;
332800100930        %subst(unalsw:15:1)='S'  ;
332900110421 5     endif  ;
333000130321 5     endif  ;
333100130321
333200101027        unadtr=%dec(datasys)    ;
333300100930        update unana2   ;
333400110421 4a    endif  ;
333500110421 4     endif  ;
333600100930
333700100908 3     endif  ;
333800100922
333900100908       ungfmv='S'  ;
334000100908 3     if ungdtm=0  ;
334100100908         ungdtm  =%dec(datasys) ;
334200100908 3     endif  ;
334300100922
334400100908
334500100908         update ungioFIS  ;
334600100907
334700100908 x2    else   ;
334800100907
334900100908  3    if vshimm=' '   ;
335000100830         update ungioFIS  ;
335100100830         else ;
335200100827         write ungiofis  ;
335300100908  3    endif  ;
335400100908 2     endif  ;
335500100907
335600100908 1     endif  ;
335700080704
335800080702        ENDSR;
335900110421       //--------------------------------------------------------------
336000110421       //?Select con SQL su ungio00F   per FOR "F"
336100110421       //--------------------------------------------------------------
336200110421       BEGSR  SelectSQLForF ;
336300110421 1     if ungdtm=0  ;
336400110421         sqdtm  =%dec(datasys) ;
336500110421         else  ;
336600110421         sqdtm=ungdtm     ;
336700110421 1     endif  ;
336800110421
336900110421       clear wtrov  ;
337000110421
337100110421         wrkgetlista='SELECT count(*)  +
337200110421                          from UNGIO00F WHERE ungcuf=''FOR'' and unginvr=''F'' +
337300110421            and ungmat=''' + ungmatr +''' and ungtip=' + %editc(ungtir:'X')  +
337400110421            ' and ungdtm=' + %editc(sqdtm :'X') ;
337500110421
337600110421         exec sql   PREPARE sF FROM :wrkgetlista   ;
337700110421
337800110421         exec sql   DECLARE aF cursor FOR  sF   ;
337900110421
338000110421         exec sql OPEN aF   ;
338100110421
338200110421 0       if  sqlcod=0   ;
338300110421         exec sql FETCH NEXT  FROM aF INTO:sqcont;
338400110421 0         if  sqcont>0   ;
338500110421             wtrov='S'  ;
338600110421 0         endif   ;
338700110421 0       endif   ;
338800110421
338900110421         exec sql CLOSE aF   ;
339000110421
339100110421       ENDSR;
339200110421
339300100830       //--------------------------------------------------------------
339400101001       //?Gestione videata 03
339500100830       //--------------------------------------------------------------
339600100830       BEGSR Gesd03;
339700100830
339800101018       // salvo la matricola: se imposta nel sfl la riporto
339900100831         savMat=vscmat  ;
340000101005         savMatr=vscmatr ;
340100100830         exsr CarRECORD  ;
340200100831         vscMat=savmat  ;
340300101005         vscMatr=savmatr  ;
340400100831
340500100830
340600100830         // Controllo opzione
340700100830         Emismod1=*off ;
340800100830         clear vsdf6           ;
340900120524         clear vsdf15          ;
341000120524         if vscopz<>'4'  and
341100120524            vscinvr<>'O' ;
341200120524           if  vshfst=' '  ;
341300120524            VSDf15=cF15BLO                ;
341400120524           else  ;
341500120524            VSDf15=cF15SBLO                ;
341600120524           endif  ;
341700120524         endif  ;
341800120524
341900100830 1       select   ;
342000100830         when vscopz='4'   ;
342100100901         vsdf6='F6=Conferma ANNULLAMENTO' ;
342200100830         v3dmod=' ANNULLAMENTO '  ;
342300100830
342400100830 1       when vscopz='2'   ;
342500100901         VSDf6='F6=CONFERMA Modifica' ;
342600120524
342700100830         v3dmod='   MODIFICA   '  ;
342800120524         // Se non conferato sproteggo riga
342900100831         if vshfmv=' ' ;
343000100831          protRiga=*off  ;
343100100831         endif  ;
343200100830
343300100830 1       when vscopz='5'   ;
343400100830         v3dmod='  VISUALIZZA  '  ;
343500100830         endsl   ;
343600100830
343700100830         // Imposto i campi diversi
343800100830         vscmag3=%editc(vscmag:'X')    ;
343900100830
344000100830 0       dow    $video='D3'   ;
344100100830
344200100830         // Emissione Testata
344300100830         if  errMessage  = *off;
344400100830           write  SDUOT01;
344500100830         endif;
344600100830
344700100830         // Emissione videata
344800100830         exfmt  SDUOD03;
344900100830
345000100830         reset errMessage;
345100100830         reset errGenerico;
345200100830         clear V1cmsg    ;
345300100830         clear wtmag     ;
345400100830         errMAG=*off     ;
345500100830         errMGC=*off     ;
345600100830         errTIP=*off     ;
345700100830         errMAT=*off     ;
345800100830         errQTA=*off     ;
345900100830         errTRU=*off     ;
346000100830         errTIR=*off     ;
346100100830         errDES=*off     ;
346200110412         errNOT=*off     ;
346300100830         errKSC=*off     ;
346400100830         errDTM=*off     ;
346500100909         errDDO=*off     ;
346600101012         errNCL=*off     ;
346700100923         errMATR=*off     ;
346800101012         errINVR=*off     ;
346900100830
347000100830 1       SELECT;
347100100830
347200100830         // - F12=Ritorno
347300100830 1         WHEN  dsp_aid = c_F12;
347400100930            chain(n) vshnrr   ungio00f   ;
347500100831            exsr CarRECORD  ;
347600100924            ErrGenerico=*off ;
347700100924            ErrMessage =*off ;
347800100924            clear v1cmsg  ;
347900100924            vscMat=savmat  ;
348000101005            vscMatr=savmatr  ;
348100100830            $Video='S2'   ;
348200100831            Emismod1=*on  ;
348300100830            leavesr  ;
348400100901
348500100901         // - F2=Gestione Unità EDP
348600100901 1         WHEN  dsp_aid = c_F02;
348700100927            clear dsunana  ;
348800100928            if vscmat<>*blanks  and
348900100928               (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
349000100927              UIMmat=vscmat  ;
349100100927             endif  ;
349200100928           if vscmatr<>*blanks  and
349300140123 2          (rnmfl= 'VSCMATR ' and v1ccuf<>'FOR' )  ;
349400100928              UIMmat=vscmatr ;
349500100928             endif  ;
349600100901           exsr CallGESUNIT     ;
349700100901
349800100909         // - F7=Ricerca Matricola
349900100901 1         WHEN  dsp_aid = c_F07;
350000100901           // Per annullamento opzione non ammmessa
350100100901           if vscopz='4'  ;
350200100901            ErrGenerico=*on   ;
350300100901            ErrMessage =*on   ;
350400100901            v1cmsg=msg(36)  ;
350500100901           else  ;
350600100901           exsr CallRicMat      ;
350700100901           endif  ;
350800101018
350900101018         // - F11=elimina "Materiale preparato"
351000101018 1         WHEN  dsp_aid = c_F11;
351100101018         chain(e) vshnrr   ungio00f   ;
351200101018         clear ungfpr  ;
351300101018         clear vshfpr  ;
351400101018         update ungiofis  ;
351500101018         RevMag=*off  ;
351600120524
351700120524         // - F15=Blocca/sblocca STAMPA BOLLA
351800120524 1         WHEN  dsp_aid = c_F15;
351900120525           //  In modifica  e visulizzazione
352000120525 2         if vscopz='2' or vscopz='5' ;
352100120524           // e solo se corretto il magazzino di destino
352200120524 3         if   vscinvr='O'   ;
352300120524            ErrGenerico=*on   ;
352400120524            ErrMessage =*on   ;
352500120524            v1cmsg=msg(60)  ;
352600120524 x3        else  ;
352700120524           // solo per magazzini fornitori che prevedono la stampa
352800120524          wmag= vscmag3         ;
352900120524          wazorg='E  '   ;
353000120524          exsr  chkmag   ;
353100120524
353200120524  4       if §omagfag='Y' or §omagfag='F' or §omagfag='A'  or
353300120524             §omaglue='S'  ;
353400120525
353500120524           chain(e) vshnrr   ungio00f   ;
353600120525           select   ;
353700120525  5        when ungfst='N'  ;
353800120524           ungfst=' '  ;
353900120524           vshfst=' '  ;
354000120525            VSDf15=cF15BLO                ;
354100120524           NOstampaBO=*off ;
354200120525
354300120525  5        when ungfst='S' ;
354400120525             v1cmsg=msg(62)  ;
354500120525             ErrGenerico=*on  ;
354600120525             ErrMessage =*on  ;
354700120525  x5         other ;
354800120524           ungfst='N'  ;
354900120524           vshfst='N'  ;
355000120525            VSDf15=cF15SBLO                ;
355100120524           NOstampaBO=*on  ;
355200120525  5       endsl   ;
355300120524         update ungiofis  ;
355400120524  x4      else  ;
355500120524         // Errore --> magazzino che non prevede la stampa
355600120524            ErrGenerico=*on   ;
355700120524            ErrMessage =*on   ;
355800120524            v1cmsg=msg(61)  ;
355900120524  4       endif  ;
356000120524  3       endif  ;
356100120524  2       endif  ;
356200120524
356300100901
356400100901         // - F14=Gestione tabella scheda clienti x scambio dati
356500100901 1         WHEN  dsp_aid = c_F14;
356600100901           // Per annullamento opzione non ammmessa
356700100901           if vscopz='4'  ;
356800100901            ErrGenerico=*on   ;
356900100901            ErrMessage =*on   ;
357000100901            v1cmsg=msg(36)  ;
357100100901           else  ;
357200100901           exsr Callpgm3R       ;
357300100901           endif  ;
357400100909
357500100830 x1      // Invio
357600100830           OTHER;
357700100830
357800100924           // non controllo record per opzione '4'
357900100924 3         if vscopz<>'4'   ;
358000100830           exsr contrRIGA ;
358100100924           endif   ;
358200100830
358300100830 2         if ErrGenerico=*Off   ;
358400100830
358500100830           //  Aggiornamento record di UNGIO
358600100830 2         if   dsp_aid = c_F06  and vscopz<>'5';
358700100831            exsr   ScriviUNGIO ;
358800100831            $Video='S2'   ;
358900100831            Emismod1=*on  ;
359000100830
359100100830           //  Ricarico i dati del record nel sfl aggiornati per opz "2"
359200100830 3         if vscopz='2'   ;
359300100830           exsr CarRECORD  ;
359400100830 x3        else  ;
359500100830           // scrivo la dicitura "ANNULLATO"
359600100830           clear  vscnot;
359700100830           vscnot='ANNULLATO  ' ;
359800100830           vshann='S' ;
359900100830 2         endif   ;
360000100830
360100100830         protRiga=*on   ;
360200100830 2       endif   ;
360300100830 2       endif   ;
360400100830
360500100830 1       ENDSL;
360600100830
360700100830 0     enddo   ;
360800100830
360900100830       ENDSR;
361000100830
361100101001       //--------------------------------------------------------------
361200101001       //?Gestione videata 04 - inserimento multiplo
361300101001       //--------------------------------------------------------------
361400101001       BEGSR Gesd04;
361500101001
361600101001         v3dmod='INSER.MULTIPLO'  ;
361700101001
361800101001         // pulisco i campi
361900101001         clear vscmag3  ;
362000101001         clear vsdmag   ;
362100101001         clear vscqta   ;
362200101001         clear vsctip   ;
362300101001         clear vsdtip3  ;
362400101001         clear vsctru   ;
362500101001         clear vsctir   ;
362600101001         clear vsdtir   ;
362700101001         clear vscksc   ;
362800101001         clear vsdksc3  ;
362900101001         clear vscdoc   ;
363000101001         clear vscddo   ;
363100101001         clear vscdtm   ;
363200101008         clear v4cncl   ;
363300101008         clear v4cpkg   ;
363400101001         clear v4cinse  ;
363500101001
363600101001 0       dow    $video='D4'   ;
363700101001
363800101001         // Emissione Testata
363900101001         if  errMessage  = *off;
364000101001           write  SDUOT01;
364100101001         endif;
364200101001
364300101001         // Emissione videata
364400101001         exfmt  SDUOD04;
364500101001
364600101001         reset errMessage;
364700101001         reset errGenerico;
364800101001         clear V1cmsg    ;
364900101001         clear wtmag     ;
365000101001         errMAG=*off     ;
365100101001         errTIP=*off     ;
365200101001         errQTA=*off     ;
365300101001         errTRU=*off     ;
365400101001         errTIR=*off     ;
365500101001         errKSC=*off     ;
365600101001         errDTM=*off     ;
365700101001         errDDO=*off     ;
365800101012         errNCL=*off     ;
365900101001         errINSE=*off     ;
366000101001
366100101001 1       SELECT;
366200101001
366300101001         // - F12=Ritorno
366400101001 1         WHEN  dsp_aid = c_F12;
366500101001            $Video='S2'   ;
366600101001            Emismod1=*on  ;
366700101001            leavesr  ;
366800101001
366900101001 x1      // Invio
367000101001           OTHER;
367100101001
367200101001           exsr contrRIGA ;
367300101001
367400101001 2         if ErrGenerico=*Off   ;
367500101001
367600101001           //  Aggiornamento record di UNGIO
367700101001 3         if   dsp_aid = c_F06 ;
367800101001            $Video='S2'   ;
367900101001            exsr   Scrivirighe ;
368000101001            Emismod1=*on  ;
368100101001
368200101001         protRiga=*on   ;
368300101001 3       endif   ;
368400101001 2       endif   ;
368500101001
368600101001 1       ENDSL;
368700101001
368800101001 0     enddo   ;
368900101001
369000101001       ENDSR;
369100101001
369200101001       //--------------------------------------------------------------
369300101001       //?Per inrimento multiplo scrivo righe nel sfl
369400101001       //--------------------------------------------------------------
369500101001       BEGSR Scrivirighe  ;
369600101001
369700101001       // salvo i campi per la scrittura del sfl
369800101001       if vscmag3>*zeros ;
369900101001       v4cmag=%int(vscmag3)  ;
370000101001       endif  ;
370100101001       v4cqta=vscqta  ;
370200101001       v4cqta=vscqta  ;
370300101001       v4ctip=vsctip  ;
370400101001       v4ctru=vsctru  ;
370500101001       v4ctir=vsctir  ;
370600101001       v4cksc=vscksc  ;
370700101001       v4dksc=vsdksc3 ;
370800101001       v4cdtm=vscdtm  ;
370900101001       v4cdoc=vscdoc  ;
371000101001       v4cddo=vscddo  ;
371100101001
371200101001
371300101001       // Dal primo vuoto in poi mi posizione e cerco il primo record libero
371400101001       s02nrr=Primovuoto  ;
371500101001
371600101001       Indx= 1 ;
371700101001       dow Indx<=v4cinse  ;
371800101001
371900101001       Lib='0'  ;
372000101001       dow Lib='0'  ;
372100101001        chain s02nrr sduos02 ;
372200101001        if %found  and (vscmag<>0 or vscdes<>*blanks  or vscksc<>0 or
372300101001         vsdksc<>*blanks  or vscnot<>*blanks or (vsctip<>*zeros and vsctip
372400101001         <>*blanks ) or vscmat<>*blanks) ;
372500101001         exsr Aggiosfl  ;
372600101001         s02nrr=s02nrr+1  ;
372700101001        else  ;
372800101001         Lib='1'  ;
372900101001        endif  ;
373000101001       enddo  ;
373100101001
373200101001       if not %found ;
373300101001       savnrr=s02nrr ;
373400101001       s02nrr=s02nrr-1 ;
373500101001       exsr RigheVuote  ;
373600101001       s02nrr=savnrr  ;
373700101001        chain s02nrr sduos02 ;
373800101001       endif  ;
373900101001       vscmag=v4cmag  ;
374000101001       vscqta=v4cqta  ;
374100101001       vscqta=v4cqta  ;
374200101001       vsctip=v4ctip  ;
374300101001       vsctru=v4ctru  ;
374400101001       vsctir=v4ctir  ;
374500101001       vscksc=v4cksc  ;
374600101001       vsdksc=v4dksc  ;
374700101001       vscdtm=v4cdtm  ;
374800101001       vscdoc=v4cdoc  ;
374900101001       vscddo=v4cddo  ;
375000101008       vscncl=v4cncl  ;
375100101008       vscpkg=v4cpkg  ;
375200101001
375300101001       exsr Aggiosfl  ;
375400101001
375500101001       s02nrr=s02nrr+1  ;
375600101001       indx=indx+1  ;
375700101001       enddo  ;
375800101001
375900101001       c02csr=s02nrr  ;
376000101001
376100101001       ENDSR;
376200080206       //--------------------------------------------------------------
376300080206       //?Operazioni finali.
376400080206       //--------------------------------------------------------------
376500080206       BEGSR RoutEnd;
376600080627
376700080704         // Chiusura pgm   ;
376800080704         clear tibs02ds  ;
376900080704         t02tla='C'      ;
377000080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
377100100903
377200100903         // Sblocco data area per pgm manut matricole
377300100907         if (§sdv0mmat='S'  and savdatblo=0) or
377400100907            (§sdv0conf='S'  and savdatblo=0) ;
377500100903         in *lock  tnsduo ;
377600100903         clear tnsduo   ;
377700100903         out tnsduo  ;
377800100903         endif  ;
377900080704
378000100920         if %open(fnlv22p) ;
378100100920         close    fnlv22p  ;
378200100920         endif  ;
378300100920
378400080206         *inLR = *on;
378500080704
378600080206         return;
378700080206
378800080206       ENDSR;
378900080206
379000080206      /end-free
379100100920     c*--------------------------------------------------------------
379200100920     c* videata richeista stampanti e ovrdbf
379300100920     c*--------------------------------------------------------------
379400100920     c     RICHSTAMPAN   BEGSR
379500140124     c* verifico la key della tabella
379600100920     C                   CLEAR                   TRUL90DS
379700100920      *
379800100920     C                   MOVEL     'S'           d90RSE
379900100920     C                   CALL      'TRUL90R'
380000100920     C                   PARM                    KPJBA
380100100920     C                   PARM                    trul90DS
380200140124     C                   PARM      'SED'         k_key             3
380300100920      *
380400100920     C* F3 - FINE
380500100920     c                   if        d90f3<>'1'
380600100920     c                   eval      comman=ovrp(1)
380700100920     c                   eval      lung=91
380800100920     c                   eval      %subst(comman:28:10)=d90pre
380900100920      *
381000100920     C*
381100100920     C                   CALL      'QCMDEXC'                            91
381200100920     C                   PARM                    COMMAN
381300100920     C                   PARM                    LUNG
381400100920      *
381500100920     C                   if        not *in91
381600100920     c                   open      fnlv22p
381700100920     c                   endif
381800100920     c                   endif
381900100920      *
382000100920     c                   ENDSR
382100080206
382200080206       //--------------------------------------------------------------
382300080206       //?Schiere a tempo di compilazione.
382400080206       //--------------------------------------------------------------
382500080206
382600080410** - MSG ------------------------------------------------------------------ -*
382700100316Utente errato                                                                   01
382800100316Data Errata                                                                     02
382900100316Magazzino Destinazione Errato                                                   03
383000100316Magazzino Correlato Errato                                                      04
383100100708Tipo unità errato                                                               05
383200100826Tipo / Matricola inesistente in anagrafica                                      06
383300100708Matricola inesistente in anagrafica                                             07
383400100708Matricola Esistente in anagrafica   ma NON univoca                              08
383500100824Scorrimento non più consentito                                                  09
383600100826L'indicazione dell'ufficio è obbligatoria                                       10
383700140124Per magazzino CED  o TLC possibile solo l'opzione "INVII"                       11
383800100908Indicare anche dtaImmissione o dtaConf. o dati Da Confermare o unità da Rendere 12
383900100826La matricola NON ci vuole per tipo unità "99"                                   13
384000140207Tipo unità "99" non accettato per "FOR" rientri fornitori o sede                14
384100101112La quantità maggiore di 1 è da immettere solo per tipo unità 99                 15
384200100826Aumento/Sostituzione unità indicabile non per tipo "99"                         16
384300100826L'unità di RESO è indicabile solo se prevista la sostituizione dell'unità       17
384400100826Tipo unità di reso errata                                                       18
384500100826Descrizione obbligatoria                                                        19
384600100930Per tipi"48 e 49"indicare Cliente destinatario unità o digita"SCORTA" nelleNOTE 20
384700100826Codice cliente errato                                                           21
384800100924La data conferma movimento non può essere maggiore di 3 mesi circa...           22
384900130321Esiste già un invio/rientro giornaliero da confermare x questo tipo/matricola   23
385000100826Tipo/matricola già inseriti in questa pagina di lavoro!!                        24
385100100830Modifica/annull. al momento non eseguibili: record vincolato da altro lavoro    25
385200100830Opzione non ammessa                                                             26
385300100831Nessun dato da visualizzare                                                     27
385400100831Indicare se Includere o Escludere il tipo unità indicato                        28
385500100831Impossibile annullare: record già confermato                                    29
385600121130Impostare data conferma movimenti                                               30
385700121130Data conferma non inferiore a oggi                                              31
385800100831In fase di conferma movimenti obbligatorio inserire la matricola                32
385900100909Numero e data documento obbligatori se RIENTRO da fornitore                     33
386000100909Per interrogare i movimenti è necessaria la matricola                           34
386100100923Matricola errata perchè di unità diversa da quella immessa e NON è modificabile 35
386200100901Tasto funzionale non ammesso in Annullamento                                    36
386300100907Opz non utilizzabile:pgm "                    "  in uso da xxxxxxxxxx il xx/xx  37
386400100903Magazzino di Destinazione e Correlato non possone essere uguali                 38
386500100908Indicare se invio o rientro da fornitore o filiale I/R/F                        39
386600100908Data AL non può essere minore data DAL                                          40
386700100908Immettere o i dati da confermare o la data conferma dal/al                      41
386800100920Il magazzino DESTINAZIONE deve essere XXX                                       42
386900101006Per opzione"9"-->Stampa Etichetta,premere tasto F10 e selezionare la stampante  43
387000100923Se immessa la matricola di reso immettere anche il TIPO unità di reso           44
387100101005Mag.Ubicazione matricola di RESO  non  uguale al mag.di INVIO. Enter forza      45
387200100923Tipo invio / rientro obbligatorio per "FOR"                                     46
387300100927Se Esclusione di tipo unità, non indicare la matricola                          47
387400100930Tasto funz. F07 abilitato solo per la ricerca matricole di RESO: posizionarsi!! 48
387500100930Matricola con tipo unità diverso da quello indicato                             49
387600101001Immettere in numero degli inserimenti da effettuare                             50
387700101001Per inserimento multiplo, immettere almeno un dato                              51
387800101012F8-Materiale preparato ammesso solo per INVII                                   52
387900120525Per opzioni: P=Materiale preparato o B=blocca stampa bolla, premere F8          53
388000101012Mancano colli e peso per il magazzino destinazione indicato                     54
388100111024Se unità in proprietà alla 923 magazzino destinazione non può essere "990 980"  55
388200110404Immettere "S" = Si per indicare il guasto fisico o lasciare vuoto               56
388300110412Nuova matricola errata perchè già esistente !!                                  57
388400110412Immettere una matricola di massino 15 caratteri                                 58
388500110610Campo note obbligatorio se guasto fisico                                        59
388600120524La stampa bolla non viene eseguita per rientro fornitore diretto in filiale     60
388700120524F15 non ammesso: il magazzino non prevede la stampa                             61
388800120525Impossibile bloccare la stampa bolla: già stampata!!                            62
388900121130Data conferma movimento no > di 3 mesi                                          63
389000121203Immettere anche data DAL per richiesta movimenti CONFERMATI                     64
389100121203Massimo un mese e possibile richiedere per i movimenti CONFERMATI               65
389200121203Impossibile modificare la data a movimenti già confermati                       66
389300130116Il magazzino di destinazione non corrisponde alla ditta manutenzione nell'anagr.67
389400130626Se unità NON in proprietà alla 923 magazzino destinazione non può essere "923"  68
389500140207La conferma dei movimenti può essere fatta solo da un utente EDP                69
389600100920**
389700100920OVRPRTF FILE(FNLV22P) OUTQ(xxxxxxxxxx) FORMTYPE('ETCH_EDP') USRDTA('Unità EDP') SHARE(*YES)
