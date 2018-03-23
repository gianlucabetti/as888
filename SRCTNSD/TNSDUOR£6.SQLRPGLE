000100080206      //--------------------------------------------------------------
000200100312      //?TNSDUOR - Gestione/interrogazione inVII GIORNALIERI UNITÀ EDP
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100100312     fTNSDUOD   cf   e             workstn indds(IndDspF)
001200080206     f                                     infds(InfDspF)
001300100315     f                                     sfile(SDUOS02 : S02nrr)
001400100827     FUNgio00F  uF a E             DISK    RENAME(UNgio000:UNgioFIS)
001500121024     FUNgio01l  iF   E           K DISK    infds(gio01)
001600100826     FUNgio04L  IF   E           K DISK    RENAME(UNgio000:UNgio4) infds(gio04)
001700100312     FUNANA01L  IF   E           K DISK
001800100930     FUNANA02L  uF   E           K DISK    RENAME(UNANA000:UNANA2)
001900100312     funana07l  if   e           k disk    rename(unana000:unana7)
002000100312     FAZORG21l  IF   E           K DISK
002100100312     fazorg01l  if   e           k disk
002200100312     fkpprf01l  if   e           k disk
002300100826     fcnaco00f  if   e           k disk
002400100922     fundot01l  uf a e           k disk
002500100920     FFNLV22P   O    F  132        PRINTER usropn
002600100312
002700080206      //---------------------------------------------------------------
002800100824      // - Numero di record in ciascuna videata di subfile
002900100827     d c_SflPag        c                   const(16)
003000100827     d W_SflPag        s              3  0 inz
003100100824     d PAgric          s              3  0 inz
003200100824     d totrig          s              5  0 inz
003300100824
003400080207      // - Tasti funzionali a video
003500080207     d c_F01           c                   const(x'31')
003600080207     d c_F02           c                   const(x'32')
003700080207     d c_F03           c                   const(x'33')
003800080207     d c_F05           c                   const(x'35')
003900080207     d c_F06           c                   const(x'36')
004000080207     d c_F07           c                   const(x'37')
004100080207     d c_F08           c                   const(x'38')
004200080207     d c_F09           c                   const(x'39')
004300080207     d c_F10           c                   const(x'3A')
004400101018     d c_F11           c                   const(x'3B')
004500080207     d c_F12           c                   const(x'3C')
004600080207     d c_F13           c                   const(x'B1')
004700080207     d c_F14           c                   const(x'B2')
004800080207     d c_F15           c                   const(x'B3')
004900080207     d c_F16           c                   const(x'B4')
005000080207     d c_F17           c                   const(x'B5')
005100080207     d c_F18           c                   const(x'B6')
005200080207     d c_F19           c                   const(x'B7')
005300080207     d c_F20           c                   const(x'B8')
005400080207     d c_F21           c                   const(x'B9')
005500080207     d c_F22           c                   const(x'BA')
005600080207     d c_F23           c                   const(x'BB')
005700080207     d c_F24           c                   const(x'BC')
005800080207     d c_Enter         c                   const(x'F1')
005900080207     d c_RollDown      c                   const(x'F4')
006000080207     d c_RollUp        c                   const(x'F5')
006100080214
006200080214      // - Attributi di visualizzazione
006300080215      //  -  DspAtr() - Normale
006400080214     d C_dspatr_Norm   c                   const(x'20')
006500080215      //  -  DspAtr(RI)
006600080214     d C_dspatr_RI     c                   const(x'21')
006700080215      //  -  DspAtr(ND)
006800080214     d C_dspatr_ND     c                   const(x'27')
006900080215      //  -  DspAtr(BL) / Color(Red)
007000080214     d C_dspatr_BL     c                   const(x'28')
007100080206
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione schiere.
007400080206      //---------------------------------------------------------------
007500080206
007600080206      // - Messaggi di errore
007700140207     d MSG             s             78    dim(69) ctdata perrcd(1)
007800100920     d OVRP            s             91    dim(1) ctdata perrcd(1)
007900100920     d
008000100928     d TIPMAT          s             17    dim(1999)
008100101012     d SiCoPe          s             10  0 dim(1999)
008200100830     d WOPZ            s              1    dim(15)
008300080206      //---------------------------------------------------------------
008400080206      //?Definizione aree dati.
008500080206      //---------------------------------------------------------------
008600080206      // - Dati utente
008700080206     d §AzUte        e ds                  extname(AZUTE00F)
008800080206     d                                     dtaara
008900080206     d §DatiUte      e ds                  extname(dDatiUte)
009000080206     d                                     dtaara
009100080206
009200100903     d TNSDUO        e ds                  extname(TNSDUODS)
009300100903     d                                     dtaara
009400080206      //---------------------------------------------------------------
009500080206      //?Definizione strutture dati.
009600080206      //---------------------------------------------------------------
009700121024     D gio01           DS
009800100827     D  gio1NRR              397    400B 0
009900100826     D gio04           DS
010000100827     D  gio4NRR              397    400B 0
010100100826
010200080206
010300080206      // - Status
010400080206     d Psds           sds
010500080206     d   SDSpgm          *proc
010600080206
010700080206      // - InfDS
010800080206     d InfDspF         ds
010900080207     d  dsp_aid              369    369a                                        AID byte
011000100923     d  cursor               370    371a                                        AID byte
011100080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
011200080207     d  min_nrr              378    379i 0                                      Subfile min rrn
011300080207     d  num_rcds             380    381i 0                                      Subfile num rcds
011400080206
011500080206      // - Indicatori su DspF
011600080208     d IndDspF         ds
011700080206        // - Indicatori di gestione del subfile
011800080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011900080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
012000080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
012100080206     d  SflEnd                        1n   overlay(IndDspF : 33)
012200080206        // - Indicatori di errore
012300080206     d  errMessage                    1n   overlay(IndDspF : 28)
012400080606     d  errGenerico                   1n   overlay(IndDspF : 99)
012500080627        // - Indicatori vari
012600100830     d  ModINT                        1n   overlay(IndDspF : 01)
012700100830     d  EmisMod1                      1n   overlay(IndDspF : 02)
012800100831     d  ImmMat                        1n   overlay(IndDspF : 03)
012900081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
013000100908     d  ModReso                       1n   overlay(IndDspF : 05)
013100100831     d  ImmCMOV                       1n   overlay(IndDspF : 06)
013200100831     d  ImmGES                        1n   overlay(IndDspF : 07)
013300100831     d  EmisCONF                      1n   overlay(IndDspF : 10)
013400100826     d  ProtperRIE                    1n   overlay(IndDspF : 11)
013500100826     d  ProtperINV                    1n   overlay(IndDspF : 12)
013600100830     d  protOPZ                       1n   overlay(IndDspF : 13)
013700100826     d  ProtINVR                      1n   overlay(IndDspF : 14)
013800080627     d  protriga                      1n   overlay(IndDspF : 16)
013900100830     d  recANN                        1n   overlay(IndDspF : 17)
014000100831     d  ProtMAT                       1n   overlay(IndDspF : 18)
014100101006     d  RevMAG                        1n   overlay(IndDspF : 19)
014200110404     d  ProtperFOR                    1n   overlay(IndDspF : 20)
014300120524     d  NostampaBO                    1n   overlay(IndDspF : 21)
014400100315     d
014500100315     d  ErrMAG                        1n   overlay(IndDspF : 40)
014600100315     d  ErrMGC                        1n   overlay(IndDspF : 41)
014700100315     d  ErrTIP                        1n   overlay(IndDspF : 42)
014800100315     d  ErrMAT                        1n   overlay(IndDspF : 43)
014900100315     d  ErrUTE                        1n   overlay(IndDspF : 44)
015000100315     d  ErrDIM                        1n   overlay(IndDspF : 45)
015100100826     d  ErrCUF                        1n   overlay(IndDspF : 46)
015200100826     d  ErrQTA                        1n   overlay(IndDspF : 47)
015300100826     d  ErrTRU                        1n   overlay(IndDspF : 48)
015400100826     d  ErrTIR                        1n   overlay(IndDspF : 49)
015500100826     d  ErrDES                        1n   overlay(IndDspF : 50)
015600100826     d  ErrKSC                        1n   overlay(IndDspF : 51)
015700100826     d  ErrDTM                        1n   overlay(IndDspF : 52)
015800100831     d  ErrIET                        1n   overlay(IndDspF : 53)
015900100903     d  ErrINVR                       1n   overlay(IndDspF : 54)
016000100908     d  ErrDTMA                       1n   overlay(IndDspF : 55)
016100100909     d  ErrDDO                        1n   overlay(IndDspF : 56)
016200100923     d  ErrMATR                       1n   overlay(IndDspF : 57)
016300101001     d  ErrINSE                       1n   overlay(IndDspF : 58)
016400101012     d  ErrNCL                        1n   overlay(IndDspF : 59)
016500110412     d  ErrNOT                        1n   overlay(IndDspF : 60)
016600080206
016700080206      // - Parametri ricevuti
016800080206     d KPJBA         e ds
016900100923     d og143         e ds
017000080206
017100080206      // - Reperimento dati utente
017200080206     d TIBS34ds      e ds
017300080206     d dUte01        e ds
017400080702     d DLAT          e ds
017500080702
017600100826     d DUNT          e ds
017700100901     d DSUNIT        e ds
017800100827     d TNSDV0DS      e ds
017900100920       // DS richeista stampanti
018000100920     d trul90ds      e ds
018100100920       // Ds stampa etichette
018200100920     d dsetiunit     e ds
018300100930       // Ds controllo magazzino
018400100930     d tnsdmagds     e ds
018500110401       // Ds del campo ungflo
018600110404     d dgio01        e ds
018700080206
018800080206      // - Ricerca/Controllo tabelle
018900080206     d TIBS02ds      e ds                  inz
019000100907
019100100907     d dsungio       e ds                  extname(ungio00f)
019200100924     d dsunana       e ds                  extname(unana00f) prefix(UIM:3)
019300100316
019400100316     d wlbdat          ds                  inz
019500100316     d  g02dat                 1      8  0
019600100316     d  g02inv                 9     16  0
019700100316     d  g02err                17     17
019800100316     d  g02tgi                18     22  0
019900121130     d
020000121130     d tnsduods_1      ds
020100121203     d   o05gma_dtm            1      6s 0
020200121203     d   o05amg_dtm            7     14s 0
020300121203     d   o05F06               15     15
020400121203     d   o05F12               16     16
020500121203     d   datapiu200           17     24s 0
020600121203     d   I05sys               25     32s 0
020700121130
020800100826
020900100827      //  ricerca alfabetica
021000100827     d  xpardut        s             30
021100100827     d  xparkut        s              1  0
021200100827     d  xparrag        s             48
021300100827     d  xparkcc        s              4  0
021400100827     d  xparsta        s              1  0
021500100827     d  xparflr        s             90
021600100827     d  xpardit        s              3
021700100827     d  xparnum        s              2  0
021800100827     d  xparkcm        s             80
021900100827     d  xparksm        s            140
022000100827     d  xparkdm        s             60
022100100827     d  xparesci       s              1
022200100827     d  xparerr        s              2
022300100827     d  xpariva        s             16
022400100827     d  xparcdf        s             16
022500100826
022600100827      // ricerca filiale su organigramma
022700100827     d   pInFIL        S              3
022800100827     d   pInFAG        S              1
022900100827     d   pInDES        S             25
023000100827     d   pInDIT        S              3
023100100316
023200080206      //---------------------------------------------------------------
023300080206      //?Definizione variabili globali.
023400080206      //---------------------------------------------------------------
023500080206
023600080206      // - Flags booleani
023700080208     d $Fine           s               n   inz(*off)
023800100312     d $InzS02         s               n   inz(*on)
023900100312     d $Inzd01         s               n   inz(*on)
024000080206
024100100312     d $Video          s              2    inz('D1')
024200100315     d S02nrr          s              4  0 inz
024300081006     d XX              s              3  0 inz
024400080627     d Indx            s              3  0 inz
024500080703     d Primavolta      s              1    inz
024600081126     d yy              s              3  0 inz
024700121203     d wdiffe          s              8  0 inz
024800121203     d w1cdim          s              8  0 inz
024900100831     d w1cdtm          s              8  0 inz
025000100908     d w1cdtmd         s              8  0 inz
025100100908     d w1cdtma         s              8  0 inz
025200121203     d w1ctgid         s                   like(g02tgi)
025300121203     d w1ctgia         s                   like(g02tgi)
025400100831     d Dataoggi        s              8  0 inz
025500100903     d savDatBlo       s              8  0 inz
025600121203     d w05dtm          s              8  0 inz
025700121203     d w05amg_dtm      s              8  0 inz
025800100908     d wazorg          s              3    Inz('   ')
025900100316     d wmag            s              3
026000100316     d wdmag           s             30
026100100909     d desmag          s             12
026200100708     d wtip            s              2
026300100824     d wOK             s              1
026400100824     d wtMAG           s              1
026500100830     d wSceltaOPZ      s              1
026600100708     d wdtip           s             30
026700100708     d qtatem          s              4  0
026800100823     d w0060           s              6  0
026900100903     d w006A           s              6
027000100823     d wpag            s              6  0
027100100823     d wresto          s              3  0
027200100827     d wTipMAT         s             17
027300101012     d wSIcope         s             10  0
027400100827     d vscslr          s              1  0
027500101012     d SiColliPeso     s              1
027600101001     d savnrr          s                   like(s02nrr)
027700101001     d ktip            s                   like(ungtip)
027800100825     d kinvr           s                   like(unginvr)
027900100823     d kmag            s                   like(ungmag)
028000100823     d kmgc            s                   like(ungmgc)
028100110412     d kmat            s                   like(ungmat)
028200100831     d SavMat          s                   like(ungmat)
028300101005     d SavMatr         s                   like(ungmat)
028400101001     d Primovuoto      s                   like(s02nrr)
028500101012     d sqDTM           s                   like(ungdtm)
028600101012     d sqNCL           s                   like(ungncl)
028700101012     d sqpkg           s                   like(ungpkg)
028800110124     d sqnrr           s              9  0
028900110421     d sqcont          s              7  0
029000100920     d comman          s            100
029100100920     d lung            s             15  5
029200100920     d wrichtrul90     s              1    inz(' ')
029300100921     d Recvuoto        s              1    inz(' ')
029400110421     d Wtrov           s              1    inz(' ')
029500101001     d Lib             s              1    inz('0')
029600121130     d savinkf         s              1    inz
029700101012     d wrkgetlista     s           4096    varying
029800101012     d invdtm          s              8  0 inz
029900101001     d
030000101001     d v4cmag          s                   like(vscmag)
030100101001     d v4cqta          s                   like(vscqta)
030200101001     d v4ctip          s                   like(vsctip)
030300101001     d v4ctir          s                   like(vsctir)
030400101001     d v4ctru          s                   like(vsctru)
030500101001     d v4cksc          s                   like(vscksc)
030600101001     d v4dksc          s                   like(vsdksc)
030700101001     d v4cddo          s                   like(vscddo)
030800101001     d v4cdoc          s                   like(vscdoc)
030900101001     d v4cdtm          s                   like(vscdtm)
031000080414
031100100315     d Dataeur         s               d   datfmt(*eur)
031200100315     d Dataiso         s               d   datfmt(*iso)
031300080605     d Dataymd         s               d   datfmt(*ymd)
031400080605     d Datadmy         s               d   datfmt(*dmy)
031500100901     d Datasys         s               d   inz(*sys)
031600100901     d timeiso         s               t   timfmt(*iso)
031700080604     d
031800080605     d                 DS
031900080605     d  AA                     1      4  0
032000080605     d  MM                     5      6  0
032100080605     d  GG                     7      8  0
032200080605     d DATA                    1      8  0
032300081009     d
032400120524     d Cf15BLO         C                   const('F15=Blocca la stampa della -
032500120524     d                                     bolla')
032600120524     d Cf15SBLO        C                   const('F15=SBLOCCA la stampa della -
032700120524     d                                     bolla')
032800140424       // - Gestione scambio dati principali
032900140424     d tnsdunr         pr                  extpgm('TNSDUNR')
033000140424     d   kpjba                             likeds(kpjba)
033100080206      //---------------------------------------------------------------
033200080206      //?Definizione procedure usate.
033300080206      //---------------------------------------------------------------
033400080414      /copy gaitrasrc/srcprotopr,tibs02r
033500080414      /copy gaitrasrc/srcprotopr,tibs34r
033600100316      /copy gaitrasrc/srcprotopr,xsrda8
033700100826      /copy gaitrasrc/srcprotopr,xalfa3br
033800100909      /copy gaitrasrc/srcprotopr,tnsd20r
033900100901      /copy gaitrasrc/srcprotopr,tnsdU1r
034000100909      /copy gaitrasrc/srcprotopr,tnsdU4r
034100100909      /copy gaitrasrc/srcprotopr,tnsdU5r
034200100930      /copy gaitrasrc/srcprotopr,tnsdMAGr
034300100920      /copy gaitrasrc/srcprotopr,qcmdexc
034400100920      /copy gaitrasrc/srcprotopr,fnlvuezs
034500100920      /copy gaitrasrc/srcprotopr,trul90r
034600121130      /copy gaitrasrc/srcprotopr,tnsduor1
034700080206
034800080206      //---------------------------------------------------------------
034900080206      //?Riepilogo indicatori.
035000080206      //---------------------------------------------------------------
035100080207      // - Comuni
035200080207      // 28    : Emissione messaggio di errore a video
035300080207      // - C01/S01
035400080207      // 30    : Condiziona SFLDSP    (*not)
035500080207      // 31    : Condiziona SFLDSPCTL (*not)
035600080207      // 30+31 : Condiziona SFLCLR
035700100824      // 32    : Condiziona SFLNXTCHG
035800080207      // 50    : Errore: Opzione errata
035900080207      // - D01
036000080207      // - Comuni
036100080207      // 99    : Generico di Errore
036200080206      //---------------------------------------------------------------
036300080206
036400080206      //---------------------------------------------------------------
036500080206      //?M A I N - L I N E
036600080206      //---------------------------------------------------------------
036700080206
036800080627     c     *Entry        plist
036900080206     c                   parm                    KPJBA
037000100827     c                   movel     kpjbu         tnsdv0ds
037100080702
037200080206      /free
037300080206
037400080206       // Operazioni iniziali
037500080206       exsr RoutInz;
037600081215
037700080206       // Gestione video
037800081215 3     DOW $Fine = *off;
037900081215 4       select;
038000080530
038100080530         // Videata di selezioni
038200100312           when $Video = 'D1';
038300100312             exsr GesD01;
038400080530
038500100830         // Videata del sfl per inserimento/visualizzazione
038600100312           when $Video = 'S2';
038700100312             exsr GesS02;
038800100830
038900080702           other   ;
039000080206             $Fine = *on;
039100081215 4       endsl;
039200081215 3     ENDDO;
039300080206
039400080206       // Operazioni finali
039500080206       exsr RoutEnd;
039600080206
039700080206       //--------------------------------------------------------------
039800080206       //?Operazioni iniziali.
039900080206       //--------------------------------------------------------------
040000080206       BEGSR RoutInz;
040100100312       $inzs02=*on;
040200100312       $inzd01=*on;
040300100831       ImmMAT=*off  ;
040400100908       ModReso=*off  ;
040500100831       ImmCMOV=*off  ;
040600100831       ImmGES =*off  ;
040700080206
040800080703       // Solo la prima volta
040900080703 1     if primavolta=' '   ;
041000080703
041100080206         // Reperimento dati job
041200080206         exsr DatiJob;
041300100824         vscpgm='TNSDUOR'         ;
041400081003
041500081003         // Se sono EDP accendo indicatore
041600081003         if %subst(knmus:1:3)<>'EDP'    ;
041700081003         UTENonEDP=*on   ;
041800100831         endif      ;
041900100826
042000100826       // Udate + 10
042100100826       dataiso=datasys ;
042200100831       dataeur=dataiso ;
042300100903       Dataoggi=%dec(dataeur) ;
042400100924       dataiso=dataiso+ %days(200);
042500100924       datapiu200=%dec(dataiso);
042600100831 1     endif      ;
042700100827
042800100830       clear vsdopz  ;
042900121130       clear vsdopz2 ;
043000100830       clear wopz   ;
043100100830       emisMod1=*on   ;
043200100831
043300100831 1     SELECT  ;
043400100830       // SE INTERROGAZIONE
043500100831 1     when §sdv0iNT='S'   ;
043600100830        modINT=*on   ;
043700100830        protriga=*on   ;
043800100830        v1dmod='    INTERROGAZIONE     '         ;
043900120525        vsdopz='5=visualizza   8=Movimenti' ;
044000100830        wopz(1)='5' ;
044100100909        wopz(2)='8' ;
044200100830
044300100831       // Se  IMMISSIONE/MANUTENZIONE
044400100831 1     WHEN §sdv0imm='S'   ;
044500100831        ImmGES =*on   ;
044600100827        protriga=*on   ;
044700100827        v1dmod='IMMISSIONE/MANUTENZIONE'     ;
044800120525        vsdopz='2=Manut 4=Annulla 5=Visual 9=Etich P=Prepara B=BlocSTA' ;
044900121130        vsdopz2='D=ModDataConf'  ;
045000100830        wopz(1)='2' ;
045100100830        wopz(2)='4' ;
045200100830        wopz(3)='5' ;
045300100920        wopz(4)='9' ;
045400101006        wopz(5)='P' ;
045500120525        wopz(6)='B' ;
045600121130        wopz(7)='D' ;
045700100831
045800100831       // Se  IMMISSIONE/MANUTENZIONE  MATRICOLE
045900100831 1     WHEN §sdv0MMAT='S'   ;
046000100831        protriga=*on   ;
046100100831        ImmMAT  =*on   ;
046200100831        v1dmod='Immissione Cd.MATRICOLE'    ;
046300120525        vsdopz='2=Manutenz. 4=Annulla  5=Visualizza  9=Etich'  ;
046400100831        wopz(1)='2' ;
046500100831        wopz(2)='4' ;
046600100831        wopz(3)='5' ;
046700101006        wopz(4)='9' ;
046800100903        // Scrivo data area per pgm in uso
046900100903        clear savdatblo ;
047000100903         in *lock   tnsduo ;
047100100903         if §sduodat=0 ;
047200100903         §sduodat=%dec(datasys) ;
047300100903         §sduoute=knmus         ;
047400100907         §sduopgm='IMMISSIONE Matricole' ;
047500100903         else ;
047600100903         savdatblo=§sduodat  ;
047700100903         endif;
047800100903         out tnsduo  ;
047900100831
048000100831       // Se  gestione RESI
048100100831 1     WHEN §sdv0RESi='S'   ;
048200100831        protriga=*on   ;
048300100908        ModReso =*on   ;
048400100831        v1dmod='GESTIONE UNITÀ DI RESO '    ;
048500120525        vsdopz='2=Manutenz.  5=Visualizza'  ;
048600100831        wopz(1)='2' ;
048700100831        wopz(2)='5' ;
048800100831
048900100831       // Se  conferma Movimenti
049000100831 1     WHEN §sdv0CONF='S'   ;
049100100831        protriga=*on   ;
049200100831        ImmCMOV =*on   ;
049300100831        v1dmod='  CONFERMA  MOVIMENTI  '    ;
049400120525        vsdopz='E=ESCLUDIdaCONFERMA  2=Manut.  5=Visual.'  ;
049500100831        wopz(1)='2' ;
049600100831        wopz(2)='5' ;
049700100831        wopz(2)='E' ;
049800100907        // Scrivo data area per pgm in uso
049900100907        clear savdatblo ;
050000100907         in *lock   tnsduo ;
050100100907         if §sduodat=0 ;
050200100907         §sduodat=%dec(datasys) ;
050300100907         §sduoute=knmus         ;
050400100907         §sduopgm='CONFERMA   Movimenti' ;
050500100907         else ;
050600100907         savdatblo=§sduodat  ;
050700100907         endif;
050800100907         out tnsduo  ;
050900100831 1     endsl   ;
051000080703
051100080627         ENDSR;
051200080206
051300080206       //--------------------------------------------------------------
051400080206       //?Reperimento Dati del job (Utente/Operativi).
051500080206       //--------------------------------------------------------------
051600080206       BEGSR DatiJob;
051700080206
051800080206         in(E) §AzUte;
051900080206         if NOT %error;
052000080206           in(E) §DatiUte;
052100080206         endif;
052200080206         if %error or RSut = *blanks;
052300080206           clear TIBS34ds;
052400080206           tibs34r(tibs34ds);
052500080206           in §AzUte;
052600080206           in §DatiUte;
052700080206         endif;
052800080206
052900080206       ENDSR;
053000100312       //--------------------------------------------------------------
053100100312       //?Gestione viodeata 01
053200100312       //--------------------------------------------------------------
053300100312       BEGSR Gesd01;
053400100312
053500100312         // Inizializzazione videata
053600100312         if  $Inzd01 = *on;
053700100312            exsr Inzd01;
053800100312            $Inzd01  = *off;
053900100312         endif;
054000100312
054100100312
054200100312         // Emissione Testata e Piede con tasti funzionali abilitati
054300100824         if  errMessage  = *off;
054400100316           write  SDUOT01;
054500100312         endif;
054600100312
054700100312         // Emissione videata
054800100316         exfmt  SDUOD01;
054900100312
055000100312         reset errMessage;
055100100312         reset errGenerico;
055200100824         clear V1cmsg    ;
055300100315         errMAG=*off     ;
055400100315         errMGC=*off     ;
055500100315         errTIP=*off     ;
055600100315         errMAT=*off     ;
055700100315         errUTE=*off     ;
055800100315         errDIM=*off     ;
055900100826         errCUF=*off     ;
056000100909         errDTM=*off     ;
056100100909         errDTMA=*off     ;
056200100909         errIET=*off     ;
056300100923         errINVR=*off     ;
056400100312
056500100312 1       SELECT;
056600100312
056700100312         // - F3=Fine
056800100312 1         WHEN  dsp_aid = c_F03;
056900100312            $Fine = *on;
057000100312
057100100312
057200100824 x1      // Invio
057300100312           OTHER;
057400100316
057500100316           exsr CTRD01  ;
057600100316
057700100316           if ErrGenerico=*ON    ;
057800100316                 leavesr;
057900100316 2             endif;
058000100316
058100100315           $video='S2' ;
058200100825           $inzs02=*on;
058300100824
058400100312 1       ENDSL;
058500100312
058600100312       ENDSR;
058700100312
058800100316       //--------------------------------------------------------------
058900100316       //?controlli video 1
059000100316       //--------------------------------------------------------------
059100100316       BEGSR CTRD01  ;
059200100316
059300100907        in tnsduo   ;
059400100907       // Pgm non utilizzabile al momento
059500100907 1     if §sdv0resi='S' or
059600100907         (§sdv0conf='S' and savdatblo>0) or
059700100907         (§sdv0mmat='S' and savdatblo>0) or
059800100907         (§sdv0imm ='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='C')  or
059900100907         (§sdv0conf='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='I')  or
060000100907         (§sdv0mmat='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='C') ;
060100100903 2      if §sduodat>0   ;
060200100907        exsr Msg37    ;
060300100903         leavesr  ;
060400100903 2     endif   ;
060500100903 1     endif   ;
060600100907
060700140207       // la conferma può essere fatta solo da un utente EDP
060800140207       if ImmCMOV=*on and %subst(knmus:1:3)<>'EDP  '  ;
060900140207           errUTE=*on ;
061000140207           v1cmsg = Msg(69)  ;
061100140207           errGenerico=*on  ;
061200140207           errMessage=*on  ;
061300140207           leavesr  ;
061400140207       endif  ;
061500140207
061600100316       //   Profilo utente
061700100708 1     if v1cute<>*blanks   ;
061800100316       chain   v1cute  kpprf01l   ;
061900100316
062000100708 2     if not %found(kpprf01l)  ;
062100100316           errUTE=*on ;
062200100316           v1cmsg = Msg(1);
062300100316           errGenerico=*on  ;
062400100316           errMessage=*on  ;
062500100316           leavesr  ;
062600100824 2       endif   ;
062700100708 1     endif   ;
062800100316
062900100316       // Data Immissione
063000100831       clear w1cdim   ;
063100100708 1     if v1cdim>0   ;
063200100316             clear wlbdat;
063300100824             g02dat = v1cdim ;
063400100316             xsrda8(wlbdat);
063500100824 2           if g02err = '1';
063600100316               errMessage  = *on;
063700100316               errGenerico = *on;
063800100708               ErrDIM    = *on;
063900100824               V1cmsg = Msg(02);
064000100316               leavesr;
064100100824 2           endif;
064200100316
064300100316             v1cdim = g02dat;
064400100316             w1cdim = g02inv;
064500100708 1     endif   ;
064600100316
064700100826       // Ufficio obbligatorio
064800100826       if v1ccuf=*blanks   ;
064900100826               errMessage  = *on;
065000100826               errGenerico = *on;
065100100826               ErrCUF    = *on;
065200100826               V1cmsg = Msg(10);
065300100826               leavesr;
065400100826       endif  ;
065500100826
065600100923       // tipo invio obbligatorio per FOR solo in immissione
065700100923       if v1ccuf='FOR' and v1cinvr= ' ' and §sdv0imm='S' ;
065800100923               errMessage  = *on;
065900100923               errGenerico = *on;
066000100923               ErrINVR   = *on;
066100100923               V1cmsg = Msg(46);
066200100923               leavesr;
066300100923       endif  ;
066400100923       if v1ccuf=*blanks   ;
066500100923               errMessage  = *on;
066600100923               errGenerico = *on;
066700100923               ErrCUF    = *on;
066800100923               V1cmsg = Msg(10);
066900100923               leavesr;
067000100923       endif  ;
067100100826       // Per MAG possibile solo I=invio
067200140123       if v1ccuf<>'FOR' and v1cinvr<>'I' and v1cinvr<>' ' ;
067300100826               errMessage  = *on;
067400100826               errGenerico = *on;
067500100826               ErrCUF    = *on;
067600100826               V1cmsg = Msg(11);
067700100826               leavesr;
067800100827       endif  ;
067900100826
068000100826       // Immettere o la data immissione o solo i DA confermare
068100100831       //  o solo unità di reso
068200121203       if v1cdim=0 and v1creso =' '
068300121203          and v1cdtm=0 and v1cdtmd = 0 and (v1cconf=' ' or v1cconf='C');
068400100826               errMessage  = *on;
068500100826               errGenerico = *on;
068600100826               ErrDIM    = *on;
068700100826               V1cmsg = Msg(12);
068800100826               leavesr;
068900100827       endif  ;
069000100831
069100100831       // Data conferma movimento: obbligatoria in conferma
069200100831       clear w1cdtm  ;
069300100831 1     if v1cdtm=0  and §sdv0conf='S' ;
069400100831               errMessage  = *on;
069500100831               errGenerico = *on;
069600100831               ErrDTM    = *on;
069700100831               V1cmsg = Msg(30);
069800100831               leavesr;
069900100831 2     endif;
070000100908
070100100831       if v1cdtm>0  ;
070200100831             clear wlbdat;
070300100831             g02dat = v1cdtm ;
070400100831             xsrda8(wlbdat);
070500100831 2           if g02err = '1';
070600100831               errMessage  = *on;
070700100831               errGenerico = *on;
070800100831               ErrDTM    = *on;
070900100831               V1cmsg = Msg(02);
071000100831               leavesr;
071100100831 2           endif;
071200100831
071300100831       // non inferiore a oggi
071400100831       if g02inv<%dec(datasys)  ;
071500100831               errMessage  = *on;
071600100831               errGenerico = *on;
071700100831               ErrDTM    = *on;
071800100831               V1cmsg = Msg(31);
071900100831               leavesr;
072000100831       endif   ;
072100100831             v1cdtm = g02dat;
072200100831             w1cdtm = g02inv;
072300100831 1     endif   ;
072400100908
072500100908       // data conferma movimenti dal / al
072600100908       clear w1cdtmd ;
072700100908       clear w1cdtma ;
072800121203       clear w1ctgid ;
072900121203       clear w1ctgia ;
073000101112       // Incongruente se richisti  movimenti DA confermare
073100110914       if (v1cdtmd>0 or v1cdtma>0) and v1cconf='S'  and ModInt=*on  ;
073200100908               errMessage  = *on;
073300100908               errGenerico = *on;
073400100908               ErrDTM    = *on;
073500100908               V1cmsg = Msg(41);
073600100908               leavesr;
073700100908       endif   ;
073800100908
073900100908 1     if v1cdtmd >0  ;
074000100908             clear wlbdat;
074100100908             g02dat = v1cdtmd;
074200100908             xsrda8(wlbdat);
074300100908 2           if g02err = '1';
074400100908               errMessage  = *on;
074500100908               errGenerico = *on;
074600100908               ErrDTM    = *on;
074700100908               V1cmsg = Msg(02);
074800100908               leavesr;
074900100908 2           endif;
075000100908             v1cdtmd= g02dat;
075100100908             w1cdtmd= g02inv;
075200121203             w1cTGId= g02TGI;
075300100908       endif   ;
075400100831
075500100908 1     if v1cdtma >0  ;
075600100908             clear wlbdat;
075700100908             g02dat = v1cdtma;
075800100908             xsrda8(wlbdat);
075900100908 2           if g02err = '1';
076000100908               errMessage  = *on;
076100100908               errGenerico = *on;
076200100908               ErrDTMA   = *on;
076300100908               V1cmsg = Msg(02);
076400100908               leavesr;
076500100908 2           endif;
076600100908             v1cdtma= g02dat;
076700100908             w1cdtma= g02inv;
076800121203             w1cTGIA= g02TGI;
076900100908
077000100908       // Data A non minore data DAL
077100100908 2     if w1cdtma<w1cdtmd  ;
077200100908               errMessage  = *on;
077300100908               errGenerico = *on;
077400100908               ErrDTMA   = *on;
077500100908               V1cmsg = Msg(40);
077600100908               leavesr;
077700100908 2     endif  ;
077800100908 1     endif   ;
077900121203
078000121203       // Se richIESTi i confermati max i mese posso richiedere
078100121203        if v1cconf<>'S' and v1cdtmd>0  and v1cdtma>0  ;
078200121203          WDIFFE=w1ctgia-w1ctgid  ;
078300121203          if wdiffe>32  ;
078400121203               errMessage  = *on;
078500121203               errGenerico = *on;
078600121203               ErrDTMA   = *on;
078700121203               V1cmsg = Msg(65)  ;
078800121203               leavesr;
078900121203          endif  ;
079000121203        endif  ;
079100100908
079200100316       // Magazzino destinazione
079300100708 1     if v1cmag<>*blanks and v1cmag<>'000' ;
079400100316
079500100909       wtmag='D'      ;
079600100908       wazorg='E  '   ;
079700100708       wmag=v1cmag    ;
079800100824       exsr  chkmag   ;
079900100824       v1cmag=wmag    ;
080000100316
080100100708       if  ErrGenerico=*ON  ;
080200100708         ErrMag=*on  ;
080300100316         leavesr  ;
080400100708       endif    ;
080500100708       else  ;
080600100708       clear v1cmag  ;
080700100708 1     endif    ;
080800100708
080900100708       // Magazzino correlato
081000100708 1     if v1cmgc<>*blanks  and v1cmgc<>'000'   ;
081100100708
081200100909       wtmag='C'      ;
081300100908       wazorg='E  '   ;
081400100708       wmag=v1cmgc    ;
081500100708       exsr  Chkmag   ;
081600100824       v1cmgc=wmag  ;
081700100708
081800100826       if  ErrGenerico=*ON  ;
081900100708         ErrMgc=*on  ;
082000100708         leavesr  ;
082100100708       endif    ;
082200100708       else  ;
082300100708       clear v1cmgc  ;
082400100708       endif    ;
082500100316
082600100708       // Tipo unita
082700100824       clear v1dtip  ;
082800100831 1     if v1ctip<>*blanks ;
082900100708         wtip=v1ctip   ;
083000100708         exsr chktip   ;
083100100824
083200100824         v1ctip=wtip  ;
083300100824         v1dtip=wdtip  ;
083400100827 2       if Errgenerico=*on  ;
083500100826          errTIP=*on       ;
083600100827 3       if ErrMessage =*on  ;
083700100826          v1cmsg=msg(05)  ;
083800100827 3       endif    ;
083900100826          leavesr  ;
084000100827 2       endif    ;
084100100831
084200100831       // Immettere anche Includi o Escludi
084300100831 2     if v1ciet=' '  ;
084400100831          errIET=*on       ;
084500100927          ErrGenerico=*on  ;
084600100927 3        ErrMessage =*on  ;
084700100831          v1cmsg=msg(28)  ;
084800100831          leavesr  ;
084900100831 2     endif   ;
085000100831
085100100827 x1    else  ;
085200100708       clear v1ctip  ;
085300100831       clear v1ciet  ;
085400100708 1     endif    ;
085500100927       // Se esclusione di un tipo, impossibile indciare la matricola
085600100927       if v1ciet='E' and v1ctip>*zeros  and v1cmat<>*blanks  ;
085700100927         Errgenerico=*on  ;
085800100927         Errmessage =*on  ;
085900100927         Errmat=*on  ;
086000100927         v1cmsg=msg(47)  ;
086100100927         leavesr  ;
086200100927 3     endif    ;
086300100708
086400100708       // Matricola
086500100708 1     if v1cmat<>*blanks  ;
086600100708
086700100824 2     if v1ctip>*zeros   ;
086800100824       ktip=%int(v1ctip)  ;
086900100930       setll (v1cmat:ktip)  unana07l    ;
087000100930 3     if not %equal(unana07l)  ;
087100100708         Errgenerico=*on  ;
087200100708         Errmessage =*on  ;
087300100708         Errmat=*on  ;
087400100708         v1cmsg=msg(06)  ;
087500100708         leavesr  ;
087600100708 3     endif    ;
087700100708
087800100708 x2    else   ;
087900100708       setll (v1cmat)  unana07l    ;
088000100708 3     if not %equal(unana07l)  ;
088100100708         Errgenerico=*on  ;
088200100708         Errmessage =*on  ;
088300100708         Errmat=*on  ;
088400100708         v1cmsg=msg(07)  ;
088500100708         leavesr  ;
088600100927 x3    //else   ;
088700100708
088800100708       // controllo se univoca
088900100927       //clear  qtatem   ;
089000100927       //reade v1cmat   unana07l   ;
089100100927 4     //dow  not %eof(unana07l) ;
089200100927 5     //if qtatem=0   ;
089300100927       //qtatem=qtatem+1  ;
089400100927       //v1ctip=%editc(unatip:'X') ;
089500100927 x5    //else   ;
089600100927       //  Errgenerico=*on  ;
089700100927       //  Errmessage =*on  ;
089800100927        // Errmat=*on  ;
089900100927        // v1cmsg=msg(08)  ;
090000100927       //leave  ;
090100100927 5     //endif   ;
090200100927       //reade v1cmat   unana07l   ;
090300100927 4     //enddo   ;
090400100708 3     endif    ;
090500100708 2     endif    ;
090600100708 1     endif    ;
090700100708
090800100316       ENDSR  ;
090900080206       //--------------------------------------------------------------
091000100312       //?Gestione SFL 02
091100080206       //--------------------------------------------------------------
091200100312       BEGSR GesS02;
091300080207
091400080207         // Inizializzazione videata
091500100312         if  $InzS02 = *on;
091600100312            exsr InzS02;
091700100824            $InzS02  = *off;
091800100824         endif;
091900100827
092000100831         // INTERRROGAZIONE: se non trovati record errore in prima videata
092100100831         if s02nrr=0   ;
092200100831         v1cmsg=msg(27)  ;
092300100831         errGenerico=*on  ;
092400100831         ErrMessage=*on  ;
092500100831         $Video='D1'  ;
092600100831         leavesr   ;
092700100831         endif  ;
092800100831
092900100831         // Posizionamento cursore
093000101005 1       if  C02csr  > *zero ;
093100101005
093200100924         // se non ci sono errori e il record +1 è vuoto, mi posiziono qui
093300101005 2       if ErrGenerico =*off ;
093400101005
093500101005 2a      if §sdv0imm='S'    ;
093600100924           s02nrr=c02csr    ;
093700100924           chain s02nrr sduos02   ;
093800101005 3         if %found ;
093900100927            exsr Aggiosfl  ;
094000101027            else  ;
094100101027            // Se non trovato riporto indietro di uno il campo per visulizzare il sfl
094200101027            C02csr=c02csr-1  ;
094300101005 3         endif  ;
094400101005 3         if %found and vsctip>=*zeros ;
094500100927
094600100924            s02nrr=c02csr+1  ;
094700100924            chain s02nrr sduos02   ;
094800101005 4          if %found and vshimm='S'  and vsctip<=*zeros ;
094900100924             c02csr=s02nrr ;
095000101005 4          endif ;
095100101005 4          if %found ;
095200100927             exsr Aggiosfl  ;
095300101005 4         endif  ;
095400101005 3         endif  ;
095500101005 2a      endif  ;
095600101005
095700101005         // Per immissione matricole mi posiziono sul record senza matricola
095800101005 2a      if §sdv0MMAT='S'    ;
095900101005           s02nrr=c02csr    ;
096000101005           chain s02nrr sduos02   ;
096100101005 3         if %found ;
096200101005            exsr Aggiosfl  ;
096300101005 3         endif  ;
096400101005 3         if %found and vscmat<>*blanks  ;
096500101005
096600101005            s02nrr=c02csr+1  ;
096700101005            chain s02nrr sduos02   ;
096800101005 4          if %found and vscmat=*blanks  ;
096900101005             c02csr=s02nrr ;
097000101005 4          endif ;
097100101005 4          if %found ;
097200101005             exsr Aggiosfl  ;
097300101005 4         endif  ;
097400101005 3         endif  ;
097500101005 2a      endif  ;
097600101005
097700101005 2       endif  ;
097800100924
097900100827           C02rcd = C02csr;
098000101005 1       endif  ;
098100080207
098200080207         // Emissione Testata e Piede con tasti funzionali abilitati
098300100824         if  errMessage  = *off;
098400100824           write  SDUOT01;
098500100824           write  SDUOZ02;
098600080207         endif;
098700080703
098800080207         // Emissione videata
098900100316         exfmt  SDUOC02;
099000080410
099100080207         reset errMessage;
099200080207         reset errGenerico;
099300100824         clear V1cmsg;
099400100830         errMAG=*off     ;
099500100830         errMGC=*off     ;
099600100830         errTIP=*off     ;
099700100830         errMAT=*off     ;
099800100830         errQTA=*off     ;
099900100830         errTRU=*off     ;
100000100830         errTIR=*off     ;
100100100830         errDES=*off     ;
100200110412         errNOT=*off     ;
100300100830         errKSC=*off     ;
100400100830         errDTM=*off     ;
100500100909         errDDO=*off     ;
100600101012         errNCL=*off     ;
100700100923         errMATR=*off     ;
100800100909         errINVR=*off     ;
100900100927         s02nrr=1  ;
101000080207
101100080523 1       SELECT;
101200080207
101300080207         // - F3=Fine
101400080523 1         WHEN  dsp_aid = c_F03;
101500080409            $Fine = *on;
101600080207
101700080207         // - F12=Ritorno
101800080523 1         WHEN  dsp_aid = c_F12;
101900100315           $video='D1'   ;
102000100824
102100100824         // - Roll-Up
102200100824           WHEN  dsp_aid = c_RollUp;
102300100831         if §sdv0imm= 'S'  ;
102400100824           // - Verifica se raggiunto il limite di records registrabili
102500100824           //   nel subfile (9999)
102600100824             if  S02nrr = *hival;
102700100824               errMessage  = *on;
102800100824               errGenerico = *on;
102900100824               V1cmsg = Msg(09);
103000100824             else;
103100100824               exsr RollUpS02;
103200100824             endif;
103300100830             endif;
103400100901
103500100901         // - F2=Gestione Unità EDP
103600100901 1         WHEN  dsp_aid = c_F02;
103700100924           clear dsunana ;
103800100924           chain c02csr sduos02 ;
103900100927           if %found ;
104000100927           exsr Aggiosfl  ;
104100100927           endif   ;
104200100928           if %found  and vscmat<>*blanks  and
104300100928 2          (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
104400100924              UIMmat=vscmat  ;
104500100927             endif  ;
104600100928           if %found  and vscmatr<>*blanks  and
104700140123 2          (rnmfl= 'VSCMATR ' and v1ccuf<>'FOR' )  ;
104800100928              UIMmat=vscmatr ;
104900100928             endif  ;
105000100901           exsr CallGESUNIT     ;
105100100824
105200100901         // - F7=Ricerca Matricola
105300100901 1         WHEN  dsp_aid = c_F07;
105400100901           chain c02csr sduos02 ;
105500100901           if %found   ;
105600100901           exsr CallRicMat      ;
105700100901           endif  ;
105800100920
105900100901
106000100901         // - F14=Gestione tabella scheda clienti x scambio dati
106100100901 1         WHEN  dsp_aid = c_F14;
106200100901           exsr Callpgm3R       ;
106300100909
106400101001         // - F15=Immissione multipla
106500101001 1         WHEN  dsp_aid = c_F15;
106600101001            $VIDEO='D4'   ;
106700101001            exsr Gesd04  ;
106800101001
106900110404         // - F21=Abilita campo opzione
107000100909 1         WHEN  dsp_aid = c_F21;
107100100909           chain c02csr sduos02 ;
107200100909           if %found   ;
107300100909           exsr Protcampi  ;
107400100909           exsr Aggiosfl    ;
107500100909           endif  ;
107600100909
107700100901
107800080530 x1      // Invio / F6
107900080207           OTHER;
108000081006
108100100315               exsr ContrS02 ;
108200100930              if %open(fnlv22p) ;
108300100930               close    fnlv22p  ;
108400100930              endif  ;
108500100930
108600100830 2             if  errGenerico = *on or wSceltaOPZ='S';
108700120524               if wsceltaOPZ='S'  ;
108800120524                 Errmessage=*off   ;
108900120524               endif  ;
109000081006                 leavesr;
109100100824 2             endif;
109200080530
109300121130         // se non ci sono errori--> pulisco la data modifica conferma mov
109400121130               clear w05dtm  ;
109500121203               clear w05amg_dtm  ;
109600121130
109700100907         // F6=conferma Aggiornamento
109800100920 1         if   dsp_aid = c_F06 ;
109900100907
110000100907         // Non posso confermare immissione se pgm CONFERMA in uso
110100100907           if   §sdv0imm='S' ;
110200100907            in tnsduo   ;
110300100907            if §sduodat>0 and %subst(§sduopgm:1:1)='C'   ;
110400100907            exsr Msg37  ;
110500100907            leavesr     ;
110600100907           endif  ;
110700100907           endif  ;
110800100907
110900080702           exsr   ConfAggio ;
111000081023
111100100315           $video='D1'   ;
111200080530           endif  ;
111300080207
111400080523 1       ENDSL;
111500080207
111600080207       ENDSR;
111700100901       //--------------------------------------------------------------
111800100901       //?chiamata pgm gestione unità EDP
111900100901       //--------------------------------------------------------------
112000100901       BEGSR CallGesUNIT ;
112100100901
112200100901       clear dsunit  ;
112300100901       dsupro='A'  ;
112400100901       if §sdv0int='S'   ;
112500100901       dsuop0=' 5'  ;
112600100901       else  ;
112700100901       dsuop0=' 2'  ;
112800100901       endif  ;
112900100901
113000100901       clear kpjbu  ;
113100100901       kpjbu=dsunit  ;
113200100924       callp TNSDU1R  (kpjba:dsunana)  ;
113300100901
113400100901       ENDSR;
113500080207
113600100901       //--------------------------------------------------------------
113700100901       //?chiamata pgm ricerca matricola unità EDP
113800100901       //--------------------------------------------------------------
113900100901       BEGSR CallricMat  ;
114000100901
114100100901       clear kpjbu  ;
114200100901       clear dsunit  ;
114300100927       clear dsunana;
114400100930
114500100930       // Nella gestione RESI si può fare F7 solo sulla mmatricola di reso
114600100927 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
114700100930 3        If ModReso=*on  ;
114800100930          Errgenerico=*on  ;
114900100930          ErrMessage=*on  ;
115000100930          v1cmsg=msg(48)  ;
115100100930          exsr Aggiosfl  ;
115200100930          leavesr;
115300100930  3       endif  ;
115400100930
115500100927       uimmat=vscmat  ;
115600100927       else   ;
115700100927       uimmat=vscmatr ;
115800100927       endif  ;
115900100901       dsupro='A'  ;
116000100901       dsuop0=' 1'  ;
116100100901
116200100901       kpjbu=dsunit  ;
116300100924       callp TNSDU1R  (kpjba:dsunana)  ;
116400100901       dsunit=kpjbu    ;
116500100901
116600100927 1     if uimtip>0 and uimcod>0  ;
116700100930
116800100901       // In manutenzione se selezionato tipo unità diverso --> errore
116900100923 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
117000100923
117100100930 3     if vsctip<=*zeros  ;
117200100901       vsctip='00' ;
117300100930 x3    else   ;
117400100901
117500100930 4     if  uimtip<>%int(vsctip)  ;
117600100901        ErrGenerico=*on  ;
117700100901        ErrMessage=*on  ;
117800100930        if vshimm=' '  ;
117900100901        v1cmsg=msg(35)  ;
118000100930        else  ;
118100100930        v1cmsg=msg(49)  ;
118200100930        endif  ;
118300100901        ErrMat=*on  ;
118400100901           write  SDUOT01;
118500100901           write  SDUOZ02;
118600100901           exsr Aggiosfl  ;
118700100901           leavesr   ;
118800100930 4     endif ;
118900100930 3     endif ;
119000100930 x2    else  ;
119100100930 3     if vsctir<=*zeros  ;
119200100930       vsctir='00' ;
119300100930 x3    else   ;
119400100930
119500100930 4     if  uimtip<>%int(vsctir)  ;
119600100930        ErrGenerico=*on  ;
119700100930        ErrMessage=*on  ;
119800100930        v1cmsg=msg(49)  ;
119900100930        ErrMatr=*on  ;
120000100930           write  SDUOT01;
120100100930           write  SDUOZ02;
120200100930           exsr Aggiosfl  ;
120300100930           leavesr   ;
120400100930 4     endif ;
120500100930 3     endif ;
120600100923 2     endif ;
120700100923
120800100927         chain (uimtip:uimcod) unana01l  ;
120900100923 2       if %found(unana01l)  ;
121000100927           wtip=%editc(uimtip:'X') ;
121100100901           exsr chktip  ;
121200100923
121300100923       // se sono posizionato su MATR imposto matricola di reso
121400100923       //  in ogni altro caso quella normale
121500100923 3     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
121600100923           vsctip=%editc(unatip:'X')   ;
121700100901           vsdtip=wdtip  ;
121800100901           vscmat=unamat  ;
121900100923 x3    else  ;
122000100923           vsctir=%editc(unatip:'X')   ;
122100100923           vsdtir=wdtip  ;
122200100923           vscmatr=unamat  ;
122300100923 3     endif ;
122400100923 2     endif   ;
122500100901 1     endif   ;
122600100923
122700100901       exsr Aggiosfl  ;
122800100901
122900100901       ENDSR;
123000100901
123100100901       //--------------------------------------------------------------
123200100901       //?chiamata pgm gesitone tabella scheda clienti per scambio dati
123300100901       //--------------------------------------------------------------
123400100901       BEGSR Callpgm3R   ;
123500100901       clear kpjbu  ;
123600140424       callp tnsdunr  (kpjba)  ;
123700100901       ENDSR;
123800100901
123900100920       //--------------------------------------------------------------
124000100920       //?Stampa etichette
124100100920       //--------------------------------------------------------------
124200100920       BEGSR staEti      ;
124300100920
124400100920       // La prima volta devo proporre la richeista stampati
124500100920       if wrichtrul90=' '  ;
124600100920       exsr RichStampan  ;
124700100920       if d90f3<>'1' and not *in91   ;
124800100920       wrichtrul90='S'  ;
124900100920       endif  ;
125000100920       endif  ;
125100100920
125200100920
125300100920       ENDSR  ;
125400100824       //--------------------------------------------------------------
125500100824       //?Caricamento pagina vuota
125600100824       //--------------------------------------------------------------
125700100824       BEGSR RollUpS02;
125800100824         S02nrr   = totrig ;
125900100824
126000100824         // Scrittura righe vuote
126100100824         exsr Righevuote  ;
126200100824
126300100824       ENDSR;
126400080526       //--------------------------------------------------------------
126500100312       //?controlli SFL02
126600080409       //--------------------------------------------------------------
126700100312       BEGSR ContrS02;
126800100315
126900100920         // - F10=Stampa etichette chiedo la stampante
127000100920 1       if      dsp_aid = c_F10;
127100100920           exsr StaEti          ;
127200100920         endif  ;
127300100920
127400100830       clear  WsceltaOPZ   ;
127500081126       yy=1           ;
127600100315       s02nrr=1  ;
127700100824       readc       sduos02    ;
127800080703
127900100824 0     dow not %eof ;
128000100830
128100100831       // RECORD GIÀ INSERITO: CONTROLLO L'OPZIONE
128200100831 1     IF vshimm=' '  and vscopz<>' '
128300100831          and  vscopz<>'E'   ;
128400100830
128500100830       // controllo opzione
128600100830       Indx=%lookup(vscopz:wopz)  ;
128700100831 2     if Indx=0  ;
128800100830         v1cmsg=msg(26)  ;
128900100830         ErrGenerico=*on  ;
129000100830         ErrMessage =*on  ;
129100100830         EXSR AggioSFL   ;
129200100830         leavesr  ;
129300100831 2     endif   ;
129400100920
129500101006       select   ;
129600101006
129700101006       // Per F08 imposto la 'P' di materiale preparato e metto il R.I. in magazzio destinaz.
129800101006 2     when      dsp_aid = c_F08 ;
129900101012       // Se no INVIO tasto funzionale non ammesso
130000120525       //if vscinvr<>'I'    ;
130100120525       //  v1cmsg=msg(52)  ;
130200120525       //  ErrGenerico=*on  ;
130300120525       //  ErrMessage =*on  ;
130400120525       //  EXSR AggioSFL   ;
130500120525       //  leavesr  ;
130600120525       //endif   ;
130700101006
130800101006 3     if vscopz='P'  ;
130900101006           chain(e) vshnrr   ungio00f   ;
131000101006 4          if %error or not %found ;
131100101006             v1cmsg=msg(25)  ;
131200101006             ErrGenerico=*on  ;
131300101006             ErrMessage =*on  ;
131400101006             EXSR AggioSFL   ;
131500101006             leavesr  ;
131600101006 x4          else  ;
131700101006             vshfpr='P'  ;
131800101006             ungfpr='P'  ;
131900101006             update ungioFIS  ;
132000101006 4          endif  ;
132100120525          CLEAR VSCOPZ  ;
132200120525 3      endif  ;
132300120525
132400120525 3     if vscopz='B'  ;
132500120525           chain(e) vshnrr   ungio00f   ;
132600120525 4          if %error or not %found ;
132700120525             v1cmsg=msg(25)  ;
132800120525             ErrGenerico=*on  ;
132900120525             ErrMessage =*on  ;
133000120525             EXSR AggioSFL   ;
133100120525             leavesr  ;
133200120525 x4          else  ;
133300120525             if ungfst='S' ;
133400120525             v1cmsg=msg(62)  ;
133500120525             ErrGenerico=*on  ;
133600120525             ErrMessage =*on  ;
133700120525             EXSR AggioSFL   ;
133800120525             leavesr  ;
133900120525 x4          else  ;
134000120525             vshfst='N'  ;
134100120525             ungfst='N'  ;
134200120525             update ungioFIS  ;
134300120525 4          endif  ;
134400120525 4          endif  ;
134500120525          CLEAR VSCOPZ  ;
134600120525 3      endif  ;
134700101006
134800101006        exsr Aggiosfl    ;
134900121130
135000101006
135100100920       // Per F10 elaboro solo opzioni di stampa  le opzioni diverse da 9 le lascio
135200101006 2     when      dsp_aid = c_F10 and d90f3<>'1';
135300100920 3     if vscopz='9'  ;
135400100930         if not %open(fnlv22p)  ;
135500100930         open fnlv22p ;
135600100930         endif        ;
135700100930
135800100920         clear dsetiunit   ;
135900100920         §eunitmag=%editc(vscmag:'X') ;
136000100920         // Decodifico magazzino
136100100920          clear wazorg  ;
136200100920          wtmag ='D  '   ;
136300100920          wmag=%editc(vscmag:'X')  ;
136400100920          exsr  chkmag   ;
136500100920         §eunitdmag=wdmag              ;
136600100920         §eunitksc =%editc(vscksc:'X') ;
136700100920          chain   (1:dutkci:vscksc) cnaco00f   ;
136800100920         §eunitdksc=acorag             ;
136900101028
137000101028         select  ;
137100101028         // Imposto il campo NOTE
137200101028         when vscnot<>*blanks  ;
137300101028          §eunitnot =vscnot             ;
137400101028         //   se vuoto e tipo 99 -->quantità + descrizione
137500101028         when vsctip='99'  ;
137600101028          §eunitnot =%editc(vscqta:'X') + ' ' + vscdes  ;
137700101028         //   se vuoto e tipo <> 99 -->solo descrizione
137800101028         when vscdes<>*blanks  ;
137900101028          §eunitnot =vscdes             ;
138000101028         other   ;
138100101028         //   se vuota anche descrizione tipo e decodifica
138200101028          §eunitnot=vsctip + '-' + vsdtip   ;
138300101028         endsl   ;
138400100920         callp FNLVUEZS  (dsetiunit) ;
138500100920
138600100920         CLEAR VSCOPZ  ;
138700100920  3    endif  ;
138800100920       exsr Aggiosfl    ;
138900100920
139000121130
139100121130         // opz "D" --> visualizzo Window per richiedere la data
139200121130         //             solo la prima volta
139300121130
139400121130 2       when  vscopz='D'   ;
139500121203
139600121203  3      if vshfmv='S'   ;
139700121203         v1cmsg=msg(66)  ;
139800121203         ErrGenerico=*on  ;
139900121203         ErrMessage =*on  ;
140000121203         EXSR AggioSFL   ;
140100121203         leavesr  ;
140200121203         endif  ;
140300121130
140400121130 4       if w05dtm= 0 ;
140500121130
140600121130         I05sys=%dec(datasys)  ;
140700121130         kpjbu=tnsduods_1 ;
140800121130
140900121130         TNSDUOR1  (kpjba)  ;
141000121130
141100121130         tnsduods_1 = kpjbu   ;
141200121130
141300121203          if   O05F06='1' and o05gma_dtm>0  ;
141400121203           w05dtm=o05gma_dtm  ;
141500121203           w05amg_dtm=o05amg_dtm  ;
141600121130          endif  ;
141700121203
141800121203          if   w05dtm=0   ;
141900121203             ErrGenerico=*on  ;
142000121203             CLEAR VSCOPZ  ;
142100121203             EXSR AggioSFL   ;
142200121203             leavesr  ;
142300121203             endif  ;
142400121203
142500121130
142600121130 4       endif   ;
142700121130
142800121130 4       if w05dtm>0  ;
142900121130           chain(e) vshnrr   ungio00f   ;
143000121130 5          if %error or not %found ;
143100121130             v1cmsg=msg(25)  ;
143200121130             ErrGenerico=*on  ;
143300121130             ErrMessage =*on  ;
143400121130             EXSR AggioSFL   ;
143500121130             leavesr  ;
143600121130 x5          else  ;
143700121130             vscdtm=w05dtm ;
143800121203             ungdtm=w05amg_dtm ;
143900121130             update ungioFIS  ;
144000121130
144100121130 5          endif  ;
144200121130 4          endif  ;
144300121130
144400121130          CLEAR VSCOPZ  ;
144500121130         EXSR AggioSFL   ;
144600121130       dsp_aid= savinkf  ;
144700121130  x2   other    ;
144800100831
144900100831       // non posso annullare record confermato
145000100920  3    if vshimm=' '  and vshfmv='S' and vscopz='4'  ;
145100100831         v1cmsg=msg(29)  ;
145200100831         ErrGenerico=*on  ;
145300100831         ErrMessage =*on  ;
145400100831         EXSR AggioSFL   ;
145500100831         leavesr  ;
145600100920  3    endif  ;
145700101006       // Per opzione '9' premere tasto funzionale F9
145800100920  3    if vshimm=' '  and   vscopz='9'  ;
145900100920         v1cmsg=msg(43)  ;
146000100920         ErrGenerico=*on  ;
146100100920         ErrMessage =*on  ;
146200100920         EXSR AggioSFL   ;
146300100920         leavesr  ;
146400121130        endif  ;
146500121130
146600101006       // Per opzione 'P' premere tasto funzionale F8
146700101006  3    if vshimm=' '  and   vscopz='P'  ;
146800101012       if vscinvr='I'   ;
146900101006         v1cmsg=msg(53)  ;
147000101006         else  ;
147100101006         v1cmsg=msg(52)  ;
147200101006         endif  ;
147300101006         ErrGenerico=*on  ;
147400101006         ErrMessage =*on  ;
147500101006         EXSR AggioSFL   ;
147600101006         leavesr  ;
147700101006  3    endif  ;
147800120525       // Per opzione 'B' premere tasto funzionale F8
147900120525  3    if vshimm=' '  and   vscopz='B'  ;
148000120525         v1cmsg=msg(53)  ;
148100120525         ErrGenerico=*on  ;
148200120525         ErrMessage =*on  ;
148300120525         EXSR AggioSFL   ;
148400120525         leavesr  ;
148500120525  3    endif  ;
148600100903
148700100921       // non posso Manutenzionare  o annullare se blocco data area
148800100921  3    if vshimm=' '  and  (vscopz='2' or vscopz='4') ;
148900100903        in tnsduo   ;
149000100909        if §sduodat>0 and (§sdv0resi='S' or §sdv0imm='S' )  ;
149100100907        exsr Msg37    ;
149200100909        clear vscopz  ;
149300100907
149400100903         EXSR AggioSFL   ;
149500100903         leavesr  ;
149600100920       endif   ;
149700100920 3     endif  ;
149800100830
149900100909       //  Alloco il record non per opz "5"  e "8"
150000100920 3       if vscopz='5' or vscopz='8'  ;
150100100830         chain(n) vshnrr   ungio00f   ;
150200100830         else  ;
150300100830         chain(e) vshnrr   ungio00f   ;
150400100920 3       endif  ;
150500100920 3       if %error or not %found ;
150600100830         v1cmsg=msg(25)  ;
150700100830         ErrGenerico=*on  ;
150800100830         ErrMessage =*on  ;
150900100830         EXSR AggioSFL   ;
151000100830         leavesr  ;
151100100920 3       endif  ;
151200100909
151300100909         // opz "8" --> interroga movimenti
151400100920 3       if vscopz='8'   ;
151500100909         exsr IntMovim  ;
151600100909         clear vscopz   ;
151700100909         EXSR AggioSFL   ;
151800100909         leavesr  ;
151900100920 3       endif  ;
152000121130
152100100830
152200100830        $VIDEO='D3'   ;
152300100830        exsr Gesd03  ;
152400100830
152500100830        wSceltaOPZ='S'  ;
152600100830        clear vscopz ;
152700100830         EXSR AggioSFL   ;
152800100830
152900100920 3     If ErrGenerico=*on  ;
153000100830         leavesr  ;
153100100920 3     endif  ;
153200101006 2     endsl ;
153300100830
153400100830 x1    else ;
153500100831
153600100924       // I record esclusi dalla conferma e annuillati non vengono controllati
153700100924  2    if vscopz<>'E'  and vshann<>'S' ;
153800100831
153900100830       // RECORD DI NUOVA IMMISSIONE: CONTROLLO
154000100830       exsr ContrRIGA  ;
154100100907       if  ErrGenerico=*on  ;
154200100907       leavesr ;
154300100907       endif  ;
154400100830
154500100830         // Per record inserito imposto utente e data ins
154600100831  3      if ErrGenerico=*off and vshimm='S' and vsctip<>*blanks ;
154700100830         vscute=KNMUS  ;
154800100830         datadmy=datasys ;
154900100830         dataeur=datasys ;
155000100830          vscdim=%dec(dataeur) ;
155100100830          w0060 =%dec(datadmy) ;
155200100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
155300100830                  %subst(%editc(w0060:'X'):3:2)    ;
155400100830
155500100831  3      endif    ;
155600100831  2      endif    ;
155700100831
155800100831   2    if ErrGenerico=*off ;
155900100831         EXSR AggioSFL   ;
156000100831   2    endif   ;
156100100831  1    endif   ;
156200100830
156300100830       readc    sduos02    ;
156400100830 0     enddo         ;
156500100830
156600100830       ENDSR  ;
156700081126
156800100907       //--------------------------------------------------------------
156900100907       //?scrivo msg 37 di blocco manutenzione
157000100907       //--------------------------------------------------------------
157100100907        BEGSR Msg37  ;
157200100907         v1cmsg=msg(37)  ;
157300100907         dataiso=%date(§sduodat) ;
157400100907         datadmy=dataiso      ;
157500100907         W006a=%editc(%dec(datadmy):'X') ;
157600100907         %subst(v1cmsg:74:2)=%subst(w006a:1:2) ;
157700100907         %subst(v1cmsg:77:2)=%subst(w006a:3:2) ;
157800100907         %subst(v1cmsg:60:10)=§sduoute          ;
157900100907         %subst(v1cmsg:27:20)=§sduopgm          ;
158000100907         ErrGenerico=*on  ;
158100100907         ErrMessage =*on  ;
158200100907        ENDSR  ;
158300100909       //--------------------------------------------------------------
158400100909       //?Interroga movimenti della unit/matricola
158500100909       //--------------------------------------------------------------
158600100909        BEGSR IntMovim;
158700100909
158800100924        // Se manca la matricola non sono interrogabili
158900100909        if vscmat=*blanks  ;
159000100909         v1cmsg=msg(34)  ;
159100100909         ErrGenerico=*on  ;
159200100909         ErrMessage =*on  ;
159300100909        else  ;
159400100909
159500100909        clear dsunit ;
159600100909        dsuop0=' 5'  ;
159700100909        dsutip=%int(vsctip)  ;
159800100930        chain (vscmat:dsutip)  unana07l    ;
159900100930        if %found(unana07l)  ;
160000110412         dsucod=unacod   ;
160100110412        else  ;
160200110412        // se non trovato provo con nuova matricola per FOR "R"
160300110412           if ProtperFOR=*on and vscnot<>*blanks   ;
160400110412           kmat=vscnot  ;
160500110412           chain (kmat:dsutip)  unana07l    ;
160600110412           if %found(unana07l)  ;
160700110412            dsucod=unacod   ;
160800110412           endif  ;
160900110412           endif  ;
161000110412
161100100909        endif  ;
161200100910        // solo se confermato, passo anche la data movimento
161300100910        if vshfmv='S'   ;
161400100910         datadmy=%date(vscdtm:*dmy)   ;
161500100910         dataiso=datadmy  ;
161600100910         dsudtm=%dec(dataiso) ;
161700100910        endif  ;
161800100909        kpjbu=dsunit   ;
161900100909        callp tnsdu4r   (kpjba)   ;
162000100909        ErrGenerico=*on  ;
162100100909        endif   ;
162200100909        ENDSR  ;
162300100830       //--------------------------------------------------------------
162400100830       //?controllo campi della riga di UNGIO00F
162500100830       //--------------------------------------------------------------
162600100830       BEGSR contrRIGA  ;
162700100830
162800100921       // controllo se inserito almeno un dato
162900101001 0     if vscmag<>0  or vscksc>0  or vscdes<>*blanks or vscnot<>*blanks
163000101001       or (vsctip<>*blanks  and vsctip<>*zeros )  or vsdksc<>*blanks or
163100101001       (vscmag3<>*blanks  and vscmag3<>*zeros and $Video<>'S2') or
163200101001       vscmat<>*blanks  ;
163300100921
163400100903       // Invii/Rientri: campo obbligatorio
163500100923 1     if vscinvr=' ' ;
163600100903           errMessage  = *on;
163700100903           errGenerico = *on;
163800100903           errINVR=*on ;
163900100903           V1cmsg = Msg(39);
164000100903           EXSR AggioSFL   ;
164100100903           leavesr  ;
164200100923 1     endif  ;
164300101001
164400101001       // Per inserimeno multiplo, inserire il numero delle ricorrenza
164500101001       if   $video='D4' and v4cinse<=0  ;
164600101001           errMessage  = *on;
164700101001           errGenerico = *on;
164800101001           errINSE=*on ;
164900101001           V1cmsg = Msg(50);
165000101001           EXSR AggioSFL   ;
165100101001           leavesr  ;
165200101001       endif        ;
165300100826
165400100824       // tipo unita
165500100927       // controllo tipo unità e matricola
165600100927       exsr CtrTipMat    ;
165700100927       if Errgenerico =*on  ;
165800100927       leavesr  ;
165900100927       endif    ;
166000120209
166100120209       // Magazzino di destinazione obbligatorio
166200120209       //  non obbligatorio per inserimento multiplo
166300120209 0     if $video<>'D4'  or vscmag3<>*blanks  ;
166400120209
166500120209       clear vsdmag  ;
166600120209       clear wazorg  ;
166700120209       wtmag ='D  '   ;
166800120209 1     if $Video='S2'   ;
166900120209       wmag=%editc(vscmag:'X')  ;
167000120209       else  ;
167100120209       wmag=vscmag3 ;
167200120209 1     endif ;
167300120209
167400120209       exsr  chkmag   ;
167500130118
167600120209 1     if $Video='S2'   ;
167700120209       vscmag=%int(wmag)    ;
167800130118
167900130118       if Errgenerico=*off ;
168000130118       if §omagfag='Y'  and v1ccuf='FOR'  and vscinvr='F'
168100130118         and vscmag<>unadms  and vscmag>*zeros  ;
168200130118           errMessage  = *on;
168300130118           errGenerico = *on;
168400130118           V1cmsg = Msg(67);
168500130118       endif  ;
168600130118       endif  ;
168700130118
168800120209       else   ;
168900120209       vscmag3=wmag  ;
169000120209 1     endif  ;
169100120209       vsdmag=wdmag  ;
169200120209 1     if Errgenerico=*on  ;
169300120209         errMAG=*on ;
169400120209         EXSR AggioSFL   ;
169500120209         leavesr  ;
169600120209 1     endif    ;
169700120209 0     endif    ;
169800100927
169900100927
170000100927         // Quantità: solo per 99 <> da 0 o 1
170100100927 1       if vscqta>1 and vsctip<> '99'  ;
170200100927           errMessage  = *on;
170300100927           errGenerico = *on;
170400100927           V1cmsg = Msg(15);
170500100927           errQTA=*on ;
170600100927           EXSR AggioSFL   ;
170700100927           leavesr  ;
170800100927 1       endif  ;
170900100831
171000100907       // Controlli sul Magazzino Correlato
171100100831 1     if  vscmgc>0   ;
171200100903       // Non può essere uguale al magazzino di destinazione
171300100923 2     if vscmgc=vscmag  ;
171400100924           errMessage  = *on;
171500100924           errGenerico = *on;
171600100903           errMGC=*on ;
171700100903           V1cmsg = Msg(38);
171800100903           EXSR AggioSFL   ;
171900100903           leavesr  ;
172000100923 2     endif  ;
172100100908
172200100908       clear wazorg ;
172300100908       wtmag='C' ;
172400100909       wmag=%editc(vscmgc:'X')  ;
172500100831       exsr  chkmag   ;
172600100909       vsdmgc=wdmag  ;
172700100831 2     if Errgenerico=*on  ;
172800100831         errMGC=*on ;
172900100831         EXSR AggioSFL   ;
173000100831         leavesr  ;
173100100831 2     endif    ;
173200100831
173300100831 1     endif    ;
173400100923
173500100923
173600100923       // Tipo unità di RESO solo se c'e' la sostituzione
173700110401       // ignoro per FOR F o R
173800110401         if protperRIE=*off  ;
173900110610
174000110610         // Aumento/Sostituz: non per 99
174100110610 1       if vsctru<>' ' and vsctip=  '99'  ;
174200110610           errMessage  = *on;
174300110610           errGenerico = *on;
174400110610           V1cmsg = Msg(16);
174500110610           errTRU=*on ;
174600110610           EXSR AggioSFL   ;
174700110610           leavesr  ;
174800110610 1       endif  ;
174900110610
175000100923 1       if (vsctru<>'S' and vsctir<>*blanks and vsctir<>'00') or
175100100923            (vsctru= 'S' and (vsctir= *blanks or vsctir=*zeros))  ;
175200100923           errMessage  = *on;
175300100923           errGenerico = *on;
175400100923           V1cmsg = Msg(17);
175500100923           errTir=*on ;
175600100923           EXSR AggioSFL   ;
175700100923           leavesr  ;
175800100923 1       endif   ;
175900110404         else  ;
176000110404         // Per FOR F o FOR  R contiene  guasto fisico. Accetto solo la "S"
176100110404         if vsctru<>'S' and vsctru<>' '   ;
176200110404           errMessage  = *on;
176300110404           errGenerico = *on;
176400110404           V1cmsg = Msg(56);
176500110404           errTru=*on ;
176600110404           EXSR AggioSFL   ;
176700110404           leavesr  ;
176800110404         endif  ;
176900110610         // Note obbligatorie
177000110610         if vscnot=*blanks and vsctru='S'   ;
177100110610           errMessage  = *on;
177200110610           errGenerico = *on;
177300110610           V1cmsg = Msg(59);
177400110610           errNot=*on ;
177500110610           EXSR AggioSFL   ;
177600110610           leavesr  ;
177700110610         endif ;
177800110610
177900110401 1       endif   ;
178000100923
178100100923         clear vsdtir  ;
178200101001 1       if vsctir<>*blanks  and vsctir<>*zeros  ;
178300100923         wtip=vsctir   ;
178400100923         exsr chktip   ;
178500100923
178600100923         vsctir=wtip  ;
178700100923         vsdtir=wdtip  ;
178800100923 2       if Errgenerico=*on  ;
178900100923           errTIR=*on ;
179000100923 3       if Errmessage =*on  ;
179100100923           V1cmsg = Msg(18);
179200100923 3       endif  ;
179300100923           EXSR AggioSFL   ;
179400100923           leavesr  ;
179500100923 2       endif    ;
179600100923 1       endif    ;
179700100923
179800100923         // Matricola unità di reso
179900100923 1       if vscmatr<>*blanks and vsctir=*blanks  ;
180000100923           errMessage  = *on;
180100100923           errGenerico = *on;
180200100923           V1cmsg = Msg(44);
180300100923           errMATr=*on ;
180400100930           *inki=*on ;
180500100923           EXSR AggioSFL   ;
180600100923           leavesr  ;
180700100923 1       endif  ;
180800100923
180900100923          clear vsdmodr  ;
181000101005          clear vscmgcr  ;
181100100923 1        if vscmatr<>*blanks ;
181200100923          ktip=%int(vsctir)  ;
181300100923          chain (vscmatr:ktip)  unana07l    ;
181400100923 2        if not %found(unana07l)  ;
181500100923           Errgenerico=*on  ;
181600100923           Errmessage =*on  ;
181700100923           ErrMATR=*on  ;
181800100923           v1cmsg=msg(06)  ;
181900100923           EXSR AggioSFL   ;
182000100923           leavesr  ;
182100100923 2        endif  ;
182200100923          vsdmodr =unamod;
182300101005          vscmgcr =unamag;
182400100923
182500100923          // Il magazzino correlato deve essere uguale a quello di destinazione
182600101005 2        if unamag<>vscmag  and unaksc=0 and vshfmat<>vscmatr  ;
182700100923           errMessage  = *on;
182800100923           errGenerico = *on;
182900100923           V1cmsg = Msg(45);
183000100923           errMATr=*on ;
183100101005           vshfmat=vscmatr ;
183200100923           EXSR AggioSFL   ;
183300100923           leavesr  ;
183400100923 2        endif  ;
183500100923 1        endif  ;
183600101005
183700101005          vshfmat=vscmatr ;
183800100826
183900100827         // Descrizione obbligatoria per Invii per d§UNTfl1='S'
184000101001 1       if $Video<>'D4'   ;
184100101001 2       if vscinvr='I' and d§untfl1='S' and vscdes=*blanks  ;
184200101112           errMessage  = *on;
184300101112           errGenerico = *on;
184400100826           errDES=*on ;
184500100826           V1cmsg = Msg(19);
184600100826           EXSR AggioSFL   ;
184700100826           leavesr  ;
184800101001 2       endif ;
184900101001 1       endif ;
185000110412
185100110412         // per FOR "F" note diventa --> nuova matricola inviata in sostiituizione
185200110412 1       if $Video<>'D4'  and vscnot<>*blanks and ProtperFOR=*on ;
185300110412         // non deve essere più lunga di 15 caratteri
185400110412
185500110412         if %subst(vscnot:16:5)<>*blanks  ;
185600110412           Errgenerico=*on  ;
185700110412           Errmessage =*on  ;
185800110412           Errnot=*on  ;
185900110412           v1cmsg=msg(58)  ;
186000110412           EXSR AggioSFL   ;
186100110412           leavesr  ;
186200110412         endif  ;
186300110412
186400110412         // non deve già esistere
186500110412         ktip=%int(vsctip)  ;
186600110412         eval kmat=vscnot    ;
186700110412          setll (kmat:ktip)  unana07l    ;
186800110412 3        if %equal(unana07l)  ;
186900110412           Errgenerico=*on  ;
187000110412           Errmessage =*on  ;
187100110412           Errnot=*on  ;
187200110412           v1cmsg=msg(57)  ;
187300110412           EXSR AggioSFL   ;
187400110412           leavesr  ;
187500110412 3        endif    ;
187600110412         endif  ;
187700100826
187800100826         // codice cliente
187900100830 1       if ($video='S2' and vsdksc <>*blanks and vscksc=0) or
188000101001            ($video<>'S2' and vsdksc3<>*blanks and vscksc=0)  ;
188100100826           clear xparsta  ;
188200100826           xparkcc=dutkci ;
188300100826           xparkut= 3     ;
188400100826           xpardut=rsut   ;
188500100830           if $video='S2' ;
188600100826           xparrag=vsdksc ;
188700100830           else  ;
188800100830           xparrag=vsdksc3;
188900100830           endif  ;
189000100826           clear xparflr ;
189100100826           clear xparkcm  ;
189200100826           clear xparksm  ;
189300100826           clear xparkdm  ;
189400100826           xparnum=1      ;
189500100826           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
189600100826                           :xparflr:xpardit:xparnum:xparkcm:xparksm
189700100826                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
189800100827 2          if xparsta<>-1  ;
189900100826            vscksc=%int(%subst(xparksm:1:7))   ;
190000100826            vsdksc=xparrag   ;
190100100830            vsdksc3=xparrag   ;
190200100827 2          endif            ;
190300100826             errGenerico = *on;
190400100826             errksc      = *on;
190500100826           EXSR AggioSFL   ;
190600100826           leavesr  ;
190700100827  1      endif ;
190800100826
190900100826         // codice cliente : obbligatorio per tipo 48/49 in invio
191000100920         //  tranne se non c'e' la parola "SCORTA" nelle note
191100100920         Indx=%scan('SCORTA':vscnot)  ;
191200100920         if Indx=0   ;
191300100920         Indx=%scan('scorta':vscnot)  ;
191400100920         endif  ;
191500100920         if Indx=0   ;
191600100920         Indx=%scan('Scorta':vscnot)  ;
191700100920         endif  ;
191800100920
191900100924  2      If Indx=0 ;
192000100924  3      if vscinvr='I' and vscksc=0 and (vsctip='48' or vsctip='49');
192100100924           errMessage  = *on;
192200100924           errGenerico = *on;
192300100826           errKSC=*on ;
192400100826           V1cmsg = Msg(20);
192500100826           EXSR AggioSFL   ;
192600100826           leavesr  ;
192700100924  3      endif ;
192800100920  1      endif ;
192900100826
193000100827  1      if vscksc>0  ;
193100100826           chain   (1:dutkci:vscksc) cnaco00f   ;
193200100827  2        if not %found(cnaco00f)   ;
193300100826             errMessage  = *on;
193400100826             errGenerico = *on;
193500100826             ErrKSC   = *on;
193600100826             V1cmsg = Msg(21);
193700100826            EXSR AggioSFL   ;
193800100826             leavesr;
193900100826             else  ;
194000100826             vsdksc=acorag  ;
194100100830             vsdksc3=acorag  ;
194200100827 2       endif  ;
194300100827 1       endif  ;
194400100825
194500100826         // Data conferma movimento: se > udate non troppo...  ;
194600100826 1       If vscdtm>0  ;
194700100826             clear wlbdat;
194800100826             g02dat = vscdtm ;
194900100826             xsrda8(wlbdat);
195000100826 2           if g02err = '1';
195100100826               errMessage  = *on;
195200100826               errGenerico = *on;
195300100826               ErrDTM    = *on;
195400100826               V1cmsg = Msg(02);
195500100826              EXSR AggioSFL   ;
195600100826               leavesr;
195700100826 2           endif;
195800100826
195900100924 2           if g02inv>datapiu200;
196000100826               errMessage  = *on;
196100100826               errGenerico = *on;
196200100826               ErrDTM    = *on;
196300100826               V1cmsg = Msg(22);
196400100826              EXSR AggioSFL   ;
196500100826               leavesr;
196600100826 2           endif   ;
196700100909 2           if vshfmv=' ' and g02inv<%dec(datasys)  ;
196800100831               errMessage  = *on;
196900100831               errGenerico = *on;
197000100831               ErrDTM    = *on;
197100100831               V1cmsg = Msg(31);
197200100831              EXSR AggioSFL   ;
197300100831               leavesr;
197400100831 2           endif   ;
197500100826 1       endif   ;
197600101012
197700101012       // Se non c'e' un altra riga per lo stesso magazzino, chiedo obbligatori peso e vol
197800101012 1     if vscinvr='I'  and $Video<>'D4'  and vscmag>0  and vshfmv=' '  ;
197900101012        exsr selectSQL   ;
198000101012
198100101012 2        if   vscncl=0 or vscpkg=0   ;
198200101012 3      if SiColliPeso=' ' ;
198300101012               errMessage  = *on;
198400101012               errGenerico = *on;
198500101012               ErrNCL    = *on;
198600101012               V1cmsg = Msg(54);
198700101012              EXSR AggioSFL   ;
198800101012               leavesr;
198900101012 3      endif  ;
199000101012
199100101012 2      endif  ;
199200101012        endif  ;
199300080703
199400101001         // Dcumento obbligatorio  se rientro da fornitore
199500101001 1       if $Video<>'D4'  ;
199600120525 2       if (vscdoc=0 and vscddo=0) and vscinvr='R' ;
199700100909               errMessage  = *on;
199800100909               errGenerico = *on;
199900100909               ErrDDO    = *on;
200000100909               V1cmsg = Msg(33);
200100100909              EXSR AggioSFL   ;
200200100909               leavesr;
200300101001 2        endif   ;
200400101001 1        endif   ;
200500100909
200600100909       // controllo la data
200700100909       if vscddo>0   ;
200800100909             clear wlbdat;
200900100909             g02dat = vscddo ;
201000100909             xsrda8(wlbdat);
201100100909 2           if g02err = '1';
201200100909               errMessage  = *on;
201300100909               errGenerico = *on;
201400100909               ErrDDO    = *on;
201500100909               V1cmsg = Msg(02);
201600100909               leavesr;
201700100909              endif  ;
201800100909       endif  ;
201900100921 x0    else  ;
202000101001       // Per inserimento multipli immettere almeno un dato da duplicare
202100101001       if $Video='D4'  ;
202200101001               errMessage  = *on;
202300101001               errGenerico = *on;
202400101001               ErrMAG    = *on;
202500101001               V1cmsg = Msg(51);
202600101001               leavesr;
202700101001       endif  ;
202800100921       clear vsdmag   ;
202900100921       clear vsdtip  ;
203000100921       clear vsdmod1 ;
203100100921       clear vsdmod2 ;
203200100921       clear vscmgc  ;
203300100921       clear vsdmgc  ;
203400100921       Recvuoto='S'   ;
203500100930       // pulisco eventuale posizione della matricola
203600100930           if s02nrr<2000 ;
203700100930            clear tipmat(s02nrr)          ;
203800101012            clear SiCoPe(s02nrr)          ;
203900100930           endif  ;
2040001009210      endif  ;
204100100909
204200080409       ENDSR;
204300101012
204400101012       //--------------------------------------------------------------
204500101012       //?Select con SQL su ungio00F
204600101012       //--------------------------------------------------------------
204700101012       BEGSR  SelectSQL ;
204800101012
204900101012       clear SiColliPeso  ;
205000101012 1     if vscdtm=0  ;
205100101012         invdtm=%dec(datasys)  ;
205200101012       else  ;
205300101012         dataeur=%date(vscdtm:*dmy)    ;
205400101012         dataiso=dataeur  ;
205500101012         invdtm=%dec(dataiso) ;
205600101012 1     endif  ;
205700101012
205800101012       // Prima verifico se già inserito in questa pagina di lavoro
205900101012
206000101012       // Cerco con: MAGAZZINO  e data conferma
206100101012          // se la data <= oggi cerco con data =0
206200101012
206300101012 1     if invdtm<=%dec(datasys)  ;
206400101012       wSiCoPe=(vscmag+100000000)            ;
206500101012       else  ;
206600101012       wSiCoPe=(vscmag+100000000) +  invdtm  ;
206700101012 1     endif  ;
206800101012
206900101012 1     if   vscncl=0 or vscpkg=0   ;
207000101012
207100101012       clear sicope(s02nrr)   ;
207200101012       Indx=%lookup(wsicope:Sicope)  ;
207300101012 2      if Indx>0  ;
207400101012         SiColliPeso='S'  ;
207500101012 2      endif  ;
207600101012
207700101012 x1     else  ;
207800101012
207900101012         if s02nrr<2000 ;
208000101012           sicope(s02nrr)=wsicope    ;
208100101012         endif  ;
208200101012         SiColliPeso='S'  ;
208300101012 1      endif  ;
208400101012
208500101012
208600101012       // Poi se non trovato controllo se già inserito in UNGIO
208700101012         if SiColliPeso =' '  ;
208800101012
208900101012         wrkgetlista='SELECT ungdtm, ungncl, ungpkg, rrn(UNGIO00f) +
209000101012                               from UNGIO00F WHERE ungcuf=''' +v1ccuf  + ''' +
209100101012         and unginvr=''I'' and ungfmv='' '' and ungmag= ' + %editc(vscmag:'X') ;
209200101012
209300101012         exec sql   PREPARE s1 FROM :wrkgetlista   ;
209400101012
209500101012         exec sql   DECLARE a1 cursor FOR  s1   ;
209600101012
209700101012         exec sql OPEN a1   ;
209800101012
209900101012 0       dow sqlcod=0   ;
210000101012
210100101012         exec sql FETCH NEXT  FROM a1 INTO: sqdtm, :sqncl, :sqpkg, :sqnrr ;
210200101012
210300101012 1       select ;
210400101012 1       when sqlcod=100 ;
210500101012
210600101012 1       when sqlcod<0   ;
210700101012         *inh1=*on   ;
210800101012
210900101012 x1      other   ;
211000101012         // escludo il record se la data conferma è <> da quello del record ins/variato
211100101012 2a       if  vshnrr<>sqnrr ;
211200101012 2        if  vscdtm=0 or invdtm<=%dec(datasys)  ;
211300101012 3         if sqdtm<=invdtm  ;
211400101012 4           if sqncl>0 and sqpkg>0  ;
211500101012               SiColliPeso='S'  ;
211600101012 4           endif  ;
211700101012 3         endif  ;
211800101012
211900101012 x2       else  ;
212000101012 3         if  sqdtm=invdtm  ;
212100101012 4           if sqncl>0 and sqpkg>0  ;
212200101012               SiColliPeso='S'  ;
212300101012 4           endif  ;
212400101012 3         endif  ;
212500101012 2a       endif  ;
212600101012 2        endif  ;
212700101012
212800101012 1       endsl  ;
212900101012
213000101012 0       enddo   ;
213100101012
213200101012         exec sql CLOSE a1   ;
213300101012         endif  ;
213400101012
213500101012       ENDSR;
213600080409
213700100927       //--------------------------------------------------------------
213800100927       //?Controllo solo tipo unità e matricola
213900100927       //--------------------------------------------------------------
214000100927       BEGSR  CtrTipMat  ;
214100100927
214200100927       // Obbligatorio se non immessa la matricola
214300101001       //  non obbligatorio per inser. multiplo
214400101001 1     if (vsctip=*blanks or vsctip=*zeros) and vscmat=*blanks and $Video<>'D4';
214500100927           errMessage  = *on;
214600100927           errGenerico = *on;
214700100927           errTIP=*on ;
214800100927           v1cmsg=msg(05)  ;
214900100927           EXSR AggioSFL   ;
215000100927           leavesr  ;
215100100927  1      endif   ;
215200100927
215300100927  1     if vsctip<>*blanks and vsctip<>*zeros  ;
215400100927         clear vsdtip  ;
215500100927         clear vsdtip3 ;
215600100927         wtip=vsctip   ;
215700100927         exsr chktip   ;
215800100927
215900100927         vsctip=wtip  ;
216000100927         vsdtip =wdtip  ;
216100100927         vsdtip3=wdtip  ;
216200100927 2       if Errgenerico=*on  ;
216300100927           errTIP=*on ;
216400100927 3       if ErrMessage =*on  ;
216500100927          v1cmsg=msg(05)  ;
216600100927 3        endif  ;
216700100927           EXSR AggioSFL   ;
216800100927           leavesr  ;
216900100927 x2      else  ;
217000100927         dunt=t02uni ;
217100100927         vsdtip =d§untdes ;
217200100927         vsdtip3=d§untdes ;
217300100927  2      endif    ;
217400100927
217500140207         // "99" accettato solo per MAG/TLC o FOR-F
217600140207  2      if vsctip='99' and v1ccuf='FOR'
217700140207           and (v1cinvr='R' or v1cinvr='S') ;
217800100927           errMessage  = *on;
217900100927           errGenerico = *on;
218000100927           errTIP=*on ;
218100100927           V1cmsg = Msg(14);
218200100927           EXSR AggioSFL   ;
218300100927           leavesr  ;
218400100927  2      endif   ;
218500100927  1      endif   ;
218600100927
218700100927         // Matricola non ci vuole per 99
218800100927 1       if v1ctip='99' and v1cmat<>*blanks  ;
218900100927           errMessage  = *on;
219000100927           errGenerico = *on;
219100100927           V1cmsg = Msg(13);
219200100927           errMAT=*on ;
219300100927           EXSR AggioSFL   ;
219400100927           leavesr  ;
219500100927 1       endif    ;
219600100927
219700100927
219800100927         // Matricola: obbligatoria in conferma
219900100927         if §sdv0conf='S' and vsctip<>'99' and vscmat=*blanks  ;
220000100927           errMessage  = *on;
220100100927           errGenerico = *on;
220200100927           V1cmsg = Msg(32);
220300100927           errMAT=*on ;
220400100927           EXSR AggioSFL   ;
220500100927           leavesr  ;
220600100927         endif   ;
220700100927
220800100927         // Matricola: se immessa controllo
220900100927         clear vsdmod1 ;
221000100927         clear vsdmod2 ;
221100100927         clear vsdmod  ;
221200100927          if vshfmv=' ' ;
221300100927          clear vscmgc  ;
221400100927          clear vsdmgc  ;
221500100927         endif  ;
221600100927 1       if vscmat<>*blanks   ;
221700100927 2       if vsctip>*zeros   ;
221800100927          ktip=%int(vsctip)  ;
221900100927          chain (vscmat:ktip)  unana07l    ;
222000100927 x2       else  ;
222100100927          chain (vscmat)       unana07l    ;
222200100927 2        endif  ;
222300100927
222400100927 2        if not %found(unana07l)  ;
222500100927          Errgenerico=*on  ;
222600100927          Errmessage =*on  ;
222700100927          ErrMAT=*on  ;
222800100927          if vsctip>*zeros  ;
222900100927          v1cmsg=msg(06)  ;
223000100927          else  ;
223100100927          v1cmsg=msg(07)  ;
223200100927          endif  ;
223300100927
223400100927           EXSR AggioSFL   ;
223500100927           leavesr  ;
223600100927 2        endif  ;
223700100927
223800100927          // se trovata verifico se univoca altrimenti ci vuole il tipo
223900100927 2        if     vsctip<=*zeros and %found(unana07l)  ;
224000100927           clear  qtatem   ;
224100100927           setll (vscmat)  unana07l    ;
224200100927           reade (vscmat)  unana07l    ;
224300100927 3         dow  not %eof(unana07l) ;
224400100927 4          if qtatem=0   ;
224500100927             qtatem=qtatem+1  ;
224600100927 x4         else   ;
224700100927             Errgenerico=*on  ;
224800100927             Errmessage =*on  ;
224900100927             Errmat=*on  ;
225000100927             v1cmsg=msg(08)  ;
225100100927             EXSR AggioSFL   ;
225200100927             leavesr;
225300100927 4          endif   ;
225400100927           reade (vscmat)  unana07l    ;
225500100927 3         enddo   ;
225600100927 2        endif   ;
225700100927
225800100927 2        if vsctip<=*zeros   ;
225900100927             vsctip=%editc(unatip:'X') ;
226000100928            ktip=unatip   ;
226100100928            wtip=vsctip   ;
226200100927            exsr chktip   ;
226300100927            dunt=t02uni ;
226400100927            vsctip=wtip  ;
226500100927            vsdtip =wdtip  ;
226600100927            vsdtip3=wdtip  ;
226700100927 2        endif ;
226800100927          vsdmod =unamod;
226900100927          vsdmod1=unamod;
227000100927          vsdmod2=%subst(unamod:7:9) ;
227100110404          // Se unità in proprietà alla 923 --> non posso accettare MAG 990
227200110404          if unasis='923' and ((vscmag = 990  and $Video='S2') or
227300111024                               (vscmag3='990' and $Video<>'S2')  or
227400111024                               (vscmag3='980' and $Video<>'S2')  or
227500111024                               (vscmag = 980  and $Video='S2')) ;
227600110401             Errgenerico=*on  ;
227700110401             Errmessage =*on  ;
227800110401             Errmag=*on  ;
227900110401             v1cmsg=msg(55)  ;
228000110401             EXSR AggioSFL   ;
228100110401             leavesr;
228200110401          endif  ;
228300130626          // Se unità in proprietà non è 923 --> non posso accettare MAG 923
228400130626          if unasis<>'923' and ((vscmag = 923  and $Video='S2') or
228500130626                               (vscmag3='923' and $Video<>'S2')) ;
228600130626             Errgenerico=*on  ;
228700130626             Errmessage =*on  ;
228800130626             Errmag=*on  ;
228900130626             v1cmsg=msg(68)  ;
229000130626             EXSR AggioSFL   ;
229100130626             leavesr;
229200130626          endif  ;
229300120209
229400120209          // Per FOR F propongo magazzino destinazione se si tratta di un fornitore
229500130116          if v1ccuf='FOR'  and vscinvr='F' ;
229600130116          chain unadms azorg21l ;
229700130116          if  vscmag<=*zeros  and
229800130116                 %found(azorg21l) and  orgfag = 'Y' ;
229900120209              vscmag=unadms  ;
230000120209              Errgenerico=*on  ;
230100120209              EXSR AggioSFL   ;
230200120209              leavesr  ;
230300120209          endif  ;
230400130116
230500130116             endif  ;
230600120209
230700100927          // Solo se record non confermato
230800100927          if vscmgc=0 or vshfmv=' ' ;
230900100927           vscmgc =unamag;
231000100927          endif  ;
231100100927
231200100928          //  controllo l'univocità tra i record non  confermati
231300130321            setll (Ktip:vscmat)  ungio01l    ;
231400130321            reade (Ktip:vscmat)  ungio01l    ;
231500130321            dow not %eof(ungio01l) ;
231600130321             if ungfmv=' '  ;
231700130321              if vshimm='S' or (vshnrr<>gio1nrr)   ;
231800130321               Errgenerico=*on  ;
231900130321               Errmessage =*on  ;
232000130321               ErrMAT=*on  ;
232100130321               v1cmsg=msg(23)  ;
232200130321               EXSR AggioSFL   ;
232300130321               leavesr  ;
232400130321              endif   ;
232500130321             endif   ;
232600130321             reade (Ktip:vscmat)  ungio01l    ;
232700130321           enddo   ;
232800100927
232900100927          // controllo che non sia già stata inserita in questa pagina di lavoro
233000100927          wtipmat=vsctip ;
233100100927          %subst(wtipmat:3:15)=vscmat   ;
233200100927          Indx=%lookup(wtipmat:tipmat)  ;
233300100928 2        if Indx>0 and Indx<>s02nrr ;
233400100927          Errgenerico=*on  ;
233500100927          Errmessage =*on  ;
233600100927          ErrMAT=*on  ;
233700100927          v1cmsg=msg(24)  ;
233800100927           EXSR AggioSFL   ;
233900100927           leavesr  ;
234000100928           else  ;
234100100928           // la memorizzo
234200100928           // solo se maggiore di 1999 non memorizzo
234300100928           if s02nrr<2000 ;
234400100928            tipmat(s02nrr)=wtipmat  ;
234500100928  2        endif    ;
234600100928 2        endif    ;
234700100927
234800101022 2        if unaksc>0  and vscksc=0 and vscinvr='F';
234900100927          vscksc =unaksc;
235000100927           chain   (1:dutkci:unaksc) cnaco00f   ;
235100100927 3         if %found(cnaco00f)   ;
235200100927             vsdksc=acorag  ;
235300100927             vsdksc3=acorag  ;
235400100927           else   ;
235500100927             vsdksc=*all'?'  ;
235600100927             vsdksc3=*all'?'  ;
235700100927 3         endif  ;
235800100927 2        endif  ;
235900100927 2        if vscdes=*blanks ;
236000100927          vscdes=unades ;
236100100927 2        endif ;
236200100927 1       endif    ;
236300100927         ENDSR    ;
236400080703       //--------------------------------------------------------------
236500080703       //?Aggiornamento sfl
236600080703       //--------------------------------------------------------------
236700080703       BEGSR AGGIOSFL  ;
236800100830
236900100830       IF $VIDEO='S2'       ;
237000100830
237100100830       exsr ProtCampi   ;
237200100830
237300100824       if v1cmsg<>*blanks   ;
237400080703       errMessage  = *on;
237500080703       errGenerico = *on;
237600100315       c02csr=s02nrr    ;
237700080703       endif    ;
237800080703
237900100921       if Recvuoto<>'S';
238000100830        Sflnxtchg=*on  ;
238100100921        endif  ;
238200100921        clear recvuoto  ;
238300100824
238400100315       update  sduos02  ;
238500100830       ENDIF   ;
238600100830
238700080703       ENDSR   ;
238800100312       //--------------------------------------------------------------
238900100312       //?Inizializzazione videata 01
239000100312       //--------------------------------------------------------------
239100100312       BEGSR Inzd01  ;
239200100312
239300100315         clear V1cmsg;
239400100825         clear V1dcuf;
239500100830         EmisMOD1    = *on ;
239600100826         errMessage  = *off;
239700100312         errGenerico = *off;
239800140207         // Per utente EDP propongo MAG  ;
239900140207         // altrimenti  TLC
240000140207         if %subst(knmus:1:3)='EDP'  ;
240100100825         v1ccuf ='MAG'   ;
240200140207         else  ;
240300140207         v1ccuf ='TLC'   ;
240400140207         endif  ;
240500100908         if modInt=*off  and modReso=*off ;
240600100826         v1cconf='S'  ;
240700100908         else  ;
240800100908         clear v1cconf ;
240900100908         endif  ;
241000100825         clear v1cinvr ;
241100100824         clear  v1cute  ;
241200100831         clear v1ciet  ;
241300100826         clear v1ctip  ;
241400100831         clear v1ctip  ;
241500100824         clear v1dtip  ;
241600100315         clear v1cmat  ;
241700100315         clear v1cmgc  ;
241800100315         clear v1cmag  ;
241900100831         clear v1creso ;
242000100831         clear v1cdtm  ;
242100101025         clear v1cnot  ;
242200121024         clear v1cdes  ;
242300100312
242400100831         // Per le MATRICOLE escludo il tipo 99
242500100831         if ImmMAT=*on   ;
242600100831          v1ciet='E'  ;
242700100831          v1ctip='99'  ;
242800100831         wtip=v1ctip   ;
242900100831         exsr chktip   ;
243000100831         v1dtip=wdtip  ;
243100100831         endif  ;
243200100831
243300101125         // Per gestine RESI imposto al "S"  e solo i confermati
243400100908         if ModReso=*on   ;
243500100831         v1creso='S'   ;
243600101125         v1cconf='C'  ;
243700100831         endif  ;
243800100831
243900100924         // Per conferma movimenti e immissione imposto data conferma = udate
244000110914         // anche per immissione matricole
244100110914         if ImmGES=*on  or ImmMAT=*on  ;
244200101112         v1cdtma=Dataoggi      ;
244300101112         endif   ;
244400101112         if ImmCMOV=*on   ;
244500100903         v1cdtm=Dataoggi      ;
244600100831         endif  ;
244700100831
244800100312         ENDSR    ;
244900080207       //--------------------------------------------------------------
245000100312       //?Inizializzazione SFL02
245100080207       //--------------------------------------------------------------
245200100312       BEGSR InzS02;
245300080207
245400080207       // Pulizia subfile
245500080207         SflDsp_N    = *on;
245600080207         SflDspCtl_N = *on;
245700100315         write  SDUOC02;
245800080207         SflDspCtl_N = *off;
245900080207         SflEnd      = *off;
246000100824         SflDsp_N    = *off;
246100100824         Sflnxtchg   = *off;
246200080530
246300121130         clear w05dtm  ;
246400100826         clear tipmat;
246500101012         clear SiCoPe  ;
246600100826         clear C02rcd;
246700100315         clear C02csr;
246800100315         S02nrr=0 ;
246900100824         clear V1cmsg;
247000100824         clear totrig;
247100080207         errMessage  = *off;
247200080207         errGenerico = *off;
247300100708
247400100826         // Decodifico ufficio scelto
247500140123         select ;
247600140123         when v1ccuf='MAG'           ;
247700100827         v1dcuf=' MAG=Magazzino  C.E.D. ' ;
247800140123         when v1ccuf='TLC'           ;
247900140123         v1dcuf=' MAG=Magazzino  T.L.C. ' ;
248000140123         other ;
248100100827         v1dcuf='FOR=Magazzino  ACQUISTI' ;
248200140123         ENDsl  ;
248300100826
248400100823         // Predispongo le letture e carico SFL
248500100823         // se movimenti MAG imposto SEDE nel tipo magazzino correlato
248600100825         kinvr=v1cinvr ;
248700140123         if v1ccuf<>'FOR' and v1cinvr=' '  ;
248800100825         eval kinvr='I' ;
248900100823         endif  ;
249000100823
249100100823         clear kmag  ;
249200100823         if v1cmag>*zeros  ;
249300100823         kmag=%int(v1cmag)  ;
249400100823         endif  ;
249500100823
249600100823         clear kmgc  ;
249700100823         if v1cmgc>*zeros  ;
249800100823         kmgc=%int(v1cmgc)  ;
249900100823         endif  ;
250000100824
250100100824         clear ktip  ;
250200100824         if v1ctip>*zeros  ;
250300100824         ktip=%int(v1ctip)  ;
250400100824         endif  ;
250500100823
250600100823         // Se presente tipo e matricola leggo per questo
250700121024         //if ktip>*zeros and v1ciet ='I'   ;
250800100823
250900121024         //select   ;
251000121024         //when v1cmat=*blanks  ;
251100121024         //  setll (ktip)                      ungio01l    ;
251200121024         //  reade (ktip)                      ungio01l    ;
251300121024         //when v1cdim>0   ;
251400121024         //  setll (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
251500121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
251600121024         //other;
251700121024         //  setll (ktip:v1cmat:v1ccuf)        ungio01l    ;
251800121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
251900121024         //endsl   ;
252000100823
252100121024         //dow not %eof(ungio02l)  ;
252200100823         // controllo parzializzazioni
252300121024         // vshnrr =gio1nrr ;
252400121024         //exsr  CtrPARZ   ;
252500121024         //if Wok='S'   ;
252600121024         //exsr CarSF02  ;
252700121024         //endif   ;
252800100823
252900121024         //select   ;
253000121024         //when v1cmat=*blanks  ;
253100121024         //  reade (ktip)                        ungio01l    ;
253200121024         //when v1cdim>0   ;
253300121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
253400121024         //other;
253500121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
253600121024         //endsl   ;
253700121024         //enddo   ;
253800100823
253900121024         //else  ;
254000100823
254100100823         select  ;
254200100831         when kinvr  =*blanks ;
254300100831           setll (v1ccuf)                      ungio04l    ;
254400100831           reade (v1ccuf)                      ungio04l    ;
254500100823         other ;
254600101012           setll (v1ccuf:kinvr:kmag)           ungio04l    ;
254700100831           reade (v1ccuf:kinvr)                ungio04l    ;
254800100824         endsl  ;
254900100823
255000100823         dow not %eof(ungio04l)  ;
255100100823         // Controllo Parzializzazioni
255200100827          vshnrr =gio4nrr ;
255300100823         exsr  CtrPARZ   ;
255400100824         if Wok='S'   ;
255500100823         exsr CarSF02  ;
255600100823         endif   ;
255700100823
255800100823         select  ;
255900100831         when kinvr=*blanks ;
256000100825           reade (v1ccuf)                      ungio04l    ;
256100100823         other ;
256200100831           reade (v1ccuf:kinvr)                ungio04l    ;
256300100823         endsl  ;
256400100823         enddo   ;
256500100823
256600121024         //endif   ;
256700100823
256800100831         select ;
256900100831         // In interrogazione  resi e conferma:
257000100831         //                 mi posiziono sul primio record  e non scrivo righe vuote
257100100831         when §sdv0int='S'  or §sdv0resi='S'  or §sdv0conf='S' ;
257200100830         c02csr=1 ;
257300100830         c02rcd=1  ;
257400100831
257500100831         // In immissione matricole mi posiziono sulla matricola ssul primo record
257600100831         when §sdv0MMat='S'  ;
257700100831         if c02csr=0 ;
257800100831         c02csr=1 ;
257900100831         endif  ;
258000100831
258100100831         other  ;
258200100831         // Solo in Gestione: Scrittura righe vuote
258300101001         // salvo il primo record vuoto che scrivo
258400101001         PrimoVuoto=s02nrr+1  ;
258500100824         exsr Righevuote  ;
258600100831         endsl   ;
258700100823
258800100824         ENDSR  ;
258900100824       //--------------------------------------------------------------
259000100824       //?Scrittura righe vuote
259100100824       //--------------------------------------------------------------
259200100824         BEGSR  Righevuote   ;
259300100824
259400100824         // Finito di leggere carico tutta la pagina con record vuoti
259500100824         if s02nrr>=c_sflPag ;
259600100824         wpag=%div(s02nrr:c_sflPag) ;
259700100824         wresto=%rem(s02nrr:c_sflPag)  ;
259800100824         if wresto>0    ;
259900100924         c02csr=s02nrr  ;
260000100824         else   ;
260100100924         c02csr=(wpag*c_sflpag)+1 ;
260200100824         endif  ;
260300100824
260400100824         else  ;
260500100824         clear wpag  ;
260600100824         wresto=s02nrr  ;
260700100824         // posizionamento sulla prtima pagina e 1° record vuoto
260800100924         c02csr=s02nrr  ;
260900100927          if c02csr=0 ;
261000100927           eval c02csr=1  ;
261100100927          endif ;
261200100824         endif ;
261300100824
261400100824         dow wresto<c_sflpag  ;
261500100823         wresto=wresto+1 ;
261600100826
261700100824         vshimm='S'   ;
261800100830         clear VSCOPZ ;
261900100830         clear VShann ;
262000100909         clear VShfmv ;
262100101006         clear VShfpr ;
262200120524         clear VShfst ;
262300100909         clear VShnrr ;
262400140123         if v1ccuf <>'FOR'  ;
262500100825         vscinvr='I'  ;
262600100824         else   ;
262700100825         vscinvr=v1cinvr;
262800100824         endif   ;
262900100823         clear vscmag ;
263000100830         clear vscmag3;
263100100827         clear vshnrr ;
263200100827         clear vscqta ;
263300100823         clear vsctip ;
263400100830         clear vsdtip ;
263500100830         clear vsdtip3;
263600100827         clear vscmgc ;
263700100827         clear vscmat ;
263800100827         clear vsdmod1 ;
263900100827         clear vsdmod2 ;
264000100830         clear vsdmod  ;
264100100823         clear vscdes ;
264200100826         clear vscnot ;
264300100823         clear vsctru ;
264400100823         clear vsctir ;
264500100923         clear vscmatr;
264600100923         clear vsdmodr;
264700101005         clear vscmgcr;
264800101005         clear vshfmat;
264900100826         clear vscksc ;
265000100826         clear vsdksc ;
265100100830         clear vsdksc3;
265200100823         clear vscdtm ;
265300100823         clear vscddo ;
265400100823         clear vscdoc ;
265500100825         clear vscute ;
265600100830         clear vscdim4;
265700100830         clear vscdim ;
265800100827         clear vscreso  ;
265900100827         clear vscslr   ;
266000101008         clear vscncl   ;
266100101008         clear vscpkg   ;
266200100825         vshdim=%dec(datasys)  ;
266300100824
266400100824          exsr Protcampi  ;
266500100824
266600100823          s02nrr=s02nrr+1 ;
266700100823          write  SDUOS02;
266800100823          enddo  ;
266900080409
267000100824         // Salvo numero righe scritte
267100100824         totrig=s02nrr  ;
267200100824
267300080702         ENDSR    ;
267400080627
267500100823       //--------------------------------------------------------------
267600100823       //?Controllo selezioni impostate in prima videata
267700100823       //--------------------------------------------------------------
267800100823          BEGSR   CtrPARZ    ;
267900100823          Wok='S'    ;
268000100908          if w1cdtmd>0  and w1cdtma=0 ;
268100100908          w1cdtma=w1cdtmd ;
268200100908          endif  ;
268300100826
268400100823          // Utente
268500100823          if v1cute<>*blanks   and ungute<>v1cute ;
268600100823            clear wok  ;
268700100823            leavesr    ;
268800100823          endif      ;
268900100823          // Data Immissione
269000100824          if v1cdim>*zeros    and ungdim<w1cdim ;
269100100823            clear wok  ;
269200100823            leavesr    ;
269300100823          endif      ;
269400100823          // Tipo movimentazione
269500100825          if ungcuf <>v1ccuf ;
269600100823            clear wok  ;
269700100823            leavesr    ;
269800100823          endif      ;
269900100823          // Tipo magazzino correlato
270000100825          if kinvr<>*blanks and unginvr<>kinvr  ;
270100100823            clear wok  ;
270200100823            leavesr    ;
270300100823          endif      ;
270400100823          // Magazzino destinazione
270500100823          if kmag>*zeros  and ungmag<>kmag   ;
270600100823            clear wok  ;
270700100823            leavesr    ;
270800100823          endif      ;
270900100823          // Magazzino correlato
271000100823          if kmgc>*zeros  and ungmgc<>kmag   ;
271100100823            clear wok  ;
271200100823            leavesr    ;
271300100823          endif      ;
271400100826          // solo i record da confermare
271500100826          if v1cconf='S' and ungfmv='S' ;
271600100826            clear wok  ;
271700100826            leavesr    ;
271800100826          endif      ;
271900100908          // solo i record confermati
272000100908          if v1cconf='C' and ungfmv=' ' ;
272100100908            clear wok  ;
272200100908            leavesr    ;
272300100908          endif      ;
272400100908          // data conferma movimenti AL
272500100831          if v1cdtm>0   and ungdtm>w1cdtm  ;
272600100831            clear wok  ;
272700100831            leavesr    ;
272800100831          endif      ;
272900100908          // data conferma movimenti DAL/AL
273000100908          if v1cdtmd>0 or v1cdtma>0 ;
273100100908          if  ungdtm<w1cdtmd or ungdtm>w1cdtma ;
273200100908            clear wok  ;
273300100908            leavesr    ;
273400100908          endif      ;
273500100908          endif      ;
273600101025
273700101025          // SCAN sulle note
273800101025          if v1cnot<>*blanks   ;
273900101025           xx=%checkr(' ':v1cnot)  ;
274000101025           Indx=%scan(%subst(v1cnot:1:xx):ungnot)  ;
274100101025           if Indx=0   ;
274200101025            clear wok  ;
274300101025            leavesr    ;
274400101025           endif      ;
274500101025          endif      ;
274600121024          // SCAN sulla descrizione
274700121024          if v1cdes<>*blanks   ;
274800121024           xx=%checkr(' ':v1cdes)  ;
274900121024           Indx=%scan(%subst(v1cdes:1:xx):ungdes)  ;
275000121024           if Indx=0   ;
275100121024            clear wok  ;
275200121024            leavesr    ;
275300121024           endif      ;
275400121024          endif      ;
275500100823
275600121024          // Tipo Unità  da ESCLUDERE
275700100831          if v1ctip<>*blanks and v1ciet='E' and ktip= ungtip ;
275800100831            clear wok  ;
275900100831            leavesr    ;
276000100831          endif      ;
276100121024          // Tipo Unità  da INCLUDERE
276200121024          if v1ctip<>*blanks and v1ciet='I' and ktip<> ungtip ;
276300121024            clear wok  ;
276400121024            leavesr    ;
276500121024          endif      ;
276600100927
276700121029          // Matricola inseribile solo per "I"-inclusione
276800100927          if v1cmat<>*blanks and  v1cmat<>ungmat ;
276900100831            clear wok  ;
277000100831            leavesr    ;
277100100831          endif      ;
277200100831
277300100831          // unità da rendere
277400100831          if v1creso='S'  and ungreso='S'   ;
277500100831            clear wok  ;
277600100831            leavesr    ;
277700100831          endif      ;
277800100831          if v1creso='S'  and ungtir=0      ;
277900100831            clear wok  ;
278000100831            leavesr    ;
278100100831          endif      ;
278200100823          ENDSR  ;
278300100823       //--------------------------------------------------------------
278400100823       //?eseguo caricamento SFL
278500100823       //--------------------------------------------------------------
278600100823          BEGSR   carSF02    ;
278700100830          clear vscopz  ;
278800100830          clear vshANN  ;
278900100830          protOPZ=*off  ;
279000100830          clear vshimm  ;
279100100830
279200100830          exsr CarRECORD  ;
279300100830
279400100830          s02nrr=s02nrr+1   ;
279500100831
279600100831          // Per l'immissione Matricole mi posizione sul 1° record senza il codice
279700100831          if §sdv0mmat='S' and c02csr=0 and  vscmat=*blanks ;
279800100831          c02csr=s02nrr  ;
279900100831          ErrMat =*on    ;
280000100831          Sflnxtchg=*on  ;
280100100831          endif  ;
280200100831
280300100831          write  SDUOS02;
280400100831          ErrMat =*off   ;
280500100831          Sflnxtchg=*off ;
280600100831
280700100830          ENDSR         ;
280800100830       //--------------------------------------------------------------
280900100830       //?Carica record
281000100830       //--------------------------------------------------------------
281100100830          BEGSR   carRECORD ;
281200100830
281300100831          vshfmv =ungfmv ;
281400101006          vshfpr =ungfpr ;
281500120524          vshfst =ungfst ;
281600100831          vscinvr=unginvr;
281700100830  1       if $video='S2' ;
281800100823          vscmag=ungmag  ;
281900100830          clear vsdmod1  ;
282000100830          clear vsdmod2  ;
282100100830          clear vsdksc   ;
282200100830          clear vscdim4  ;
282300100830          clear vsdtip   ;
282400100830          else ;
282500100830          vscmag3=%editc(ungmag:'X')    ;
282600100909          wtmag='D'      ;
282700100908          wazorg='E  '   ;
282800100830          wmag=vscmag3   ;
282900100830          exsr  chkmag   ;
283000100830          vsdmag=wdmag    ;
283100100830
283200100830          clear vsdmod   ;
283300100830          clear vsdksc3  ;
283400100830          clear vscdim   ;
283500100830          clear vsdtip3  ;
283600100830  1       endif ;
283700100830
283800100826          vscqta=ungqta  ;
283900101008          vscncl=ungncl  ;
284000101008          vscpkg=ungpkg  ;
284100100826
284200100924          clear vsctip  ;
284300100830 1        if ungtip>0 ;
284400100830          vsctip=%editc(ungtip:'X') ;
284500100830          wtip=vsctip   ;
284600100830          exsr chktip   ;
284700100830 3          if $Video='S2' ;
284800100830            vsdtip=wdtip  ;
284900100830            else  ;
285000100830            vsdtip3=wdtip  ;
285100100830            endif ;
285200100830  1       endif ;
285300100826          vscmat=ungmat  ;
285400100826          clear vscksc   ;
285500100826          // Se c'e' tipo e matricola prendo il modello e lo visualizzo
285600100826 1        if ungtip>0 and ungmat<>*blanks ;
285700100930          chain (ungmat:ungtip)  unana07l    ;
285800100930 2        if  %found(unana07l)  ;
285900100830
286000100830 3        if $Video='S2' ;
286100100830          vsdmod1=unamod;
286200100827          vsdmod2=%subst(unamod:7:9) ;
286300100830          else  ;
286400100830          vsdmod =unamod;
286500100830 3        endif  ;
286600100826
286700100830          // se non è invio visualizzo cliente da anagrafica
286800100830 3        if  vscinvr<>'I'  and ungksc=0;
286900100826          vscksc =unaksc;
287000100830 4        if unaksc>0  ;
287100100826           chain   (1:dutkci:unaksc) cnaco00f   ;
287200100830 5         if not %found(cnaco00f)   ;
287300100830             acorag=*all'?'  ;
287400100830 5         endif  ;
287500100830 5         if $Video='S2' ;
287600100830             vsdksc=acorag  ;
287700100830           else  ;
287800100830             vsdksc3=acorag  ;
287900100830 5         endif  ;
288000100830
288100100830 4        endif ;
288200100830 3        endif ;
288300100826 2        endif ;
288400100826 1        endif ;
288500100826
288600100827          vscslr =ungslr  ;
288700100827          vscreso=ungreso ;
288800110404          dgio01=ungflo  ;
288900110404          if unginvr='R' or unginvr='F'  ;
289000110404           vsctru=§giogsf  ;
289100110404          else  ;
289200110404           vsctru=ungtru  ;
289300110404          endif  ;
289400100923
289500100927          clear vsdtir ;
289600100827  1       if ungtir>0 ;
289700100830          vsctir=%editc(ungtir:'X')   ;
289800100927          wtip=vsctir   ;
289900100927          exsr chktip   ;
290000100927          vsdtir=wdtip  ;
290100100923          vscmatr=ungmatr ;
290200101005          vshfmat=ungmatr ;
290300101005          clear vscmgcr;
290400101005          clear vsdmodr;
290500100923 2        if ungtir>0 and ungmatr<>*blanks ;
290600100930          chain (ungmatr:ungtir)  unana07l    ;
290700100930 3        if  %found(unana07l)  ;
290800100923          vsdmodr=unamod  ;
290900101005          vscmgcr=unamag  ;
291000100923 3        endif  ;
291100100923 2        endif  ;
291200100923 xx1      else ;
291300100826          clear vsctir ;
291400100923          clear vscmatr;
291500101005          clear vshfmat;
291600100923          clear vsdmodr;
291700101005          clear vscmgcr;
291800100827  1       endif ;
291900100826          vscmgc=ungmgc  ;
292000100923          clear vsdmgc  ;
292100100909          if ungmgc>0   ;
292200100909          wmag=%editc(ungmgc:'X');
292300100909          wtmag='C' ;
292400100909          wazorg='E  '   ;
292500100909          exsr  chkmag   ;
292600100909          vsdmgc=wdmag    ;
292700100909          endif   ;
292800100826          vscdes=ungdes  ;
292900100826          vscnot=ungnot  ;
293000100830
293100100830          // se pieno CLIENTE decodifico
293200100827 1        if ungksc>0   ;
293300100830           vscksc=ungksc  ;
293400100826           chain   (1:dutkci:ungksc) cnaco00f   ;
293500100830 2         if not %found(cnaco00f)   ;
293600100830             vsdksc=*all'?' ;
293700100830 2         endif  ;
293800100830 2         if $Video='S2' ;
293900100830             vsdksc=acorag  ;
294000100830           else  ;
294100100830             vsdksc3=acorag  ;
294200100830 2         endif  ;
294300100830
294400100827 1         endif  ;
294500100823
294600100823          clear vscdtm   ;
294700100823          if ungdtm>0    ;
294800100823          dataiso=%date(ungdtm)   ;
294900100823          datadmy=dataiso ;
295000100823          vscdtm=%dec(datadmy)  ;
295100100823          endif   ;
295200100823
295300100823          clear vscddo   ;
295400100823          if ungddo>0    ;
295500100823          dataiso=%date(ungddo)   ;
295600100823          datadmy=dataiso ;
295700100823          vscddo=%dec(datadmy)  ;
295800100823          endif   ;
295900100823          vscdoc=ungdoc  ;
296000100823
296100100826          vscute=ungute  ;
296200100823          if ungdim>0    ;
296300100823          dataiso=%date(ungdim)   ;
296400100823          datadmy=dataiso ;
296500100830          dataeur=dataiso ;
296600100823          w0060 =%dec(datadmy)  ;
296700100830          if $Video='D3'  ;
296800100830          vscdim=%dec(dataeur)  ;
296900100920          vschim=unghim         ;
297000100830          else  ;
297100100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
297200100827                  %subst(%editc(w0060:'X'):3:2)    ;
297300100830          endif  ;
297400100824          vshdim=ungdim  ;
297500100823          endif   ;
297600100823
297700100909          ErrGenerico=*off ;
297800100909          ErrMessage =*off ;
297900100826          exsr ProtCampi  ;
298000100823          ENDSR  ;
298100080702       //--------------------------------------------------------------
298200080702       //?Gestione protezione campi del sfl
298300080702       //--------------------------------------------------------------
298400080702          BEGSR   ProtCampi   ;
298500100830           protOPZ=*off ;
298600100826           ProtperINV=*off ;
298700100826           ProtperRIE=*off ;
298800110404           ProtperFOR=*off ;
298900100826           ProtINVR  =*off ;
299000100831           ProtMAT   =*off ;
299100100831           EmisCONF  =*off ;
299200100827           protrIGA=*off   ;
299300100830           RecANN  =*off   ;
299400100909           EmisCONF  =*off ;
299500101006           RevMAG    =*off ;
299600120524           NoSTAMPABO=*off ;
299700100824
299800100830          // Record di nuova immissione
299900100824          if vshimm='S'  ;
300000100830          protOPZ=*on  ;
300100100827          else  ;
300200100827           protrIGA=*on    ;
300300100830          endif    ;
300400100831
300500100903          // Per Int  e resi proteggo matricola
300600100903          if §sdv0resi='S'  or §sdv0int='S';
300700100831           protMAT =*on    ;
300800100831          endif   ;
300900100903          // Per immissione, solo per i record già inseriti
301000100903          if §sdv0imm='S' and vshimm=' ' ;
301100100903           protMAT =*on    ;
301200100903          endif  ;
301300100909
301400100909          // Se immissione matricole proteggo opzione  tranne che per F21
301500100909          if §sdv0mmat='S'  and dsp_aid <> c_F21    ;
301600100909           protOPZ =*on    ;
301700100909          endif   ;
301800100831
301900100831          // Se già confermato visualizzo
302000100831          if vshfmv='S'   ;
302100100831           EmisCONF  =*on  ;
302200100831          endif ;
302300101006          // Se già preparato materiale
302400101006          if vshfpr='P'   ;
302500101006           RevMAG   =*on  ;
302600101006          endif ;
302700100831
302800100831          // In fase di conferma accendo sflnexc per controllare tutti i record
302900100831          if §sdv0conf='S'   ;
303000100831           Sflnxtchg=*on  ;
303100100831          endif   ;
303200100830
303300100830          // Record annullato
303400100830          if vshANN='S'  ;
303500100830          protOPZ=*on  ;
303600100830          protrIGA=*on    ;
303700100830          recANN  =*on    ;
303800100830          endif  ;
303900100824
304000100826          if v1ccuf ='MAG'  or vscinvr='I'  ;
304100100826          protperINV=*on  ;
304200100824          endif    ;
304300100826
304400120523          if vscinvr='R' or vscinvr='F' or vscinvr='O';
304500110404           protperRIE=*on  ;
304600110404
304700120523          if vscinvr='R' or vscinvr='O';
304800110404           protperFOR=*on  ;
304900110404          endif  ;
305000100826          endif   ;
305100100826
305200100830          // Se immesso in proma videata il tipo invio/rientro lo proteggo
305300100826          if v1cinvr<>' ' ;
305400100826          protINVR=*on ;
305500100826          endif  ;
305600120524          // se non voglio la stampa della bolla visualizzo
305700120524          if vshfst='N'  ;
305800120524           NOstampaBO=*on  ;
305900120524          endif  ;
306000080703
306100080702          ENDSR    ;
306200100316       //--------------------------------------------------------------
306300100708       //?controllo Magazzino
306400100316       //--------------------------------------------------------------
306500100316        BEGSR ChkMag                 ;
306600100930        clear v1cmsg   ;
306700100930
306800100930        clear tnsdmagds  ;
306900100930        §magmag=wmag                            ;
307000100930        §imagtmag=wtmag                         ;
307100100930        §imagtorg=wazorg                        ;
307200100930        §imagcuf =v1ccuf                        ;
307300100930        §imaginvr=vscinvr                       ;
307400100930
307500100930        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
307600100930
307700100930        // Eseguita ricerca
307800100930        if §omagerr='R'   ;
307900100930        wmag=§magmag  ;
308000100930        wdmag = §omagdes  ;
308100100930        ErrGenerico=*on  ;
308200100930        leavesr   ;
308300100930        endif  ;
308400100930
308500100930        // Errore
308600100930        if §omagerr='E'   ;
308700100930        v1cmsg=§omagmsg  ;
308800100930        Errgenerico=*on  ;
308900100930        else   ;
309000100930        wdmag=§omagdes;
309100100930        endif  ;
309200100316
309300100316         if ErrGenerico =*on ;
309400100316         ErrMessage=*on  ;
309500100316         wdmag=*all'?'   ;
309600100316         endif  ;
309700100316
309800100316          ENDSR    ;
309900100708       //--------------------------------------------------------------
310000100708       //?controllo Tipo Unità
310100100708       //--------------------------------------------------------------
310200100708        BEGSR ChkTip                 ;
310300100708        clear WDTIP    ;
310400100824        indx=%scan('?':wtip )   ;
310500100827
310600100824        if Indx>0             ;
310700100708         clear tibs02ds  ;
310800100708         t02tla=' '      ;
310900100708         t02mod='R'      ;
311000100708         t02sif=knsif    ;
311100100708         t02cod='UNT'    ;
311200100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
311300100708         if t02ke1<>*blanks;
311400100824         wtip =t02ke1    ;
311500100708         wdtip=t02uni    ;
311600100708         else   ;
311700100708         clear  wtip ;
311800100708         endif  ;
311900100708
312000100708        ErrGenerico=*on   ;
312100100708        leavesr  ;
312200100708        endif   ;
312300100708
312400100708         clear tibs02ds  ;
312500100708         t02tla=' '      ;
312600100708         t02mod='C'      ;
312700100708         t02sif=knsif    ;
312800100708         t02cod='UNT'    ;
312900100708         t02ke1=wtip     ;
313000100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
313100100708
313200100708         if  t02err<>*blanks ;
313300100824         ErrGenerico=*on  ;
313400100824         ErrMessage =*on  ;
313500100824         wdtip=*all'?'   ;
313600100708         else  ;
313700100824         wdtip=t02uni    ;
313800100708         endif    ;
313900100708
314000100708          ENDSR    ;
314100080702       //--------------------------------------------------------------
314200100830       //?Aggiornamento record nuova immissione da SFL
314300080702       //--------------------------------------------------------------
314400080702        BEGSR ConfAGGIO              ;
314500081003
314600100315       s02nrr=1  ;
314700100824       readc    SDUOs02    ;
314800080704
314900100824 1     dow not %eof  ;
315000100831       // Per Gestione, se manca opzione, non c'e' nulla da aggiornare
315100100924       // Per immisssione, devono esserci tipo e mag destinazione
315200100924 2     if (§sdv0imm='S' and vshimm=' ' and vscopz=' ') or
315300100924 2        (§sdv0imm='S' and vshimm='S' and (vsctip<=*zeros or vscmag=0));
315400100831 x2    else   ;
315500100831
315600100831         // chain record
315700100831 3       if vshimm=' '    ;
315800100831           chain(e) vshnrr   ungio00f   ;
315900100831 4          if %error or not %found ;
316000100831             v1cmsg=msg(25)  ;
316100100831             ErrGenerico=*on  ;
316200100831             ErrMessage =*on  ;
316300100831             EXSR AggioSFL   ;
316400100831             leavesr  ;
316500100831 4          endif  ;
316600100831 3       endif  ;
316700100831
316800100907          exsr ScriviUNGIO  ;
316900100907 2     endif  ;
317000100830
317100100830       readc    SDUOs02    ;
317200100831 0      enddo           ;
317300100830       ENDSR            ;
317400100830       //--------------------------------------------------------------
317500100830       //?Aggiornamento file UNGIO
317600100830       //--------------------------------------------------------------
317700100830        BEGSR SCriviUNGIO            ;
317800100830
317900100830 1     if    vshimm=' '  and vscopz='4'  ;
318000100830       delete ungioFIS  ;
318100100830
318200100830 x1    else ;
318300100830
318400100830 2     if    vshimm='S'   ;
318500100827        clear ungiofis  ;
318600100901        timeiso=%time        ;
318700100901        unghim =%dec(timeiso);
318800100930        else   ;
318900100930        // richaino il record per non avere problemi  di "sporco" nei campi
319000100930         chain(e) vshnrr   ungio00f   ;
319100100830 2      endif   ;
319200100830
319300100824       ungdim  =vshdim  ;
319400100824       ungute  =vscute  ;
319500100826       ungcuf  =v1ccuf  ;
319600100825       if vsctip>*zeros  ;
319700100824       ungtip  =%int(vsctip) ;
319800100825       else  ;
319900100825       clear ungtip ;
320000100825       endif  ;
320100100824       ungmat  =vscmat  ;
320200100827       ungqta  =vscqta  ;
320300100827       if ungqta=0 ;
320400100827       ungqta=1 ;
320500100827       endif ;
320600100830       if $Video='S2' ;
320700100825       ungmag  =vscmag  ;
320800100830       else  ;
320900100830       ungmag  =%int(vscmag3)  ;
321000100830       endif  ;
321100100827       ungmgc  =vscmgc  ;
321200100825       unginvr =vscinvr ;
321300100824       ungdes  =vscdes  ;
321400100827       ungnot  =vscnot  ;
321500110404       // Per mag memorizzo vsctru un ungtru  come unità in aumento/sostituz
321600110404       if ProtperRIE=*off  ;
321700110404         ungtru  =vsctru  ;
321800110404         clear §giogsf  ;
321900110404       else  ;
322000110404       // Per "FOR" memorizzo nella DS DGIO01 come guasto fisico
322100110404        §giogsf=vsctru  ;
322200110404        clear ungtru  ;
322300120524       // per FOR "O" aggiorno "N" nella stampa bolla
322400120524       if ImmCMOV=*on  and  vscinvr='O'  ;
322500120524        ungfst='N'  ;
322600120524       endif  ;
322700110404       endif  ;
322800110404
322900110404       ungflo=dgio01  ;
323000110404
323100100827       clear ungksc   ;
323200100827       if vscksc>0 and vscinvr='I'  ;
323300100827       ungksc  =vscksc  ;
323400100827       endif  ;
323500100825       if vsctir>*zeros  ;
323600100824       ungtir  =%int(vsctir) ;
323700100923       ungmatr=vscmatr ;
323800100825       else  ;
323900100825       clear ungtir ;
324000100923       clear ungmatr;
324100100825       endif  ;
324200100827       if vscdtm>0 ;
324300100827       dataeur=%date(vscdtm:*dmy)  ;
324400100827       dataiso=dataeur  ;
324500100827       ungdtm=%dec(dataiso)  ;
324600100922       else  ;
324700100922       clear ungdtm  ;
324800100827       endif  ;
324900100922
325000100827       if vscddo>0 ;
325100100827       dataeur=%date(vscddo:*dmy)  ;
325200100827       dataiso=dataeur  ;
325300100827       ungddo=%dec(dataiso)  ;
325400100922       else  ;
325500100922       clear ungddo ;
325600100827       endif  ;
325700100827       ungdoc=vscdoc  ;
325800100827       ungreso=vscreso ;
325900100827       ungslr =vscslr  ;
326000101008       ungncl= vscncl  ;
326100101008       ungpkg= vscpkg  ;
326200100824
326300100908       // Richiamo pgm che crea i movimenti
326400100930       //  se non è escluso e se sono in conferma
326500100922 2     if §sdv0conf='S'  and vscopz= ' '  ;
326600100908
326700100908       // il tipo "99" lo flaggo senza richiamo
326800100908 3     if ungtip<> 99  ;
326900110412
327000110412       // Se FOR"F" e c'e' nuova matricola la scrivo in UNANA prima di RENDERE
327100110412
327200110412 4     if ProtperFOR=*on and ungnot<>*blanks ;
327300110412       chain (ungtip:ungmat)  unana02l    ;
327400110412 5     if %found(unana02l) ;
327500110412        unamat=ungnot;
327600110412        unadtr=%dec(datasys)    ;
327700110412        update unana2   ;
327800110412 5     endif  ;
327900110412 4     endif  ;
328000100922
328100100907       clear dsunit  ;
328200100907       dsuop1='F6N'  ;
328300100907       dsutip=ungtip  ;
328400100907       kpjbu = dsunit  ;
328500100907       callp TNSDU5R (kpjba: dsungio)  ;
328600100922
328700100922       // Aggiorno file dotazioni se previsto e se tipo unità inviato e di reso diversi
328800101026 4     if unginvr='I'    and ungtru<>'  ' and ungtip<>ungtir and ungksc=0 ;
328900100922       chain (ungtip:ungmag)  undot01l   ;
329000100922 5     if %found(undot01l)  ;
329100100922        dotqta=dotqta+1  ;
329200100922        update undot000 ;
329300100922 x5     else    ;
329400100922        clear undot000;
329500100922        dottip=ungtip  ;
329600100922        dotqta=1 ;
329700100922        dotfil= ungmag ;
329800100922        write undot000 ;
329900100922 5      endif  ;
330000100922
3301001009225      if ungtru='S' and ungtir>0   ;
330200100922       chain (ungtir:ungmag)  undot01l   ;
3303001009226      if %found(undot01l)  ;
330400100922        dotqta=dotqta-1  ;
330500100922        update undot000 ;
3306001009226      endif  ;
3307001010285      endif  ;
3308001010284      endif  ;
330900100922
331000100930       // Se presente anche la matricola aggiorno UNANA con la "S" di RENDERE
331100101028 4     if ungmatr<>*blanks  ;
331200110421
331300110421       //  non imposto al "S" di rientro se già rientrata in data di oggi con FOR "F"
331400110421       exsr SelectSQLFORF   ;
331500110421
331600100930       chain (ungtir:ungmatr)  unana02l    ;
331700101028 5     if %found(unana02l) ;
331800110421
331900110421 4a    if wtrov=' ' ;
332000130321       // se però il magazzino è sede --> non imposto lo stesso la "S"
332100130321          clear tnsdmagds  ;
332200130321          §magmag=%editc(unamag:'X')  ;
332300130321          §imagtmag='C' ;
332400130321          §imagtorg='E  '   ;
332500130321
332600130321        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
332700130321
332800130321        if §omagerr<>' ' or (§omagfag<>'W' and §omagfag<>'Y')  ;
332900100930        %subst(unalsw:15:1)='S'  ;
333000110421 5     endif  ;
333100130321 5     endif  ;
333200130321
333300101027        unadtr=%dec(datasys)    ;
333400100930        update unana2   ;
333500110421 4a    endif  ;
333600110421 4     endif  ;
333700100930
333800100908 3     endif  ;
333900100922
334000100908       ungfmv='S'  ;
334100100908 3     if ungdtm=0  ;
334200100908         ungdtm  =%dec(datasys) ;
334300100908 3     endif  ;
334400100922
334500100908
334600100908         update ungioFIS  ;
334700100907
334800100908 x2    else   ;
334900100907
335000100908  3    if vshimm=' '   ;
335100100830         update ungioFIS  ;
335200100830         else ;
335300100827         write ungiofis  ;
335400100908  3    endif  ;
335500100908 2     endif  ;
335600100907
335700100908 1     endif  ;
335800080704
335900080702        ENDSR;
336000110421       //--------------------------------------------------------------
336100110421       //?Select con SQL su ungio00F   per FOR "F"
336200110421       //--------------------------------------------------------------
336300110421       BEGSR  SelectSQLForF ;
336400110421 1     if ungdtm=0  ;
336500110421         sqdtm  =%dec(datasys) ;
336600110421         else  ;
336700110421         sqdtm=ungdtm     ;
336800110421 1     endif  ;
336900110421
337000110421       clear wtrov  ;
337100110421
337200110421         wrkgetlista='SELECT count(*)  +
337300110421                          from UNGIO00F WHERE ungcuf=''FOR'' and unginvr=''F'' +
337400110421            and ungmat=''' + ungmatr +''' and ungtip=' + %editc(ungtir:'X')  +
337500110421            ' and ungdtm=' + %editc(sqdtm :'X') ;
337600110421
337700110421         exec sql   PREPARE sF FROM :wrkgetlista   ;
337800110421
337900110421         exec sql   DECLARE aF cursor FOR  sF   ;
338000110421
338100110421         exec sql OPEN aF   ;
338200110421
338300110421 0       if  sqlcod=0   ;
338400110421         exec sql FETCH NEXT  FROM aF INTO:sqcont;
338500110421 0         if  sqcont>0   ;
338600110421             wtrov='S'  ;
338700110421 0         endif   ;
338800110421 0       endif   ;
338900110421
339000110421         exec sql CLOSE aF   ;
339100110421
339200110421       ENDSR;
339300110421
339400100830       //--------------------------------------------------------------
339500101001       //?Gestione videata 03
339600100830       //--------------------------------------------------------------
339700100830       BEGSR Gesd03;
339800100830
339900101018       // salvo la matricola: se imposta nel sfl la riporto
340000100831         savMat=vscmat  ;
340100101005         savMatr=vscmatr ;
340200100830         exsr CarRECORD  ;
340300100831         vscMat=savmat  ;
340400101005         vscMatr=savmatr  ;
340500100831
340600100830
340700100830         // Controllo opzione
340800100830         Emismod1=*off ;
340900100830         clear vsdf6           ;
341000120524         clear vsdf15          ;
341100120524         if vscopz<>'4'  and
341200120524            vscinvr<>'O' ;
341300120524           if  vshfst=' '  ;
341400120524            VSDf15=cF15BLO                ;
341500120524           else  ;
341600120524            VSDf15=cF15SBLO                ;
341700120524           endif  ;
341800120524         endif  ;
341900120524
342000100830 1       select   ;
342100100830         when vscopz='4'   ;
342200100901         vsdf6='F6=Conferma ANNULLAMENTO' ;
342300100830         v3dmod=' ANNULLAMENTO '  ;
342400100830
342500100830 1       when vscopz='2'   ;
342600100901         VSDf6='F6=CONFERMA Modifica' ;
342700120524
342800100830         v3dmod='   MODIFICA   '  ;
342900120524         // Se non conferato sproteggo riga
343000100831         if vshfmv=' ' ;
343100100831          protRiga=*off  ;
343200100831         endif  ;
343300100830
343400100830 1       when vscopz='5'   ;
343500100830         v3dmod='  VISUALIZZA  '  ;
343600100830         endsl   ;
343700100830
343800100830         // Imposto i campi diversi
343900100830         vscmag3=%editc(vscmag:'X')    ;
344000100830
344100100830 0       dow    $video='D3'   ;
344200100830
344300100830         // Emissione Testata
344400100830         if  errMessage  = *off;
344500100830           write  SDUOT01;
344600100830         endif;
344700100830
344800100830         // Emissione videata
344900100830         exfmt  SDUOD03;
345000100830
345100100830         reset errMessage;
345200100830         reset errGenerico;
345300100830         clear V1cmsg    ;
345400100830         clear wtmag     ;
345500100830         errMAG=*off     ;
345600100830         errMGC=*off     ;
345700100830         errTIP=*off     ;
345800100830         errMAT=*off     ;
345900100830         errQTA=*off     ;
346000100830         errTRU=*off     ;
346100100830         errTIR=*off     ;
346200100830         errDES=*off     ;
346300110412         errNOT=*off     ;
346400100830         errKSC=*off     ;
346500100830         errDTM=*off     ;
346600100909         errDDO=*off     ;
346700101012         errNCL=*off     ;
346800100923         errMATR=*off     ;
346900101012         errINVR=*off     ;
347000100830
347100100830 1       SELECT;
347200100830
347300100830         // - F12=Ritorno
347400100830 1         WHEN  dsp_aid = c_F12;
347500100930            chain(n) vshnrr   ungio00f   ;
347600100831            exsr CarRECORD  ;
347700100924            ErrGenerico=*off ;
347800100924            ErrMessage =*off ;
347900100924            clear v1cmsg  ;
348000100924            vscMat=savmat  ;
348100101005            vscMatr=savmatr  ;
348200100830            $Video='S2'   ;
348300100831            Emismod1=*on  ;
348400100830            leavesr  ;
348500100901
348600100901         // - F2=Gestione Unità EDP
348700100901 1         WHEN  dsp_aid = c_F02;
348800100927            clear dsunana  ;
348900100928            if vscmat<>*blanks  and
349000100928               (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
349100100927              UIMmat=vscmat  ;
349200100927             endif  ;
349300100928           if vscmatr<>*blanks  and
349400140123 2          (rnmfl= 'VSCMATR ' and v1ccuf<>'FOR' )  ;
349500100928              UIMmat=vscmatr ;
349600100928             endif  ;
349700100901           exsr CallGESUNIT     ;
349800100901
349900100909         // - F7=Ricerca Matricola
350000100901 1         WHEN  dsp_aid = c_F07;
350100100901           // Per annullamento opzione non ammmessa
350200100901           if vscopz='4'  ;
350300100901            ErrGenerico=*on   ;
350400100901            ErrMessage =*on   ;
350500100901            v1cmsg=msg(36)  ;
350600100901           else  ;
350700100901           exsr CallRicMat      ;
350800100901           endif  ;
350900101018
351000101018         // - F11=elimina "Materiale preparato"
351100101018 1         WHEN  dsp_aid = c_F11;
351200101018         chain(e) vshnrr   ungio00f   ;
351300101018         clear ungfpr  ;
351400101018         clear vshfpr  ;
351500101018         update ungiofis  ;
351600101018         RevMag=*off  ;
351700120524
351800120524         // - F15=Blocca/sblocca STAMPA BOLLA
351900120524 1         WHEN  dsp_aid = c_F15;
352000120525           //  In modifica  e visulizzazione
352100120525 2         if vscopz='2' or vscopz='5' ;
352200120524           // e solo se corretto il magazzino di destino
352300120524 3         if   vscinvr='O'   ;
352400120524            ErrGenerico=*on   ;
352500120524            ErrMessage =*on   ;
352600120524            v1cmsg=msg(60)  ;
352700120524 x3        else  ;
352800120524           // solo per magazzini fornitori che prevedono la stampa
352900120524          wmag= vscmag3         ;
353000120524          wazorg='E  '   ;
353100120524          exsr  chkmag   ;
353200120524
353300120524  4       if §omagfag='Y' or §omagfag='F' or §omagfag='A'  or
353400120524             §omaglue='S'  ;
353500120525
353600120524           chain(e) vshnrr   ungio00f   ;
353700120525           select   ;
353800120525  5        when ungfst='N'  ;
353900120524           ungfst=' '  ;
354000120524           vshfst=' '  ;
354100120525            VSDf15=cF15BLO                ;
354200120524           NOstampaBO=*off ;
354300120525
354400120525  5        when ungfst='S' ;
354500120525             v1cmsg=msg(62)  ;
354600120525             ErrGenerico=*on  ;
354700120525             ErrMessage =*on  ;
354800120525  x5         other ;
354900120524           ungfst='N'  ;
355000120524           vshfst='N'  ;
355100120525            VSDf15=cF15SBLO                ;
355200120524           NOstampaBO=*on  ;
355300120525  5       endsl   ;
355400120524         update ungiofis  ;
355500120524  x4      else  ;
355600120524         // Errore --> magazzino che non prevede la stampa
355700120524            ErrGenerico=*on   ;
355800120524            ErrMessage =*on   ;
355900120524            v1cmsg=msg(61)  ;
356000120524  4       endif  ;
356100120524  3       endif  ;
356200120524  2       endif  ;
356300120524
356400100901
356500100901         // - F14=Gestione tabella scheda clienti x scambio dati
356600100901 1         WHEN  dsp_aid = c_F14;
356700100901           // Per annullamento opzione non ammmessa
356800100901           if vscopz='4'  ;
356900100901            ErrGenerico=*on   ;
357000100901            ErrMessage =*on   ;
357100100901            v1cmsg=msg(36)  ;
357200100901           else  ;
357300100901           exsr Callpgm3R       ;
357400100901           endif  ;
357500100909
357600100830 x1      // Invio
357700100830           OTHER;
357800100830
357900100924           // non controllo record per opzione '4'
358000100924 3         if vscopz<>'4'   ;
358100100830           exsr contrRIGA ;
358200100924           endif   ;
358300100830
358400100830 2         if ErrGenerico=*Off   ;
358500100830
358600100830           //  Aggiornamento record di UNGIO
358700100830 2         if   dsp_aid = c_F06  and vscopz<>'5';
358800100831            exsr   ScriviUNGIO ;
358900100831            $Video='S2'   ;
359000100831            Emismod1=*on  ;
359100100830
359200100830           //  Ricarico i dati del record nel sfl aggiornati per opz "2"
359300100830 3         if vscopz='2'   ;
359400100830           exsr CarRECORD  ;
359500100830 x3        else  ;
359600100830           // scrivo la dicitura "ANNULLATO"
359700100830           clear  vscnot;
359800100830           vscnot='ANNULLATO  ' ;
359900100830           vshann='S' ;
360000100830 2         endif   ;
360100100830
360200100830         protRiga=*on   ;
360300100830 2       endif   ;
360400100830 2       endif   ;
360500100830
360600100830 1       ENDSL;
360700100830
360800100830 0     enddo   ;
360900100830
361000100830       ENDSR;
361100100830
361200101001       //--------------------------------------------------------------
361300101001       //?Gestione videata 04 - inserimento multiplo
361400101001       //--------------------------------------------------------------
361500101001       BEGSR Gesd04;
361600101001
361700101001         v3dmod='INSER.MULTIPLO'  ;
361800101001
361900101001         // pulisco i campi
362000101001         clear vscmag3  ;
362100101001         clear vsdmag   ;
362200101001         clear vscqta   ;
362300101001         clear vsctip   ;
362400101001         clear vsdtip3  ;
362500101001         clear vsctru   ;
362600101001         clear vsctir   ;
362700101001         clear vsdtir   ;
362800101001         clear vscksc   ;
362900101001         clear vsdksc3  ;
363000101001         clear vscdoc   ;
363100101001         clear vscddo   ;
363200101001         clear vscdtm   ;
363300101008         clear v4cncl   ;
363400101008         clear v4cpkg   ;
363500101001         clear v4cinse  ;
363600101001
363700101001 0       dow    $video='D4'   ;
363800101001
363900101001         // Emissione Testata
364000101001         if  errMessage  = *off;
364100101001           write  SDUOT01;
364200101001         endif;
364300101001
364400101001         // Emissione videata
364500101001         exfmt  SDUOD04;
364600101001
364700101001         reset errMessage;
364800101001         reset errGenerico;
364900101001         clear V1cmsg    ;
365000101001         clear wtmag     ;
365100101001         errMAG=*off     ;
365200101001         errTIP=*off     ;
365300101001         errQTA=*off     ;
365400101001         errTRU=*off     ;
365500101001         errTIR=*off     ;
365600101001         errKSC=*off     ;
365700101001         errDTM=*off     ;
365800101001         errDDO=*off     ;
365900101012         errNCL=*off     ;
366000101001         errINSE=*off     ;
366100101001
366200101001 1       SELECT;
366300101001
366400101001         // - F12=Ritorno
366500101001 1         WHEN  dsp_aid = c_F12;
366600101001            $Video='S2'   ;
366700101001            Emismod1=*on  ;
366800101001            leavesr  ;
366900101001
367000101001 x1      // Invio
367100101001           OTHER;
367200101001
367300101001           exsr contrRIGA ;
367400101001
367500101001 2         if ErrGenerico=*Off   ;
367600101001
367700101001           //  Aggiornamento record di UNGIO
367800101001 3         if   dsp_aid = c_F06 ;
367900101001            $Video='S2'   ;
368000101001            exsr   Scrivirighe ;
368100101001            Emismod1=*on  ;
368200101001
368300101001         protRiga=*on   ;
368400101001 3       endif   ;
368500101001 2       endif   ;
368600101001
368700101001 1       ENDSL;
368800101001
368900101001 0     enddo   ;
369000101001
369100101001       ENDSR;
369200101001
369300101001       //--------------------------------------------------------------
369400101001       //?Per inrimento multiplo scrivo righe nel sfl
369500101001       //--------------------------------------------------------------
369600101001       BEGSR Scrivirighe  ;
369700101001
369800101001       // salvo i campi per la scrittura del sfl
369900101001       if vscmag3>*zeros ;
370000101001       v4cmag=%int(vscmag3)  ;
370100101001       endif  ;
370200101001       v4cqta=vscqta  ;
370300101001       v4cqta=vscqta  ;
370400101001       v4ctip=vsctip  ;
370500101001       v4ctru=vsctru  ;
370600101001       v4ctir=vsctir  ;
370700101001       v4cksc=vscksc  ;
370800101001       v4dksc=vsdksc3 ;
370900101001       v4cdtm=vscdtm  ;
371000101001       v4cdoc=vscdoc  ;
371100101001       v4cddo=vscddo  ;
371200101001
371300101001
371400101001       // Dal primo vuoto in poi mi posizione e cerco il primo record libero
371500101001       s02nrr=Primovuoto  ;
371600101001
371700101001       Indx= 1 ;
371800101001       dow Indx<=v4cinse  ;
371900101001
372000101001       Lib='0'  ;
372100101001       dow Lib='0'  ;
372200101001        chain s02nrr sduos02 ;
372300101001        if %found  and (vscmag<>0 or vscdes<>*blanks  or vscksc<>0 or
372400101001         vsdksc<>*blanks  or vscnot<>*blanks or (vsctip<>*zeros and vsctip
372500101001         <>*blanks ) or vscmat<>*blanks) ;
372600101001         exsr Aggiosfl  ;
372700101001         s02nrr=s02nrr+1  ;
372800101001        else  ;
372900101001         Lib='1'  ;
373000101001        endif  ;
373100101001       enddo  ;
373200101001
373300101001       if not %found ;
373400101001       savnrr=s02nrr ;
373500101001       s02nrr=s02nrr-1 ;
373600101001       exsr RigheVuote  ;
373700101001       s02nrr=savnrr  ;
373800101001        chain s02nrr sduos02 ;
373900101001       endif  ;
374000101001       vscmag=v4cmag  ;
374100101001       vscqta=v4cqta  ;
374200101001       vscqta=v4cqta  ;
374300101001       vsctip=v4ctip  ;
374400101001       vsctru=v4ctru  ;
374500101001       vsctir=v4ctir  ;
374600101001       vscksc=v4cksc  ;
374700101001       vsdksc=v4dksc  ;
374800101001       vscdtm=v4cdtm  ;
374900101001       vscdoc=v4cdoc  ;
375000101001       vscddo=v4cddo  ;
375100101008       vscncl=v4cncl  ;
375200101008       vscpkg=v4cpkg  ;
375300101001
375400101001       exsr Aggiosfl  ;
375500101001
375600101001       s02nrr=s02nrr+1  ;
375700101001       indx=indx+1  ;
375800101001       enddo  ;
375900101001
376000101001       c02csr=s02nrr  ;
376100101001
376200101001       ENDSR;
376300080206       //--------------------------------------------------------------
376400080206       //?Operazioni finali.
376500080206       //--------------------------------------------------------------
376600080206       BEGSR RoutEnd;
376700080627
376800080704         // Chiusura pgm   ;
376900080704         clear tibs02ds  ;
377000080704         t02tla='C'      ;
377100080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
377200100903
377300100903         // Sblocco data area per pgm manut matricole
377400100907         if (§sdv0mmat='S'  and savdatblo=0) or
377500100907            (§sdv0conf='S'  and savdatblo=0) ;
377600100903         in *lock  tnsduo ;
377700100903         clear tnsduo   ;
377800100903         out tnsduo  ;
377900100903         endif  ;
378000080704
378100100920         if %open(fnlv22p) ;
378200100920         close    fnlv22p  ;
378300100920         endif  ;
378400100920
378500080206         *inLR = *on;
378600080704
378700080206         return;
378800080206
378900080206       ENDSR;
379000080206
379100080206      /end-free
379200100920     c*--------------------------------------------------------------
379300100920     c* videata richeista stampanti e ovrdbf
379400100920     c*--------------------------------------------------------------
379500100920     c     RICHSTAMPAN   BEGSR
379600140124     c* verifico la key della tabella
379700100920     C                   CLEAR                   TRUL90DS
379800100920      *
379900100920     C                   MOVEL     'S'           d90RSE
380000100920     C                   CALL      'TRUL90R'
380100100920     C                   PARM                    KPJBA
380200100920     C                   PARM                    trul90DS
380300140124     C                   PARM      'SED'         k_key             3
380400100920      *
380500100920     C* F3 - FINE
380600100920     c                   if        d90f3<>'1'
380700100920     c                   eval      comman=ovrp(1)
380800100920     c                   eval      lung=91
380900100920     c                   eval      %subst(comman:28:10)=d90pre
381000100920      *
381100100920     C*
381200100920     C                   CALL      'QCMDEXC'                            91
381300100920     C                   PARM                    COMMAN
381400100920     C                   PARM                    LUNG
381500100920      *
381600100920     C                   if        not *in91
381700100920     c                   open      fnlv22p
381800100920     c                   endif
381900100920     c                   endif
382000100920      *
382100100920     c                   ENDSR
382200080206
382300080206       //--------------------------------------------------------------
382400080206       //?Schiere a tempo di compilazione.
382500080206       //--------------------------------------------------------------
382600080206
382700080410** - MSG ------------------------------------------------------------------ -*
382800100316Utente errato                                                                   01
382900100316Data Errata                                                                     02
383000100316Magazzino Destinazione Errato                                                   03
383100100316Magazzino Correlato Errato                                                      04
383200100708Tipo unità errato                                                               05
383300100826Tipo / Matricola inesistente in anagrafica                                      06
383400100708Matricola inesistente in anagrafica                                             07
383500100708Matricola Esistente in anagrafica   ma NON univoca                              08
383600100824Scorrimento non più consentito                                                  09
383700100826L'indicazione dell'ufficio è obbligatoria                                       10
383800140124Per magazzino CED  o TLC possibile solo l'opzione "INVII"                       11
383900100908Indicare anche dtaImmissione o dtaConf. o dati Da Confermare o unità da Rendere 12
384000100826La matricola NON ci vuole per tipo unità "99"                                   13
384100140207Tipo unità "99" non accettato per "FOR" rientri fornitori o sede                14
384200101112La quantità maggiore di 1 è da immettere solo per tipo unità 99                 15
384300100826Aumento/Sostituzione unità indicabile non per tipo "99"                         16
384400100826L'unità di RESO è indicabile solo se prevista la sostituizione dell'unità       17
384500100826Tipo unità di reso errata                                                       18
384600100826Descrizione obbligatoria                                                        19
384700100930Per tipi"48 e 49"indicare Cliente destinatario unità o digita"SCORTA" nelleNOTE 20
384800100826Codice cliente errato                                                           21
384900100924La data conferma movimento non può essere maggiore di 3 mesi circa...           22
385000130321Esiste già un invio/rientro giornaliero da confermare x questo tipo/matricola   23
385100100826Tipo/matricola già inseriti in questa pagina di lavoro!!                        24
385200100830Modifica/annull. al momento non eseguibili: record vincolato da altro lavoro    25
385300100830Opzione non ammessa                                                             26
385400100831Nessun dato da visualizzare                                                     27
385500100831Indicare se Includere o Escludere il tipo unità indicato                        28
385600100831Impossibile annullare: record già confermato                                    29
385700121130Impostare data conferma movimenti                                               30
385800121130Data conferma non inferiore a oggi                                              31
385900100831In fase di conferma movimenti obbligatorio inserire la matricola                32
386000100909Numero e data documento obbligatori se RIENTRO da fornitore                     33
386100100909Per interrogare i movimenti è necessaria la matricola                           34
386200100923Matricola errata perchè di unità diversa da quella immessa e NON è modificabile 35
386300100901Tasto funzionale non ammesso in Annullamento                                    36
386400100907Opz non utilizzabile:pgm "                    "  in uso da xxxxxxxxxx il xx/xx  37
386500100903Magazzino di Destinazione e Correlato non possone essere uguali                 38
386600100908Indicare se invio o rientro da fornitore o filiale I/R/F                        39
386700100908Data AL non può essere minore data DAL                                          40
386800100908Immettere o i dati da confermare o la data conferma dal/al                      41
386900100920Il magazzino DESTINAZIONE deve essere XXX                                       42
387000101006Per opzione"9"-->Stampa Etichetta,premere tasto F10 e selezionare la stampante  43
387100100923Se immessa la matricola di reso immettere anche il TIPO unità di reso           44
387200101005Mag.Ubicazione matricola di RESO  non  uguale al mag.di INVIO. Enter forza      45
387300100923Tipo invio / rientro obbligatorio per "FOR"                                     46
387400100927Se Esclusione di tipo unità, non indicare la matricola                          47
387500100930Tasto funz. F07 abilitato solo per la ricerca matricole di RESO: posizionarsi!! 48
387600100930Matricola con tipo unità diverso da quello indicato                             49
387700101001Immettere in numero degli inserimenti da effettuare                             50
387800101001Per inserimento multiplo, immettere almeno un dato                              51
387900101012F8-Materiale preparato ammesso solo per INVII                                   52
388000120525Per opzioni: P=Materiale preparato o B=blocca stampa bolla, premere F8          53
388100101012Mancano colli e peso per il magazzino destinazione indicato                     54
388200111024Se unità in proprietà alla 923 magazzino destinazione non può essere "990 980"  55
388300110404Immettere "S" = Si per indicare il guasto fisico o lasciare vuoto               56
388400110412Nuova matricola errata perchè già esistente !!                                  57
388500110412Immettere una matricola di massino 15 caratteri                                 58
388600110610Campo note obbligatorio se guasto fisico                                        59
388700120524La stampa bolla non viene eseguita per rientro fornitore diretto in filiale     60
388800120524F15 non ammesso: il magazzino non prevede la stampa                             61
388900120525Impossibile bloccare la stampa bolla: già stampata!!                            62
389000121130Data conferma movimento no > di 3 mesi                                          63
389100121203Immettere anche data DAL per richiesta movimenti CONFERMATI                     64
389200121203Massimo un mese e possibile richiedere per i movimenti CONFERMATI               65
389300121203Impossibile modificare la data a movimenti già confermati                       66
389400130116Il magazzino di destinazione non corrisponde alla ditta manutenzione nell'anagr.67
389500130626Se unità NON in proprietà alla 923 magazzino destinazione non può essere "923"  68
389600140207La conferma dei movimenti può essere fatta solo da un utente EDP                69
389700100920**
389800100920OVRPRTF FILE(FNLV22P) OUTQ(xxxxxxxxxx) FORMTYPE('ETCH_EDP') USRDTA('Unità EDP') SHARE(*YES)
