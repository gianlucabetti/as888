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
041600161011         if %subst(knmus:1:3)<>'EDP'
041700161011         and %subst(knmus:1:3)<>'EST'    ;
041800081003         UTENonEDP=*on   ;
041900100831         endif      ;
042000100826
042100100826       // Udate + 10
042200100826       dataiso=datasys ;
042300100831       dataeur=dataiso ;
042400100903       Dataoggi=%dec(dataeur) ;
042500100924       dataiso=dataiso+ %days(200);
042600100924       datapiu200=%dec(dataiso);
042700100831 1     endif      ;
042800100827
042900100830       clear vsdopz  ;
043000121130       clear vsdopz2 ;
043100100830       clear wopz   ;
043200100830       emisMod1=*on   ;
043300100831
043400100831 1     SELECT  ;
043500100830       // SE INTERROGAZIONE
043600100831 1     when §sdv0iNT='S'   ;
043700100830        modINT=*on   ;
043800100830        protriga=*on   ;
043900100830        v1dmod='    INTERROGAZIONE     '         ;
044000120525        vsdopz='5=visualizza   8=Movimenti' ;
044100100830        wopz(1)='5' ;
044200100909        wopz(2)='8' ;
044300100830
044400100831       // Se  IMMISSIONE/MANUTENZIONE
044500100831 1     WHEN §sdv0imm='S'   ;
044600100831        ImmGES =*on   ;
044700100827        protriga=*on   ;
044800100827        v1dmod='IMMISSIONE/MANUTENZIONE'     ;
044900120525        vsdopz='2=Manut 4=Annulla 5=Visual 9=Etich P=Prepara B=BlocSTA' ;
045000121130        vsdopz2='D=ModDataConf'  ;
045100100830        wopz(1)='2' ;
045200100830        wopz(2)='4' ;
045300100830        wopz(3)='5' ;
045400100920        wopz(4)='9' ;
045500101006        wopz(5)='P' ;
045600120525        wopz(6)='B' ;
045700121130        wopz(7)='D' ;
045800100831
045900100831       // Se  IMMISSIONE/MANUTENZIONE  MATRICOLE
046000100831 1     WHEN §sdv0MMAT='S'   ;
046100100831        protriga=*on   ;
046200100831        ImmMAT  =*on   ;
046300100831        v1dmod='Immissione Cd.MATRICOLE'    ;
046400120525        vsdopz='2=Manutenz. 4=Annulla  5=Visualizza  9=Etich'  ;
046500100831        wopz(1)='2' ;
046600100831        wopz(2)='4' ;
046700100831        wopz(3)='5' ;
046800101006        wopz(4)='9' ;
046900100903        // Scrivo data area per pgm in uso
047000100903        clear savdatblo ;
047100100903         in *lock   tnsduo ;
047200100903         if §sduodat=0 ;
047300100903         §sduodat=%dec(datasys) ;
047400100903         §sduoute=knmus         ;
047500100907         §sduopgm='IMMISSIONE Matricole' ;
047600100903         else ;
047700100903         savdatblo=§sduodat  ;
047800100903         endif;
047900100903         out tnsduo  ;
048000100831
048100100831       // Se  gestione RESI
048200100831 1     WHEN §sdv0RESi='S'   ;
048300100831        protriga=*on   ;
048400100908        ModReso =*on   ;
048500100831        v1dmod='GESTIONE UNITÀ DI RESO '    ;
048600120525        vsdopz='2=Manutenz.  5=Visualizza'  ;
048700100831        wopz(1)='2' ;
048800100831        wopz(2)='5' ;
048900100831
049000100831       // Se  conferma Movimenti
049100100831 1     WHEN §sdv0CONF='S'   ;
049200100831        protriga=*on   ;
049300100831        ImmCMOV =*on   ;
049400100831        v1dmod='  CONFERMA  MOVIMENTI  '    ;
049500120525        vsdopz='E=ESCLUDIdaCONFERMA  2=Manut.  5=Visual.'  ;
049600100831        wopz(1)='2' ;
049700100831        wopz(2)='5' ;
049800100831        wopz(2)='E' ;
049900100907        // Scrivo data area per pgm in uso
050000100907        clear savdatblo ;
050100100907         in *lock   tnsduo ;
050200100907         if §sduodat=0 ;
050300100907         §sduodat=%dec(datasys) ;
050400100907         §sduoute=knmus         ;
050500100907         §sduopgm='CONFERMA   Movimenti' ;
050600100907         else ;
050700100907         savdatblo=§sduodat  ;
050800100907         endif;
050900100907         out tnsduo  ;
051000100831 1     endsl   ;
051100080703
051200080627         ENDSR;
051300080206
051400080206       //--------------------------------------------------------------
051500080206       //?Reperimento Dati del job (Utente/Operativi).
051600080206       //--------------------------------------------------------------
051700080206       BEGSR DatiJob;
051800080206
051900080206         in(E) §AzUte;
052000080206         if NOT %error;
052100080206           in(E) §DatiUte;
052200080206         endif;
052300080206         if %error or RSut = *blanks;
052400080206           clear TIBS34ds;
052500080206           tibs34r(tibs34ds);
052600080206           in §AzUte;
052700080206           in §DatiUte;
052800080206         endif;
052900080206
053000080206       ENDSR;
053100100312       //--------------------------------------------------------------
053200100312       //?Gestione viodeata 01
053300100312       //--------------------------------------------------------------
053400100312       BEGSR Gesd01;
053500100312
053600100312         // Inizializzazione videata
053700100312         if  $Inzd01 = *on;
053800100312            exsr Inzd01;
053900100312            $Inzd01  = *off;
054000100312         endif;
054100100312
054200100312
054300100312         // Emissione Testata e Piede con tasti funzionali abilitati
054400100824         if  errMessage  = *off;
054500100316           write  SDUOT01;
054600100312         endif;
054700100312
054800100312         // Emissione videata
054900100316         exfmt  SDUOD01;
055000100312
055100100312         reset errMessage;
055200100312         reset errGenerico;
055300100824         clear V1cmsg    ;
055400100315         errMAG=*off     ;
055500100315         errMGC=*off     ;
055600100315         errTIP=*off     ;
055700100315         errMAT=*off     ;
055800100315         errUTE=*off     ;
055900100315         errDIM=*off     ;
056000100826         errCUF=*off     ;
056100100909         errDTM=*off     ;
056200100909         errDTMA=*off     ;
056300100909         errIET=*off     ;
056400100923         errINVR=*off     ;
056500100312
056600100312 1       SELECT;
056700100312
056800100312         // - F3=Fine
056900100312 1         WHEN  dsp_aid = c_F03;
057000100312            $Fine = *on;
057100100312
057200100312
057300100824 x1      // Invio
057400100312           OTHER;
057500100316
057600100316           exsr CTRD01  ;
057700100316
057800100316           if ErrGenerico=*ON    ;
057900100316                 leavesr;
058000100316 2             endif;
058100100316
058200100315           $video='S2' ;
058300100825           $inzs02=*on;
058400100824
058500100312 1       ENDSL;
058600100312
058700100312       ENDSR;
058800100312
058900100316       //--------------------------------------------------------------
059000100316       //?controlli video 1
059100100316       //--------------------------------------------------------------
059200100316       BEGSR CTRD01  ;
059300100316
059400100907        in tnsduo   ;
059500100907       // Pgm non utilizzabile al momento
059600100907 1     if §sdv0resi='S' or
059700100907         (§sdv0conf='S' and savdatblo>0) or
059800100907         (§sdv0mmat='S' and savdatblo>0) or
059900100907         (§sdv0imm ='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='C')  or
060000100907         (§sdv0conf='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='I')  or
060100100907         (§sdv0mmat='S'  and §sduodat>0 and %subst(§sduopgm:1:1)='C') ;
060200100903 2      if §sduodat>0   ;
060300100907        exsr Msg37    ;
060400100903         leavesr  ;
060500100903 2     endif   ;
060600100903 1     endif   ;
060700100907
060800161011       // la conferma può essere fatta solo da un utente EDP o EST
060900161011       if ImmCMOV=*on and %subst(knmus:1:3)<>'EDP' and
061000161011           %subst(knmus:1:3)<>'EST'  ;
061100140207           errUTE=*on ;
061200140207           v1cmsg = Msg(69)  ;
061300140207           errGenerico=*on  ;
061400140207           errMessage=*on  ;
061500140207           leavesr  ;
061600140207       endif  ;
061700140207
061800100316       //   Profilo utente
061900100708 1     if v1cute<>*blanks   ;
062000100316       chain   v1cute  kpprf01l   ;
062100100316
062200100708 2     if not %found(kpprf01l)  ;
062300100316           errUTE=*on ;
062400100316           v1cmsg = Msg(1);
062500100316           errGenerico=*on  ;
062600100316           errMessage=*on  ;
062700100316           leavesr  ;
062800100824 2       endif   ;
062900100708 1     endif   ;
063000100316
063100100316       // Data Immissione
063200100831       clear w1cdim   ;
063300100708 1     if v1cdim>0   ;
063400100316             clear wlbdat;
063500100824             g02dat = v1cdim ;
063600100316             xsrda8(wlbdat);
063700100824 2           if g02err = '1';
063800100316               errMessage  = *on;
063900100316               errGenerico = *on;
064000100708               ErrDIM    = *on;
064100100824               V1cmsg = Msg(02);
064200100316               leavesr;
064300100824 2           endif;
064400100316
064500100316             v1cdim = g02dat;
064600100316             w1cdim = g02inv;
064700100708 1     endif   ;
064800100316
064900100826       // Ufficio obbligatorio
065000100826       if v1ccuf=*blanks   ;
065100100826               errMessage  = *on;
065200100826               errGenerico = *on;
065300100826               ErrCUF    = *on;
065400100826               V1cmsg = Msg(10);
065500100826               leavesr;
065600100826       endif  ;
065700100826
065800100923       // tipo invio obbligatorio per FOR solo in immissione
065900100923       if v1ccuf='FOR' and v1cinvr= ' ' and §sdv0imm='S' ;
066000100923               errMessage  = *on;
066100100923               errGenerico = *on;
066200100923               ErrINVR   = *on;
066300100923               V1cmsg = Msg(46);
066400100923               leavesr;
066500100923       endif  ;
066600100923       if v1ccuf=*blanks   ;
066700100923               errMessage  = *on;
066800100923               errGenerico = *on;
066900100923               ErrCUF    = *on;
067000100923               V1cmsg = Msg(10);
067100100923               leavesr;
067200100923       endif  ;
067300100826       // Per MAG possibile solo I=invio
067400140123       if v1ccuf<>'FOR' and v1cinvr<>'I' and v1cinvr<>' ' ;
067500100826               errMessage  = *on;
067600100826               errGenerico = *on;
067700100826               ErrCUF    = *on;
067800100826               V1cmsg = Msg(11);
067900100826               leavesr;
068000100827       endif  ;
068100100826
068200100826       // Immettere o la data immissione o solo i DA confermare
068300100831       //  o solo unità di reso
068400121203       if v1cdim=0 and v1creso =' '
068500121203          and v1cdtm=0 and v1cdtmd = 0 and (v1cconf=' ' or v1cconf='C');
068600100826               errMessage  = *on;
068700100826               errGenerico = *on;
068800100826               ErrDIM    = *on;
068900100826               V1cmsg = Msg(12);
069000100826               leavesr;
069100100827       endif  ;
069200100831
069300100831       // Data conferma movimento: obbligatoria in conferma
069400100831       clear w1cdtm  ;
069500100831 1     if v1cdtm=0  and §sdv0conf='S' ;
069600100831               errMessage  = *on;
069700100831               errGenerico = *on;
069800100831               ErrDTM    = *on;
069900100831               V1cmsg = Msg(30);
070000100831               leavesr;
070100100831 2     endif;
070200100908
070300100831       if v1cdtm>0  ;
070400100831             clear wlbdat;
070500100831             g02dat = v1cdtm ;
070600100831             xsrda8(wlbdat);
070700100831 2           if g02err = '1';
070800100831               errMessage  = *on;
070900100831               errGenerico = *on;
071000100831               ErrDTM    = *on;
071100100831               V1cmsg = Msg(02);
071200100831               leavesr;
071300100831 2           endif;
071400100831
071500100831       // non inferiore a oggi
071600100831       if g02inv<%dec(datasys)  ;
071700100831               errMessage  = *on;
071800100831               errGenerico = *on;
071900100831               ErrDTM    = *on;
072000100831               V1cmsg = Msg(31);
072100100831               leavesr;
072200100831       endif   ;
072300100831             v1cdtm = g02dat;
072400100831             w1cdtm = g02inv;
072500100831 1     endif   ;
072600100908
072700100908       // data conferma movimenti dal / al
072800100908       clear w1cdtmd ;
072900100908       clear w1cdtma ;
073000121203       clear w1ctgid ;
073100121203       clear w1ctgia ;
073200101112       // Incongruente se richisti  movimenti DA confermare
073300110914       if (v1cdtmd>0 or v1cdtma>0) and v1cconf='S'  and ModInt=*on  ;
073400100908               errMessage  = *on;
073500100908               errGenerico = *on;
073600100908               ErrDTM    = *on;
073700100908               V1cmsg = Msg(41);
073800100908               leavesr;
073900100908       endif   ;
074000100908
074100100908 1     if v1cdtmd >0  ;
074200100908             clear wlbdat;
074300100908             g02dat = v1cdtmd;
074400100908             xsrda8(wlbdat);
074500100908 2           if g02err = '1';
074600100908               errMessage  = *on;
074700100908               errGenerico = *on;
074800100908               ErrDTM    = *on;
074900100908               V1cmsg = Msg(02);
075000100908               leavesr;
075100100908 2           endif;
075200100908             v1cdtmd= g02dat;
075300100908             w1cdtmd= g02inv;
075400121203             w1cTGId= g02TGI;
075500100908       endif   ;
075600100831
075700100908 1     if v1cdtma >0  ;
075800100908             clear wlbdat;
075900100908             g02dat = v1cdtma;
076000100908             xsrda8(wlbdat);
076100100908 2           if g02err = '1';
076200100908               errMessage  = *on;
076300100908               errGenerico = *on;
076400100908               ErrDTMA   = *on;
076500100908               V1cmsg = Msg(02);
076600100908               leavesr;
076700100908 2           endif;
076800100908             v1cdtma= g02dat;
076900100908             w1cdtma= g02inv;
077000121203             w1cTGIA= g02TGI;
077100100908
077200100908       // Data A non minore data DAL
077300100908 2     if w1cdtma<w1cdtmd  ;
077400100908               errMessage  = *on;
077500100908               errGenerico = *on;
077600100908               ErrDTMA   = *on;
077700100908               V1cmsg = Msg(40);
077800100908               leavesr;
077900100908 2     endif  ;
078000100908 1     endif   ;
078100121203
078200121203       // Se richIESTi i confermati max i mese posso richiedere
078300121203        if v1cconf<>'S' and v1cdtmd>0  and v1cdtma>0  ;
078400121203          WDIFFE=w1ctgia-w1ctgid  ;
078500121203          if wdiffe>32  ;
078600121203               errMessage  = *on;
078700121203               errGenerico = *on;
078800121203               ErrDTMA   = *on;
078900121203               V1cmsg = Msg(65)  ;
079000121203               leavesr;
079100121203          endif  ;
079200121203        endif  ;
079300100908
079400100316       // Magazzino destinazione
079500100708 1     if v1cmag<>*blanks and v1cmag<>'000' ;
079600100316
079700100909       wtmag='D'      ;
079800100908       wazorg='E  '   ;
079900100708       wmag=v1cmag    ;
080000100824       exsr  chkmag   ;
080100100824       v1cmag=wmag    ;
080200100316
080300100708       if  ErrGenerico=*ON  ;
080400100708         ErrMag=*on  ;
080500100316         leavesr  ;
080600100708       endif    ;
080700100708       else  ;
080800100708       clear v1cmag  ;
080900100708 1     endif    ;
081000100708
081100100708       // Magazzino correlato
081200100708 1     if v1cmgc<>*blanks  and v1cmgc<>'000'   ;
081300100708
081400100909       wtmag='C'      ;
081500100908       wazorg='E  '   ;
081600100708       wmag=v1cmgc    ;
081700100708       exsr  Chkmag   ;
081800100824       v1cmgc=wmag  ;
081900100708
082000100826       if  ErrGenerico=*ON  ;
082100100708         ErrMgc=*on  ;
082200100708         leavesr  ;
082300100708       endif    ;
082400100708       else  ;
082500100708       clear v1cmgc  ;
082600100708       endif    ;
082700100316
082800100708       // Tipo unita
082900100824       clear v1dtip  ;
083000100831 1     if v1ctip<>*blanks ;
083100100708         wtip=v1ctip   ;
083200100708         exsr chktip   ;
083300100824
083400100824         v1ctip=wtip  ;
083500100824         v1dtip=wdtip  ;
083600100827 2       if Errgenerico=*on  ;
083700100826          errTIP=*on       ;
083800100827 3       if ErrMessage =*on  ;
083900100826          v1cmsg=msg(05)  ;
084000100827 3       endif    ;
084100100826          leavesr  ;
084200100827 2       endif    ;
084300100831
084400100831       // Immettere anche Includi o Escludi
084500100831 2     if v1ciet=' '  ;
084600100831          errIET=*on       ;
084700100927          ErrGenerico=*on  ;
084800100927 3        ErrMessage =*on  ;
084900100831          v1cmsg=msg(28)  ;
085000100831          leavesr  ;
085100100831 2     endif   ;
085200100831
085300100827 x1    else  ;
085400100708       clear v1ctip  ;
085500100831       clear v1ciet  ;
085600100708 1     endif    ;
085700100927       // Se esclusione di un tipo, impossibile indciare la matricola
085800100927       if v1ciet='E' and v1ctip>*zeros  and v1cmat<>*blanks  ;
085900100927         Errgenerico=*on  ;
086000100927         Errmessage =*on  ;
086100100927         Errmat=*on  ;
086200100927         v1cmsg=msg(47)  ;
086300100927         leavesr  ;
086400100927 3     endif    ;
086500100708
086600100708       // Matricola
086700100708 1     if v1cmat<>*blanks  ;
086800100708
086900100824 2     if v1ctip>*zeros   ;
087000100824       ktip=%int(v1ctip)  ;
087100100930       setll (v1cmat:ktip)  unana07l    ;
087200100930 3     if not %equal(unana07l)  ;
087300100708         Errgenerico=*on  ;
087400100708         Errmessage =*on  ;
087500100708         Errmat=*on  ;
087600100708         v1cmsg=msg(06)  ;
087700100708         leavesr  ;
087800100708 3     endif    ;
087900100708
088000100708 x2    else   ;
088100100708       setll (v1cmat)  unana07l    ;
088200100708 3     if not %equal(unana07l)  ;
088300100708         Errgenerico=*on  ;
088400100708         Errmessage =*on  ;
088500100708         Errmat=*on  ;
088600100708         v1cmsg=msg(07)  ;
088700100708         leavesr  ;
088800100927 x3    //else   ;
088900100708
089000100708       // controllo se univoca
089100100927       //clear  qtatem   ;
089200100927       //reade v1cmat   unana07l   ;
089300100927 4     //dow  not %eof(unana07l) ;
089400100927 5     //if qtatem=0   ;
089500100927       //qtatem=qtatem+1  ;
089600100927       //v1ctip=%editc(unatip:'X') ;
089700100927 x5    //else   ;
089800100927       //  Errgenerico=*on  ;
089900100927       //  Errmessage =*on  ;
090000100927        // Errmat=*on  ;
090100100927        // v1cmsg=msg(08)  ;
090200100927       //leave  ;
090300100927 5     //endif   ;
090400100927       //reade v1cmat   unana07l   ;
090500100927 4     //enddo   ;
090600100708 3     endif    ;
090700100708 2     endif    ;
090800100708 1     endif    ;
090900100708
091000100316       ENDSR  ;
091100080206       //--------------------------------------------------------------
091200100312       //?Gestione SFL 02
091300080206       //--------------------------------------------------------------
091400100312       BEGSR GesS02;
091500080207
091600080207         // Inizializzazione videata
091700100312         if  $InzS02 = *on;
091800100312            exsr InzS02;
091900100824            $InzS02  = *off;
092000100824         endif;
092100100827
092200100831         // INTERRROGAZIONE: se non trovati record errore in prima videata
092300100831         if s02nrr=0   ;
092400100831         v1cmsg=msg(27)  ;
092500100831         errGenerico=*on  ;
092600100831         ErrMessage=*on  ;
092700100831         $Video='D1'  ;
092800100831         leavesr   ;
092900100831         endif  ;
093000100831
093100100831         // Posizionamento cursore
093200101005 1       if  C02csr  > *zero ;
093300101005
093400100924         // se non ci sono errori e il record +1 è vuoto, mi posiziono qui
093500101005 2       if ErrGenerico =*off ;
093600101005
093700101005 2a      if §sdv0imm='S'    ;
093800100924           s02nrr=c02csr    ;
093900100924           chain s02nrr sduos02   ;
094000101005 3         if %found ;
094100100927            exsr Aggiosfl  ;
094200101027            else  ;
094300101027            // Se non trovato riporto indietro di uno il campo per visulizzare il sfl
094400101027            C02csr=c02csr-1  ;
094500101005 3         endif  ;
094600101005 3         if %found and vsctip>=*zeros ;
094700100927
094800100924            s02nrr=c02csr+1  ;
094900100924            chain s02nrr sduos02   ;
095000101005 4          if %found and vshimm='S'  and vsctip<=*zeros ;
095100100924             c02csr=s02nrr ;
095200101005 4          endif ;
095300101005 4          if %found ;
095400100927             exsr Aggiosfl  ;
095500101005 4         endif  ;
095600101005 3         endif  ;
095700101005 2a      endif  ;
095800101005
095900101005         // Per immissione matricole mi posiziono sul record senza matricola
096000101005 2a      if §sdv0MMAT='S'    ;
096100101005           s02nrr=c02csr    ;
096200101005           chain s02nrr sduos02   ;
096300101005 3         if %found ;
096400101005            exsr Aggiosfl  ;
096500101005 3         endif  ;
096600101005 3         if %found and vscmat<>*blanks  ;
096700101005
096800101005            s02nrr=c02csr+1  ;
096900101005            chain s02nrr sduos02   ;
097000101005 4          if %found and vscmat=*blanks  ;
097100101005             c02csr=s02nrr ;
097200101005 4          endif ;
097300101005 4          if %found ;
097400101005             exsr Aggiosfl  ;
097500101005 4         endif  ;
097600101005 3         endif  ;
097700101005 2a      endif  ;
097800101005
097900101005 2       endif  ;
098000100924
098100100827           C02rcd = C02csr;
098200101005 1       endif  ;
098300080207
098400080207         // Emissione Testata e Piede con tasti funzionali abilitati
098500100824         if  errMessage  = *off;
098600100824           write  SDUOT01;
098700100824           write  SDUOZ02;
098800080207         endif;
098900080703
099000080207         // Emissione videata
099100100316         exfmt  SDUOC02;
099200080410
099300080207         reset errMessage;
099400080207         reset errGenerico;
099500100824         clear V1cmsg;
099600100830         errMAG=*off     ;
099700100830         errMGC=*off     ;
099800100830         errTIP=*off     ;
099900100830         errMAT=*off     ;
100000100830         errQTA=*off     ;
100100100830         errTRU=*off     ;
100200100830         errTIR=*off     ;
100300100830         errDES=*off     ;
100400110412         errNOT=*off     ;
100500100830         errKSC=*off     ;
100600100830         errDTM=*off     ;
100700100909         errDDO=*off     ;
100800101012         errNCL=*off     ;
100900100923         errMATR=*off     ;
101000100909         errINVR=*off     ;
101100100927         s02nrr=1  ;
101200080207
101300080523 1       SELECT;
101400080207
101500080207         // - F3=Fine
101600080523 1         WHEN  dsp_aid = c_F03;
101700080409            $Fine = *on;
101800080207
101900080207         // - F12=Ritorno
102000080523 1         WHEN  dsp_aid = c_F12;
102100100315           $video='D1'   ;
102200100824
102300100824         // - Roll-Up
102400100824           WHEN  dsp_aid = c_RollUp;
102500100831         if §sdv0imm= 'S'  ;
102600100824           // - Verifica se raggiunto il limite di records registrabili
102700100824           //   nel subfile (9999)
102800100824             if  S02nrr = *hival;
102900100824               errMessage  = *on;
103000100824               errGenerico = *on;
103100100824               V1cmsg = Msg(09);
103200100824             else;
103300100824               exsr RollUpS02;
103400100824             endif;
103500100830             endif;
103600100901
103700100901         // - F2=Gestione Unità EDP
103800100901 1         WHEN  dsp_aid = c_F02;
103900100924           clear dsunana ;
104000100924           chain c02csr sduos02 ;
104100100927           if %found ;
104200100927           exsr Aggiosfl  ;
104300100927           endif   ;
104400100928           if %found  and vscmat<>*blanks  and
104500100928 2          (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
104600100924              UIMmat=vscmat  ;
104700100927             endif  ;
104800100928           if %found  and vscmatr<>*blanks  and
104900140123 2          (rnmfl= 'VSCMATR ' and v1ccuf<>'FOR' )  ;
105000100928              UIMmat=vscmatr ;
105100100928             endif  ;
105200100901           exsr CallGESUNIT     ;
105300100824
105400100901         // - F7=Ricerca Matricola
105500100901 1         WHEN  dsp_aid = c_F07;
105600100901           chain c02csr sduos02 ;
105700100901           if %found   ;
105800100901           exsr CallRicMat      ;
105900100901           endif  ;
106000100920
106100100901
106200100901         // - F14=Gestione tabella scheda clienti x scambio dati
106300100901 1         WHEN  dsp_aid = c_F14;
106400100901           exsr Callpgm3R       ;
106500100909
106600101001         // - F15=Immissione multipla
106700101001 1         WHEN  dsp_aid = c_F15;
106800101001            $VIDEO='D4'   ;
106900101001            exsr Gesd04  ;
107000101001
107100110404         // - F21=Abilita campo opzione
107200100909 1         WHEN  dsp_aid = c_F21;
107300100909           chain c02csr sduos02 ;
107400100909           if %found   ;
107500100909           exsr Protcampi  ;
107600100909           exsr Aggiosfl    ;
107700100909           endif  ;
107800100909
107900100901
108000080530 x1      // Invio / F6
108100080207           OTHER;
108200081006
108300100315               exsr ContrS02 ;
108400100930              if %open(fnlv22p) ;
108500100930               close    fnlv22p  ;
108600100930              endif  ;
108700100930
108800100830 2             if  errGenerico = *on or wSceltaOPZ='S';
108900120524               if wsceltaOPZ='S'  ;
109000120524                 Errmessage=*off   ;
109100120524               endif  ;
109200081006                 leavesr;
109300100824 2             endif;
109400080530
109500121130         // se non ci sono errori--> pulisco la data modifica conferma mov
109600121130               clear w05dtm  ;
109700121203               clear w05amg_dtm  ;
109800121130
109900100907         // F6=conferma Aggiornamento
110000100920 1         if   dsp_aid = c_F06 ;
110100100907
110200100907         // Non posso confermare immissione se pgm CONFERMA in uso
110300100907           if   §sdv0imm='S' ;
110400100907            in tnsduo   ;
110500100907            if §sduodat>0 and %subst(§sduopgm:1:1)='C'   ;
110600100907            exsr Msg37  ;
110700100907            leavesr     ;
110800100907           endif  ;
110900100907           endif  ;
111000100907
111100080702           exsr   ConfAggio ;
111200081023
111300100315           $video='D1'   ;
111400080530           endif  ;
111500080207
111600080523 1       ENDSL;
111700080207
111800080207       ENDSR;
111900100901       //--------------------------------------------------------------
112000100901       //?chiamata pgm gestione unità EDP
112100100901       //--------------------------------------------------------------
112200100901       BEGSR CallGesUNIT ;
112300100901
112400100901       clear dsunit  ;
112500100901       dsupro='A'  ;
112600100901       if §sdv0int='S'   ;
112700100901       dsuop0=' 5'  ;
112800100901       else  ;
112900100901       dsuop0=' 2'  ;
113000100901       endif  ;
113100100901
113200100901       clear kpjbu  ;
113300100901       kpjbu=dsunit  ;
113400100924       callp TNSDU1R  (kpjba:dsunana)  ;
113500100901
113600100901       ENDSR;
113700080207
113800100901       //--------------------------------------------------------------
113900100901       //?chiamata pgm ricerca matricola unità EDP
114000100901       //--------------------------------------------------------------
114100100901       BEGSR CallricMat  ;
114200100901
114300100901       clear kpjbu  ;
114400100901       clear dsunit  ;
114500100927       clear dsunana;
114600100930
114700100930       // Nella gestione RESI si può fare F7 solo sulla mmatricola di reso
114800100927 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
114900100930 3        If ModReso=*on  ;
115000100930          Errgenerico=*on  ;
115100100930          ErrMessage=*on  ;
115200100930          v1cmsg=msg(48)  ;
115300100930          exsr Aggiosfl  ;
115400100930          leavesr;
115500100930  3       endif  ;
115600100930
115700100927       uimmat=vscmat  ;
115800100927       else   ;
115900100927       uimmat=vscmatr ;
116000100927       endif  ;
116100100901       dsupro='A'  ;
116200100901       dsuop0=' 1'  ;
116300100901
116400100901       kpjbu=dsunit  ;
116500100924       callp TNSDU1R  (kpjba:dsunana)  ;
116600100901       dsunit=kpjbu    ;
116700100901
116800100927 1     if uimtip>0 and uimcod>0  ;
116900100930
117000100901       // In manutenzione se selezionato tipo unità diverso --> errore
117100100923 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
117200100923
117300100930 3     if vsctip<=*zeros  ;
117400100901       vsctip='00' ;
117500100930 x3    else   ;
117600100901
117700100930 4     if  uimtip<>%int(vsctip)  ;
117800100901        ErrGenerico=*on  ;
117900100901        ErrMessage=*on  ;
118000100930        if vshimm=' '  ;
118100100901        v1cmsg=msg(35)  ;
118200100930        else  ;
118300100930        v1cmsg=msg(49)  ;
118400100930        endif  ;
118500100901        ErrMat=*on  ;
118600100901           write  SDUOT01;
118700100901           write  SDUOZ02;
118800100901           exsr Aggiosfl  ;
118900100901           leavesr   ;
119000100930 4     endif ;
119100100930 3     endif ;
119200100930 x2    else  ;
119300100930 3     if vsctir<=*zeros  ;
119400100930       vsctir='00' ;
119500100930 x3    else   ;
119600100930
119700100930 4     if  uimtip<>%int(vsctir)  ;
119800100930        ErrGenerico=*on  ;
119900100930        ErrMessage=*on  ;
120000100930        v1cmsg=msg(49)  ;
120100100930        ErrMatr=*on  ;
120200100930           write  SDUOT01;
120300100930           write  SDUOZ02;
120400100930           exsr Aggiosfl  ;
120500100930           leavesr   ;
120600100930 4     endif ;
120700100930 3     endif ;
120800100923 2     endif ;
120900100923
121000100927         chain (uimtip:uimcod) unana01l  ;
121100100923 2       if %found(unana01l)  ;
121200100927           wtip=%editc(uimtip:'X') ;
121300100901           exsr chktip  ;
121400100923
121500100923       // se sono posizionato su MATR imposto matricola di reso
121600100923       //  in ogni altro caso quella normale
121700100923 3     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
121800100923           vsctip=%editc(unatip:'X')   ;
121900100901           vsdtip=wdtip  ;
122000100901           vscmat=unamat  ;
122100100923 x3    else  ;
122200100923           vsctir=%editc(unatip:'X')   ;
122300100923           vsdtir=wdtip  ;
122400100923           vscmatr=unamat  ;
122500100923 3     endif ;
122600100923 2     endif   ;
122700100901 1     endif   ;
122800100923
122900100901       exsr Aggiosfl  ;
123000100901
123100100901       ENDSR;
123200100901
123300100901       //--------------------------------------------------------------
123400100901       //?chiamata pgm gesitone tabella scheda clienti per scambio dati
123500100901       //--------------------------------------------------------------
123600100901       BEGSR Callpgm3R   ;
123700100901       clear kpjbu  ;
123800140424       callp tnsdunr  (kpjba)  ;
123900100901       ENDSR;
124000100901
124100100920       //--------------------------------------------------------------
124200100920       //?Stampa etichette
124300100920       //--------------------------------------------------------------
124400100920       BEGSR staEti      ;
124500100920
124600100920       // La prima volta devo proporre la richeista stampati
124700100920       if wrichtrul90=' '  ;
124800100920       exsr RichStampan  ;
124900100920       if d90f3<>'1' and not *in91   ;
125000100920       wrichtrul90='S'  ;
125100100920       endif  ;
125200100920       endif  ;
125300100920
125400100920
125500100920       ENDSR  ;
125600100824       //--------------------------------------------------------------
125700100824       //?Caricamento pagina vuota
125800100824       //--------------------------------------------------------------
125900100824       BEGSR RollUpS02;
126000100824         S02nrr   = totrig ;
126100100824
126200100824         // Scrittura righe vuote
126300100824         exsr Righevuote  ;
126400100824
126500100824       ENDSR;
126600080526       //--------------------------------------------------------------
126700100312       //?controlli SFL02
126800080409       //--------------------------------------------------------------
126900100312       BEGSR ContrS02;
127000100315
127100100920         // - F10=Stampa etichette chiedo la stampante
127200100920 1       if      dsp_aid = c_F10;
127300100920           exsr StaEti          ;
127400100920         endif  ;
127500100920
127600100830       clear  WsceltaOPZ   ;
127700081126       yy=1           ;
127800100315       s02nrr=1  ;
127900100824       readc       sduos02    ;
128000080703
128100100824 0     dow not %eof ;
128200100830
128300100831       // RECORD GIÀ INSERITO: CONTROLLO L'OPZIONE
128400100831 1     IF vshimm=' '  and vscopz<>' '
128500100831          and  vscopz<>'E'   ;
128600100830
128700100830       // controllo opzione
128800100830       Indx=%lookup(vscopz:wopz)  ;
128900100831 2     if Indx=0  ;
129000100830         v1cmsg=msg(26)  ;
129100100830         ErrGenerico=*on  ;
129200100830         ErrMessage =*on  ;
129300100830         EXSR AggioSFL   ;
129400100830         leavesr  ;
129500100831 2     endif   ;
129600100920
129700101006       select   ;
129800101006
129900101006       // Per F08 imposto la 'P' di materiale preparato e metto il R.I. in magazzio destinaz.
130000101006 2     when      dsp_aid = c_F08 ;
130100101012       // Se no INVIO tasto funzionale non ammesso
130200120525       //if vscinvr<>'I'    ;
130300120525       //  v1cmsg=msg(52)  ;
130400120525       //  ErrGenerico=*on  ;
130500120525       //  ErrMessage =*on  ;
130600120525       //  EXSR AggioSFL   ;
130700120525       //  leavesr  ;
130800120525       //endif   ;
130900101006
131000101006 3     if vscopz='P'  ;
131100101006           chain(e) vshnrr   ungio00f   ;
131200101006 4          if %error or not %found ;
131300101006             v1cmsg=msg(25)  ;
131400101006             ErrGenerico=*on  ;
131500101006             ErrMessage =*on  ;
131600101006             EXSR AggioSFL   ;
131700101006             leavesr  ;
131800101006 x4          else  ;
131900101006             vshfpr='P'  ;
132000101006             ungfpr='P'  ;
132100101006             update ungioFIS  ;
132200101006 4          endif  ;
132300120525          CLEAR VSCOPZ  ;
132400120525 3      endif  ;
132500120525
132600120525 3     if vscopz='B'  ;
132700120525           chain(e) vshnrr   ungio00f   ;
132800120525 4          if %error or not %found ;
132900120525             v1cmsg=msg(25)  ;
133000120525             ErrGenerico=*on  ;
133100120525             ErrMessage =*on  ;
133200120525             EXSR AggioSFL   ;
133300120525             leavesr  ;
133400120525 x4          else  ;
133500120525             if ungfst='S' ;
133600120525             v1cmsg=msg(62)  ;
133700120525             ErrGenerico=*on  ;
133800120525             ErrMessage =*on  ;
133900120525             EXSR AggioSFL   ;
134000120525             leavesr  ;
134100120525 x4          else  ;
134200120525             vshfst='N'  ;
134300120525             ungfst='N'  ;
134400120525             update ungioFIS  ;
134500120525 4          endif  ;
134600120525 4          endif  ;
134700120525          CLEAR VSCOPZ  ;
134800120525 3      endif  ;
134900101006
135000101006        exsr Aggiosfl    ;
135100121130
135200101006
135300100920       // Per F10 elaboro solo opzioni di stampa  le opzioni diverse da 9 le lascio
135400101006 2     when      dsp_aid = c_F10 and d90f3<>'1';
135500100920 3     if vscopz='9'  ;
135600100930         if not %open(fnlv22p)  ;
135700100930         open fnlv22p ;
135800100930         endif        ;
135900100930
136000100920         clear dsetiunit   ;
136100100920         §eunitmag=%editc(vscmag:'X') ;
136200100920         // Decodifico magazzino
136300100920          clear wazorg  ;
136400100920          wtmag ='D  '   ;
136500100920          wmag=%editc(vscmag:'X')  ;
136600100920          exsr  chkmag   ;
136700100920         §eunitdmag=wdmag              ;
136800100920         §eunitksc =%editc(vscksc:'X') ;
136900100920          chain   (1:dutkci:vscksc) cnaco00f   ;
137000100920         §eunitdksc=acorag             ;
137100101028
137200101028         select  ;
137300101028         // Imposto il campo NOTE
137400101028         when vscnot<>*blanks  ;
137500101028          §eunitnot =vscnot             ;
137600101028         //   se vuoto e tipo 99 -->quantità + descrizione
137700101028         when vsctip='99'  ;
137800101028          §eunitnot =%editc(vscqta:'X') + ' ' + vscdes  ;
137900101028         //   se vuoto e tipo <> 99 -->solo descrizione
138000101028         when vscdes<>*blanks  ;
138100101028          §eunitnot =vscdes             ;
138200101028         other   ;
138300101028         //   se vuota anche descrizione tipo e decodifica
138400101028          §eunitnot=vsctip + '-' + vsdtip   ;
138500101028         endsl   ;
138600100920         callp FNLVUEZS  (dsetiunit) ;
138700100920
138800100920         CLEAR VSCOPZ  ;
138900100920  3    endif  ;
139000100920       exsr Aggiosfl    ;
139100100920
139200121130
139300121130         // opz "D" --> visualizzo Window per richiedere la data
139400121130         //             solo la prima volta
139500121130
139600121130 2       when  vscopz='D'   ;
139700121203
139800121203  3      if vshfmv='S'   ;
139900121203         v1cmsg=msg(66)  ;
140000121203         ErrGenerico=*on  ;
140100121203         ErrMessage =*on  ;
140200121203         EXSR AggioSFL   ;
140300121203         leavesr  ;
140400121203         endif  ;
140500121130
140600121130 4       if w05dtm= 0 ;
140700121130
140800121130         I05sys=%dec(datasys)  ;
140900121130         kpjbu=tnsduods_1 ;
141000121130
141100121130         TNSDUOR1  (kpjba)  ;
141200121130
141300121130         tnsduods_1 = kpjbu   ;
141400121130
141500121203          if   O05F06='1' and o05gma_dtm>0  ;
141600121203           w05dtm=o05gma_dtm  ;
141700121203           w05amg_dtm=o05amg_dtm  ;
141800121130          endif  ;
141900121203
142000121203          if   w05dtm=0   ;
142100121203             ErrGenerico=*on  ;
142200121203             CLEAR VSCOPZ  ;
142300121203             EXSR AggioSFL   ;
142400121203             leavesr  ;
142500121203             endif  ;
142600121203
142700121130
142800121130 4       endif   ;
142900121130
143000121130 4       if w05dtm>0  ;
143100121130           chain(e) vshnrr   ungio00f   ;
143200121130 5          if %error or not %found ;
143300121130             v1cmsg=msg(25)  ;
143400121130             ErrGenerico=*on  ;
143500121130             ErrMessage =*on  ;
143600121130             EXSR AggioSFL   ;
143700121130             leavesr  ;
143800121130 x5          else  ;
143900121130             vscdtm=w05dtm ;
144000121203             ungdtm=w05amg_dtm ;
144100121130             update ungioFIS  ;
144200121130
144300121130 5          endif  ;
144400121130 4          endif  ;
144500121130
144600121130          CLEAR VSCOPZ  ;
144700121130         EXSR AggioSFL   ;
144800121130       dsp_aid= savinkf  ;
144900121130  x2   other    ;
145000100831
145100100831       // non posso annullare record confermato
145200100920  3    if vshimm=' '  and vshfmv='S' and vscopz='4'  ;
145300100831         v1cmsg=msg(29)  ;
145400100831         ErrGenerico=*on  ;
145500100831         ErrMessage =*on  ;
145600100831         EXSR AggioSFL   ;
145700100831         leavesr  ;
145800100920  3    endif  ;
145900101006       // Per opzione '9' premere tasto funzionale F9
146000100920  3    if vshimm=' '  and   vscopz='9'  ;
146100100920         v1cmsg=msg(43)  ;
146200100920         ErrGenerico=*on  ;
146300100920         ErrMessage =*on  ;
146400100920         EXSR AggioSFL   ;
146500100920         leavesr  ;
146600121130        endif  ;
146700121130
146800101006       // Per opzione 'P' premere tasto funzionale F8
146900101006  3    if vshimm=' '  and   vscopz='P'  ;
147000101012       if vscinvr='I'   ;
147100101006         v1cmsg=msg(53)  ;
147200101006         else  ;
147300101006         v1cmsg=msg(52)  ;
147400101006         endif  ;
147500101006         ErrGenerico=*on  ;
147600101006         ErrMessage =*on  ;
147700101006         EXSR AggioSFL   ;
147800101006         leavesr  ;
147900101006  3    endif  ;
148000120525       // Per opzione 'B' premere tasto funzionale F8
148100120525  3    if vshimm=' '  and   vscopz='B'  ;
148200120525         v1cmsg=msg(53)  ;
148300120525         ErrGenerico=*on  ;
148400120525         ErrMessage =*on  ;
148500120525         EXSR AggioSFL   ;
148600120525         leavesr  ;
148700120525  3    endif  ;
148800100903
148900100921       // non posso Manutenzionare  o annullare se blocco data area
149000100921  3    if vshimm=' '  and  (vscopz='2' or vscopz='4') ;
149100100903        in tnsduo   ;
149200100909        if §sduodat>0 and (§sdv0resi='S' or §sdv0imm='S' )  ;
149300100907        exsr Msg37    ;
149400100909        clear vscopz  ;
149500100907
149600100903         EXSR AggioSFL   ;
149700100903         leavesr  ;
149800100920       endif   ;
149900100920 3     endif  ;
150000100830
150100100909       //  Alloco il record non per opz "5"  e "8"
150200100920 3       if vscopz='5' or vscopz='8'  ;
150300100830         chain(n) vshnrr   ungio00f   ;
150400100830         else  ;
150500100830         chain(e) vshnrr   ungio00f   ;
150600100920 3       endif  ;
150700100920 3       if %error or not %found ;
150800100830         v1cmsg=msg(25)  ;
150900100830         ErrGenerico=*on  ;
151000100830         ErrMessage =*on  ;
151100100830         EXSR AggioSFL   ;
151200100830         leavesr  ;
151300100920 3       endif  ;
151400100909
151500100909         // opz "8" --> interroga movimenti
151600100920 3       if vscopz='8'   ;
151700100909         exsr IntMovim  ;
151800100909         clear vscopz   ;
151900100909         EXSR AggioSFL   ;
152000100909         leavesr  ;
152100100920 3       endif  ;
152200121130
152300100830
152400100830        $VIDEO='D3'   ;
152500100830        exsr Gesd03  ;
152600100830
152700100830        wSceltaOPZ='S'  ;
152800100830        clear vscopz ;
152900100830         EXSR AggioSFL   ;
153000100830
153100100920 3     If ErrGenerico=*on  ;
153200100830         leavesr  ;
153300100920 3     endif  ;
153400101006 2     endsl ;
153500100830
153600100830 x1    else ;
153700100831
153800100924       // I record esclusi dalla conferma e annuillati non vengono controllati
153900100924  2    if vscopz<>'E'  and vshann<>'S' ;
154000100831
154100100830       // RECORD DI NUOVA IMMISSIONE: CONTROLLO
154200100830       exsr ContrRIGA  ;
154300100907       if  ErrGenerico=*on  ;
154400100907       leavesr ;
154500100907       endif  ;
154600100830
154700100830         // Per record inserito imposto utente e data ins
154800100831  3      if ErrGenerico=*off and vshimm='S' and vsctip<>*blanks ;
154900100830         vscute=KNMUS  ;
155000100830         datadmy=datasys ;
155100100830         dataeur=datasys ;
155200100830          vscdim=%dec(dataeur) ;
155300100830          w0060 =%dec(datadmy) ;
155400100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
155500100830                  %subst(%editc(w0060:'X'):3:2)    ;
155600100830
155700100831  3      endif    ;
155800100831  2      endif    ;
155900100831
156000100831   2    if ErrGenerico=*off ;
156100100831         EXSR AggioSFL   ;
156200100831   2    endif   ;
156300100831  1    endif   ;
156400100830
156500100830       readc    sduos02    ;
156600100830 0     enddo         ;
156700100830
156800100830       ENDSR  ;
156900081126
157000100907       //--------------------------------------------------------------
157100100907       //?scrivo msg 37 di blocco manutenzione
157200100907       //--------------------------------------------------------------
157300100907        BEGSR Msg37  ;
157400100907         v1cmsg=msg(37)  ;
157500100907         dataiso=%date(§sduodat) ;
157600100907         datadmy=dataiso      ;
157700100907         W006a=%editc(%dec(datadmy):'X') ;
157800100907         %subst(v1cmsg:74:2)=%subst(w006a:1:2) ;
157900100907         %subst(v1cmsg:77:2)=%subst(w006a:3:2) ;
158000100907         %subst(v1cmsg:60:10)=§sduoute          ;
158100100907         %subst(v1cmsg:27:20)=§sduopgm          ;
158200100907         ErrGenerico=*on  ;
158300100907         ErrMessage =*on  ;
158400100907        ENDSR  ;
158500100909       //--------------------------------------------------------------
158600100909       //?Interroga movimenti della unit/matricola
158700100909       //--------------------------------------------------------------
158800100909        BEGSR IntMovim;
158900100909
159000100924        // Se manca la matricola non sono interrogabili
159100100909        if vscmat=*blanks  ;
159200100909         v1cmsg=msg(34)  ;
159300100909         ErrGenerico=*on  ;
159400100909         ErrMessage =*on  ;
159500100909        else  ;
159600100909
159700100909        clear dsunit ;
159800100909        dsuop0=' 5'  ;
159900100909        dsutip=%int(vsctip)  ;
160000100930        chain (vscmat:dsutip)  unana07l    ;
160100100930        if %found(unana07l)  ;
160200110412         dsucod=unacod   ;
160300110412        else  ;
160400110412        // se non trovato provo con nuova matricola per FOR "R"
160500110412           if ProtperFOR=*on and vscnot<>*blanks   ;
160600110412           kmat=vscnot  ;
160700110412           chain (kmat:dsutip)  unana07l    ;
160800110412           if %found(unana07l)  ;
160900110412            dsucod=unacod   ;
161000110412           endif  ;
161100110412           endif  ;
161200110412
161300100909        endif  ;
161400100910        // solo se confermato, passo anche la data movimento
161500100910        if vshfmv='S'   ;
161600100910         datadmy=%date(vscdtm:*dmy)   ;
161700100910         dataiso=datadmy  ;
161800100910         dsudtm=%dec(dataiso) ;
161900100910        endif  ;
162000100909        kpjbu=dsunit   ;
162100100909        callp tnsdu4r   (kpjba)   ;
162200100909        ErrGenerico=*on  ;
162300100909        endif   ;
162400100909        ENDSR  ;
162500100830       //--------------------------------------------------------------
162600100830       //?controllo campi della riga di UNGIO00F
162700100830       //--------------------------------------------------------------
162800100830       BEGSR contrRIGA  ;
162900100830
163000100921       // controllo se inserito almeno un dato
163100101001 0     if vscmag<>0  or vscksc>0  or vscdes<>*blanks or vscnot<>*blanks
163200101001       or (vsctip<>*blanks  and vsctip<>*zeros )  or vsdksc<>*blanks or
163300101001       (vscmag3<>*blanks  and vscmag3<>*zeros and $Video<>'S2') or
163400101001       vscmat<>*blanks  ;
163500100921
163600100903       // Invii/Rientri: campo obbligatorio
163700100923 1     if vscinvr=' ' ;
163800100903           errMessage  = *on;
163900100903           errGenerico = *on;
164000100903           errINVR=*on ;
164100100903           V1cmsg = Msg(39);
164200100903           EXSR AggioSFL   ;
164300100903           leavesr  ;
164400100923 1     endif  ;
164500101001
164600101001       // Per inserimeno multiplo, inserire il numero delle ricorrenza
164700101001       if   $video='D4' and v4cinse<=0  ;
164800101001           errMessage  = *on;
164900101001           errGenerico = *on;
165000101001           errINSE=*on ;
165100101001           V1cmsg = Msg(50);
165200101001           EXSR AggioSFL   ;
165300101001           leavesr  ;
165400101001       endif        ;
165500100826
165600100824       // tipo unita
165700100927       // controllo tipo unità e matricola
165800100927       exsr CtrTipMat    ;
165900100927       if Errgenerico =*on  ;
166000100927       leavesr  ;
166100100927       endif    ;
166200120209
166300120209       // Magazzino di destinazione obbligatorio
166400120209       //  non obbligatorio per inserimento multiplo
166500120209 0     if $video<>'D4'  or vscmag3<>*blanks  ;
166600120209
166700120209       clear vsdmag  ;
166800120209       clear wazorg  ;
166900120209       wtmag ='D  '   ;
167000120209 1     if $Video='S2'   ;
167100120209       wmag=%editc(vscmag:'X')  ;
167200120209       else  ;
167300120209       wmag=vscmag3 ;
167400120209 1     endif ;
167500120209
167600120209       exsr  chkmag   ;
167700130118
167800120209 1     if $Video='S2'   ;
167900120209       vscmag=%int(wmag)    ;
168000130118
168100130118       if Errgenerico=*off ;
168200130118       if §omagfag='Y'  and v1ccuf='FOR'  and vscinvr='F'
168300130118         and vscmag<>unadms  and vscmag>*zeros  ;
168400130118           errMessage  = *on;
168500130118           errGenerico = *on;
168600130118           V1cmsg = Msg(67);
168700130118       endif  ;
168800130118       endif  ;
168900130118
169000120209       else   ;
169100120209       vscmag3=wmag  ;
169200120209 1     endif  ;
169300120209       vsdmag=wdmag  ;
169400120209 1     if Errgenerico=*on  ;
169500120209         errMAG=*on ;
169600120209         EXSR AggioSFL   ;
169700120209         leavesr  ;
169800120209 1     endif    ;
169900120209 0     endif    ;
170000100927
170100100927
170200100927         // Quantità: solo per 99 <> da 0 o 1
170300100927 1       if vscqta>1 and vsctip<> '99'  ;
170400100927           errMessage  = *on;
170500100927           errGenerico = *on;
170600100927           V1cmsg = Msg(15);
170700100927           errQTA=*on ;
170800100927           EXSR AggioSFL   ;
170900100927           leavesr  ;
171000100927 1       endif  ;
171100100831
171200100907       // Controlli sul Magazzino Correlato
171300100831 1     if  vscmgc>0   ;
171400100903       // Non può essere uguale al magazzino di destinazione
171500100923 2     if vscmgc=vscmag  ;
171600100924           errMessage  = *on;
171700100924           errGenerico = *on;
171800100903           errMGC=*on ;
171900100903           V1cmsg = Msg(38);
172000100903           EXSR AggioSFL   ;
172100100903           leavesr  ;
172200100923 2     endif  ;
172300100908
172400100908       clear wazorg ;
172500100908       wtmag='C' ;
172600100909       wmag=%editc(vscmgc:'X')  ;
172700100831       exsr  chkmag   ;
172800100909       vsdmgc=wdmag  ;
172900100831 2     if Errgenerico=*on  ;
173000100831         errMGC=*on ;
173100100831         EXSR AggioSFL   ;
173200100831         leavesr  ;
173300100831 2     endif    ;
173400100831
173500100831 1     endif    ;
173600100923
173700100923
173800100923       // Tipo unità di RESO solo se c'e' la sostituzione
173900110401       // ignoro per FOR F o R
174000110401         if protperRIE=*off  ;
174100110610
174200110610         // Aumento/Sostituz: non per 99
174300110610 1       if vsctru<>' ' and vsctip=  '99'  ;
174400110610           errMessage  = *on;
174500110610           errGenerico = *on;
174600110610           V1cmsg = Msg(16);
174700110610           errTRU=*on ;
174800110610           EXSR AggioSFL   ;
174900110610           leavesr  ;
175000110610 1       endif  ;
175100110610
175200100923 1       if (vsctru<>'S' and vsctir<>*blanks and vsctir<>'00') or
175300100923            (vsctru= 'S' and (vsctir= *blanks or vsctir=*zeros))  ;
175400100923           errMessage  = *on;
175500100923           errGenerico = *on;
175600100923           V1cmsg = Msg(17);
175700100923           errTir=*on ;
175800100923           EXSR AggioSFL   ;
175900100923           leavesr  ;
176000100923 1       endif   ;
176100110404         else  ;
176200110404         // Per FOR F o FOR  R contiene  guasto fisico. Accetto solo la "S"
176300110404         if vsctru<>'S' and vsctru<>' '   ;
176400110404           errMessage  = *on;
176500110404           errGenerico = *on;
176600110404           V1cmsg = Msg(56);
176700110404           errTru=*on ;
176800110404           EXSR AggioSFL   ;
176900110404           leavesr  ;
177000110404         endif  ;
177100110610         // Note obbligatorie
177200110610         if vscnot=*blanks and vsctru='S'   ;
177300110610           errMessage  = *on;
177400110610           errGenerico = *on;
177500110610           V1cmsg = Msg(59);
177600110610           errNot=*on ;
177700110610           EXSR AggioSFL   ;
177800110610           leavesr  ;
177900110610         endif ;
178000110610
178100110401 1       endif   ;
178200100923
178300100923         clear vsdtir  ;
178400101001 1       if vsctir<>*blanks  and vsctir<>*zeros  ;
178500100923         wtip=vsctir   ;
178600100923         exsr chktip   ;
178700100923
178800100923         vsctir=wtip  ;
178900100923         vsdtir=wdtip  ;
179000100923 2       if Errgenerico=*on  ;
179100100923           errTIR=*on ;
179200100923 3       if Errmessage =*on  ;
179300100923           V1cmsg = Msg(18);
179400100923 3       endif  ;
179500100923           EXSR AggioSFL   ;
179600100923           leavesr  ;
179700100923 2       endif    ;
179800100923 1       endif    ;
179900100923
180000100923         // Matricola unità di reso
180100100923 1       if vscmatr<>*blanks and vsctir=*blanks  ;
180200100923           errMessage  = *on;
180300100923           errGenerico = *on;
180400100923           V1cmsg = Msg(44);
180500100923           errMATr=*on ;
180600100930           *inki=*on ;
180700100923           EXSR AggioSFL   ;
180800100923           leavesr  ;
180900100923 1       endif  ;
181000100923
181100100923          clear vsdmodr  ;
181200101005          clear vscmgcr  ;
181300100923 1        if vscmatr<>*blanks ;
181400100923          ktip=%int(vsctir)  ;
181500100923          chain (vscmatr:ktip)  unana07l    ;
181600100923 2        if not %found(unana07l)  ;
181700100923           Errgenerico=*on  ;
181800100923           Errmessage =*on  ;
181900100923           ErrMATR=*on  ;
182000100923           v1cmsg=msg(06)  ;
182100100923           EXSR AggioSFL   ;
182200100923           leavesr  ;
182300100923 2        endif  ;
182400100923          vsdmodr =unamod;
182500101005          vscmgcr =unamag;
182600100923
182700100923          // Il magazzino correlato deve essere uguale a quello di destinazione
182800101005 2        if unamag<>vscmag  and unaksc=0 and vshfmat<>vscmatr  ;
182900100923           errMessage  = *on;
183000100923           errGenerico = *on;
183100100923           V1cmsg = Msg(45);
183200100923           errMATr=*on ;
183300101005           vshfmat=vscmatr ;
183400100923           EXSR AggioSFL   ;
183500100923           leavesr  ;
183600100923 2        endif  ;
183700100923 1        endif  ;
183800101005
183900101005          vshfmat=vscmatr ;
184000100826
184100100827         // Descrizione obbligatoria per Invii per d§UNTfl1='S'
184200101001 1       if $Video<>'D4'   ;
184300101001 2       if vscinvr='I' and d§untfl1='S' and vscdes=*blanks  ;
184400101112           errMessage  = *on;
184500101112           errGenerico = *on;
184600100826           errDES=*on ;
184700100826           V1cmsg = Msg(19);
184800100826           EXSR AggioSFL   ;
184900100826           leavesr  ;
185000101001 2       endif ;
185100101001 1       endif ;
185200110412
185300110412         // per FOR "F" note diventa --> nuova matricola inviata in sostiituizione
185400110412 1       if $Video<>'D4'  and vscnot<>*blanks and ProtperFOR=*on ;
185500110412         // non deve essere più lunga di 15 caratteri
185600110412
185700110412         if %subst(vscnot:16:5)<>*blanks  ;
185800110412           Errgenerico=*on  ;
185900110412           Errmessage =*on  ;
186000110412           Errnot=*on  ;
186100110412           v1cmsg=msg(58)  ;
186200110412           EXSR AggioSFL   ;
186300110412           leavesr  ;
186400110412         endif  ;
186500110412
186600110412         // non deve già esistere
186700110412         ktip=%int(vsctip)  ;
186800110412         eval kmat=vscnot    ;
186900110412          setll (kmat:ktip)  unana07l    ;
187000110412 3        if %equal(unana07l)  ;
187100110412           Errgenerico=*on  ;
187200110412           Errmessage =*on  ;
187300110412           Errnot=*on  ;
187400110412           v1cmsg=msg(57)  ;
187500110412           EXSR AggioSFL   ;
187600110412           leavesr  ;
187700110412 3        endif    ;
187800110412         endif  ;
187900100826
188000100826         // codice cliente
188100100830 1       if ($video='S2' and vsdksc <>*blanks and vscksc=0) or
188200101001            ($video<>'S2' and vsdksc3<>*blanks and vscksc=0)  ;
188300100826           clear xparsta  ;
188400100826           xparkcc=dutkci ;
188500100826           xparkut= 3     ;
188600100826           xpardut=rsut   ;
188700100830           if $video='S2' ;
188800100826           xparrag=vsdksc ;
188900100830           else  ;
189000100830           xparrag=vsdksc3;
189100100830           endif  ;
189200100826           clear xparflr ;
189300100826           clear xparkcm  ;
189400100826           clear xparksm  ;
189500100826           clear xparkdm  ;
189600100826           xparnum=1      ;
189700100826           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
189800100826                           :xparflr:xpardit:xparnum:xparkcm:xparksm
189900100826                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
190000100827 2          if xparsta<>-1  ;
190100100826            vscksc=%int(%subst(xparksm:1:7))   ;
190200100826            vsdksc=xparrag   ;
190300100830            vsdksc3=xparrag   ;
190400100827 2          endif            ;
190500100826             errGenerico = *on;
190600100826             errksc      = *on;
190700100826           EXSR AggioSFL   ;
190800100826           leavesr  ;
190900100827  1      endif ;
191000100826
191100100826         // codice cliente : obbligatorio per tipo 48/49 in invio
191200100920         //  tranne se non c'e' la parola "SCORTA" nelle note
191300100920         Indx=%scan('SCORTA':vscnot)  ;
191400100920         if Indx=0   ;
191500100920         Indx=%scan('scorta':vscnot)  ;
191600100920         endif  ;
191700100920         if Indx=0   ;
191800100920         Indx=%scan('Scorta':vscnot)  ;
191900100920         endif  ;
192000100920
192100100924  2      If Indx=0 ;
192200100924  3      if vscinvr='I' and vscksc=0 and (vsctip='48' or vsctip='49');
192300100924           errMessage  = *on;
192400100924           errGenerico = *on;
192500100826           errKSC=*on ;
192600100826           V1cmsg = Msg(20);
192700100826           EXSR AggioSFL   ;
192800100826           leavesr  ;
192900100924  3      endif ;
193000100920  1      endif ;
193100100826
193200100827  1      if vscksc>0  ;
193300100826           chain   (1:dutkci:vscksc) cnaco00f   ;
193400100827  2        if not %found(cnaco00f)   ;
193500100826             errMessage  = *on;
193600100826             errGenerico = *on;
193700100826             ErrKSC   = *on;
193800100826             V1cmsg = Msg(21);
193900100826            EXSR AggioSFL   ;
194000100826             leavesr;
194100100826             else  ;
194200100826             vsdksc=acorag  ;
194300100830             vsdksc3=acorag  ;
194400100827 2       endif  ;
194500100827 1       endif  ;
194600100825
194700100826         // Data conferma movimento: se > udate non troppo...  ;
194800100826 1       If vscdtm>0  ;
194900100826             clear wlbdat;
195000100826             g02dat = vscdtm ;
195100100826             xsrda8(wlbdat);
195200100826 2           if g02err = '1';
195300100826               errMessage  = *on;
195400100826               errGenerico = *on;
195500100826               ErrDTM    = *on;
195600100826               V1cmsg = Msg(02);
195700100826              EXSR AggioSFL   ;
195800100826               leavesr;
195900100826 2           endif;
196000100826
196100100924 2           if g02inv>datapiu200;
196200100826               errMessage  = *on;
196300100826               errGenerico = *on;
196400100826               ErrDTM    = *on;
196500100826               V1cmsg = Msg(22);
196600100826              EXSR AggioSFL   ;
196700100826               leavesr;
196800100826 2           endif   ;
196900100909 2           if vshfmv=' ' and g02inv<%dec(datasys)  ;
197000100831               errMessage  = *on;
197100100831               errGenerico = *on;
197200100831               ErrDTM    = *on;
197300100831               V1cmsg = Msg(31);
197400100831              EXSR AggioSFL   ;
197500100831               leavesr;
197600100831 2           endif   ;
197700100826 1       endif   ;
197800101012
197900101012       // Se non c'e' un altra riga per lo stesso magazzino, chiedo obbligatori peso e vol
198000101012 1     if vscinvr='I'  and $Video<>'D4'  and vscmag>0  and vshfmv=' '  ;
198100101012        exsr selectSQL   ;
198200101012
198300101012 2        if   vscncl=0 or vscpkg=0   ;
198400101012 3      if SiColliPeso=' ' ;
198500101012               errMessage  = *on;
198600101012               errGenerico = *on;
198700101012               ErrNCL    = *on;
198800101012               V1cmsg = Msg(54);
198900101012              EXSR AggioSFL   ;
199000101012               leavesr;
199100101012 3      endif  ;
199200101012
199300101012 2      endif  ;
199400101012        endif  ;
199500080703
199600101001         // Dcumento obbligatorio  se rientro da fornitore
199700101001 1       if $Video<>'D4'  ;
199800120525 2       if (vscdoc=0 and vscddo=0) and vscinvr='R' ;
199900100909               errMessage  = *on;
200000100909               errGenerico = *on;
200100100909               ErrDDO    = *on;
200200100909               V1cmsg = Msg(33);
200300100909              EXSR AggioSFL   ;
200400100909               leavesr;
200500101001 2        endif   ;
200600101001 1        endif   ;
200700100909
200800100909       // controllo la data
200900100909       if vscddo>0   ;
201000100909             clear wlbdat;
201100100909             g02dat = vscddo ;
201200100909             xsrda8(wlbdat);
201300100909 2           if g02err = '1';
201400100909               errMessage  = *on;
201500100909               errGenerico = *on;
201600100909               ErrDDO    = *on;
201700100909               V1cmsg = Msg(02);
201800100909               leavesr;
201900100909              endif  ;
202000100909       endif  ;
202100100921 x0    else  ;
202200101001       // Per inserimento multipli immettere almeno un dato da duplicare
202300101001       if $Video='D4'  ;
202400101001               errMessage  = *on;
202500101001               errGenerico = *on;
202600101001               ErrMAG    = *on;
202700101001               V1cmsg = Msg(51);
202800101001               leavesr;
202900101001       endif  ;
203000100921       clear vsdmag   ;
203100100921       clear vsdtip  ;
203200100921       clear vsdmod1 ;
203300100921       clear vsdmod2 ;
203400100921       clear vscmgc  ;
203500100921       clear vsdmgc  ;
203600100921       Recvuoto='S'   ;
203700100930       // pulisco eventuale posizione della matricola
203800100930           if s02nrr<2000 ;
203900100930            clear tipmat(s02nrr)          ;
204000101012            clear SiCoPe(s02nrr)          ;
204100100930           endif  ;
2042001009210      endif  ;
204300100909
204400080409       ENDSR;
204500101012
204600101012       //--------------------------------------------------------------
204700101012       //?Select con SQL su ungio00F
204800101012       //--------------------------------------------------------------
204900101012       BEGSR  SelectSQL ;
205000101012
205100101012       clear SiColliPeso  ;
205200101012 1     if vscdtm=0  ;
205300101012         invdtm=%dec(datasys)  ;
205400101012       else  ;
205500101012         dataeur=%date(vscdtm:*dmy)    ;
205600101012         dataiso=dataeur  ;
205700101012         invdtm=%dec(dataiso) ;
205800101012 1     endif  ;
205900101012
206000101012       // Prima verifico se già inserito in questa pagina di lavoro
206100101012
206200101012       // Cerco con: MAGAZZINO  e data conferma
206300101012          // se la data <= oggi cerco con data =0
206400101012
206500101012 1     if invdtm<=%dec(datasys)  ;
206600101012       wSiCoPe=(vscmag+100000000)            ;
206700101012       else  ;
206800101012       wSiCoPe=(vscmag+100000000) +  invdtm  ;
206900101012 1     endif  ;
207000101012
207100101012 1     if   vscncl=0 or vscpkg=0   ;
207200101012
207300101012       clear sicope(s02nrr)   ;
207400101012       Indx=%lookup(wsicope:Sicope)  ;
207500101012 2      if Indx>0  ;
207600101012         SiColliPeso='S'  ;
207700101012 2      endif  ;
207800101012
207900101012 x1     else  ;
208000101012
208100101012         if s02nrr<2000 ;
208200101012           sicope(s02nrr)=wsicope    ;
208300101012         endif  ;
208400101012         SiColliPeso='S'  ;
208500101012 1      endif  ;
208600101012
208700101012
208800101012       // Poi se non trovato controllo se già inserito in UNGIO
208900101012         if SiColliPeso =' '  ;
209000101012
209100101012         wrkgetlista='SELECT ungdtm, ungncl, ungpkg, rrn(UNGIO00f) +
209200101012                               from UNGIO00F WHERE ungcuf=''' +v1ccuf  + ''' +
209300101012         and unginvr=''I'' and ungfmv='' '' and ungmag= ' + %editc(vscmag:'X') ;
209400101012
209500101012         exec sql   PREPARE s1 FROM :wrkgetlista   ;
209600101012
209700101012         exec sql   DECLARE a1 cursor FOR  s1   ;
209800101012
209900101012         exec sql OPEN a1   ;
210000101012
210100101012 0       dow sqlcod=0   ;
210200101012
210300101012         exec sql FETCH NEXT  FROM a1 INTO: sqdtm, :sqncl, :sqpkg, :sqnrr ;
210400101012
210500101012 1       select ;
210600101012 1       when sqlcod=100 ;
210700101012
210800101012 1       when sqlcod<0   ;
210900101012         *inh1=*on   ;
211000101012
211100101012 x1      other   ;
211200101012         // escludo il record se la data conferma è <> da quello del record ins/variato
211300101012 2a       if  vshnrr<>sqnrr ;
211400101012 2        if  vscdtm=0 or invdtm<=%dec(datasys)  ;
211500101012 3         if sqdtm<=invdtm  ;
211600101012 4           if sqncl>0 and sqpkg>0  ;
211700101012               SiColliPeso='S'  ;
211800101012 4           endif  ;
211900101012 3         endif  ;
212000101012
212100101012 x2       else  ;
212200101012 3         if  sqdtm=invdtm  ;
212300101012 4           if sqncl>0 and sqpkg>0  ;
212400101012               SiColliPeso='S'  ;
212500101012 4           endif  ;
212600101012 3         endif  ;
212700101012 2a       endif  ;
212800101012 2        endif  ;
212900101012
213000101012 1       endsl  ;
213100101012
213200101012 0       enddo   ;
213300101012
213400101012         exec sql CLOSE a1   ;
213500101012         endif  ;
213600101012
213700101012       ENDSR;
213800080409
213900100927       //--------------------------------------------------------------
214000100927       //?Controllo solo tipo unità e matricola
214100100927       //--------------------------------------------------------------
214200100927       BEGSR  CtrTipMat  ;
214300100927
214400100927       // Obbligatorio se non immessa la matricola
214500101001       //  non obbligatorio per inser. multiplo
214600101001 1     if (vsctip=*blanks or vsctip=*zeros) and vscmat=*blanks and $Video<>'D4';
214700100927           errMessage  = *on;
214800100927           errGenerico = *on;
214900100927           errTIP=*on ;
215000100927           v1cmsg=msg(05)  ;
215100100927           EXSR AggioSFL   ;
215200100927           leavesr  ;
215300100927  1      endif   ;
215400100927
215500100927  1     if vsctip<>*blanks and vsctip<>*zeros  ;
215600100927         clear vsdtip  ;
215700100927         clear vsdtip3 ;
215800100927         wtip=vsctip   ;
215900100927         exsr chktip   ;
216000100927
216100100927         vsctip=wtip  ;
216200100927         vsdtip =wdtip  ;
216300100927         vsdtip3=wdtip  ;
216400100927 2       if Errgenerico=*on  ;
216500100927           errTIP=*on ;
216600100927 3       if ErrMessage =*on  ;
216700100927          v1cmsg=msg(05)  ;
216800100927 3        endif  ;
216900100927           EXSR AggioSFL   ;
217000100927           leavesr  ;
217100100927 x2      else  ;
217200100927         dunt=t02uni ;
217300100927         vsdtip =d§untdes ;
217400100927         vsdtip3=d§untdes ;
217500100927  2      endif    ;
217600100927
217700140207         // "99" accettato solo per MAG/TLC o FOR-F
217800140207  2      if vsctip='99' and v1ccuf='FOR'
217900140207           and (v1cinvr='R' or v1cinvr='S') ;
218000100927           errMessage  = *on;
218100100927           errGenerico = *on;
218200100927           errTIP=*on ;
218300100927           V1cmsg = Msg(14);
218400100927           EXSR AggioSFL   ;
218500100927           leavesr  ;
218600100927  2      endif   ;
218700100927  1      endif   ;
218800100927
218900100927         // Matricola non ci vuole per 99
219000100927 1       if v1ctip='99' and v1cmat<>*blanks  ;
219100100927           errMessage  = *on;
219200100927           errGenerico = *on;
219300100927           V1cmsg = Msg(13);
219400100927           errMAT=*on ;
219500100927           EXSR AggioSFL   ;
219600100927           leavesr  ;
219700100927 1       endif    ;
219800100927
219900100927
220000100927         // Matricola: obbligatoria in conferma
220100100927         if §sdv0conf='S' and vsctip<>'99' and vscmat=*blanks  ;
220200100927           errMessage  = *on;
220300100927           errGenerico = *on;
220400100927           V1cmsg = Msg(32);
220500100927           errMAT=*on ;
220600100927           EXSR AggioSFL   ;
220700100927           leavesr  ;
220800100927         endif   ;
220900100927
221000100927         // Matricola: se immessa controllo
221100100927         clear vsdmod1 ;
221200100927         clear vsdmod2 ;
221300100927         clear vsdmod  ;
221400100927          if vshfmv=' ' ;
221500100927          clear vscmgc  ;
221600100927          clear vsdmgc  ;
221700100927         endif  ;
221800100927 1       if vscmat<>*blanks   ;
221900100927 2       if vsctip>*zeros   ;
222000100927          ktip=%int(vsctip)  ;
222100100927          chain (vscmat:ktip)  unana07l    ;
222200100927 x2       else  ;
222300100927          chain (vscmat)       unana07l    ;
222400100927 2        endif  ;
222500100927
222600100927 2        if not %found(unana07l)  ;
222700100927          Errgenerico=*on  ;
222800100927          Errmessage =*on  ;
222900100927          ErrMAT=*on  ;
223000100927          if vsctip>*zeros  ;
223100100927          v1cmsg=msg(06)  ;
223200100927          else  ;
223300100927          v1cmsg=msg(07)  ;
223400100927          endif  ;
223500100927
223600100927           EXSR AggioSFL   ;
223700100927           leavesr  ;
223800100927 2        endif  ;
223900100927
224000100927          // se trovata verifico se univoca altrimenti ci vuole il tipo
224100100927 2        if     vsctip<=*zeros and %found(unana07l)  ;
224200100927           clear  qtatem   ;
224300100927           setll (vscmat)  unana07l    ;
224400100927           reade (vscmat)  unana07l    ;
224500100927 3         dow  not %eof(unana07l) ;
224600100927 4          if qtatem=0   ;
224700100927             qtatem=qtatem+1  ;
224800100927 x4         else   ;
224900100927             Errgenerico=*on  ;
225000100927             Errmessage =*on  ;
225100100927             Errmat=*on  ;
225200100927             v1cmsg=msg(08)  ;
225300100927             EXSR AggioSFL   ;
225400100927             leavesr;
225500100927 4          endif   ;
225600100927           reade (vscmat)  unana07l    ;
225700100927 3         enddo   ;
225800100927 2        endif   ;
225900100927
226000100927 2        if vsctip<=*zeros   ;
226100100927             vsctip=%editc(unatip:'X') ;
226200100928            ktip=unatip   ;
226300100928            wtip=vsctip   ;
226400100927            exsr chktip   ;
226500100927            dunt=t02uni ;
226600100927            vsctip=wtip  ;
226700100927            vsdtip =wdtip  ;
226800100927            vsdtip3=wdtip  ;
226900100927 2        endif ;
227000100927          vsdmod =unamod;
227100100927          vsdmod1=unamod;
227200100927          vsdmod2=%subst(unamod:7:9) ;
227300110404          // Se unità in proprietà alla 923 --> non posso accettare MAG 990
227400110404          if unasis='923' and ((vscmag = 990  and $Video='S2') or
227500111024                               (vscmag3='990' and $Video<>'S2')  or
227600111024                               (vscmag3='980' and $Video<>'S2')  or
227700111024                               (vscmag = 980  and $Video='S2')) ;
227800110401             Errgenerico=*on  ;
227900110401             Errmessage =*on  ;
228000110401             Errmag=*on  ;
228100110401             v1cmsg=msg(55)  ;
228200110401             EXSR AggioSFL   ;
228300110401             leavesr;
228400110401          endif  ;
228500130626          // Se unità in proprietà non è 923 --> non posso accettare MAG 923
228600130626          if unasis<>'923' and ((vscmag = 923  and $Video='S2') or
228700130626                               (vscmag3='923' and $Video<>'S2')) ;
228800130626             Errgenerico=*on  ;
228900130626             Errmessage =*on  ;
229000130626             Errmag=*on  ;
229100130626             v1cmsg=msg(68)  ;
229200130626             EXSR AggioSFL   ;
229300130626             leavesr;
229400130626          endif  ;
229500120209
229600120209          // Per FOR F propongo magazzino destinazione se si tratta di un fornitore
229700130116          if v1ccuf='FOR'  and vscinvr='F' ;
229800130116          chain unadms azorg21l ;
229900130116          if  vscmag<=*zeros  and
230000130116                 %found(azorg21l) and  orgfag = 'Y' ;
230100120209              vscmag=unadms  ;
230200120209              Errgenerico=*on  ;
230300120209              EXSR AggioSFL   ;
230400120209              leavesr  ;
230500120209          endif  ;
230600130116
230700130116             endif  ;
230800120209
230900100927          // Solo se record non confermato
231000100927          if vscmgc=0 or vshfmv=' ' ;
231100100927           vscmgc =unamag;
231200100927          endif  ;
231300100927
231400100928          //  controllo l'univocità tra i record non  confermati
231500130321            setll (Ktip:vscmat)  ungio01l    ;
231600130321            reade (Ktip:vscmat)  ungio01l    ;
231700130321            dow not %eof(ungio01l) ;
231800130321             if ungfmv=' '  ;
231900130321              if vshimm='S' or (vshnrr<>gio1nrr)   ;
232000130321               Errgenerico=*on  ;
232100130321               Errmessage =*on  ;
232200130321               ErrMAT=*on  ;
232300130321               v1cmsg=msg(23)  ;
232400130321               EXSR AggioSFL   ;
232500130321               leavesr  ;
232600130321              endif   ;
232700130321             endif   ;
232800130321             reade (Ktip:vscmat)  ungio01l    ;
232900130321           enddo   ;
233000100927
233100100927          // controllo che non sia già stata inserita in questa pagina di lavoro
233200100927          wtipmat=vsctip ;
233300100927          %subst(wtipmat:3:15)=vscmat   ;
233400100927          Indx=%lookup(wtipmat:tipmat)  ;
233500100928 2        if Indx>0 and Indx<>s02nrr ;
233600100927          Errgenerico=*on  ;
233700100927          Errmessage =*on  ;
233800100927          ErrMAT=*on  ;
233900100927          v1cmsg=msg(24)  ;
234000100927           EXSR AggioSFL   ;
234100100927           leavesr  ;
234200100928           else  ;
234300100928           // la memorizzo
234400100928           // solo se maggiore di 1999 non memorizzo
234500100928           if s02nrr<2000 ;
234600100928            tipmat(s02nrr)=wtipmat  ;
234700100928  2        endif    ;
234800100928 2        endif    ;
234900100927
235000101022 2        if unaksc>0  and vscksc=0 and vscinvr='F';
235100100927          vscksc =unaksc;
235200100927           chain   (1:dutkci:unaksc) cnaco00f   ;
235300100927 3         if %found(cnaco00f)   ;
235400100927             vsdksc=acorag  ;
235500100927             vsdksc3=acorag  ;
235600100927           else   ;
235700100927             vsdksc=*all'?'  ;
235800100927             vsdksc3=*all'?'  ;
235900100927 3         endif  ;
236000100927 2        endif  ;
236100100927 2        if vscdes=*blanks ;
236200100927          vscdes=unades ;
236300100927 2        endif ;
236400100927 1       endif    ;
236500100927         ENDSR    ;
236600080703       //--------------------------------------------------------------
236700080703       //?Aggiornamento sfl
236800080703       //--------------------------------------------------------------
236900080703       BEGSR AGGIOSFL  ;
237000100830
237100100830       IF $VIDEO='S2'       ;
237200100830
237300100830       exsr ProtCampi   ;
237400100830
237500100824       if v1cmsg<>*blanks   ;
237600080703       errMessage  = *on;
237700080703       errGenerico = *on;
237800100315       c02csr=s02nrr    ;
237900080703       endif    ;
238000080703
238100100921       if Recvuoto<>'S';
238200100830        Sflnxtchg=*on  ;
238300100921        endif  ;
238400100921        clear recvuoto  ;
238500100824
238600100315       update  sduos02  ;
238700100830       ENDIF   ;
238800100830
238900080703       ENDSR   ;
239000100312       //--------------------------------------------------------------
239100100312       //?Inizializzazione videata 01
239200100312       //--------------------------------------------------------------
239300100312       BEGSR Inzd01  ;
239400100312
239500100315         clear V1cmsg;
239600100825         clear V1dcuf;
239700100830         EmisMOD1    = *on ;
239800100826         errMessage  = *off;
239900100312         errGenerico = *off;
240000140207         // Per utente EDP propongo MAG  ;
240100140207         // altrimenti  TLC
240200161011         if %subst(knmus:1:3)='EDP'
240300161011          or %subst(knmus:1:3)='EST'  ;
240400100825         v1ccuf ='MAG'   ;
240500140207         else  ;
240600140207         v1ccuf ='TLC'   ;
240700140207         endif  ;
240800100908         if modInt=*off  and modReso=*off ;
240900100826         v1cconf='S'  ;
241000100908         else  ;
241100100908         clear v1cconf ;
241200100908         endif  ;
241300100825         clear v1cinvr ;
241400100824         clear  v1cute  ;
241500100831         clear v1ciet  ;
241600100826         clear v1ctip  ;
241700100831         clear v1ctip  ;
241800100824         clear v1dtip  ;
241900100315         clear v1cmat  ;
242000100315         clear v1cmgc  ;
242100100315         clear v1cmag  ;
242200100831         clear v1creso ;
242300100831         clear v1cdtm  ;
242400101025         clear v1cnot  ;
242500121024         clear v1cdes  ;
242600100312
242700100831         // Per le MATRICOLE escludo il tipo 99
242800100831         if ImmMAT=*on   ;
242900100831          v1ciet='E'  ;
243000100831          v1ctip='99'  ;
243100100831         wtip=v1ctip   ;
243200100831         exsr chktip   ;
243300100831         v1dtip=wdtip  ;
243400100831         endif  ;
243500100831
243600101125         // Per gestine RESI imposto al "S"  e solo i confermati
243700100908         if ModReso=*on   ;
243800100831         v1creso='S'   ;
243900101125         v1cconf='C'  ;
244000100831         endif  ;
244100100831
244200100924         // Per conferma movimenti e immissione imposto data conferma = udate
244300110914         // anche per immissione matricole
244400110914         if ImmGES=*on  or ImmMAT=*on  ;
244500101112         v1cdtma=Dataoggi      ;
244600101112         endif   ;
244700101112         if ImmCMOV=*on   ;
244800100903         v1cdtm=Dataoggi      ;
244900100831         endif  ;
245000100831
245100100312         ENDSR    ;
245200080207       //--------------------------------------------------------------
245300100312       //?Inizializzazione SFL02
245400080207       //--------------------------------------------------------------
245500100312       BEGSR InzS02;
245600080207
245700080207       // Pulizia subfile
245800080207         SflDsp_N    = *on;
245900080207         SflDspCtl_N = *on;
246000100315         write  SDUOC02;
246100080207         SflDspCtl_N = *off;
246200080207         SflEnd      = *off;
246300100824         SflDsp_N    = *off;
246400100824         Sflnxtchg   = *off;
246500080530
246600121130         clear w05dtm  ;
246700100826         clear tipmat;
246800101012         clear SiCoPe  ;
246900100826         clear C02rcd;
247000100315         clear C02csr;
247100100315         S02nrr=0 ;
247200100824         clear V1cmsg;
247300100824         clear totrig;
247400080207         errMessage  = *off;
247500080207         errGenerico = *off;
247600100708
247700100826         // Decodifico ufficio scelto
247800140123         select ;
247900140123         when v1ccuf='MAG'           ;
248000100827         v1dcuf=' MAG=Magazzino  C.E.D. ' ;
248100140123         when v1ccuf='TLC'           ;
248200140123         v1dcuf=' MAG=Magazzino  T.L.C. ' ;
248300140123         other ;
248400100827         v1dcuf='FOR=Magazzino  ACQUISTI' ;
248500140123         ENDsl  ;
248600100826
248700100823         // Predispongo le letture e carico SFL
248800100823         // se movimenti MAG imposto SEDE nel tipo magazzino correlato
248900100825         kinvr=v1cinvr ;
249000140123         if v1ccuf<>'FOR' and v1cinvr=' '  ;
249100100825         eval kinvr='I' ;
249200100823         endif  ;
249300100823
249400100823         clear kmag  ;
249500100823         if v1cmag>*zeros  ;
249600100823         kmag=%int(v1cmag)  ;
249700100823         endif  ;
249800100823
249900100823         clear kmgc  ;
250000100823         if v1cmgc>*zeros  ;
250100100823         kmgc=%int(v1cmgc)  ;
250200100823         endif  ;
250300100824
250400100824         clear ktip  ;
250500100824         if v1ctip>*zeros  ;
250600100824         ktip=%int(v1ctip)  ;
250700100824         endif  ;
250800100823
250900100823         // Se presente tipo e matricola leggo per questo
251000121024         //if ktip>*zeros and v1ciet ='I'   ;
251100100823
251200121024         //select   ;
251300121024         //when v1cmat=*blanks  ;
251400121024         //  setll (ktip)                      ungio01l    ;
251500121024         //  reade (ktip)                      ungio01l    ;
251600121024         //when v1cdim>0   ;
251700121024         //  setll (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
251800121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
251900121024         //other;
252000121024         //  setll (ktip:v1cmat:v1ccuf)        ungio01l    ;
252100121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
252200121024         //endsl   ;
252300100823
252400121024         //dow not %eof(ungio02l)  ;
252500100823         // controllo parzializzazioni
252600121024         // vshnrr =gio1nrr ;
252700121024         //exsr  CtrPARZ   ;
252800121024         //if Wok='S'   ;
252900121024         //exsr CarSF02  ;
253000121024         //endif   ;
253100100823
253200121024         //select   ;
253300121024         //when v1cmat=*blanks  ;
253400121024         //  reade (ktip)                        ungio01l    ;
253500121024         //when v1cdim>0   ;
253600121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
253700121024         //other;
253800121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
253900121024         //endsl   ;
254000121024         //enddo   ;
254100100823
254200121024         //else  ;
254300100823
254400100823         select  ;
254500100831         when kinvr  =*blanks ;
254600100831           setll (v1ccuf)                      ungio04l    ;
254700100831           reade (v1ccuf)                      ungio04l    ;
254800100823         other ;
254900101012           setll (v1ccuf:kinvr:kmag)           ungio04l    ;
255000100831           reade (v1ccuf:kinvr)                ungio04l    ;
255100100824         endsl  ;
255200100823
255300100823         dow not %eof(ungio04l)  ;
255400100823         // Controllo Parzializzazioni
255500100827          vshnrr =gio4nrr ;
255600100823         exsr  CtrPARZ   ;
255700100824         if Wok='S'   ;
255800100823         exsr CarSF02  ;
255900100823         endif   ;
256000100823
256100100823         select  ;
256200100831         when kinvr=*blanks ;
256300100825           reade (v1ccuf)                      ungio04l    ;
256400100823         other ;
256500100831           reade (v1ccuf:kinvr)                ungio04l    ;
256600100823         endsl  ;
256700100823         enddo   ;
256800100823
256900121024         //endif   ;
257000100823
257100100831         select ;
257200100831         // In interrogazione  resi e conferma:
257300100831         //                 mi posiziono sul primio record  e non scrivo righe vuote
257400100831         when §sdv0int='S'  or §sdv0resi='S'  or §sdv0conf='S' ;
257500100830         c02csr=1 ;
257600100830         c02rcd=1  ;
257700100831
257800100831         // In immissione matricole mi posiziono sulla matricola ssul primo record
257900100831         when §sdv0MMat='S'  ;
258000100831         if c02csr=0 ;
258100100831         c02csr=1 ;
258200100831         endif  ;
258300100831
258400100831         other  ;
258500100831         // Solo in Gestione: Scrittura righe vuote
258600101001         // salvo il primo record vuoto che scrivo
258700101001         PrimoVuoto=s02nrr+1  ;
258800100824         exsr Righevuote  ;
258900100831         endsl   ;
259000100823
259100100824         ENDSR  ;
259200100824       //--------------------------------------------------------------
259300100824       //?Scrittura righe vuote
259400100824       //--------------------------------------------------------------
259500100824         BEGSR  Righevuote   ;
259600100824
259700100824         // Finito di leggere carico tutta la pagina con record vuoti
259800100824         if s02nrr>=c_sflPag ;
259900100824         wpag=%div(s02nrr:c_sflPag) ;
260000100824         wresto=%rem(s02nrr:c_sflPag)  ;
260100100824         if wresto>0    ;
260200100924         c02csr=s02nrr  ;
260300100824         else   ;
260400100924         c02csr=(wpag*c_sflpag)+1 ;
260500100824         endif  ;
260600100824
260700100824         else  ;
260800100824         clear wpag  ;
260900100824         wresto=s02nrr  ;
261000100824         // posizionamento sulla prtima pagina e 1° record vuoto
261100100924         c02csr=s02nrr  ;
261200100927          if c02csr=0 ;
261300100927           eval c02csr=1  ;
261400100927          endif ;
261500100824         endif ;
261600100824
261700100824         dow wresto<c_sflpag  ;
261800100823         wresto=wresto+1 ;
261900100826
262000100824         vshimm='S'   ;
262100100830         clear VSCOPZ ;
262200100830         clear VShann ;
262300100909         clear VShfmv ;
262400101006         clear VShfpr ;
262500120524         clear VShfst ;
262600100909         clear VShnrr ;
262700140123         if v1ccuf <>'FOR'  ;
262800100825         vscinvr='I'  ;
262900100824         else   ;
263000100825         vscinvr=v1cinvr;
263100100824         endif   ;
263200100823         clear vscmag ;
263300100830         clear vscmag3;
263400100827         clear vshnrr ;
263500100827         clear vscqta ;
263600100823         clear vsctip ;
263700100830         clear vsdtip ;
263800100830         clear vsdtip3;
263900100827         clear vscmgc ;
264000100827         clear vscmat ;
264100100827         clear vsdmod1 ;
264200100827         clear vsdmod2 ;
264300100830         clear vsdmod  ;
264400100823         clear vscdes ;
264500100826         clear vscnot ;
264600100823         clear vsctru ;
264700100823         clear vsctir ;
264800100923         clear vscmatr;
264900100923         clear vsdmodr;
265000101005         clear vscmgcr;
265100101005         clear vshfmat;
265200100826         clear vscksc ;
265300100826         clear vsdksc ;
265400100830         clear vsdksc3;
265500100823         clear vscdtm ;
265600100823         clear vscddo ;
265700100823         clear vscdoc ;
265800100825         clear vscute ;
265900100830         clear vscdim4;
266000100830         clear vscdim ;
266100100827         clear vscreso  ;
266200100827         clear vscslr   ;
266300101008         clear vscncl   ;
266400101008         clear vscpkg   ;
266500100825         vshdim=%dec(datasys)  ;
266600100824
266700100824          exsr Protcampi  ;
266800100824
266900100823          s02nrr=s02nrr+1 ;
267000100823          write  SDUOS02;
267100100823          enddo  ;
267200080409
267300100824         // Salvo numero righe scritte
267400100824         totrig=s02nrr  ;
267500100824
267600080702         ENDSR    ;
267700080627
267800100823       //--------------------------------------------------------------
267900100823       //?Controllo selezioni impostate in prima videata
268000100823       //--------------------------------------------------------------
268100100823          BEGSR   CtrPARZ    ;
268200100823          Wok='S'    ;
268300100908          if w1cdtmd>0  and w1cdtma=0 ;
268400100908          w1cdtma=w1cdtmd ;
268500100908          endif  ;
268600100826
268700100823          // Utente
268800100823          if v1cute<>*blanks   and ungute<>v1cute ;
268900100823            clear wok  ;
269000100823            leavesr    ;
269100100823          endif      ;
269200100823          // Data Immissione
269300100824          if v1cdim>*zeros    and ungdim<w1cdim ;
269400100823            clear wok  ;
269500100823            leavesr    ;
269600100823          endif      ;
269700100823          // Tipo movimentazione
269800100825          if ungcuf <>v1ccuf ;
269900100823            clear wok  ;
270000100823            leavesr    ;
270100100823          endif      ;
270200100823          // Tipo magazzino correlato
270300100825          if kinvr<>*blanks and unginvr<>kinvr  ;
270400100823            clear wok  ;
270500100823            leavesr    ;
270600100823          endif      ;
270700100823          // Magazzino destinazione
270800100823          if kmag>*zeros  and ungmag<>kmag   ;
270900100823            clear wok  ;
271000100823            leavesr    ;
271100100823          endif      ;
271200100823          // Magazzino correlato
271300100823          if kmgc>*zeros  and ungmgc<>kmag   ;
271400100823            clear wok  ;
271500100823            leavesr    ;
271600100823          endif      ;
271700100826          // solo i record da confermare
271800100826          if v1cconf='S' and ungfmv='S' ;
271900100826            clear wok  ;
272000100826            leavesr    ;
272100100826          endif      ;
272200100908          // solo i record confermati
272300100908          if v1cconf='C' and ungfmv=' ' ;
272400100908            clear wok  ;
272500100908            leavesr    ;
272600100908          endif      ;
272700100908          // data conferma movimenti AL
272800100831          if v1cdtm>0   and ungdtm>w1cdtm  ;
272900100831            clear wok  ;
273000100831            leavesr    ;
273100100831          endif      ;
273200100908          // data conferma movimenti DAL/AL
273300100908          if v1cdtmd>0 or v1cdtma>0 ;
273400100908          if  ungdtm<w1cdtmd or ungdtm>w1cdtma ;
273500100908            clear wok  ;
273600100908            leavesr    ;
273700100908          endif      ;
273800100908          endif      ;
273900101025
274000101025          // SCAN sulle note
274100101025          if v1cnot<>*blanks   ;
274200101025           xx=%checkr(' ':v1cnot)  ;
274300101025           Indx=%scan(%subst(v1cnot:1:xx):ungnot)  ;
274400101025           if Indx=0   ;
274500101025            clear wok  ;
274600101025            leavesr    ;
274700101025           endif      ;
274800101025          endif      ;
274900121024          // SCAN sulla descrizione
275000121024          if v1cdes<>*blanks   ;
275100121024           xx=%checkr(' ':v1cdes)  ;
275200121024           Indx=%scan(%subst(v1cdes:1:xx):ungdes)  ;
275300121024           if Indx=0   ;
275400121024            clear wok  ;
275500121024            leavesr    ;
275600121024           endif      ;
275700121024          endif      ;
275800100823
275900121024          // Tipo Unità  da ESCLUDERE
276000100831          if v1ctip<>*blanks and v1ciet='E' and ktip= ungtip ;
276100100831            clear wok  ;
276200100831            leavesr    ;
276300100831          endif      ;
276400121024          // Tipo Unità  da INCLUDERE
276500121024          if v1ctip<>*blanks and v1ciet='I' and ktip<> ungtip ;
276600121024            clear wok  ;
276700121024            leavesr    ;
276800121024          endif      ;
276900100927
277000121029          // Matricola inseribile solo per "I"-inclusione
277100100927          if v1cmat<>*blanks and  v1cmat<>ungmat ;
277200100831            clear wok  ;
277300100831            leavesr    ;
277400100831          endif      ;
277500100831
277600100831          // unità da rendere
277700100831          if v1creso='S'  and ungreso='S'   ;
277800100831            clear wok  ;
277900100831            leavesr    ;
278000100831          endif      ;
278100100831          if v1creso='S'  and ungtir=0      ;
278200100831            clear wok  ;
278300100831            leavesr    ;
278400100831          endif      ;
278500100823          ENDSR  ;
278600100823       //--------------------------------------------------------------
278700100823       //?eseguo caricamento SFL
278800100823       //--------------------------------------------------------------
278900100823          BEGSR   carSF02    ;
279000100830          clear vscopz  ;
279100100830          clear vshANN  ;
279200100830          protOPZ=*off  ;
279300100830          clear vshimm  ;
279400100830
279500100830          exsr CarRECORD  ;
279600100830
279700100830          s02nrr=s02nrr+1   ;
279800100831
279900100831          // Per l'immissione Matricole mi posizione sul 1° record senza il codice
280000100831          if §sdv0mmat='S' and c02csr=0 and  vscmat=*blanks ;
280100100831          c02csr=s02nrr  ;
280200100831          ErrMat =*on    ;
280300100831          Sflnxtchg=*on  ;
280400100831          endif  ;
280500100831
280600100831          write  SDUOS02;
280700100831          ErrMat =*off   ;
280800100831          Sflnxtchg=*off ;
280900100831
281000100830          ENDSR         ;
281100100830       //--------------------------------------------------------------
281200100830       //?Carica record
281300100830       //--------------------------------------------------------------
281400100830          BEGSR   carRECORD ;
281500100830
281600100831          vshfmv =ungfmv ;
281700101006          vshfpr =ungfpr ;
281800120524          vshfst =ungfst ;
281900100831          vscinvr=unginvr;
282000100830  1       if $video='S2' ;
282100100823          vscmag=ungmag  ;
282200100830          clear vsdmod1  ;
282300100830          clear vsdmod2  ;
282400100830          clear vsdksc   ;
282500100830          clear vscdim4  ;
282600100830          clear vsdtip   ;
282700100830          else ;
282800100830          vscmag3=%editc(ungmag:'X')    ;
282900100909          wtmag='D'      ;
283000100908          wazorg='E  '   ;
283100100830          wmag=vscmag3   ;
283200100830          exsr  chkmag   ;
283300100830          vsdmag=wdmag    ;
283400100830
283500100830          clear vsdmod   ;
283600100830          clear vsdksc3  ;
283700100830          clear vscdim   ;
283800100830          clear vsdtip3  ;
283900100830  1       endif ;
284000100830
284100100826          vscqta=ungqta  ;
284200101008          vscncl=ungncl  ;
284300101008          vscpkg=ungpkg  ;
284400100826
284500100924          clear vsctip  ;
284600100830 1        if ungtip>0 ;
284700100830          vsctip=%editc(ungtip:'X') ;
284800100830          wtip=vsctip   ;
284900100830          exsr chktip   ;
285000100830 3          if $Video='S2' ;
285100100830            vsdtip=wdtip  ;
285200100830            else  ;
285300100830            vsdtip3=wdtip  ;
285400100830            endif ;
285500100830  1       endif ;
285600100826          vscmat=ungmat  ;
285700100826          clear vscksc   ;
285800100826          // Se c'e' tipo e matricola prendo il modello e lo visualizzo
285900100826 1        if ungtip>0 and ungmat<>*blanks ;
286000100930          chain (ungmat:ungtip)  unana07l    ;
286100100930 2        if  %found(unana07l)  ;
286200100830
286300100830 3        if $Video='S2' ;
286400100830          vsdmod1=unamod;
286500100827          vsdmod2=%subst(unamod:7:9) ;
286600100830          else  ;
286700100830          vsdmod =unamod;
286800100830 3        endif  ;
286900100826
287000100830          // se non è invio visualizzo cliente da anagrafica
287100100830 3        if  vscinvr<>'I'  and ungksc=0;
287200100826          vscksc =unaksc;
287300100830 4        if unaksc>0  ;
287400100826           chain   (1:dutkci:unaksc) cnaco00f   ;
287500100830 5         if not %found(cnaco00f)   ;
287600100830             acorag=*all'?'  ;
287700100830 5         endif  ;
287800100830 5         if $Video='S2' ;
287900100830             vsdksc=acorag  ;
288000100830           else  ;
288100100830             vsdksc3=acorag  ;
288200100830 5         endif  ;
288300100830
288400100830 4        endif ;
288500100830 3        endif ;
288600100826 2        endif ;
288700100826 1        endif ;
288800100826
288900100827          vscslr =ungslr  ;
289000100827          vscreso=ungreso ;
289100110404          dgio01=ungflo  ;
289200110404          if unginvr='R' or unginvr='F'  ;
289300110404           vsctru=§giogsf  ;
289400110404          else  ;
289500110404           vsctru=ungtru  ;
289600110404          endif  ;
289700100923
289800100927          clear vsdtir ;
289900100827  1       if ungtir>0 ;
290000100830          vsctir=%editc(ungtir:'X')   ;
290100100927          wtip=vsctir   ;
290200100927          exsr chktip   ;
290300100927          vsdtir=wdtip  ;
290400100923          vscmatr=ungmatr ;
290500101005          vshfmat=ungmatr ;
290600101005          clear vscmgcr;
290700101005          clear vsdmodr;
290800100923 2        if ungtir>0 and ungmatr<>*blanks ;
290900100930          chain (ungmatr:ungtir)  unana07l    ;
291000100930 3        if  %found(unana07l)  ;
291100100923          vsdmodr=unamod  ;
291200101005          vscmgcr=unamag  ;
291300100923 3        endif  ;
291400100923 2        endif  ;
291500100923 xx1      else ;
291600100826          clear vsctir ;
291700100923          clear vscmatr;
291800101005          clear vshfmat;
291900100923          clear vsdmodr;
292000101005          clear vscmgcr;
292100100827  1       endif ;
292200100826          vscmgc=ungmgc  ;
292300100923          clear vsdmgc  ;
292400100909          if ungmgc>0   ;
292500100909          wmag=%editc(ungmgc:'X');
292600100909          wtmag='C' ;
292700100909          wazorg='E  '   ;
292800100909          exsr  chkmag   ;
292900100909          vsdmgc=wdmag    ;
293000100909          endif   ;
293100100826          vscdes=ungdes  ;
293200100826          vscnot=ungnot  ;
293300100830
293400100830          // se pieno CLIENTE decodifico
293500100827 1        if ungksc>0   ;
293600100830           vscksc=ungksc  ;
293700100826           chain   (1:dutkci:ungksc) cnaco00f   ;
293800100830 2         if not %found(cnaco00f)   ;
293900100830             vsdksc=*all'?' ;
294000100830 2         endif  ;
294100100830 2         if $Video='S2' ;
294200100830             vsdksc=acorag  ;
294300100830           else  ;
294400100830             vsdksc3=acorag  ;
294500100830 2         endif  ;
294600100830
294700100827 1         endif  ;
294800100823
294900100823          clear vscdtm   ;
295000100823          if ungdtm>0    ;
295100100823          dataiso=%date(ungdtm)   ;
295200100823          datadmy=dataiso ;
295300100823          vscdtm=%dec(datadmy)  ;
295400100823          endif   ;
295500100823
295600100823          clear vscddo   ;
295700100823          if ungddo>0    ;
295800100823          dataiso=%date(ungddo)   ;
295900100823          datadmy=dataiso ;
296000100823          vscddo=%dec(datadmy)  ;
296100100823          endif   ;
296200100823          vscdoc=ungdoc  ;
296300100823
296400100826          vscute=ungute  ;
296500100823          if ungdim>0    ;
296600100823          dataiso=%date(ungdim)   ;
296700100823          datadmy=dataiso ;
296800100830          dataeur=dataiso ;
296900100823          w0060 =%dec(datadmy)  ;
297000100830          if $Video='D3'  ;
297100100830          vscdim=%dec(dataeur)  ;
297200100920          vschim=unghim         ;
297300100830          else  ;
297400100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
297500100827                  %subst(%editc(w0060:'X'):3:2)    ;
297600100830          endif  ;
297700100824          vshdim=ungdim  ;
297800100823          endif   ;
297900100823
298000100909          ErrGenerico=*off ;
298100100909          ErrMessage =*off ;
298200100826          exsr ProtCampi  ;
298300100823          ENDSR  ;
298400080702       //--------------------------------------------------------------
298500080702       //?Gestione protezione campi del sfl
298600080702       //--------------------------------------------------------------
298700080702          BEGSR   ProtCampi   ;
298800100830           protOPZ=*off ;
298900100826           ProtperINV=*off ;
299000100826           ProtperRIE=*off ;
299100110404           ProtperFOR=*off ;
299200100826           ProtINVR  =*off ;
299300100831           ProtMAT   =*off ;
299400100831           EmisCONF  =*off ;
299500100827           protrIGA=*off   ;
299600100830           RecANN  =*off   ;
299700100909           EmisCONF  =*off ;
299800101006           RevMAG    =*off ;
299900120524           NoSTAMPABO=*off ;
300000100824
300100100830          // Record di nuova immissione
300200100824          if vshimm='S'  ;
300300100830          protOPZ=*on  ;
300400100827          else  ;
300500100827           protrIGA=*on    ;
300600100830          endif    ;
300700100831
300800100903          // Per Int  e resi proteggo matricola
300900100903          if §sdv0resi='S'  or §sdv0int='S';
301000100831           protMAT =*on    ;
301100100831          endif   ;
301200100903          // Per immissione, solo per i record già inseriti
301300100903          if §sdv0imm='S' and vshimm=' ' ;
301400100903           protMAT =*on    ;
301500100903          endif  ;
301600100909
301700100909          // Se immissione matricole proteggo opzione  tranne che per F21
301800100909          if §sdv0mmat='S'  and dsp_aid <> c_F21    ;
301900100909           protOPZ =*on    ;
302000100909          endif   ;
302100100831
302200100831          // Se già confermato visualizzo
302300100831          if vshfmv='S'   ;
302400100831           EmisCONF  =*on  ;
302500100831          endif ;
302600101006          // Se già preparato materiale
302700101006          if vshfpr='P'   ;
302800101006           RevMAG   =*on  ;
302900101006          endif ;
303000100831
303100100831          // In fase di conferma accendo sflnexc per controllare tutti i record
303200100831          if §sdv0conf='S'   ;
303300100831           Sflnxtchg=*on  ;
303400100831          endif   ;
303500100830
303600100830          // Record annullato
303700100830          if vshANN='S'  ;
303800100830          protOPZ=*on  ;
303900100830          protrIGA=*on    ;
304000100830          recANN  =*on    ;
304100100830          endif  ;
304200100824
304300100826          if v1ccuf ='MAG'  or vscinvr='I'  ;
304400100826          protperINV=*on  ;
304500100824          endif    ;
304600100826
304700120523          if vscinvr='R' or vscinvr='F' or vscinvr='O';
304800110404           protperRIE=*on  ;
304900110404
305000120523          if vscinvr='R' or vscinvr='O';
305100110404           protperFOR=*on  ;
305200110404          endif  ;
305300100826          endif   ;
305400100826
305500100830          // Se immesso in proma videata il tipo invio/rientro lo proteggo
305600100826          if v1cinvr<>' ' ;
305700100826          protINVR=*on ;
305800100826          endif  ;
305900120524          // se non voglio la stampa della bolla visualizzo
306000120524          if vshfst='N'  ;
306100120524           NOstampaBO=*on  ;
306200120524          endif  ;
306300080703
306400080702          ENDSR    ;
306500100316       //--------------------------------------------------------------
306600100708       //?controllo Magazzino
306700100316       //--------------------------------------------------------------
306800100316        BEGSR ChkMag                 ;
306900100930        clear v1cmsg   ;
307000100930
307100100930        clear tnsdmagds  ;
307200100930        §magmag=wmag                            ;
307300100930        §imagtmag=wtmag                         ;
307400100930        §imagtorg=wazorg                        ;
307500100930        §imagcuf =v1ccuf                        ;
307600100930        §imaginvr=vscinvr                       ;
307700100930
307800100930        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
307900100930
308000100930        // Eseguita ricerca
308100100930        if §omagerr='R'   ;
308200100930        wmag=§magmag  ;
308300100930        wdmag = §omagdes  ;
308400100930        ErrGenerico=*on  ;
308500100930        leavesr   ;
308600100930        endif  ;
308700100930
308800100930        // Errore
308900100930        if §omagerr='E'   ;
309000100930        v1cmsg=§omagmsg  ;
309100100930        Errgenerico=*on  ;
309200100930        else   ;
309300100930        wdmag=§omagdes;
309400100930        endif  ;
309500100316
309600100316         if ErrGenerico =*on ;
309700100316         ErrMessage=*on  ;
309800100316         wdmag=*all'?'   ;
309900100316         endif  ;
310000100316
310100100316          ENDSR    ;
310200100708       //--------------------------------------------------------------
310300100708       //?controllo Tipo Unità
310400100708       //--------------------------------------------------------------
310500100708        BEGSR ChkTip                 ;
310600100708        clear WDTIP    ;
310700100824        indx=%scan('?':wtip )   ;
310800100827
310900100824        if Indx>0             ;
311000100708         clear tibs02ds  ;
311100100708         t02tla=' '      ;
311200100708         t02mod='R'      ;
311300100708         t02sif=knsif    ;
311400100708         t02cod='UNT'    ;
311500100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
311600100708         if t02ke1<>*blanks;
311700100824         wtip =t02ke1    ;
311800100708         wdtip=t02uni    ;
311900100708         else   ;
312000100708         clear  wtip ;
312100100708         endif  ;
312200100708
312300100708        ErrGenerico=*on   ;
312400100708        leavesr  ;
312500100708        endif   ;
312600100708
312700100708         clear tibs02ds  ;
312800100708         t02tla=' '      ;
312900100708         t02mod='C'      ;
313000100708         t02sif=knsif    ;
313100100708         t02cod='UNT'    ;
313200100708         t02ke1=wtip     ;
313300100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
313400100708
313500100708         if  t02err<>*blanks ;
313600100824         ErrGenerico=*on  ;
313700100824         ErrMessage =*on  ;
313800100824         wdtip=*all'?'   ;
313900100708         else  ;
314000100824         wdtip=t02uni    ;
314100100708         endif    ;
314200100708
314300100708          ENDSR    ;
314400080702       //--------------------------------------------------------------
314500100830       //?Aggiornamento record nuova immissione da SFL
314600080702       //--------------------------------------------------------------
314700080702        BEGSR ConfAGGIO              ;
314800081003
314900100315       s02nrr=1  ;
315000100824       readc    SDUOs02    ;
315100080704
315200100824 1     dow not %eof  ;
315300100831       // Per Gestione, se manca opzione, non c'e' nulla da aggiornare
315400100924       // Per immisssione, devono esserci tipo e mag destinazione
315500100924 2     if (§sdv0imm='S' and vshimm=' ' and vscopz=' ') or
315600100924 2        (§sdv0imm='S' and vshimm='S' and (vsctip<=*zeros or vscmag=0));
315700100831 x2    else   ;
315800100831
315900100831         // chain record
316000100831 3       if vshimm=' '    ;
316100100831           chain(e) vshnrr   ungio00f   ;
316200100831 4          if %error or not %found ;
316300100831             v1cmsg=msg(25)  ;
316400100831             ErrGenerico=*on  ;
316500100831             ErrMessage =*on  ;
316600100831             EXSR AggioSFL   ;
316700100831             leavesr  ;
316800100831 4          endif  ;
316900100831 3       endif  ;
317000100831
317100100907          exsr ScriviUNGIO  ;
317200100907 2     endif  ;
317300100830
317400100830       readc    SDUOs02    ;
317500100831 0      enddo           ;
317600100830       ENDSR            ;
317700100830       //--------------------------------------------------------------
317800100830       //?Aggiornamento file UNGIO
317900100830       //--------------------------------------------------------------
318000100830        BEGSR SCriviUNGIO            ;
318100100830
318200100830 1     if    vshimm=' '  and vscopz='4'  ;
318300100830       delete ungioFIS  ;
318400100830
318500100830 x1    else ;
318600100830
318700100830 2     if    vshimm='S'   ;
318800100827        clear ungiofis  ;
318900100901        timeiso=%time        ;
319000100901        unghim =%dec(timeiso);
319100100930        else   ;
319200100930        // richaino il record per non avere problemi  di "sporco" nei campi
319300100930         chain(e) vshnrr   ungio00f   ;
319400100830 2      endif   ;
319500100830
319600100824       ungdim  =vshdim  ;
319700100824       ungute  =vscute  ;
319800100826       ungcuf  =v1ccuf  ;
319900100825       if vsctip>*zeros  ;
320000100824       ungtip  =%int(vsctip) ;
320100100825       else  ;
320200100825       clear ungtip ;
320300100825       endif  ;
320400100824       ungmat  =vscmat  ;
320500100827       ungqta  =vscqta  ;
320600100827       if ungqta=0 ;
320700100827       ungqta=1 ;
320800100827       endif ;
320900100830       if $Video='S2' ;
321000100825       ungmag  =vscmag  ;
321100100830       else  ;
321200100830       ungmag  =%int(vscmag3)  ;
321300100830       endif  ;
321400100827       ungmgc  =vscmgc  ;
321500100825       unginvr =vscinvr ;
321600100824       ungdes  =vscdes  ;
321700100827       ungnot  =vscnot  ;
321800110404       // Per mag memorizzo vsctru un ungtru  come unità in aumento/sostituz
321900110404       if ProtperRIE=*off  ;
322000110404         ungtru  =vsctru  ;
322100110404         clear §giogsf  ;
322200110404       else  ;
322300110404       // Per "FOR" memorizzo nella DS DGIO01 come guasto fisico
322400110404        §giogsf=vsctru  ;
322500110404        clear ungtru  ;
322600120524       // per FOR "O" aggiorno "N" nella stampa bolla
322700120524       if ImmCMOV=*on  and  vscinvr='O'  ;
322800120524        ungfst='N'  ;
322900120524       endif  ;
323000110404       endif  ;
323100110404
323200110404       ungflo=dgio01  ;
323300110404
323400100827       clear ungksc   ;
323500100827       if vscksc>0 and vscinvr='I'  ;
323600100827       ungksc  =vscksc  ;
323700100827       endif  ;
323800100825       if vsctir>*zeros  ;
323900100824       ungtir  =%int(vsctir) ;
324000100923       ungmatr=vscmatr ;
324100100825       else  ;
324200100825       clear ungtir ;
324300100923       clear ungmatr;
324400100825       endif  ;
324500100827       if vscdtm>0 ;
324600100827       dataeur=%date(vscdtm:*dmy)  ;
324700100827       dataiso=dataeur  ;
324800100827       ungdtm=%dec(dataiso)  ;
324900100922       else  ;
325000100922       clear ungdtm  ;
325100100827       endif  ;
325200100922
325300100827       if vscddo>0 ;
325400100827       dataeur=%date(vscddo:*dmy)  ;
325500100827       dataiso=dataeur  ;
325600100827       ungddo=%dec(dataiso)  ;
325700100922       else  ;
325800100922       clear ungddo ;
325900100827       endif  ;
326000100827       ungdoc=vscdoc  ;
326100100827       ungreso=vscreso ;
326200100827       ungslr =vscslr  ;
326300101008       ungncl= vscncl  ;
326400101008       ungpkg= vscpkg  ;
326500100824
326600100908       // Richiamo pgm che crea i movimenti
326700100930       //  se non è escluso e se sono in conferma
326800100922 2     if §sdv0conf='S'  and vscopz= ' '  ;
326900100908
327000100908       // il tipo "99" lo flaggo senza richiamo
327100100908 3     if ungtip<> 99  ;
327200110412
327300110412       // Se FOR"F" e c'e' nuova matricola la scrivo in UNANA prima di RENDERE
327400110412
327500110412 4     if ProtperFOR=*on and ungnot<>*blanks ;
327600110412       chain (ungtip:ungmat)  unana02l    ;
327700110412 5     if %found(unana02l) ;
327800110412        unamat=ungnot;
327900110412        unadtr=%dec(datasys)    ;
328000110412        update unana2   ;
328100110412 5     endif  ;
328200110412 4     endif  ;
328300100922
328400100907       clear dsunit  ;
328500100907       dsuop1='F6N'  ;
328600100907       dsutip=ungtip  ;
328700100907       kpjbu = dsunit  ;
328800100907       callp TNSDU5R (kpjba: dsungio)  ;
328900100922
329000100922       // Aggiorno file dotazioni se previsto e se tipo unità inviato e di reso diversi
329100101026 4     if unginvr='I'    and ungtru<>'  ' and ungtip<>ungtir and ungksc=0 ;
329200100922       chain (ungtip:ungmag)  undot01l   ;
329300100922 5     if %found(undot01l)  ;
329400100922        dotqta=dotqta+1  ;
329500100922        update undot000 ;
329600100922 x5     else    ;
329700100922        clear undot000;
329800100922        dottip=ungtip  ;
329900100922        dotqta=1 ;
330000100922        dotfil= ungmag ;
330100100922        write undot000 ;
330200100922 5      endif  ;
330300100922
3304001009225      if ungtru='S' and ungtir>0   ;
330500100922       chain (ungtir:ungmag)  undot01l   ;
3306001009226      if %found(undot01l)  ;
330700100922        dotqta=dotqta-1  ;
330800100922        update undot000 ;
3309001009226      endif  ;
3310001010285      endif  ;
3311001010284      endif  ;
331200100922
331300100930       // Se presente anche la matricola aggiorno UNANA con la "S" di RENDERE
331400101028 4     if ungmatr<>*blanks  ;
331500110421
331600110421       //  non imposto al "S" di rientro se già rientrata in data di oggi con FOR "F"
331700110421       exsr SelectSQLFORF   ;
331800110421
331900100930       chain (ungtir:ungmatr)  unana02l    ;
332000101028 5     if %found(unana02l) ;
332100110421
332200110421 4a    if wtrov=' ' ;
332300130321       // se però il magazzino è sede --> non imposto lo stesso la "S"
332400130321          clear tnsdmagds  ;
332500130321          §magmag=%editc(unamag:'X')  ;
332600130321          §imagtmag='C' ;
332700130321          §imagtorg='E  '   ;
332800130321
332900130321        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
333000130321
333100130321        if §omagerr<>' ' or (§omagfag<>'W' and §omagfag<>'Y')  ;
333200100930        %subst(unalsw:15:1)='S'  ;
333300110421 5     endif  ;
333400130321 5     endif  ;
333500130321
333600101027        unadtr=%dec(datasys)    ;
333700100930        update unana2   ;
333800110421 4a    endif  ;
333900110421 4     endif  ;
334000100930
334100100908 3     endif  ;
334200100922
334300100908       ungfmv='S'  ;
334400100908 3     if ungdtm=0  ;
334500100908         ungdtm  =%dec(datasys) ;
334600100908 3     endif  ;
334700100922
334800100908
334900100908         update ungioFIS  ;
335000100907
335100100908 x2    else   ;
335200100907
335300100908  3    if vshimm=' '   ;
335400100830         update ungioFIS  ;
335500100830         else ;
335600100827         write ungiofis  ;
335700100908  3    endif  ;
335800100908 2     endif  ;
335900100907
336000100908 1     endif  ;
336100080704
336200080702        ENDSR;
336300110421       //--------------------------------------------------------------
336400110421       //?Select con SQL su ungio00F   per FOR "F"
336500110421       //--------------------------------------------------------------
336600110421       BEGSR  SelectSQLForF ;
336700110421 1     if ungdtm=0  ;
336800110421         sqdtm  =%dec(datasys) ;
336900110421         else  ;
337000110421         sqdtm=ungdtm     ;
337100110421 1     endif  ;
337200110421
337300110421       clear wtrov  ;
337400110421
337500110421         wrkgetlista='SELECT count(*)  +
337600110421                          from UNGIO00F WHERE ungcuf=''FOR'' and unginvr=''F'' +
337700110421            and ungmat=''' + ungmatr +''' and ungtip=' + %editc(ungtir:'X')  +
337800110421            ' and ungdtm=' + %editc(sqdtm :'X') ;
337900110421
338000110421         exec sql   PREPARE sF FROM :wrkgetlista   ;
338100110421
338200110421         exec sql   DECLARE aF cursor FOR  sF   ;
338300110421
338400110421         exec sql OPEN aF   ;
338500110421
338600110421 0       if  sqlcod=0   ;
338700110421         exec sql FETCH NEXT  FROM aF INTO:sqcont;
338800110421 0         if  sqcont>0   ;
338900110421             wtrov='S'  ;
339000110421 0         endif   ;
339100110421 0       endif   ;
339200110421
339300110421         exec sql CLOSE aF   ;
339400110421
339500110421       ENDSR;
339600110421
339700100830       //--------------------------------------------------------------
339800101001       //?Gestione videata 03
339900100830       //--------------------------------------------------------------
340000100830       BEGSR Gesd03;
340100100830
340200101018       // salvo la matricola: se imposta nel sfl la riporto
340300100831         savMat=vscmat  ;
340400101005         savMatr=vscmatr ;
340500100830         exsr CarRECORD  ;
340600100831         vscMat=savmat  ;
340700101005         vscMatr=savmatr  ;
340800100831
340900100830
341000100830         // Controllo opzione
341100100830         Emismod1=*off ;
341200100830         clear vsdf6           ;
341300120524         clear vsdf15          ;
341400120524         if vscopz<>'4'  and
341500120524            vscinvr<>'O' ;
341600120524           if  vshfst=' '  ;
341700120524            VSDf15=cF15BLO                ;
341800120524           else  ;
341900120524            VSDf15=cF15SBLO                ;
342000120524           endif  ;
342100120524         endif  ;
342200120524
342300100830 1       select   ;
342400100830         when vscopz='4'   ;
342500100901         vsdf6='F6=Conferma ANNULLAMENTO' ;
342600100830         v3dmod=' ANNULLAMENTO '  ;
342700100830
342800100830 1       when vscopz='2'   ;
342900100901         VSDf6='F6=CONFERMA Modifica' ;
343000120524
343100100830         v3dmod='   MODIFICA   '  ;
343200120524         // Se non conferato sproteggo riga
343300100831         if vshfmv=' ' ;
343400100831          protRiga=*off  ;
343500100831         endif  ;
343600100830
343700100830 1       when vscopz='5'   ;
343800100830         v3dmod='  VISUALIZZA  '  ;
343900100830         endsl   ;
344000100830
344100100830         // Imposto i campi diversi
344200100830         vscmag3=%editc(vscmag:'X')    ;
344300100830
344400100830 0       dow    $video='D3'   ;
344500100830
344600100830         // Emissione Testata
344700100830         if  errMessage  = *off;
344800100830           write  SDUOT01;
344900100830         endif;
345000100830
345100100830         // Emissione videata
345200100830         exfmt  SDUOD03;
345300100830
345400100830         reset errMessage;
345500100830         reset errGenerico;
345600100830         clear V1cmsg    ;
345700100830         clear wtmag     ;
345800100830         errMAG=*off     ;
345900100830         errMGC=*off     ;
346000100830         errTIP=*off     ;
346100100830         errMAT=*off     ;
346200100830         errQTA=*off     ;
346300100830         errTRU=*off     ;
346400100830         errTIR=*off     ;
346500100830         errDES=*off     ;
346600110412         errNOT=*off     ;
346700100830         errKSC=*off     ;
346800100830         errDTM=*off     ;
346900100909         errDDO=*off     ;
347000101012         errNCL=*off     ;
347100100923         errMATR=*off     ;
347200101012         errINVR=*off     ;
347300100830
347400100830 1       SELECT;
347500100830
347600100830         // - F12=Ritorno
347700100830 1         WHEN  dsp_aid = c_F12;
347800100930            chain(n) vshnrr   ungio00f   ;
347900100831            exsr CarRECORD  ;
348000100924            ErrGenerico=*off ;
348100100924            ErrMessage =*off ;
348200100924            clear v1cmsg  ;
348300100924            vscMat=savmat  ;
348400101005            vscMatr=savmatr  ;
348500100830            $Video='S2'   ;
348600100831            Emismod1=*on  ;
348700100830            leavesr  ;
348800100901
348900100901         // - F2=Gestione Unità EDP
349000100901 1         WHEN  dsp_aid = c_F02;
349100100927            clear dsunana  ;
349200100928            if vscmat<>*blanks  and
349300100928               (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
349400100927              UIMmat=vscmat  ;
349500100927             endif  ;
349600100928           if vscmatr<>*blanks  and
349700140123 2          (rnmfl= 'VSCMATR ' and v1ccuf<>'FOR' )  ;
349800100928              UIMmat=vscmatr ;
349900100928             endif  ;
350000100901           exsr CallGESUNIT     ;
350100100901
350200100909         // - F7=Ricerca Matricola
350300100901 1         WHEN  dsp_aid = c_F07;
350400100901           // Per annullamento opzione non ammmessa
350500100901           if vscopz='4'  ;
350600100901            ErrGenerico=*on   ;
350700100901            ErrMessage =*on   ;
350800100901            v1cmsg=msg(36)  ;
350900100901           else  ;
351000100901           exsr CallRicMat      ;
351100100901           endif  ;
351200101018
351300101018         // - F11=elimina "Materiale preparato"
351400101018 1         WHEN  dsp_aid = c_F11;
351500101018         chain(e) vshnrr   ungio00f   ;
351600101018         clear ungfpr  ;
351700101018         clear vshfpr  ;
351800101018         update ungiofis  ;
351900101018         RevMag=*off  ;
352000120524
352100120524         // - F15=Blocca/sblocca STAMPA BOLLA
352200120524 1         WHEN  dsp_aid = c_F15;
352300120525           //  In modifica  e visulizzazione
352400120525 2         if vscopz='2' or vscopz='5' ;
352500120524           // e solo se corretto il magazzino di destino
352600120524 3         if   vscinvr='O'   ;
352700120524            ErrGenerico=*on   ;
352800120524            ErrMessage =*on   ;
352900120524            v1cmsg=msg(60)  ;
353000120524 x3        else  ;
353100120524           // solo per magazzini fornitori che prevedono la stampa
353200120524          wmag= vscmag3         ;
353300120524          wazorg='E  '   ;
353400120524          exsr  chkmag   ;
353500120524
353600120524  4       if §omagfag='Y' or §omagfag='F' or §omagfag='A'  or
353700120524             §omaglue='S'  ;
353800120525
353900120524           chain(e) vshnrr   ungio00f   ;
354000120525           select   ;
354100120525  5        when ungfst='N'  ;
354200120524           ungfst=' '  ;
354300120524           vshfst=' '  ;
354400120525            VSDf15=cF15BLO                ;
354500120524           NOstampaBO=*off ;
354600120525
354700120525  5        when ungfst='S' ;
354800120525             v1cmsg=msg(62)  ;
354900120525             ErrGenerico=*on  ;
355000120525             ErrMessage =*on  ;
355100120525  x5         other ;
355200120524           ungfst='N'  ;
355300120524           vshfst='N'  ;
355400120525            VSDf15=cF15SBLO                ;
355500120524           NOstampaBO=*on  ;
355600120525  5       endsl   ;
355700120524         update ungiofis  ;
355800120524  x4      else  ;
355900120524         // Errore --> magazzino che non prevede la stampa
356000120524            ErrGenerico=*on   ;
356100120524            ErrMessage =*on   ;
356200120524            v1cmsg=msg(61)  ;
356300120524  4       endif  ;
356400120524  3       endif  ;
356500120524  2       endif  ;
356600120524
356700100901
356800100901         // - F14=Gestione tabella scheda clienti x scambio dati
356900100901 1         WHEN  dsp_aid = c_F14;
357000100901           // Per annullamento opzione non ammmessa
357100100901           if vscopz='4'  ;
357200100901            ErrGenerico=*on   ;
357300100901            ErrMessage =*on   ;
357400100901            v1cmsg=msg(36)  ;
357500100901           else  ;
357600100901           exsr Callpgm3R       ;
357700100901           endif  ;
357800100909
357900100830 x1      // Invio
358000100830           OTHER;
358100100830
358200100924           // non controllo record per opzione '4'
358300100924 3         if vscopz<>'4'   ;
358400100830           exsr contrRIGA ;
358500100924           endif   ;
358600100830
358700100830 2         if ErrGenerico=*Off   ;
358800100830
358900100830           //  Aggiornamento record di UNGIO
359000100830 2         if   dsp_aid = c_F06  and vscopz<>'5';
359100100831            exsr   ScriviUNGIO ;
359200100831            $Video='S2'   ;
359300100831            Emismod1=*on  ;
359400100830
359500100830           //  Ricarico i dati del record nel sfl aggiornati per opz "2"
359600100830 3         if vscopz='2'   ;
359700100830           exsr CarRECORD  ;
359800100830 x3        else  ;
359900100830           // scrivo la dicitura "ANNULLATO"
360000100830           clear  vscnot;
360100100830           vscnot='ANNULLATO  ' ;
360200100830           vshann='S' ;
360300100830 2         endif   ;
360400100830
360500100830         protRiga=*on   ;
360600100830 2       endif   ;
360700100830 2       endif   ;
360800100830
360900100830 1       ENDSL;
361000100830
361100100830 0     enddo   ;
361200100830
361300100830       ENDSR;
361400100830
361500101001       //--------------------------------------------------------------
361600101001       //?Gestione videata 04 - inserimento multiplo
361700101001       //--------------------------------------------------------------
361800101001       BEGSR Gesd04;
361900101001
362000101001         v3dmod='INSER.MULTIPLO'  ;
362100101001
362200101001         // pulisco i campi
362300101001         clear vscmag3  ;
362400101001         clear vsdmag   ;
362500101001         clear vscqta   ;
362600101001         clear vsctip   ;
362700101001         clear vsdtip3  ;
362800101001         clear vsctru   ;
362900101001         clear vsctir   ;
363000101001         clear vsdtir   ;
363100101001         clear vscksc   ;
363200101001         clear vsdksc3  ;
363300101001         clear vscdoc   ;
363400101001         clear vscddo   ;
363500101001         clear vscdtm   ;
363600101008         clear v4cncl   ;
363700101008         clear v4cpkg   ;
363800101001         clear v4cinse  ;
363900101001
364000101001 0       dow    $video='D4'   ;
364100101001
364200101001         // Emissione Testata
364300101001         if  errMessage  = *off;
364400101001           write  SDUOT01;
364500101001         endif;
364600101001
364700101001         // Emissione videata
364800101001         exfmt  SDUOD04;
364900101001
365000101001         reset errMessage;
365100101001         reset errGenerico;
365200101001         clear V1cmsg    ;
365300101001         clear wtmag     ;
365400101001         errMAG=*off     ;
365500101001         errTIP=*off     ;
365600101001         errQTA=*off     ;
365700101001         errTRU=*off     ;
365800101001         errTIR=*off     ;
365900101001         errKSC=*off     ;
366000101001         errDTM=*off     ;
366100101001         errDDO=*off     ;
366200101012         errNCL=*off     ;
366300101001         errINSE=*off     ;
366400101001
366500101001 1       SELECT;
366600101001
366700101001         // - F12=Ritorno
366800101001 1         WHEN  dsp_aid = c_F12;
366900101001            $Video='S2'   ;
367000101001            Emismod1=*on  ;
367100101001            leavesr  ;
367200101001
367300101001 x1      // Invio
367400101001           OTHER;
367500101001
367600101001           exsr contrRIGA ;
367700101001
367800101001 2         if ErrGenerico=*Off   ;
367900101001
368000101001           //  Aggiornamento record di UNGIO
368100101001 3         if   dsp_aid = c_F06 ;
368200101001            $Video='S2'   ;
368300101001            exsr   Scrivirighe ;
368400101001            Emismod1=*on  ;
368500101001
368600101001         protRiga=*on   ;
368700101001 3       endif   ;
368800101001 2       endif   ;
368900101001
369000101001 1       ENDSL;
369100101001
369200101001 0     enddo   ;
369300101001
369400101001       ENDSR;
369500101001
369600101001       //--------------------------------------------------------------
369700101001       //?Per inrimento multiplo scrivo righe nel sfl
369800101001       //--------------------------------------------------------------
369900101001       BEGSR Scrivirighe  ;
370000101001
370100101001       // salvo i campi per la scrittura del sfl
370200101001       if vscmag3>*zeros ;
370300101001       v4cmag=%int(vscmag3)  ;
370400101001       endif  ;
370500101001       v4cqta=vscqta  ;
370600101001       v4cqta=vscqta  ;
370700101001       v4ctip=vsctip  ;
370800101001       v4ctru=vsctru  ;
370900101001       v4ctir=vsctir  ;
371000101001       v4cksc=vscksc  ;
371100101001       v4dksc=vsdksc3 ;
371200101001       v4cdtm=vscdtm  ;
371300101001       v4cdoc=vscdoc  ;
371400101001       v4cddo=vscddo  ;
371500101001
371600101001
371700101001       // Dal primo vuoto in poi mi posizione e cerco il primo record libero
371800101001       s02nrr=Primovuoto  ;
371900101001
372000101001       Indx= 1 ;
372100101001       dow Indx<=v4cinse  ;
372200101001
372300101001       Lib='0'  ;
372400101001       dow Lib='0'  ;
372500101001        chain s02nrr sduos02 ;
372600101001        if %found  and (vscmag<>0 or vscdes<>*blanks  or vscksc<>0 or
372700101001         vsdksc<>*blanks  or vscnot<>*blanks or (vsctip<>*zeros and vsctip
372800101001         <>*blanks ) or vscmat<>*blanks) ;
372900101001         exsr Aggiosfl  ;
373000101001         s02nrr=s02nrr+1  ;
373100101001        else  ;
373200101001         Lib='1'  ;
373300101001        endif  ;
373400101001       enddo  ;
373500101001
373600101001       if not %found ;
373700101001       savnrr=s02nrr ;
373800101001       s02nrr=s02nrr-1 ;
373900101001       exsr RigheVuote  ;
374000101001       s02nrr=savnrr  ;
374100101001        chain s02nrr sduos02 ;
374200101001       endif  ;
374300101001       vscmag=v4cmag  ;
374400101001       vscqta=v4cqta  ;
374500101001       vscqta=v4cqta  ;
374600101001       vsctip=v4ctip  ;
374700101001       vsctru=v4ctru  ;
374800101001       vsctir=v4ctir  ;
374900101001       vscksc=v4cksc  ;
375000101001       vsdksc=v4dksc  ;
375100101001       vscdtm=v4cdtm  ;
375200101001       vscdoc=v4cdoc  ;
375300101001       vscddo=v4cddo  ;
375400101008       vscncl=v4cncl  ;
375500101008       vscpkg=v4cpkg  ;
375600101001
375700101001       exsr Aggiosfl  ;
375800101001
375900101001       s02nrr=s02nrr+1  ;
376000101001       indx=indx+1  ;
376100101001       enddo  ;
376200101001
376300101001       c02csr=s02nrr  ;
376400101001
376500101001       ENDSR;
376600080206       //--------------------------------------------------------------
376700080206       //?Operazioni finali.
376800080206       //--------------------------------------------------------------
376900080206       BEGSR RoutEnd;
377000080627
377100080704         // Chiusura pgm   ;
377200080704         clear tibs02ds  ;
377300080704         t02tla='C'      ;
377400080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
377500100903
377600100903         // Sblocco data area per pgm manut matricole
377700100907         if (§sdv0mmat='S'  and savdatblo=0) or
377800100907            (§sdv0conf='S'  and savdatblo=0) ;
377900100903         in *lock  tnsduo ;
378000100903         clear tnsduo   ;
378100100903         out tnsduo  ;
378200100903         endif  ;
378300080704
378400100920         if %open(fnlv22p) ;
378500100920         close    fnlv22p  ;
378600100920         endif  ;
378700100920
378800080206         *inLR = *on;
378900080704
379000080206         return;
379100080206
379200080206       ENDSR;
379300080206
379400080206      /end-free
379500100920     c*--------------------------------------------------------------
379600100920     c* videata richeista stampanti e ovrdbf
379700100920     c*--------------------------------------------------------------
379800100920     c     RICHSTAMPAN   BEGSR
379900140124     c* verifico la key della tabella
380000100920     C                   CLEAR                   TRUL90DS
380100100920      *
380200100920     C                   MOVEL     'S'           d90RSE
380300100920     C                   CALL      'TRUL90R'
380400100920     C                   PARM                    KPJBA
380500100920     C                   PARM                    trul90DS
380600140124     C                   PARM      'SED'         k_key             3
380700100920      *
380800100920     C* F3 - FINE
380900100920     c                   if        d90f3<>'1'
381000100920     c                   eval      comman=ovrp(1)
381100100920     c                   eval      lung=91
381200100920     c                   eval      %subst(comman:28:10)=d90pre
381300100920      *
381400100920     C*
381500100920     C                   CALL      'QCMDEXC'                            91
381600100920     C                   PARM                    COMMAN
381700100920     C                   PARM                    LUNG
381800100920      *
381900100920     C                   if        not *in91
382000100920     c                   open      fnlv22p
382100100920     c                   endif
382200100920     c                   endif
382300100920      *
382400100920     c                   ENDSR
382500080206
382600080206       //--------------------------------------------------------------
382700080206       //?Schiere a tempo di compilazione.
382800080206       //--------------------------------------------------------------
382900080206
383000080410** - MSG ------------------------------------------------------------------ -*
383100100316Utente errato                                                                   01
383200100316Data Errata                                                                     02
383300100316Magazzino Destinazione Errato                                                   03
383400100316Magazzino Correlato Errato                                                      04
383500100708Tipo unità errato                                                               05
383600100826Tipo / Matricola inesistente in anagrafica                                      06
383700100708Matricola inesistente in anagrafica                                             07
383800100708Matricola Esistente in anagrafica   ma NON univoca                              08
383900100824Scorrimento non più consentito                                                  09
384000100826L'indicazione dell'ufficio è obbligatoria                                       10
384100140124Per magazzino CED  o TLC possibile solo l'opzione "INVII"                       11
384200100908Indicare anche dtaImmissione o dtaConf. o dati Da Confermare o unità da Rendere 12
384300100826La matricola NON ci vuole per tipo unità "99"                                   13
384400140207Tipo unità "99" non accettato per "FOR" rientri fornitori o sede                14
384500101112La quantità maggiore di 1 è da immettere solo per tipo unità 99                 15
384600100826Aumento/Sostituzione unità indicabile non per tipo "99"                         16
384700100826L'unità di RESO è indicabile solo se prevista la sostituizione dell'unità       17
384800100826Tipo unità di reso errata                                                       18
384900100826Descrizione obbligatoria                                                        19
385000100930Per tipi"48 e 49"indicare Cliente destinatario unità o digita"SCORTA" nelleNOTE 20
385100100826Codice cliente errato                                                           21
385200100924La data conferma movimento non può essere maggiore di 3 mesi circa...           22
385300130321Esiste già un invio/rientro giornaliero da confermare x questo tipo/matricola   23
385400100826Tipo/matricola già inseriti in questa pagina di lavoro!!                        24
385500100830Modifica/annull. al momento non eseguibili: record vincolato da altro lavoro    25
385600100830Opzione non ammessa                                                             26
385700100831Nessun dato da visualizzare                                                     27
385800100831Indicare se Includere o Escludere il tipo unità indicato                        28
385900100831Impossibile annullare: record già confermato                                    29
386000121130Impostare data conferma movimenti                                               30
386100121130Data conferma non inferiore a oggi                                              31
386200100831In fase di conferma movimenti obbligatorio inserire la matricola                32
386300100909Numero e data documento obbligatori se RIENTRO da fornitore                     33
386400100909Per interrogare i movimenti è necessaria la matricola                           34
386500100923Matricola errata perchè di unità diversa da quella immessa e NON è modificabile 35
386600100901Tasto funzionale non ammesso in Annullamento                                    36
386700100907Opz non utilizzabile:pgm "                    "  in uso da xxxxxxxxxx il xx/xx  37
386800100903Magazzino di Destinazione e Correlato non possone essere uguali                 38
386900100908Indicare se invio o rientro da fornitore o filiale I/R/F                        39
387000100908Data AL non può essere minore data DAL                                          40
387100100908Immettere o i dati da confermare o la data conferma dal/al                      41
387200100920Il magazzino DESTINAZIONE deve essere XXX                                       42
387300101006Per opzione"9"-->Stampa Etichetta,premere tasto F10 e selezionare la stampante  43
387400100923Se immessa la matricola di reso immettere anche il TIPO unità di reso           44
387500101005Mag.Ubicazione matricola di RESO  non  uguale al mag.di INVIO. Enter forza      45
387600100923Tipo invio / rientro obbligatorio per "FOR"                                     46
387700100927Se Esclusione di tipo unità, non indicare la matricola                          47
387800100930Tasto funz. F07 abilitato solo per la ricerca matricole di RESO: posizionarsi!! 48
387900100930Matricola con tipo unità diverso da quello indicato                             49
388000101001Immettere in numero degli inserimenti da effettuare                             50
388100101001Per inserimento multiplo, immettere almeno un dato                              51
388200101012F8-Materiale preparato ammesso solo per INVII                                   52
388300120525Per opzioni: P=Materiale preparato o B=blocca stampa bolla, premere F8          53
388400101012Mancano colli e peso per il magazzino destinazione indicato                     54
388500111024Se unità in proprietà alla 923 magazzino destinazione non può essere "990 980"  55
388600110404Immettere "S" = Si per indicare il guasto fisico o lasciare vuoto               56
388700110412Nuova matricola errata perchè già esistente !!                                  57
388800110412Immettere una matricola di massino 15 caratteri                                 58
388900110610Campo note obbligatorio se guasto fisico                                        59
389000120524La stampa bolla non viene eseguita per rientro fornitore diretto in filiale     60
389100120524F15 non ammesso: il magazzino non prevede la stampa                             61
389200120525Impossibile bloccare la stampa bolla: già stampata!!                            62
389300121130Data conferma movimento no > di 3 mesi                                          63
389400121203Immettere anche data DAL per richiesta movimenti CONFERMATI                     64
389500121203Massimo un mese e possibile richiedere per i movimenti CONFERMATI               65
389600121203Impossibile modificare la data a movimenti già confermati                       66
389700130116Il magazzino di destinazione non corrisponde alla ditta manutenzione nell'anagr.67
389800130626Se unità NON in proprietà alla 923 magazzino destinazione non può essere "923"  68
389900140207La conferma dei movimenti può essere fatta solo da un utente EDP                69
390000100920**
390100100920OVRPRTF FILE(FNLV22P) OUTQ(xxxxxxxxxx) FORMTYPE('ETCH_EDP') USRDTA('Unità EDP') SHARE(*YES)
