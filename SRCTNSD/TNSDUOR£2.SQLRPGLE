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
007800130116     d MSG             s             78    dim(67) ctdata perrcd(1)
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
060600100316       //   Profilo utente
060700100708 1     if v1cute<>*blanks   ;
060800100316       chain   v1cute  kpprf01l   ;
060900100316
061000100708 2     if not %found(kpprf01l)  ;
061100100316           errUTE=*on ;
061200100316           v1cmsg = Msg(1);
061300100316           errGenerico=*on  ;
061400100316           errMessage=*on  ;
061500100316           leavesr  ;
061600100824 2       endif   ;
061700100708 1     endif   ;
061800100316
061900100316       // Data Immissione
062000100831       clear w1cdim   ;
062100100708 1     if v1cdim>0   ;
062200100316             clear wlbdat;
062300100824             g02dat = v1cdim ;
062400100316             xsrda8(wlbdat);
062500100824 2           if g02err = '1';
062600100316               errMessage  = *on;
062700100316               errGenerico = *on;
062800100708               ErrDIM    = *on;
062900100824               V1cmsg = Msg(02);
063000100316               leavesr;
063100100824 2           endif;
063200100316
063300100316             v1cdim = g02dat;
063400100316             w1cdim = g02inv;
063500100708 1     endif   ;
063600100316
063700100826       // Ufficio obbligatorio
063800100826       if v1ccuf=*blanks   ;
063900100826               errMessage  = *on;
064000100826               errGenerico = *on;
064100100826               ErrCUF    = *on;
064200100826               V1cmsg = Msg(10);
064300100826               leavesr;
064400100826       endif  ;
064500100826
064600100923       // tipo invio obbligatorio per FOR solo in immissione
064700100923       if v1ccuf='FOR' and v1cinvr= ' ' and §sdv0imm='S' ;
064800100923               errMessage  = *on;
064900100923               errGenerico = *on;
065000100923               ErrINVR   = *on;
065100100923               V1cmsg = Msg(46);
065200100923               leavesr;
065300100923       endif  ;
065400100923       if v1ccuf=*blanks   ;
065500100923               errMessage  = *on;
065600100923               errGenerico = *on;
065700100923               ErrCUF    = *on;
065800100923               V1cmsg = Msg(10);
065900100923               leavesr;
066000100923       endif  ;
066100100826       // Per MAG possibile solo I=invio
066200100826       if v1ccuf='MAG' and v1cinvr<>'I' and v1cinvr<>' ' ;
066300100826               errMessage  = *on;
066400100826               errGenerico = *on;
066500100826               ErrCUF    = *on;
066600100826               V1cmsg = Msg(11);
066700100826               leavesr;
066800100827       endif  ;
066900100826
067000100826       // Immettere o la data immissione o solo i DA confermare
067100100831       //  o solo unità di reso
067200121203       if v1cdim=0 and v1creso =' '
067300121203          and v1cdtm=0 and v1cdtmd = 0 and (v1cconf=' ' or v1cconf='C');
067400100826               errMessage  = *on;
067500100826               errGenerico = *on;
067600100826               ErrDIM    = *on;
067700100826               V1cmsg = Msg(12);
067800100826               leavesr;
067900100827       endif  ;
068000100831
068100100831       // Data conferma movimento: obbligatoria in conferma
068200100831       clear w1cdtm  ;
068300100831 1     if v1cdtm=0  and §sdv0conf='S' ;
068400100831               errMessage  = *on;
068500100831               errGenerico = *on;
068600100831               ErrDTM    = *on;
068700100831               V1cmsg = Msg(30);
068800100831               leavesr;
068900100831 2     endif;
069000100908
069100100831       if v1cdtm>0  ;
069200100831             clear wlbdat;
069300100831             g02dat = v1cdtm ;
069400100831             xsrda8(wlbdat);
069500100831 2           if g02err = '1';
069600100831               errMessage  = *on;
069700100831               errGenerico = *on;
069800100831               ErrDTM    = *on;
069900100831               V1cmsg = Msg(02);
070000100831               leavesr;
070100100831 2           endif;
070200100831
070300100831       // non inferiore a oggi
070400100831       if g02inv<%dec(datasys)  ;
070500100831               errMessage  = *on;
070600100831               errGenerico = *on;
070700100831               ErrDTM    = *on;
070800100831               V1cmsg = Msg(31);
070900100831               leavesr;
071000100831       endif   ;
071100100831             v1cdtm = g02dat;
071200100831             w1cdtm = g02inv;
071300100831 1     endif   ;
071400100908
071500100908       // data conferma movimenti dal / al
071600100908       clear w1cdtmd ;
071700100908       clear w1cdtma ;
071800121203       clear w1ctgid ;
071900121203       clear w1ctgia ;
072000101112       // Incongruente se richisti  movimenti DA confermare
072100110914       if (v1cdtmd>0 or v1cdtma>0) and v1cconf='S'  and ModInt=*on  ;
072200100908               errMessage  = *on;
072300100908               errGenerico = *on;
072400100908               ErrDTM    = *on;
072500100908               V1cmsg = Msg(41);
072600100908               leavesr;
072700100908       endif   ;
072800100908
072900100908 1     if v1cdtmd >0  ;
073000100908             clear wlbdat;
073100100908             g02dat = v1cdtmd;
073200100908             xsrda8(wlbdat);
073300100908 2           if g02err = '1';
073400100908               errMessage  = *on;
073500100908               errGenerico = *on;
073600100908               ErrDTM    = *on;
073700100908               V1cmsg = Msg(02);
073800100908               leavesr;
073900100908 2           endif;
074000100908             v1cdtmd= g02dat;
074100100908             w1cdtmd= g02inv;
074200121203             w1cTGId= g02TGI;
074300100908       endif   ;
074400100831
074500100908 1     if v1cdtma >0  ;
074600100908             clear wlbdat;
074700100908             g02dat = v1cdtma;
074800100908             xsrda8(wlbdat);
074900100908 2           if g02err = '1';
075000100908               errMessage  = *on;
075100100908               errGenerico = *on;
075200100908               ErrDTMA   = *on;
075300100908               V1cmsg = Msg(02);
075400100908               leavesr;
075500100908 2           endif;
075600100908             v1cdtma= g02dat;
075700100908             w1cdtma= g02inv;
075800121203             w1cTGIA= g02TGI;
075900100908
076000100908       // Data A non minore data DAL
076100100908 2     if w1cdtma<w1cdtmd  ;
076200100908               errMessage  = *on;
076300100908               errGenerico = *on;
076400100908               ErrDTMA   = *on;
076500100908               V1cmsg = Msg(40);
076600100908               leavesr;
076700100908 2     endif  ;
076800100908 1     endif   ;
076900121203
077000121203       // Se richIESTi i confermati max i mese posso richiedere
077100121203        if v1cconf<>'S' and v1cdtmd>0  and v1cdtma>0  ;
077200121203          WDIFFE=w1ctgia-w1ctgid  ;
077300121203          if wdiffe>32  ;
077400121203               errMessage  = *on;
077500121203               errGenerico = *on;
077600121203               ErrDTMA   = *on;
077700121203               V1cmsg = Msg(65)  ;
077800121203               leavesr;
077900121203          endif  ;
078000121203        endif  ;
078100100908
078200100316       // Magazzino destinazione
078300100708 1     if v1cmag<>*blanks and v1cmag<>'000' ;
078400100316
078500100909       wtmag='D'      ;
078600100908       wazorg='E  '   ;
078700100708       wmag=v1cmag    ;
078800100824       exsr  chkmag   ;
078900100824       v1cmag=wmag    ;
079000100316
079100100708       if  ErrGenerico=*ON  ;
079200100708         ErrMag=*on  ;
079300100316         leavesr  ;
079400100708       endif    ;
079500100708       else  ;
079600100708       clear v1cmag  ;
079700100708 1     endif    ;
079800100708
079900100708       // Magazzino correlato
080000100708 1     if v1cmgc<>*blanks  and v1cmgc<>'000'   ;
080100100708
080200100909       wtmag='C'      ;
080300100908       wazorg='E  '   ;
080400100708       wmag=v1cmgc    ;
080500100708       exsr  Chkmag   ;
080600100824       v1cmgc=wmag  ;
080700100708
080800100826       if  ErrGenerico=*ON  ;
080900100708         ErrMgc=*on  ;
081000100708         leavesr  ;
081100100708       endif    ;
081200100708       else  ;
081300100708       clear v1cmgc  ;
081400100708       endif    ;
081500100316
081600100708       // Tipo unita
081700100824       clear v1dtip  ;
081800100831 1     if v1ctip<>*blanks ;
081900100708         wtip=v1ctip   ;
082000100708         exsr chktip   ;
082100100824
082200100824         v1ctip=wtip  ;
082300100824         v1dtip=wdtip  ;
082400100827 2       if Errgenerico=*on  ;
082500100826          errTIP=*on       ;
082600100827 3       if ErrMessage =*on  ;
082700100826          v1cmsg=msg(05)  ;
082800100827 3       endif    ;
082900100826          leavesr  ;
083000100827 2       endif    ;
083100100831
083200100831       // Immettere anche Includi o Escludi
083300100831 2     if v1ciet=' '  ;
083400100831          errIET=*on       ;
083500100927          ErrGenerico=*on  ;
083600100927 3        ErrMessage =*on  ;
083700100831          v1cmsg=msg(28)  ;
083800100831          leavesr  ;
083900100831 2     endif   ;
084000100831
084100100827 x1    else  ;
084200100708       clear v1ctip  ;
084300100831       clear v1ciet  ;
084400100708 1     endif    ;
084500100927       // Se esclusione di un tipo, impossibile indciare la matricola
084600100927       if v1ciet='E' and v1ctip>*zeros  and v1cmat<>*blanks  ;
084700100927         Errgenerico=*on  ;
084800100927         Errmessage =*on  ;
084900100927         Errmat=*on  ;
085000100927         v1cmsg=msg(47)  ;
085100100927         leavesr  ;
085200100927 3     endif    ;
085300100708
085400100708       // Matricola
085500100708 1     if v1cmat<>*blanks  ;
085600100708
085700100824 2     if v1ctip>*zeros   ;
085800100824       ktip=%int(v1ctip)  ;
085900100930       setll (v1cmat:ktip)  unana07l    ;
086000100930 3     if not %equal(unana07l)  ;
086100100708         Errgenerico=*on  ;
086200100708         Errmessage =*on  ;
086300100708         Errmat=*on  ;
086400100708         v1cmsg=msg(06)  ;
086500100708         leavesr  ;
086600100708 3     endif    ;
086700100708
086800100708 x2    else   ;
086900100708       setll (v1cmat)  unana07l    ;
087000100708 3     if not %equal(unana07l)  ;
087100100708         Errgenerico=*on  ;
087200100708         Errmessage =*on  ;
087300100708         Errmat=*on  ;
087400100708         v1cmsg=msg(07)  ;
087500100708         leavesr  ;
087600100927 x3    //else   ;
087700100708
087800100708       // controllo se univoca
087900100927       //clear  qtatem   ;
088000100927       //reade v1cmat   unana07l   ;
088100100927 4     //dow  not %eof(unana07l) ;
088200100927 5     //if qtatem=0   ;
088300100927       //qtatem=qtatem+1  ;
088400100927       //v1ctip=%editc(unatip:'X') ;
088500100927 x5    //else   ;
088600100927       //  Errgenerico=*on  ;
088700100927       //  Errmessage =*on  ;
088800100927        // Errmat=*on  ;
088900100927        // v1cmsg=msg(08)  ;
089000100927       //leave  ;
089100100927 5     //endif   ;
089200100927       //reade v1cmat   unana07l   ;
089300100927 4     //enddo   ;
089400100708 3     endif    ;
089500100708 2     endif    ;
089600100708 1     endif    ;
089700100708
089800100316       ENDSR  ;
089900080206       //--------------------------------------------------------------
090000100312       //?Gestione SFL 02
090100080206       //--------------------------------------------------------------
090200100312       BEGSR GesS02;
090300080207
090400080207         // Inizializzazione videata
090500100312         if  $InzS02 = *on;
090600100312            exsr InzS02;
090700100824            $InzS02  = *off;
090800100824         endif;
090900100827
091000100831         // INTERRROGAZIONE: se non trovati record errore in prima videata
091100100831         if s02nrr=0   ;
091200100831         v1cmsg=msg(27)  ;
091300100831         errGenerico=*on  ;
091400100831         ErrMessage=*on  ;
091500100831         $Video='D1'  ;
091600100831         leavesr   ;
091700100831         endif  ;
091800100831
091900100831         // Posizionamento cursore
092000101005 1       if  C02csr  > *zero ;
092100101005
092200100924         // se non ci sono errori e il record +1 è vuoto, mi posiziono qui
092300101005 2       if ErrGenerico =*off ;
092400101005
092500101005 2a      if §sdv0imm='S'    ;
092600100924           s02nrr=c02csr    ;
092700100924           chain s02nrr sduos02   ;
092800101005 3         if %found ;
092900100927            exsr Aggiosfl  ;
093000101027            else  ;
093100101027            // Se non trovato riporto indietro di uno il campo per visulizzare il sfl
093200101027            C02csr=c02csr-1  ;
093300101005 3         endif  ;
093400101005 3         if %found and vsctip>=*zeros ;
093500100927
093600100924            s02nrr=c02csr+1  ;
093700100924            chain s02nrr sduos02   ;
093800101005 4          if %found and vshimm='S'  and vsctip<=*zeros ;
093900100924             c02csr=s02nrr ;
094000101005 4          endif ;
094100101005 4          if %found ;
094200100927             exsr Aggiosfl  ;
094300101005 4         endif  ;
094400101005 3         endif  ;
094500101005 2a      endif  ;
094600101005
094700101005         // Per immissione matricole mi posiziono sul record senza matricola
094800101005 2a      if §sdv0MMAT='S'    ;
094900101005           s02nrr=c02csr    ;
095000101005           chain s02nrr sduos02   ;
095100101005 3         if %found ;
095200101005            exsr Aggiosfl  ;
095300101005 3         endif  ;
095400101005 3         if %found and vscmat<>*blanks  ;
095500101005
095600101005            s02nrr=c02csr+1  ;
095700101005            chain s02nrr sduos02   ;
095800101005 4          if %found and vscmat=*blanks  ;
095900101005             c02csr=s02nrr ;
096000101005 4          endif ;
096100101005 4          if %found ;
096200101005             exsr Aggiosfl  ;
096300101005 4         endif  ;
096400101005 3         endif  ;
096500101005 2a      endif  ;
096600101005
096700101005 2       endif  ;
096800100924
096900100827           C02rcd = C02csr;
097000101005 1       endif  ;
097100080207
097200080207         // Emissione Testata e Piede con tasti funzionali abilitati
097300100824         if  errMessage  = *off;
097400100824           write  SDUOT01;
097500100824           write  SDUOZ02;
097600080207         endif;
097700080703
097800080207         // Emissione videata
097900100316         exfmt  SDUOC02;
098000080410
098100080207         reset errMessage;
098200080207         reset errGenerico;
098300100824         clear V1cmsg;
098400100830         errMAG=*off     ;
098500100830         errMGC=*off     ;
098600100830         errTIP=*off     ;
098700100830         errMAT=*off     ;
098800100830         errQTA=*off     ;
098900100830         errTRU=*off     ;
099000100830         errTIR=*off     ;
099100100830         errDES=*off     ;
099200110412         errNOT=*off     ;
099300100830         errKSC=*off     ;
099400100830         errDTM=*off     ;
099500100909         errDDO=*off     ;
099600101012         errNCL=*off     ;
099700100923         errMATR=*off     ;
099800100909         errINVR=*off     ;
099900100927         s02nrr=1  ;
100000080207
100100080523 1       SELECT;
100200080207
100300080207         // - F3=Fine
100400080523 1         WHEN  dsp_aid = c_F03;
100500080409            $Fine = *on;
100600080207
100700080207         // - F12=Ritorno
100800080523 1         WHEN  dsp_aid = c_F12;
100900100315           $video='D1'   ;
101000100824
101100100824         // - Roll-Up
101200100824           WHEN  dsp_aid = c_RollUp;
101300100831         if §sdv0imm= 'S'  ;
101400100824           // - Verifica se raggiunto il limite di records registrabili
101500100824           //   nel subfile (9999)
101600100824             if  S02nrr = *hival;
101700100824               errMessage  = *on;
101800100824               errGenerico = *on;
101900100824               V1cmsg = Msg(09);
102000100824             else;
102100100824               exsr RollUpS02;
102200100824             endif;
102300100830             endif;
102400100901
102500100901         // - F2=Gestione Unità EDP
102600100901 1         WHEN  dsp_aid = c_F02;
102700100924           clear dsunana ;
102800100924           chain c02csr sduos02 ;
102900100927           if %found ;
103000100927           exsr Aggiosfl  ;
103100100927           endif   ;
103200100928           if %found  and vscmat<>*blanks  and
103300100928 2          (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
103400100924              UIMmat=vscmat  ;
103500100927             endif  ;
103600100928           if %found  and vscmatr<>*blanks  and
103700100928 2          (rnmfl= 'VSCMATR ' and v1ccuf='MAG' )  ;
103800100928              UIMmat=vscmatr ;
103900100928             endif  ;
104000100901           exsr CallGESUNIT     ;
104100100824
104200100901         // - F7=Ricerca Matricola
104300100901 1         WHEN  dsp_aid = c_F07;
104400100901           chain c02csr sduos02 ;
104500100901           if %found   ;
104600100901           exsr CallRicMat      ;
104700100901           endif  ;
104800100920
104900100901
105000100901         // - F14=Gestione tabella scheda clienti x scambio dati
105100100901 1         WHEN  dsp_aid = c_F14;
105200100901           exsr Callpgm3R       ;
105300100909
105400101001         // - F15=Immissione multipla
105500101001 1         WHEN  dsp_aid = c_F15;
105600101001            $VIDEO='D4'   ;
105700101001            exsr Gesd04  ;
105800101001
105900110404         // - F21=Abilita campo opzione
106000100909 1         WHEN  dsp_aid = c_F21;
106100100909           chain c02csr sduos02 ;
106200100909           if %found   ;
106300100909           exsr Protcampi  ;
106400100909           exsr Aggiosfl    ;
106500100909           endif  ;
106600100909
106700100901
106800080530 x1      // Invio / F6
106900080207           OTHER;
107000081006
107100100315               exsr ContrS02 ;
107200100930              if %open(fnlv22p) ;
107300100930               close    fnlv22p  ;
107400100930              endif  ;
107500100930
107600100830 2             if  errGenerico = *on or wSceltaOPZ='S';
107700120524               if wsceltaOPZ='S'  ;
107800120524                 Errmessage=*off   ;
107900120524               endif  ;
108000081006                 leavesr;
108100100824 2             endif;
108200080530
108300121130         // se non ci sono errori--> pulisco la data modifica conferma mov
108400121130               clear w05dtm  ;
108500121203               clear w05amg_dtm  ;
108600121130
108700100907         // F6=conferma Aggiornamento
108800100920 1         if   dsp_aid = c_F06 ;
108900100907
109000100907         // Non posso confermare immissione se pgm CONFERMA in uso
109100100907           if   §sdv0imm='S' ;
109200100907            in tnsduo   ;
109300100907            if §sduodat>0 and %subst(§sduopgm:1:1)='C'   ;
109400100907            exsr Msg37  ;
109500100907            leavesr     ;
109600100907           endif  ;
109700100907           endif  ;
109800100907
109900080702           exsr   ConfAggio ;
110000081023
110100100315           $video='D1'   ;
110200080530           endif  ;
110300080207
110400080523 1       ENDSL;
110500080207
110600080207       ENDSR;
110700100901       //--------------------------------------------------------------
110800100901       //?chiamata pgm gestione unità EDP
110900100901       //--------------------------------------------------------------
111000100901       BEGSR CallGesUNIT ;
111100100901
111200100901       clear dsunit  ;
111300100901       dsupro='A'  ;
111400100901       if §sdv0int='S'   ;
111500100901       dsuop0=' 5'  ;
111600100901       else  ;
111700100901       dsuop0=' 2'  ;
111800100901       endif  ;
111900100901
112000100901       clear kpjbu  ;
112100100901       kpjbu=dsunit  ;
112200100924       callp TNSDU1R  (kpjba:dsunana)  ;
112300100901
112400100901       ENDSR;
112500080207
112600100901       //--------------------------------------------------------------
112700100901       //?chiamata pgm ricerca matricola unità EDP
112800100901       //--------------------------------------------------------------
112900100901       BEGSR CallricMat  ;
113000100901
113100100901       clear kpjbu  ;
113200100901       clear dsunit  ;
113300100927       clear dsunana;
113400100930
113500100930       // Nella gestione RESI si può fare F7 solo sulla mmatricola di reso
113600100927 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
113700100930 3        If ModReso=*on  ;
113800100930          Errgenerico=*on  ;
113900100930          ErrMessage=*on  ;
114000100930          v1cmsg=msg(48)  ;
114100100930          exsr Aggiosfl  ;
114200100930          leavesr;
114300100930  3       endif  ;
114400100930
114500100927       uimmat=vscmat  ;
114600100927       else   ;
114700100927       uimmat=vscmatr ;
114800100927       endif  ;
114900100901       dsupro='A'  ;
115000100901       dsuop0=' 1'  ;
115100100901
115200100901       kpjbu=dsunit  ;
115300100924       callp TNSDU1R  (kpjba:dsunana)  ;
115400100901       dsunit=kpjbu    ;
115500100901
115600100927 1     if uimtip>0 and uimcod>0  ;
115700100930
115800100901       // In manutenzione se selezionato tipo unità diverso --> errore
115900100923 2     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
116000100923
116100100930 3     if vsctip<=*zeros  ;
116200100901       vsctip='00' ;
116300100930 x3    else   ;
116400100901
116500100930 4     if  uimtip<>%int(vsctip)  ;
116600100901        ErrGenerico=*on  ;
116700100901        ErrMessage=*on  ;
116800100930        if vshimm=' '  ;
116900100901        v1cmsg=msg(35)  ;
117000100930        else  ;
117100100930        v1cmsg=msg(49)  ;
117200100930        endif  ;
117300100901        ErrMat=*on  ;
117400100901           write  SDUOT01;
117500100901           write  SDUOZ02;
117600100901           exsr Aggiosfl  ;
117700100901           leavesr   ;
117800100930 4     endif ;
117900100930 3     endif ;
118000100930 x2    else  ;
118100100930 3     if vsctir<=*zeros  ;
118200100930       vsctir='00' ;
118300100930 x3    else   ;
118400100930
118500100930 4     if  uimtip<>%int(vsctir)  ;
118600100930        ErrGenerico=*on  ;
118700100930        ErrMessage=*on  ;
118800100930        v1cmsg=msg(49)  ;
118900100930        ErrMatr=*on  ;
119000100930           write  SDUOT01;
119100100930           write  SDUOZ02;
119200100930           exsr Aggiosfl  ;
119300100930           leavesr   ;
119400100930 4     endif ;
119500100930 3     endif ;
119600100923 2     endif ;
119700100923
119800100927         chain (uimtip:uimcod) unana01l  ;
119900100923 2       if %found(unana01l)  ;
120000100927           wtip=%editc(uimtip:'X') ;
120100100901           exsr chktip  ;
120200100923
120300100923       // se sono posizionato su MATR imposto matricola di reso
120400100923       //  in ogni altro caso quella normale
120500100923 3     if rnmfl<>'VSCMATR ' or v1ccuf='FOR' ;
120600100923           vsctip=%editc(unatip:'X')   ;
120700100901           vsdtip=wdtip  ;
120800100901           vscmat=unamat  ;
120900100923 x3    else  ;
121000100923           vsctir=%editc(unatip:'X')   ;
121100100923           vsdtir=wdtip  ;
121200100923           vscmatr=unamat  ;
121300100923 3     endif ;
121400100923 2     endif   ;
121500100901 1     endif   ;
121600100923
121700100901       exsr Aggiosfl  ;
121800100901
121900100901       ENDSR;
122000100901
122100100901       //--------------------------------------------------------------
122200100901       //?chiamata pgm gesitone tabella scheda clienti per scambio dati
122300100901       //--------------------------------------------------------------
122400100901       BEGSR Callpgm3R   ;
122500100901       clear kpjbu  ;
122600100901       callp TRTB47R  (kpjba)  ;
122700100901       ENDSR;
122800100901
122900100920       //--------------------------------------------------------------
123000100920       //?Stampa etichette
123100100920       //--------------------------------------------------------------
123200100920       BEGSR staEti      ;
123300100920
123400100920       // La prima volta devo proporre la richeista stampati
123500100920       if wrichtrul90=' '  ;
123600100920       exsr RichStampan  ;
123700100920       if d90f3<>'1' and not *in91   ;
123800100920       wrichtrul90='S'  ;
123900100920       endif  ;
124000100920       endif  ;
124100100920
124200100920
124300100920       ENDSR  ;
124400100824       //--------------------------------------------------------------
124500100824       //?Caricamento pagina vuota
124600100824       //--------------------------------------------------------------
124700100824       BEGSR RollUpS02;
124800100824         S02nrr   = totrig ;
124900100824
125000100824         // Scrittura righe vuote
125100100824         exsr Righevuote  ;
125200100824
125300100824       ENDSR;
125400080526       //--------------------------------------------------------------
125500100312       //?controlli SFL02
125600080409       //--------------------------------------------------------------
125700100312       BEGSR ContrS02;
125800100315
125900100920         // - F10=Stampa etichette chiedo la stampante
126000100920 1       if      dsp_aid = c_F10;
126100100920           exsr StaEti          ;
126200100920         endif  ;
126300100920
126400100830       clear  WsceltaOPZ   ;
126500081126       yy=1           ;
126600100315       s02nrr=1  ;
126700100824       readc       sduos02    ;
126800080703
126900100824 0     dow not %eof ;
127000100830
127100100831       // RECORD GIÀ INSERITO: CONTROLLO L'OPZIONE
127200100831 1     IF vshimm=' '  and vscopz<>' '
127300100831          and  vscopz<>'E'   ;
127400100830
127500100830       // controllo opzione
127600100830       Indx=%lookup(vscopz:wopz)  ;
127700100831 2     if Indx=0  ;
127800100830         v1cmsg=msg(26)  ;
127900100830         ErrGenerico=*on  ;
128000100830         ErrMessage =*on  ;
128100100830         EXSR AggioSFL   ;
128200100830         leavesr  ;
128300100831 2     endif   ;
128400100920
128500101006       select   ;
128600101006
128700101006       // Per F08 imposto la 'P' di materiale preparato e metto il R.I. in magazzio destinaz.
128800101006 2     when      dsp_aid = c_F08 ;
128900101012       // Se no INVIO tasto funzionale non ammesso
129000120525       //if vscinvr<>'I'    ;
129100120525       //  v1cmsg=msg(52)  ;
129200120525       //  ErrGenerico=*on  ;
129300120525       //  ErrMessage =*on  ;
129400120525       //  EXSR AggioSFL   ;
129500120525       //  leavesr  ;
129600120525       //endif   ;
129700101006
129800101006 3     if vscopz='P'  ;
129900101006           chain(e) vshnrr   ungio00f   ;
130000101006 4          if %error or not %found ;
130100101006             v1cmsg=msg(25)  ;
130200101006             ErrGenerico=*on  ;
130300101006             ErrMessage =*on  ;
130400101006             EXSR AggioSFL   ;
130500101006             leavesr  ;
130600101006 x4          else  ;
130700101006             vshfpr='P'  ;
130800101006             ungfpr='P'  ;
130900101006             update ungioFIS  ;
131000101006 4          endif  ;
131100120525          CLEAR VSCOPZ  ;
131200120525 3      endif  ;
131300120525
131400120525 3     if vscopz='B'  ;
131500120525           chain(e) vshnrr   ungio00f   ;
131600120525 4          if %error or not %found ;
131700120525             v1cmsg=msg(25)  ;
131800120525             ErrGenerico=*on  ;
131900120525             ErrMessage =*on  ;
132000120525             EXSR AggioSFL   ;
132100120525             leavesr  ;
132200120525 x4          else  ;
132300120525             if ungfst='S' ;
132400120525             v1cmsg=msg(62)  ;
132500120525             ErrGenerico=*on  ;
132600120525             ErrMessage =*on  ;
132700120525             EXSR AggioSFL   ;
132800120525             leavesr  ;
132900120525 x4          else  ;
133000120525             vshfst='N'  ;
133100120525             ungfst='N'  ;
133200120525             update ungioFIS  ;
133300120525 4          endif  ;
133400120525 4          endif  ;
133500120525          CLEAR VSCOPZ  ;
133600120525 3      endif  ;
133700101006
133800101006        exsr Aggiosfl    ;
133900121130
134000101006
134100100920       // Per F10 elaboro solo opzioni di stampa  le opzioni diverse da 9 le lascio
134200101006 2     when      dsp_aid = c_F10 and d90f3<>'1';
134300100920 3     if vscopz='9'  ;
134400100930         if not %open(fnlv22p)  ;
134500100930         open fnlv22p ;
134600100930         endif        ;
134700100930
134800100920         clear dsetiunit   ;
134900100920         §eunitmag=%editc(vscmag:'X') ;
135000100920         // Decodifico magazzino
135100100920          clear wazorg  ;
135200100920          wtmag ='D  '   ;
135300100920          wmag=%editc(vscmag:'X')  ;
135400100920          exsr  chkmag   ;
135500100920         §eunitdmag=wdmag              ;
135600100920         §eunitksc =%editc(vscksc:'X') ;
135700100920          chain   (1:dutkci:vscksc) cnaco00f   ;
135800100920         §eunitdksc=acorag             ;
135900101028
136000101028         select  ;
136100101028         // Imposto il campo NOTE
136200101028         when vscnot<>*blanks  ;
136300101028          §eunitnot =vscnot             ;
136400101028         //   se vuoto e tipo 99 -->quantità + descrizione
136500101028         when vsctip='99'  ;
136600101028          §eunitnot =%editc(vscqta:'X') + ' ' + vscdes  ;
136700101028         //   se vuoto e tipo <> 99 -->solo descrizione
136800101028         when vscdes<>*blanks  ;
136900101028          §eunitnot =vscdes             ;
137000101028         other   ;
137100101028         //   se vuota anche descrizione tipo e decodifica
137200101028          §eunitnot=vsctip + '-' + vsdtip   ;
137300101028         endsl   ;
137400100920         callp FNLVUEZS  (dsetiunit) ;
137500100920
137600100920         CLEAR VSCOPZ  ;
137700100920  3    endif  ;
137800100920       exsr Aggiosfl    ;
137900100920
138000121130
138100121130         // opz "D" --> visualizzo Window per richiedere la data
138200121130         //             solo la prima volta
138300121130
138400121130 2       when  vscopz='D'   ;
138500121203
138600121203  3      if vshfmv='S'   ;
138700121203         v1cmsg=msg(66)  ;
138800121203         ErrGenerico=*on  ;
138900121203         ErrMessage =*on  ;
139000121203         EXSR AggioSFL   ;
139100121203         leavesr  ;
139200121203         endif  ;
139300121130
139400121130 4       if w05dtm= 0 ;
139500121130
139600121130         I05sys=%dec(datasys)  ;
139700121130         kpjbu=tnsduods_1 ;
139800121130
139900121130         TNSDUOR1  (kpjba)  ;
140000121130
140100121130         tnsduods_1 = kpjbu   ;
140200121130
140300121203          if   O05F06='1' and o05gma_dtm>0  ;
140400121203           w05dtm=o05gma_dtm  ;
140500121203           w05amg_dtm=o05amg_dtm  ;
140600121130          endif  ;
140700121203
140800121203          if   w05dtm=0   ;
140900121203             ErrGenerico=*on  ;
141000121203             CLEAR VSCOPZ  ;
141100121203             EXSR AggioSFL   ;
141200121203             leavesr  ;
141300121203             endif  ;
141400121203
141500121130
141600121130 4       endif   ;
141700121130
141800121130 4       if w05dtm>0  ;
141900121130           chain(e) vshnrr   ungio00f   ;
142000121130 5          if %error or not %found ;
142100121130             v1cmsg=msg(25)  ;
142200121130             ErrGenerico=*on  ;
142300121130             ErrMessage =*on  ;
142400121130             EXSR AggioSFL   ;
142500121130             leavesr  ;
142600121130 x5          else  ;
142700121130             vscdtm=w05dtm ;
142800121203             ungdtm=w05amg_dtm ;
142900121130             update ungioFIS  ;
143000121130
143100121130 5          endif  ;
143200121130 4          endif  ;
143300121130
143400121130          CLEAR VSCOPZ  ;
143500121130         EXSR AggioSFL   ;
143600121130       dsp_aid= savinkf  ;
143700121130  x2   other    ;
143800100831
143900100831       // non posso annullare record confermato
144000100920  3    if vshimm=' '  and vshfmv='S' and vscopz='4'  ;
144100100831         v1cmsg=msg(29)  ;
144200100831         ErrGenerico=*on  ;
144300100831         ErrMessage =*on  ;
144400100831         EXSR AggioSFL   ;
144500100831         leavesr  ;
144600100920  3    endif  ;
144700101006       // Per opzione '9' premere tasto funzionale F9
144800100920  3    if vshimm=' '  and   vscopz='9'  ;
144900100920         v1cmsg=msg(43)  ;
145000100920         ErrGenerico=*on  ;
145100100920         ErrMessage =*on  ;
145200100920         EXSR AggioSFL   ;
145300100920         leavesr  ;
145400121130        endif  ;
145500121130
145600101006       // Per opzione 'P' premere tasto funzionale F8
145700101006  3    if vshimm=' '  and   vscopz='P'  ;
145800101012       if vscinvr='I'   ;
145900101006         v1cmsg=msg(53)  ;
146000101006         else  ;
146100101006         v1cmsg=msg(52)  ;
146200101006         endif  ;
146300101006         ErrGenerico=*on  ;
146400101006         ErrMessage =*on  ;
146500101006         EXSR AggioSFL   ;
146600101006         leavesr  ;
146700101006  3    endif  ;
146800120525       // Per opzione 'B' premere tasto funzionale F8
146900120525  3    if vshimm=' '  and   vscopz='B'  ;
147000120525         v1cmsg=msg(53)  ;
147100120525         ErrGenerico=*on  ;
147200120525         ErrMessage =*on  ;
147300120525         EXSR AggioSFL   ;
147400120525         leavesr  ;
147500120525  3    endif  ;
147600100903
147700100921       // non posso Manutenzionare  o annullare se blocco data area
147800100921  3    if vshimm=' '  and  (vscopz='2' or vscopz='4') ;
147900100903        in tnsduo   ;
148000100909        if §sduodat>0 and (§sdv0resi='S' or §sdv0imm='S' )  ;
148100100907        exsr Msg37    ;
148200100909        clear vscopz  ;
148300100907
148400100903         EXSR AggioSFL   ;
148500100903         leavesr  ;
148600100920       endif   ;
148700100920 3     endif  ;
148800100830
148900100909       //  Alloco il record non per opz "5"  e "8"
149000100920 3       if vscopz='5' or vscopz='8'  ;
149100100830         chain(n) vshnrr   ungio00f   ;
149200100830         else  ;
149300100830         chain(e) vshnrr   ungio00f   ;
149400100920 3       endif  ;
149500100920 3       if %error or not %found ;
149600100830         v1cmsg=msg(25)  ;
149700100830         ErrGenerico=*on  ;
149800100830         ErrMessage =*on  ;
149900100830         EXSR AggioSFL   ;
150000100830         leavesr  ;
150100100920 3       endif  ;
150200100909
150300100909         // opz "8" --> interroga movimenti
150400100920 3       if vscopz='8'   ;
150500100909         exsr IntMovim  ;
150600100909         clear vscopz   ;
150700100909         EXSR AggioSFL   ;
150800100909         leavesr  ;
150900100920 3       endif  ;
151000121130
151100100830
151200100830        $VIDEO='D3'   ;
151300100830        exsr Gesd03  ;
151400100830
151500100830        wSceltaOPZ='S'  ;
151600100830        clear vscopz ;
151700100830         EXSR AggioSFL   ;
151800100830
151900100920 3     If ErrGenerico=*on  ;
152000100830         leavesr  ;
152100100920 3     endif  ;
152200101006 2     endsl ;
152300100830
152400100830 x1    else ;
152500100831
152600100924       // I record esclusi dalla conferma e annuillati non vengono controllati
152700100924  2    if vscopz<>'E'  and vshann<>'S' ;
152800100831
152900100830       // RECORD DI NUOVA IMMISSIONE: CONTROLLO
153000100830       exsr ContrRIGA  ;
153100100907       if  ErrGenerico=*on  ;
153200100907       leavesr ;
153300100907       endif  ;
153400100830
153500100830         // Per record inserito imposto utente e data ins
153600100831  3      if ErrGenerico=*off and vshimm='S' and vsctip<>*blanks ;
153700100830         vscute=KNMUS  ;
153800100830         datadmy=datasys ;
153900100830         dataeur=datasys ;
154000100830          vscdim=%dec(dataeur) ;
154100100830          w0060 =%dec(datadmy) ;
154200100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
154300100830                  %subst(%editc(w0060:'X'):3:2)    ;
154400100830
154500100831  3      endif    ;
154600100831  2      endif    ;
154700100831
154800100831   2    if ErrGenerico=*off ;
154900100831         EXSR AggioSFL   ;
155000100831   2    endif   ;
155100100831  1    endif   ;
155200100830
155300100830       readc    sduos02    ;
155400100830 0     enddo         ;
155500100830
155600100830       ENDSR  ;
155700081126
155800100907       //--------------------------------------------------------------
155900100907       //?scrivo msg 37 di blocco manutenzione
156000100907       //--------------------------------------------------------------
156100100907        BEGSR Msg37  ;
156200100907         v1cmsg=msg(37)  ;
156300100907         dataiso=%date(§sduodat) ;
156400100907         datadmy=dataiso      ;
156500100907         W006a=%editc(%dec(datadmy):'X') ;
156600100907         %subst(v1cmsg:74:2)=%subst(w006a:1:2) ;
156700100907         %subst(v1cmsg:77:2)=%subst(w006a:3:2) ;
156800100907         %subst(v1cmsg:60:10)=§sduoute          ;
156900100907         %subst(v1cmsg:27:20)=§sduopgm          ;
157000100907         ErrGenerico=*on  ;
157100100907         ErrMessage =*on  ;
157200100907        ENDSR  ;
157300100909       //--------------------------------------------------------------
157400100909       //?Interroga movimenti della unit/matricola
157500100909       //--------------------------------------------------------------
157600100909        BEGSR IntMovim;
157700100909
157800100924        // Se manca la matricola non sono interrogabili
157900100909        if vscmat=*blanks  ;
158000100909         v1cmsg=msg(34)  ;
158100100909         ErrGenerico=*on  ;
158200100909         ErrMessage =*on  ;
158300100909        else  ;
158400100909
158500100909        clear dsunit ;
158600100909        dsuop0=' 5'  ;
158700100909        dsutip=%int(vsctip)  ;
158800100930        chain (vscmat:dsutip)  unana07l    ;
158900100930        if %found(unana07l)  ;
159000110412         dsucod=unacod   ;
159100110412        else  ;
159200110412        // se non trovato provo con nuova matricola per FOR "R"
159300110412           if ProtperFOR=*on and vscnot<>*blanks   ;
159400110412           kmat=vscnot  ;
159500110412           chain (kmat:dsutip)  unana07l    ;
159600110412           if %found(unana07l)  ;
159700110412            dsucod=unacod   ;
159800110412           endif  ;
159900110412           endif  ;
160000110412
160100100909        endif  ;
160200100910        // solo se confermato, passo anche la data movimento
160300100910        if vshfmv='S'   ;
160400100910         datadmy=%date(vscdtm:*dmy)   ;
160500100910         dataiso=datadmy  ;
160600100910         dsudtm=%dec(dataiso) ;
160700100910        endif  ;
160800100909        kpjbu=dsunit   ;
160900100909        callp tnsdu4r   (kpjba)   ;
161000100909        ErrGenerico=*on  ;
161100100909        endif   ;
161200100909        ENDSR  ;
161300100830       //--------------------------------------------------------------
161400100830       //?controllo campi della riga di UNGIO00F
161500100830       //--------------------------------------------------------------
161600100830       BEGSR contrRIGA  ;
161700100830
161800100921       // controllo se inserito almeno un dato
161900101001 0     if vscmag<>0  or vscksc>0  or vscdes<>*blanks or vscnot<>*blanks
162000101001       or (vsctip<>*blanks  and vsctip<>*zeros )  or vsdksc<>*blanks or
162100101001       (vscmag3<>*blanks  and vscmag3<>*zeros and $Video<>'S2') or
162200101001       vscmat<>*blanks  ;
162300100921
162400100903       // Invii/Rientri: campo obbligatorio
162500100923 1     if vscinvr=' ' ;
162600100903           errMessage  = *on;
162700100903           errGenerico = *on;
162800100903           errINVR=*on ;
162900100903           V1cmsg = Msg(39);
163000100903           EXSR AggioSFL   ;
163100100903           leavesr  ;
163200100923 1     endif  ;
163300101001
163400101001       // Per inserimeno multiplo, inserire il numero delle ricorrenza
163500101001       if   $video='D4' and v4cinse<=0  ;
163600101001           errMessage  = *on;
163700101001           errGenerico = *on;
163800101001           errINSE=*on ;
163900101001           V1cmsg = Msg(50);
164000101001           EXSR AggioSFL   ;
164100101001           leavesr  ;
164200101001       endif        ;
164300100826
164400100824       // tipo unita
164500100927       // controllo tipo unità e matricola
164600100927       exsr CtrTipMat    ;
164700100927       if Errgenerico =*on  ;
164800100927       leavesr  ;
164900100927       endif    ;
165000120209
165100120209       // Magazzino di destinazione obbligatorio
165200120209       //  non obbligatorio per inserimento multiplo
165300120209 0     if $video<>'D4'  or vscmag3<>*blanks  ;
165400120209
165500120209       clear vsdmag  ;
165600120209       clear wazorg  ;
165700120209       wtmag ='D  '   ;
165800120209 1     if $Video='S2'   ;
165900120209       wmag=%editc(vscmag:'X')  ;
166000120209       else  ;
166100120209       wmag=vscmag3 ;
166200120209 1     endif ;
166300120209
166400120209       exsr  chkmag   ;
166500130118
166600120209 1     if $Video='S2'   ;
166700120209       vscmag=%int(wmag)    ;
166800130118
166900130118       if Errgenerico=*off ;
167000130118       if §omagfag='Y'  and v1ccuf='FOR'  and vscinvr='F'
167100130118         and vscmag<>unadms  and vscmag>*zeros  ;
167200130118           errMessage  = *on;
167300130118           errGenerico = *on;
167400130118           V1cmsg = Msg(67);
167500130118       endif  ;
167600130118       endif  ;
167700130118
167800120209       else   ;
167900120209       vscmag3=wmag  ;
168000120209 1     endif  ;
168100120209       vsdmag=wdmag  ;
168200120209 1     if Errgenerico=*on  ;
168300120209         errMAG=*on ;
168400120209         EXSR AggioSFL   ;
168500120209         leavesr  ;
168600120209 1     endif    ;
168700120209 0     endif    ;
168800100927
168900100927
169000100927         // Quantità: solo per 99 <> da 0 o 1
169100100927 1       if vscqta>1 and vsctip<> '99'  ;
169200100927           errMessage  = *on;
169300100927           errGenerico = *on;
169400100927           V1cmsg = Msg(15);
169500100927           errQTA=*on ;
169600100927           EXSR AggioSFL   ;
169700100927           leavesr  ;
169800100927 1       endif  ;
169900100831
170000100907       // Controlli sul Magazzino Correlato
170100100831 1     if  vscmgc>0   ;
170200100903       // Non può essere uguale al magazzino di destinazione
170300100923 2     if vscmgc=vscmag  ;
170400100924           errMessage  = *on;
170500100924           errGenerico = *on;
170600100903           errMGC=*on ;
170700100903           V1cmsg = Msg(38);
170800100903           EXSR AggioSFL   ;
170900100903           leavesr  ;
171000100923 2     endif  ;
171100100908
171200100908       clear wazorg ;
171300100908       wtmag='C' ;
171400100909       wmag=%editc(vscmgc:'X')  ;
171500100831       exsr  chkmag   ;
171600100909       vsdmgc=wdmag  ;
171700100831 2     if Errgenerico=*on  ;
171800100831         errMGC=*on ;
171900100831         EXSR AggioSFL   ;
172000100831         leavesr  ;
172100100831 2     endif    ;
172200100831
172300100831 1     endif    ;
172400100923
172500100923
172600100923       // Tipo unità di RESO solo se c'e' la sostituzione
172700110401       // ignoro per FOR F o R
172800110401         if protperRIE=*off  ;
172900110610
173000110610         // Aumento/Sostituz: non per 99
173100110610 1       if vsctru<>' ' and vsctip=  '99'  ;
173200110610           errMessage  = *on;
173300110610           errGenerico = *on;
173400110610           V1cmsg = Msg(16);
173500110610           errTRU=*on ;
173600110610           EXSR AggioSFL   ;
173700110610           leavesr  ;
173800110610 1       endif  ;
173900110610
174000100923 1       if (vsctru<>'S' and vsctir<>*blanks and vsctir<>'00') or
174100100923            (vsctru= 'S' and (vsctir= *blanks or vsctir=*zeros))  ;
174200100923           errMessage  = *on;
174300100923           errGenerico = *on;
174400100923           V1cmsg = Msg(17);
174500100923           errTir=*on ;
174600100923           EXSR AggioSFL   ;
174700100923           leavesr  ;
174800100923 1       endif   ;
174900110404         else  ;
175000110404         // Per FOR F o FOR  R contiene  guasto fisico. Accetto solo la "S"
175100110404         if vsctru<>'S' and vsctru<>' '   ;
175200110404           errMessage  = *on;
175300110404           errGenerico = *on;
175400110404           V1cmsg = Msg(56);
175500110404           errTru=*on ;
175600110404           EXSR AggioSFL   ;
175700110404           leavesr  ;
175800110404         endif  ;
175900110610         // Note obbligatorie
176000110610         if vscnot=*blanks and vsctru='S'   ;
176100110610           errMessage  = *on;
176200110610           errGenerico = *on;
176300110610           V1cmsg = Msg(59);
176400110610           errNot=*on ;
176500110610           EXSR AggioSFL   ;
176600110610           leavesr  ;
176700110610         endif ;
176800110610
176900110401 1       endif   ;
177000100923
177100100923         clear vsdtir  ;
177200101001 1       if vsctir<>*blanks  and vsctir<>*zeros  ;
177300100923         wtip=vsctir   ;
177400100923         exsr chktip   ;
177500100923
177600100923         vsctir=wtip  ;
177700100923         vsdtir=wdtip  ;
177800100923 2       if Errgenerico=*on  ;
177900100923           errTIR=*on ;
178000100923 3       if Errmessage =*on  ;
178100100923           V1cmsg = Msg(18);
178200100923 3       endif  ;
178300100923           EXSR AggioSFL   ;
178400100923           leavesr  ;
178500100923 2       endif    ;
178600100923 1       endif    ;
178700100923
178800100923         // Matricola unità di reso
178900100923 1       if vscmatr<>*blanks and vsctir=*blanks  ;
179000100923           errMessage  = *on;
179100100923           errGenerico = *on;
179200100923           V1cmsg = Msg(44);
179300100923           errMATr=*on ;
179400100930           *inki=*on ;
179500100923           EXSR AggioSFL   ;
179600100923           leavesr  ;
179700100923 1       endif  ;
179800100923
179900100923          clear vsdmodr  ;
180000101005          clear vscmgcr  ;
180100100923 1        if vscmatr<>*blanks ;
180200100923          ktip=%int(vsctir)  ;
180300100923          chain (vscmatr:ktip)  unana07l    ;
180400100923 2        if not %found(unana07l)  ;
180500100923           Errgenerico=*on  ;
180600100923           Errmessage =*on  ;
180700100923           ErrMATR=*on  ;
180800100923           v1cmsg=msg(06)  ;
180900100923           EXSR AggioSFL   ;
181000100923           leavesr  ;
181100100923 2        endif  ;
181200100923          vsdmodr =unamod;
181300101005          vscmgcr =unamag;
181400100923
181500100923          // Il magazzino correlato deve essere uguale a quello di destinazione
181600101005 2        if unamag<>vscmag  and unaksc=0 and vshfmat<>vscmatr  ;
181700100923           errMessage  = *on;
181800100923           errGenerico = *on;
181900100923           V1cmsg = Msg(45);
182000100923           errMATr=*on ;
182100101005           vshfmat=vscmatr ;
182200100923           EXSR AggioSFL   ;
182300100923           leavesr  ;
182400100923 2        endif  ;
182500100923 1        endif  ;
182600101005
182700101005          vshfmat=vscmatr ;
182800100826
182900100827         // Descrizione obbligatoria per Invii per d§UNTfl1='S'
183000101001 1       if $Video<>'D4'   ;
183100101001 2       if vscinvr='I' and d§untfl1='S' and vscdes=*blanks  ;
183200101112           errMessage  = *on;
183300101112           errGenerico = *on;
183400100826           errDES=*on ;
183500100826           V1cmsg = Msg(19);
183600100826           EXSR AggioSFL   ;
183700100826           leavesr  ;
183800101001 2       endif ;
183900101001 1       endif ;
184000110412
184100110412         // per FOR "F" note diventa --> nuova matricola inviata in sostiituizione
184200110412 1       if $Video<>'D4'  and vscnot<>*blanks and ProtperFOR=*on ;
184300110412         // non deve essere più lunga di 15 caratteri
184400110412
184500110412         if %subst(vscnot:16:5)<>*blanks  ;
184600110412           Errgenerico=*on  ;
184700110412           Errmessage =*on  ;
184800110412           Errnot=*on  ;
184900110412           v1cmsg=msg(58)  ;
185000110412           EXSR AggioSFL   ;
185100110412           leavesr  ;
185200110412         endif  ;
185300110412
185400110412         // non deve già esistere
185500110412         ktip=%int(vsctip)  ;
185600110412         eval kmat=vscnot    ;
185700110412          setll (kmat:ktip)  unana07l    ;
185800110412 3        if %equal(unana07l)  ;
185900110412           Errgenerico=*on  ;
186000110412           Errmessage =*on  ;
186100110412           Errnot=*on  ;
186200110412           v1cmsg=msg(57)  ;
186300110412           EXSR AggioSFL   ;
186400110412           leavesr  ;
186500110412 3        endif    ;
186600110412         endif  ;
186700100826
186800100826         // codice cliente
186900100830 1       if ($video='S2' and vsdksc <>*blanks and vscksc=0) or
187000101001            ($video<>'S2' and vsdksc3<>*blanks and vscksc=0)  ;
187100100826           clear xparsta  ;
187200100826           xparkcc=dutkci ;
187300100826           xparkut= 3     ;
187400100826           xpardut=rsut   ;
187500100830           if $video='S2' ;
187600100826           xparrag=vsdksc ;
187700100830           else  ;
187800100830           xparrag=vsdksc3;
187900100830           endif  ;
188000100826           clear xparflr ;
188100100826           clear xparkcm  ;
188200100826           clear xparksm  ;
188300100826           clear xparkdm  ;
188400100826           xparnum=1      ;
188500100826           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
188600100826                           :xparflr:xpardit:xparnum:xparkcm:xparksm
188700100826                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
188800100827 2          if xparsta<>-1  ;
188900100826            vscksc=%int(%subst(xparksm:1:7))   ;
189000100826            vsdksc=xparrag   ;
189100100830            vsdksc3=xparrag   ;
189200100827 2          endif            ;
189300100826             errGenerico = *on;
189400100826             errksc      = *on;
189500100826           EXSR AggioSFL   ;
189600100826           leavesr  ;
189700100827  1      endif ;
189800100826
189900100826         // codice cliente : obbligatorio per tipo 48/49 in invio
190000100920         //  tranne se non c'e' la parola "SCORTA" nelle note
190100100920         Indx=%scan('SCORTA':vscnot)  ;
190200100920         if Indx=0   ;
190300100920         Indx=%scan('scorta':vscnot)  ;
190400100920         endif  ;
190500100920         if Indx=0   ;
190600100920         Indx=%scan('Scorta':vscnot)  ;
190700100920         endif  ;
190800100920
190900100924  2      If Indx=0 ;
191000100924  3      if vscinvr='I' and vscksc=0 and (vsctip='48' or vsctip='49');
191100100924           errMessage  = *on;
191200100924           errGenerico = *on;
191300100826           errKSC=*on ;
191400100826           V1cmsg = Msg(20);
191500100826           EXSR AggioSFL   ;
191600100826           leavesr  ;
191700100924  3      endif ;
191800100920  1      endif ;
191900100826
192000100827  1      if vscksc>0  ;
192100100826           chain   (1:dutkci:vscksc) cnaco00f   ;
192200100827  2        if not %found(cnaco00f)   ;
192300100826             errMessage  = *on;
192400100826             errGenerico = *on;
192500100826             ErrKSC   = *on;
192600100826             V1cmsg = Msg(21);
192700100826            EXSR AggioSFL   ;
192800100826             leavesr;
192900100826             else  ;
193000100826             vsdksc=acorag  ;
193100100830             vsdksc3=acorag  ;
193200100827 2       endif  ;
193300100827 1       endif  ;
193400100825
193500100826         // Data conferma movimento: se > udate non troppo...  ;
193600100826 1       If vscdtm>0  ;
193700100826             clear wlbdat;
193800100826             g02dat = vscdtm ;
193900100826             xsrda8(wlbdat);
194000100826 2           if g02err = '1';
194100100826               errMessage  = *on;
194200100826               errGenerico = *on;
194300100826               ErrDTM    = *on;
194400100826               V1cmsg = Msg(02);
194500100826              EXSR AggioSFL   ;
194600100826               leavesr;
194700100826 2           endif;
194800100826
194900100924 2           if g02inv>datapiu200;
195000100826               errMessage  = *on;
195100100826               errGenerico = *on;
195200100826               ErrDTM    = *on;
195300100826               V1cmsg = Msg(22);
195400100826              EXSR AggioSFL   ;
195500100826               leavesr;
195600100826 2           endif   ;
195700100909 2           if vshfmv=' ' and g02inv<%dec(datasys)  ;
195800100831               errMessage  = *on;
195900100831               errGenerico = *on;
196000100831               ErrDTM    = *on;
196100100831               V1cmsg = Msg(31);
196200100831              EXSR AggioSFL   ;
196300100831               leavesr;
196400100831 2           endif   ;
196500100826 1       endif   ;
196600101012
196700101012       // Se non c'e' un altra riga per lo stesso magazzino, chiedo obbligatori peso e vol
196800101012 1     if vscinvr='I'  and $Video<>'D4'  and vscmag>0  and vshfmv=' '  ;
196900101012        exsr selectSQL   ;
197000101012
197100101012 2        if   vscncl=0 or vscpkg=0   ;
197200101012 3      if SiColliPeso=' ' ;
197300101012               errMessage  = *on;
197400101012               errGenerico = *on;
197500101012               ErrNCL    = *on;
197600101012               V1cmsg = Msg(54);
197700101012              EXSR AggioSFL   ;
197800101012               leavesr;
197900101012 3      endif  ;
198000101012
198100101012 2      endif  ;
198200101012        endif  ;
198300080703
198400101001         // Dcumento obbligatorio  se rientro da fornitore
198500101001 1       if $Video<>'D4'  ;
198600120525 2       if (vscdoc=0 and vscddo=0) and vscinvr='R' ;
198700100909               errMessage  = *on;
198800100909               errGenerico = *on;
198900100909               ErrDDO    = *on;
199000100909               V1cmsg = Msg(33);
199100100909              EXSR AggioSFL   ;
199200100909               leavesr;
199300101001 2        endif   ;
199400101001 1        endif   ;
199500100909
199600100909       // controllo la data
199700100909       if vscddo>0   ;
199800100909             clear wlbdat;
199900100909             g02dat = vscddo ;
200000100909             xsrda8(wlbdat);
200100100909 2           if g02err = '1';
200200100909               errMessage  = *on;
200300100909               errGenerico = *on;
200400100909               ErrDDO    = *on;
200500100909               V1cmsg = Msg(02);
200600100909               leavesr;
200700100909              endif  ;
200800100909       endif  ;
200900100921 x0    else  ;
201000101001       // Per inserimento multipli immettere almeno un dato da duplicare
201100101001       if $Video='D4'  ;
201200101001               errMessage  = *on;
201300101001               errGenerico = *on;
201400101001               ErrMAG    = *on;
201500101001               V1cmsg = Msg(51);
201600101001               leavesr;
201700101001       endif  ;
201800100921       clear vsdmag   ;
201900100921       clear vsdtip  ;
202000100921       clear vsdmod1 ;
202100100921       clear vsdmod2 ;
202200100921       clear vscmgc  ;
202300100921       clear vsdmgc  ;
202400100921       Recvuoto='S'   ;
202500100930       // pulisco eventuale posizione della matricola
202600100930           if s02nrr<2000 ;
202700100930            clear tipmat(s02nrr)          ;
202800101012            clear SiCoPe(s02nrr)          ;
202900100930           endif  ;
2030001009210      endif  ;
203100100909
203200080409       ENDSR;
203300101012
203400101012       //--------------------------------------------------------------
203500101012       //?Select con SQL su ungio00F
203600101012       //--------------------------------------------------------------
203700101012       BEGSR  SelectSQL ;
203800101012
203900101012       clear SiColliPeso  ;
204000101012 1     if vscdtm=0  ;
204100101012         invdtm=%dec(datasys)  ;
204200101012       else  ;
204300101012         dataeur=%date(vscdtm:*dmy)    ;
204400101012         dataiso=dataeur  ;
204500101012         invdtm=%dec(dataiso) ;
204600101012 1     endif  ;
204700101012
204800101012       // Prima verifico se già inserito in questa pagina di lavoro
204900101012
205000101012       // Cerco con: MAGAZZINO  e data conferma
205100101012          // se la data <= oggi cerco con data =0
205200101012
205300101012 1     if invdtm<=%dec(datasys)  ;
205400101012       wSiCoPe=(vscmag+100000000)            ;
205500101012       else  ;
205600101012       wSiCoPe=(vscmag+100000000) +  invdtm  ;
205700101012 1     endif  ;
205800101012
205900101012 1     if   vscncl=0 or vscpkg=0   ;
206000101012
206100101012       clear sicope(s02nrr)   ;
206200101012       Indx=%lookup(wsicope:Sicope)  ;
206300101012 2      if Indx>0  ;
206400101012         SiColliPeso='S'  ;
206500101012 2      endif  ;
206600101012
206700101012 x1     else  ;
206800101012
206900101012         if s02nrr<2000 ;
207000101012           sicope(s02nrr)=wsicope    ;
207100101012         endif  ;
207200101012         SiColliPeso='S'  ;
207300101012 1      endif  ;
207400101012
207500101012
207600101012       // Poi se non trovato controllo se già inserito in UNGIO
207700101012         if SiColliPeso =' '  ;
207800101012
207900101012         wrkgetlista='SELECT ungdtm, ungncl, ungpkg, rrn(UNGIO00f) +
208000101012                               from UNGIO00F WHERE ungcuf=''' +v1ccuf  + ''' +
208100101012         and unginvr=''I'' and ungfmv='' '' and ungmag= ' + %editc(vscmag:'X') ;
208200101012
208300101012         exec sql   PREPARE s1 FROM :wrkgetlista   ;
208400101012
208500101012         exec sql   DECLARE a1 cursor FOR  s1   ;
208600101012
208700101012         exec sql OPEN a1   ;
208800101012
208900101012 0       dow sqlcod=0   ;
209000101012
209100101012         exec sql FETCH NEXT  FROM a1 INTO: sqdtm, :sqncl, :sqpkg, :sqnrr ;
209200101012
209300101012 1       select ;
209400101012 1       when sqlcod=100 ;
209500101012
209600101012 1       when sqlcod<0   ;
209700101012         *inh1=*on   ;
209800101012
209900101012 x1      other   ;
210000101012         // escludo il record se la data conferma è <> da quello del record ins/variato
210100101012 2a       if  vshnrr<>sqnrr ;
210200101012 2        if  vscdtm=0 or invdtm<=%dec(datasys)  ;
210300101012 3         if sqdtm<=invdtm  ;
210400101012 4           if sqncl>0 and sqpkg>0  ;
210500101012               SiColliPeso='S'  ;
210600101012 4           endif  ;
210700101012 3         endif  ;
210800101012
210900101012 x2       else  ;
211000101012 3         if  sqdtm=invdtm  ;
211100101012 4           if sqncl>0 and sqpkg>0  ;
211200101012               SiColliPeso='S'  ;
211300101012 4           endif  ;
211400101012 3         endif  ;
211500101012 2a       endif  ;
211600101012 2        endif  ;
211700101012
211800101012 1       endsl  ;
211900101012
212000101012 0       enddo   ;
212100101012
212200101012         exec sql CLOSE a1   ;
212300101012         endif  ;
212400101012
212500101012       ENDSR;
212600080409
212700100927       //--------------------------------------------------------------
212800100927       //?Controllo solo tipo unità e matricola
212900100927       //--------------------------------------------------------------
213000100927       BEGSR  CtrTipMat  ;
213100100927
213200100927       // Obbligatorio se non immessa la matricola
213300101001       //  non obbligatorio per inser. multiplo
213400101001 1     if (vsctip=*blanks or vsctip=*zeros) and vscmat=*blanks and $Video<>'D4';
213500100927           errMessage  = *on;
213600100927           errGenerico = *on;
213700100927           errTIP=*on ;
213800100927           v1cmsg=msg(05)  ;
213900100927           EXSR AggioSFL   ;
214000100927           leavesr  ;
214100100927  1      endif   ;
214200100927
214300100927  1     if vsctip<>*blanks and vsctip<>*zeros  ;
214400100927         clear vsdtip  ;
214500100927         clear vsdtip3 ;
214600100927         wtip=vsctip   ;
214700100927         exsr chktip   ;
214800100927
214900100927         vsctip=wtip  ;
215000100927         vsdtip =wdtip  ;
215100100927         vsdtip3=wdtip  ;
215200100927 2       if Errgenerico=*on  ;
215300100927           errTIP=*on ;
215400100927 3       if ErrMessage =*on  ;
215500100927          v1cmsg=msg(05)  ;
215600100927 3        endif  ;
215700100927           EXSR AggioSFL   ;
215800100927           leavesr  ;
215900100927 x2      else  ;
216000100927         dunt=t02uni ;
216100100927         vsdtip =d§untdes ;
216200100927         vsdtip3=d§untdes ;
216300100927  2      endif    ;
216400100927
216500100927         // "99" accettato solo per MAG
216600100927  2      if vsctip='99' and v1ccuf='FOR'  ;
216700100927           errMessage  = *on;
216800100927           errGenerico = *on;
216900100927           errTIP=*on ;
217000100927           V1cmsg = Msg(14);
217100100927           EXSR AggioSFL   ;
217200100927           leavesr  ;
217300100927  2      endif   ;
217400100927  1      endif   ;
217500100927
217600100927         // Matricola non ci vuole per 99
217700100927 1       if v1ctip='99' and v1cmat<>*blanks  ;
217800100927           errMessage  = *on;
217900100927           errGenerico = *on;
218000100927           V1cmsg = Msg(13);
218100100927           errMAT=*on ;
218200100927           EXSR AggioSFL   ;
218300100927           leavesr  ;
218400100927 1       endif    ;
218500100927
218600100927
218700100927         // Matricola: obbligatoria in conferma
218800100927         if §sdv0conf='S' and vsctip<>'99' and vscmat=*blanks  ;
218900100927           errMessage  = *on;
219000100927           errGenerico = *on;
219100100927           V1cmsg = Msg(32);
219200100927           errMAT=*on ;
219300100927           EXSR AggioSFL   ;
219400100927           leavesr  ;
219500100927         endif   ;
219600100927
219700100927         // Matricola: se immessa controllo
219800100927         clear vsdmod1 ;
219900100927         clear vsdmod2 ;
220000100927         clear vsdmod  ;
220100100927          if vshfmv=' ' ;
220200100927          clear vscmgc  ;
220300100927          clear vsdmgc  ;
220400100927         endif  ;
220500100927 1       if vscmat<>*blanks   ;
220600100927 2       if vsctip>*zeros   ;
220700100927          ktip=%int(vsctip)  ;
220800100927          chain (vscmat:ktip)  unana07l    ;
220900100927 x2       else  ;
221000100927          chain (vscmat)       unana07l    ;
221100100927 2        endif  ;
221200100927
221300100927 2        if not %found(unana07l)  ;
221400100927          Errgenerico=*on  ;
221500100927          Errmessage =*on  ;
221600100927          ErrMAT=*on  ;
221700100927          if vsctip>*zeros  ;
221800100927          v1cmsg=msg(06)  ;
221900100927          else  ;
222000100927          v1cmsg=msg(07)  ;
222100100927          endif  ;
222200100927
222300100927           EXSR AggioSFL   ;
222400100927           leavesr  ;
222500100927 2        endif  ;
222600100927
222700100927          // se trovata verifico se univoca altrimenti ci vuole il tipo
222800100927 2        if     vsctip<=*zeros and %found(unana07l)  ;
222900100927           clear  qtatem   ;
223000100927           setll (vscmat)  unana07l    ;
223100100927           reade (vscmat)  unana07l    ;
223200100927 3         dow  not %eof(unana07l) ;
223300100927 4          if qtatem=0   ;
223400100927             qtatem=qtatem+1  ;
223500100927 x4         else   ;
223600100927             Errgenerico=*on  ;
223700100927             Errmessage =*on  ;
223800100927             Errmat=*on  ;
223900100927             v1cmsg=msg(08)  ;
224000100927             EXSR AggioSFL   ;
224100100927             leavesr;
224200100927 4          endif   ;
224300100927           reade (vscmat)  unana07l    ;
224400100927 3         enddo   ;
224500100927 2        endif   ;
224600100927
224700100927 2        if vsctip<=*zeros   ;
224800100927             vsctip=%editc(unatip:'X') ;
224900100928            ktip=unatip   ;
225000100928            wtip=vsctip   ;
225100100927            exsr chktip   ;
225200100927            dunt=t02uni ;
225300100927            vsctip=wtip  ;
225400100927            vsdtip =wdtip  ;
225500100927            vsdtip3=wdtip  ;
225600100927 2        endif ;
225700100927          vsdmod =unamod;
225800100927          vsdmod1=unamod;
225900100927          vsdmod2=%subst(unamod:7:9) ;
226000110404          // Se unità in proprietà alla 923 --> non posso accettare MAG 990
226100110404          if unasis='923' and ((vscmag = 990  and $Video='S2') or
226200111024                               (vscmag3='990' and $Video<>'S2')  or
226300111024                               (vscmag3='980' and $Video<>'S2')  or
226400111024                               (vscmag = 980  and $Video='S2')) ;
226500110401             Errgenerico=*on  ;
226600110401             Errmessage =*on  ;
226700110401             Errmag=*on  ;
226800110401             v1cmsg=msg(55)  ;
226900110401             EXSR AggioSFL   ;
227000110401             leavesr;
227100110401          endif  ;
227200120209
227300120209          // Per FOR F propongo magazzino destinazione se si tratta di un fornitore
227400130116          if v1ccuf='FOR'  and vscinvr='F' ;
227500130116          chain unadms azorg21l ;
227600130116          if  vscmag<=*zeros  and
227700130116                 %found(azorg21l) and  orgfag = 'Y' ;
227800120209              vscmag=unadms  ;
227900120209              Errgenerico=*on  ;
228000120209              EXSR AggioSFL   ;
228100120209              leavesr  ;
228200120209          endif  ;
228300130116
228400130116             endif  ;
228500120209
228600100927          // Solo se record non confermato
228700100927          if vscmgc=0 or vshfmv=' ' ;
228800100927           vscmgc =unamag;
228900100927          endif  ;
229000100927
229100100928          //  controllo l'univocità tra i record non  confermati
229200130321            setll (Ktip:vscmat)  ungio01l    ;
229300130321            reade (Ktip:vscmat)  ungio01l    ;
229400130321            dow not %eof(ungio01l) ;
229500130321             if ungfmv=' '  ;
229600130321              if vshimm='S' or (vshnrr<>gio1nrr)   ;
229700130321               Errgenerico=*on  ;
229800130321               Errmessage =*on  ;
229900130321               ErrMAT=*on  ;
230000130321               v1cmsg=msg(23)  ;
230100130321               EXSR AggioSFL   ;
230200130321               leavesr  ;
230300130321              endif   ;
230400130321             endif   ;
230500130321             reade (Ktip:vscmat)  ungio01l    ;
230600130321           enddo   ;
230700100927
230800100927          // controllo che non sia già stata inserita in questa pagina di lavoro
230900100927          wtipmat=vsctip ;
231000100927          %subst(wtipmat:3:15)=vscmat   ;
231100100927          Indx=%lookup(wtipmat:tipmat)  ;
231200100928 2        if Indx>0 and Indx<>s02nrr ;
231300100927          Errgenerico=*on  ;
231400100927          Errmessage =*on  ;
231500100927          ErrMAT=*on  ;
231600100927          v1cmsg=msg(24)  ;
231700100927           EXSR AggioSFL   ;
231800100927           leavesr  ;
231900100928           else  ;
232000100928           // la memorizzo
232100100928           // solo se maggiore di 1999 non memorizzo
232200100928           if s02nrr<2000 ;
232300100928            tipmat(s02nrr)=wtipmat  ;
232400100928  2        endif    ;
232500100928 2        endif    ;
232600100927
232700101022 2        if unaksc>0  and vscksc=0 and vscinvr='F';
232800100927          vscksc =unaksc;
232900100927           chain   (1:dutkci:unaksc) cnaco00f   ;
233000100927 3         if %found(cnaco00f)   ;
233100100927             vsdksc=acorag  ;
233200100927             vsdksc3=acorag  ;
233300100927           else   ;
233400100927             vsdksc=*all'?'  ;
233500100927             vsdksc3=*all'?'  ;
233600100927 3         endif  ;
233700100927 2        endif  ;
233800100927 2        if vscdes=*blanks ;
233900100927          vscdes=unades ;
234000100927 2        endif ;
234100100927 1       endif    ;
234200100927         ENDSR    ;
234300080703       //--------------------------------------------------------------
234400080703       //?Aggiornamento sfl
234500080703       //--------------------------------------------------------------
234600080703       BEGSR AGGIOSFL  ;
234700100830
234800100830       IF $VIDEO='S2'       ;
234900100830
235000100830       exsr ProtCampi   ;
235100100830
235200100824       if v1cmsg<>*blanks   ;
235300080703       errMessage  = *on;
235400080703       errGenerico = *on;
235500100315       c02csr=s02nrr    ;
235600080703       endif    ;
235700080703
235800100921       if Recvuoto<>'S';
235900100830        Sflnxtchg=*on  ;
236000100921        endif  ;
236100100921        clear recvuoto  ;
236200100824
236300100315       update  sduos02  ;
236400100830       ENDIF   ;
236500100830
236600080703       ENDSR   ;
236700100312       //--------------------------------------------------------------
236800100312       //?Inizializzazione videata 01
236900100312       //--------------------------------------------------------------
237000100312       BEGSR Inzd01  ;
237100100312
237200100315         clear V1cmsg;
237300100825         clear V1dcuf;
237400100830         EmisMOD1    = *on ;
237500100826         errMessage  = *off;
237600100312         errGenerico = *off;
237700100825         v1ccuf ='MAG'   ;
237800100908         if modInt=*off  and modReso=*off ;
237900100826         v1cconf='S'  ;
238000100908         else  ;
238100100908         clear v1cconf ;
238200100908         endif  ;
238300100825         clear v1cinvr ;
238400100824         clear  v1cute  ;
238500100831         clear v1ciet  ;
238600100826         clear v1ctip  ;
238700100831         clear v1ctip  ;
238800100824         clear v1dtip  ;
238900100315         clear v1cmat  ;
239000100315         clear v1cmgc  ;
239100100315         clear v1cmag  ;
239200100831         clear v1creso ;
239300100831         clear v1cdtm  ;
239400101025         clear v1cnot  ;
239500121024         clear v1cdes  ;
239600100312
239700100831         // Per le MATRICOLE escludo il tipo 99
239800100831         if ImmMAT=*on   ;
239900100831          v1ciet='E'  ;
240000100831          v1ctip='99'  ;
240100100831         wtip=v1ctip   ;
240200100831         exsr chktip   ;
240300100831         v1dtip=wdtip  ;
240400100831         endif  ;
240500100831
240600101125         // Per gestine RESI imposto al "S"  e solo i confermati
240700100908         if ModReso=*on   ;
240800100831         v1creso='S'   ;
240900101125         v1cconf='C'  ;
241000100831         endif  ;
241100100831
241200100924         // Per conferma movimenti e immissione imposto data conferma = udate
241300110914         // anche per immissione matricole
241400110914         if ImmGES=*on  or ImmMAT=*on  ;
241500101112         v1cdtma=Dataoggi      ;
241600101112         endif   ;
241700101112         if ImmCMOV=*on   ;
241800100903         v1cdtm=Dataoggi      ;
241900100831         endif  ;
242000100831
242100100312         ENDSR    ;
242200080207       //--------------------------------------------------------------
242300100312       //?Inizializzazione SFL02
242400080207       //--------------------------------------------------------------
242500100312       BEGSR InzS02;
242600080207
242700080207       // Pulizia subfile
242800080207         SflDsp_N    = *on;
242900080207         SflDspCtl_N = *on;
243000100315         write  SDUOC02;
243100080207         SflDspCtl_N = *off;
243200080207         SflEnd      = *off;
243300100824         SflDsp_N    = *off;
243400100824         Sflnxtchg   = *off;
243500080530
243600121130         clear w05dtm  ;
243700100826         clear tipmat;
243800101012         clear SiCoPe  ;
243900100826         clear C02rcd;
244000100315         clear C02csr;
244100100315         S02nrr=0 ;
244200100824         clear V1cmsg;
244300100824         clear totrig;
244400080207         errMessage  = *off;
244500080207         errGenerico = *off;
244600100708
244700100826         // Decodifico ufficio scelto
244800100826         if v1ccuf='MAG'           ;
244900100827         v1dcuf=' MAG=Magazzino  C.E.D. ' ;
245000100826         ELSE  ;
245100100827         v1dcuf='FOR=Magazzino  ACQUISTI' ;
245200100826         ENDIF  ;
245300100826
245400100823         // Predispongo le letture e carico SFL
245500100823         // se movimenti MAG imposto SEDE nel tipo magazzino correlato
245600100825         kinvr=v1cinvr ;
245700100825         if v1ccuf ='MAG' and v1cinvr=' '  ;
245800100825         eval kinvr='I' ;
245900100823         endif  ;
246000100823
246100100823         clear kmag  ;
246200100823         if v1cmag>*zeros  ;
246300100823         kmag=%int(v1cmag)  ;
246400100823         endif  ;
246500100823
246600100823         clear kmgc  ;
246700100823         if v1cmgc>*zeros  ;
246800100823         kmgc=%int(v1cmgc)  ;
246900100823         endif  ;
247000100824
247100100824         clear ktip  ;
247200100824         if v1ctip>*zeros  ;
247300100824         ktip=%int(v1ctip)  ;
247400100824         endif  ;
247500100823
247600100823         // Se presente tipo e matricola leggo per questo
247700121024         //if ktip>*zeros and v1ciet ='I'   ;
247800100823
247900121024         //select   ;
248000121024         //when v1cmat=*blanks  ;
248100121024         //  setll (ktip)                      ungio01l    ;
248200121024         //  reade (ktip)                      ungio01l    ;
248300121024         //when v1cdim>0   ;
248400121024         //  setll (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
248500121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
248600121024         //other;
248700121024         //  setll (ktip:v1cmat:v1ccuf)        ungio01l    ;
248800121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
248900121024         //endsl   ;
249000100823
249100121024         //dow not %eof(ungio02l)  ;
249200100823         // controllo parzializzazioni
249300121024         // vshnrr =gio1nrr ;
249400121024         //exsr  CtrPARZ   ;
249500121024         //if Wok='S'   ;
249600121024         //exsr CarSF02  ;
249700121024         //endif   ;
249800100823
249900121024         //select   ;
250000121024         //when v1cmat=*blanks  ;
250100121024         //  reade (ktip)                        ungio01l    ;
250200121024         //when v1cdim>0   ;
250300121024         //  reade (ktip:v1cmat:v1ccuf:w1cdim) ungio01l    ;
250400121024         //other;
250500121024         //  reade (ktip:v1cmat:v1ccuf)        ungio01l    ;
250600121024         //endsl   ;
250700121024         //enddo   ;
250800100823
250900121024         //else  ;
251000100823
251100100823         select  ;
251200100831         when kinvr  =*blanks ;
251300100831           setll (v1ccuf)                      ungio04l    ;
251400100831           reade (v1ccuf)                      ungio04l    ;
251500100823         other ;
251600101012           setll (v1ccuf:kinvr:kmag)           ungio04l    ;
251700100831           reade (v1ccuf:kinvr)                ungio04l    ;
251800100824         endsl  ;
251900100823
252000100823         dow not %eof(ungio04l)  ;
252100100823         // Controllo Parzializzazioni
252200100827          vshnrr =gio4nrr ;
252300100823         exsr  CtrPARZ   ;
252400100824         if Wok='S'   ;
252500100823         exsr CarSF02  ;
252600100823         endif   ;
252700100823
252800100823         select  ;
252900100831         when kinvr=*blanks ;
253000100825           reade (v1ccuf)                      ungio04l    ;
253100100823         other ;
253200100831           reade (v1ccuf:kinvr)                ungio04l    ;
253300100823         endsl  ;
253400100823         enddo   ;
253500100823
253600121024         //endif   ;
253700100823
253800100831         select ;
253900100831         // In interrogazione  resi e conferma:
254000100831         //                 mi posiziono sul primio record  e non scrivo righe vuote
254100100831         when §sdv0int='S'  or §sdv0resi='S'  or §sdv0conf='S' ;
254200100830         c02csr=1 ;
254300100830         c02rcd=1  ;
254400100831
254500100831         // In immissione matricole mi posiziono sulla matricola ssul primo record
254600100831         when §sdv0MMat='S'  ;
254700100831         if c02csr=0 ;
254800100831         c02csr=1 ;
254900100831         endif  ;
255000100831
255100100831         other  ;
255200100831         // Solo in Gestione: Scrittura righe vuote
255300101001         // salvo il primo record vuoto che scrivo
255400101001         PrimoVuoto=s02nrr+1  ;
255500100824         exsr Righevuote  ;
255600100831         endsl   ;
255700100823
255800100824         ENDSR  ;
255900100824       //--------------------------------------------------------------
256000100824       //?Scrittura righe vuote
256100100824       //--------------------------------------------------------------
256200100824         BEGSR  Righevuote   ;
256300100824
256400100824         // Finito di leggere carico tutta la pagina con record vuoti
256500100824         if s02nrr>=c_sflPag ;
256600100824         wpag=%div(s02nrr:c_sflPag) ;
256700100824         wresto=%rem(s02nrr:c_sflPag)  ;
256800100824         if wresto>0    ;
256900100924         c02csr=s02nrr  ;
257000100824         else   ;
257100100924         c02csr=(wpag*c_sflpag)+1 ;
257200100824         endif  ;
257300100824
257400100824         else  ;
257500100824         clear wpag  ;
257600100824         wresto=s02nrr  ;
257700100824         // posizionamento sulla prtima pagina e 1° record vuoto
257800100924         c02csr=s02nrr  ;
257900100927          if c02csr=0 ;
258000100927           eval c02csr=1  ;
258100100927          endif ;
258200100824         endif ;
258300100824
258400100824         dow wresto<c_sflpag  ;
258500100823         wresto=wresto+1 ;
258600100826
258700100824         vshimm='S'   ;
258800100830         clear VSCOPZ ;
258900100830         clear VShann ;
259000100909         clear VShfmv ;
259100101006         clear VShfpr ;
259200120524         clear VShfst ;
259300100909         clear VShnrr ;
259400100825         if v1ccuf ='MAG'  ;
259500100825         vscinvr='I'  ;
259600100824         else   ;
259700100825         vscinvr=v1cinvr;
259800100824         endif   ;
259900100823         clear vscmag ;
260000100830         clear vscmag3;
260100100827         clear vshnrr ;
260200100827         clear vscqta ;
260300100823         clear vsctip ;
260400100830         clear vsdtip ;
260500100830         clear vsdtip3;
260600100827         clear vscmgc ;
260700100827         clear vscmat ;
260800100827         clear vsdmod1 ;
260900100827         clear vsdmod2 ;
261000100830         clear vsdmod  ;
261100100823         clear vscdes ;
261200100826         clear vscnot ;
261300100823         clear vsctru ;
261400100823         clear vsctir ;
261500100923         clear vscmatr;
261600100923         clear vsdmodr;
261700101005         clear vscmgcr;
261800101005         clear vshfmat;
261900100826         clear vscksc ;
262000100826         clear vsdksc ;
262100100830         clear vsdksc3;
262200100823         clear vscdtm ;
262300100823         clear vscddo ;
262400100823         clear vscdoc ;
262500100825         clear vscute ;
262600100830         clear vscdim4;
262700100830         clear vscdim ;
262800100827         clear vscreso  ;
262900100827         clear vscslr   ;
263000101008         clear vscncl   ;
263100101008         clear vscpkg   ;
263200100825         vshdim=%dec(datasys)  ;
263300100824
263400100824          exsr Protcampi  ;
263500100824
263600100823          s02nrr=s02nrr+1 ;
263700100823          write  SDUOS02;
263800100823          enddo  ;
263900080409
264000100824         // Salvo numero righe scritte
264100100824         totrig=s02nrr  ;
264200100824
264300080702         ENDSR    ;
264400080627
264500100823       //--------------------------------------------------------------
264600100823       //?Controllo selezioni impostate in prima videata
264700100823       //--------------------------------------------------------------
264800100823          BEGSR   CtrPARZ    ;
264900100823          Wok='S'    ;
265000100908          if w1cdtmd>0  and w1cdtma=0 ;
265100100908          w1cdtma=w1cdtmd ;
265200100908          endif  ;
265300100826
265400100823          // Utente
265500100823          if v1cute<>*blanks   and ungute<>v1cute ;
265600100823            clear wok  ;
265700100823            leavesr    ;
265800100823          endif      ;
265900100823          // Data Immissione
266000100824          if v1cdim>*zeros    and ungdim<w1cdim ;
266100100823            clear wok  ;
266200100823            leavesr    ;
266300100823          endif      ;
266400100823          // Tipo movimentazione
266500100825          if ungcuf <>v1ccuf ;
266600100823            clear wok  ;
266700100823            leavesr    ;
266800100823          endif      ;
266900100823          // Tipo magazzino correlato
267000100825          if kinvr<>*blanks and unginvr<>kinvr  ;
267100100823            clear wok  ;
267200100823            leavesr    ;
267300100823          endif      ;
267400100823          // Magazzino destinazione
267500100823          if kmag>*zeros  and ungmag<>kmag   ;
267600100823            clear wok  ;
267700100823            leavesr    ;
267800100823          endif      ;
267900100823          // Magazzino correlato
268000100823          if kmgc>*zeros  and ungmgc<>kmag   ;
268100100823            clear wok  ;
268200100823            leavesr    ;
268300100823          endif      ;
268400100826          // solo i record da confermare
268500100826          if v1cconf='S' and ungfmv='S' ;
268600100826            clear wok  ;
268700100826            leavesr    ;
268800100826          endif      ;
268900100908          // solo i record confermati
269000100908          if v1cconf='C' and ungfmv=' ' ;
269100100908            clear wok  ;
269200100908            leavesr    ;
269300100908          endif      ;
269400100908          // data conferma movimenti AL
269500100831          if v1cdtm>0   and ungdtm>w1cdtm  ;
269600100831            clear wok  ;
269700100831            leavesr    ;
269800100831          endif      ;
269900100908          // data conferma movimenti DAL/AL
270000100908          if v1cdtmd>0 or v1cdtma>0 ;
270100100908          if  ungdtm<w1cdtmd or ungdtm>w1cdtma ;
270200100908            clear wok  ;
270300100908            leavesr    ;
270400100908          endif      ;
270500100908          endif      ;
270600101025
270700101025          // SCAN sulle note
270800101025          if v1cnot<>*blanks   ;
270900101025           xx=%checkr(' ':v1cnot)  ;
271000101025           Indx=%scan(%subst(v1cnot:1:xx):ungnot)  ;
271100101025           if Indx=0   ;
271200101025            clear wok  ;
271300101025            leavesr    ;
271400101025           endif      ;
271500101025          endif      ;
271600121024          // SCAN sulla descrizione
271700121024          if v1cdes<>*blanks   ;
271800121024           xx=%checkr(' ':v1cdes)  ;
271900121024           Indx=%scan(%subst(v1cdes:1:xx):ungdes)  ;
272000121024           if Indx=0   ;
272100121024            clear wok  ;
272200121024            leavesr    ;
272300121024           endif      ;
272400121024          endif      ;
272500100823
272600121024          // Tipo Unità  da ESCLUDERE
272700100831          if v1ctip<>*blanks and v1ciet='E' and ktip= ungtip ;
272800100831            clear wok  ;
272900100831            leavesr    ;
273000100831          endif      ;
273100121024          // Tipo Unità  da INCLUDERE
273200121024          if v1ctip<>*blanks and v1ciet='I' and ktip<> ungtip ;
273300121024            clear wok  ;
273400121024            leavesr    ;
273500121024          endif      ;
273600100927
273700121029          // Matricola inseribile solo per "I"-inclusione
273800100927          if v1cmat<>*blanks and  v1cmat<>ungmat ;
273900100831            clear wok  ;
274000100831            leavesr    ;
274100100831          endif      ;
274200100831
274300100831          // unità da rendere
274400100831          if v1creso='S'  and ungreso='S'   ;
274500100831            clear wok  ;
274600100831            leavesr    ;
274700100831          endif      ;
274800100831          if v1creso='S'  and ungtir=0      ;
274900100831            clear wok  ;
275000100831            leavesr    ;
275100100831          endif      ;
275200100823          ENDSR  ;
275300100823       //--------------------------------------------------------------
275400100823       //?eseguo caricamento SFL
275500100823       //--------------------------------------------------------------
275600100823          BEGSR   carSF02    ;
275700100830          clear vscopz  ;
275800100830          clear vshANN  ;
275900100830          protOPZ=*off  ;
276000100830          clear vshimm  ;
276100100830
276200100830          exsr CarRECORD  ;
276300100830
276400100830          s02nrr=s02nrr+1   ;
276500100831
276600100831          // Per l'immissione Matricole mi posizione sul 1° record senza il codice
276700100831          if §sdv0mmat='S' and c02csr=0 and  vscmat=*blanks ;
276800100831          c02csr=s02nrr  ;
276900100831          ErrMat =*on    ;
277000100831          Sflnxtchg=*on  ;
277100100831          endif  ;
277200100831
277300100831          write  SDUOS02;
277400100831          ErrMat =*off   ;
277500100831          Sflnxtchg=*off ;
277600100831
277700100830          ENDSR         ;
277800100830       //--------------------------------------------------------------
277900100830       //?Carica record
278000100830       //--------------------------------------------------------------
278100100830          BEGSR   carRECORD ;
278200100830
278300100831          vshfmv =ungfmv ;
278400101006          vshfpr =ungfpr ;
278500120524          vshfst =ungfst ;
278600100831          vscinvr=unginvr;
278700100830  1       if $video='S2' ;
278800100823          vscmag=ungmag  ;
278900100830          clear vsdmod1  ;
279000100830          clear vsdmod2  ;
279100100830          clear vsdksc   ;
279200100830          clear vscdim4  ;
279300100830          clear vsdtip   ;
279400100830          else ;
279500100830          vscmag3=%editc(ungmag:'X')    ;
279600100909          wtmag='D'      ;
279700100908          wazorg='E  '   ;
279800100830          wmag=vscmag3   ;
279900100830          exsr  chkmag   ;
280000100830          vsdmag=wdmag    ;
280100100830
280200100830          clear vsdmod   ;
280300100830          clear vsdksc3  ;
280400100830          clear vscdim   ;
280500100830          clear vsdtip3  ;
280600100830  1       endif ;
280700100830
280800100826          vscqta=ungqta  ;
280900101008          vscncl=ungncl  ;
281000101008          vscpkg=ungpkg  ;
281100100826
281200100924          clear vsctip  ;
281300100830 1        if ungtip>0 ;
281400100830          vsctip=%editc(ungtip:'X') ;
281500100830          wtip=vsctip   ;
281600100830          exsr chktip   ;
281700100830 3          if $Video='S2' ;
281800100830            vsdtip=wdtip  ;
281900100830            else  ;
282000100830            vsdtip3=wdtip  ;
282100100830            endif ;
282200100830  1       endif ;
282300100826          vscmat=ungmat  ;
282400100826          clear vscksc   ;
282500100826          // Se c'e' tipo e matricola prendo il modello e lo visualizzo
282600100826 1        if ungtip>0 and ungmat<>*blanks ;
282700100930          chain (ungmat:ungtip)  unana07l    ;
282800100930 2        if  %found(unana07l)  ;
282900100830
283000100830 3        if $Video='S2' ;
283100100830          vsdmod1=unamod;
283200100827          vsdmod2=%subst(unamod:7:9) ;
283300100830          else  ;
283400100830          vsdmod =unamod;
283500100830 3        endif  ;
283600100826
283700100830          // se non è invio visualizzo cliente da anagrafica
283800100830 3        if  vscinvr<>'I'  and ungksc=0;
283900100826          vscksc =unaksc;
284000100830 4        if unaksc>0  ;
284100100826           chain   (1:dutkci:unaksc) cnaco00f   ;
284200100830 5         if not %found(cnaco00f)   ;
284300100830             acorag=*all'?'  ;
284400100830 5         endif  ;
284500100830 5         if $Video='S2' ;
284600100830             vsdksc=acorag  ;
284700100830           else  ;
284800100830             vsdksc3=acorag  ;
284900100830 5         endif  ;
285000100830
285100100830 4        endif ;
285200100830 3        endif ;
285300100826 2        endif ;
285400100826 1        endif ;
285500100826
285600100827          vscslr =ungslr  ;
285700100827          vscreso=ungreso ;
285800110404          dgio01=ungflo  ;
285900110404          if unginvr='R' or unginvr='F'  ;
286000110404           vsctru=§giogsf  ;
286100110404          else  ;
286200110404           vsctru=ungtru  ;
286300110404          endif  ;
286400100923
286500100927          clear vsdtir ;
286600100827  1       if ungtir>0 ;
286700100830          vsctir=%editc(ungtir:'X')   ;
286800100927          wtip=vsctir   ;
286900100927          exsr chktip   ;
287000100927          vsdtir=wdtip  ;
287100100923          vscmatr=ungmatr ;
287200101005          vshfmat=ungmatr ;
287300101005          clear vscmgcr;
287400101005          clear vsdmodr;
287500100923 2        if ungtir>0 and ungmatr<>*blanks ;
287600100930          chain (ungmatr:ungtir)  unana07l    ;
287700100930 3        if  %found(unana07l)  ;
287800100923          vsdmodr=unamod  ;
287900101005          vscmgcr=unamag  ;
288000100923 3        endif  ;
288100100923 2        endif  ;
288200100923 xx1      else ;
288300100826          clear vsctir ;
288400100923          clear vscmatr;
288500101005          clear vshfmat;
288600100923          clear vsdmodr;
288700101005          clear vscmgcr;
288800100827  1       endif ;
288900100826          vscmgc=ungmgc  ;
289000100923          clear vsdmgc  ;
289100100909          if ungmgc>0   ;
289200100909          wmag=%editc(ungmgc:'X');
289300100909          wtmag='C' ;
289400100909          wazorg='E  '   ;
289500100909          exsr  chkmag   ;
289600100909          vsdmgc=wdmag    ;
289700100909          endif   ;
289800100826          vscdes=ungdes  ;
289900100826          vscnot=ungnot  ;
290000100830
290100100830          // se pieno CLIENTE decodifico
290200100827 1        if ungksc>0   ;
290300100830           vscksc=ungksc  ;
290400100826           chain   (1:dutkci:ungksc) cnaco00f   ;
290500100830 2         if not %found(cnaco00f)   ;
290600100830             vsdksc=*all'?' ;
290700100830 2         endif  ;
290800100830 2         if $Video='S2' ;
290900100830             vsdksc=acorag  ;
291000100830           else  ;
291100100830             vsdksc3=acorag  ;
291200100830 2         endif  ;
291300100830
291400100827 1         endif  ;
291500100823
291600100823          clear vscdtm   ;
291700100823          if ungdtm>0    ;
291800100823          dataiso=%date(ungdtm)   ;
291900100823          datadmy=dataiso ;
292000100823          vscdtm=%dec(datadmy)  ;
292100100823          endif   ;
292200100823
292300100823          clear vscddo   ;
292400100823          if ungddo>0    ;
292500100823          dataiso=%date(ungddo)   ;
292600100823          datadmy=dataiso ;
292700100823          vscddo=%dec(datadmy)  ;
292800100823          endif   ;
292900100823          vscdoc=ungdoc  ;
293000100823
293100100826          vscute=ungute  ;
293200100823          if ungdim>0    ;
293300100823          dataiso=%date(ungdim)   ;
293400100823          datadmy=dataiso ;
293500100830          dataeur=dataiso ;
293600100823          w0060 =%dec(datadmy)  ;
293700100830          if $Video='D3'  ;
293800100830          vscdim=%dec(dataeur)  ;
293900100920          vschim=unghim         ;
294000100830          else  ;
294100100830          vscdim4=%subst(%editc(w0060:'X'):1:2) +'/' +
294200100827                  %subst(%editc(w0060:'X'):3:2)    ;
294300100830          endif  ;
294400100824          vshdim=ungdim  ;
294500100823          endif   ;
294600100823
294700100909          ErrGenerico=*off ;
294800100909          ErrMessage =*off ;
294900100826          exsr ProtCampi  ;
295000100823          ENDSR  ;
295100080702       //--------------------------------------------------------------
295200080702       //?Gestione protezione campi del sfl
295300080702       //--------------------------------------------------------------
295400080702          BEGSR   ProtCampi   ;
295500100830           protOPZ=*off ;
295600100826           ProtperINV=*off ;
295700100826           ProtperRIE=*off ;
295800110404           ProtperFOR=*off ;
295900100826           ProtINVR  =*off ;
296000100831           ProtMAT   =*off ;
296100100831           EmisCONF  =*off ;
296200100827           protrIGA=*off   ;
296300100830           RecANN  =*off   ;
296400100909           EmisCONF  =*off ;
296500101006           RevMAG    =*off ;
296600120524           NoSTAMPABO=*off ;
296700100824
296800100830          // Record di nuova immissione
296900100824          if vshimm='S'  ;
297000100830          protOPZ=*on  ;
297100100827          else  ;
297200100827           protrIGA=*on    ;
297300100830          endif    ;
297400100831
297500100903          // Per Int  e resi proteggo matricola
297600100903          if §sdv0resi='S'  or §sdv0int='S';
297700100831           protMAT =*on    ;
297800100831          endif   ;
297900100903          // Per immissione, solo per i record già inseriti
298000100903          if §sdv0imm='S' and vshimm=' ' ;
298100100903           protMAT =*on    ;
298200100903          endif  ;
298300100909
298400100909          // Se immissione matricole proteggo opzione  tranne che per F21
298500100909          if §sdv0mmat='S'  and dsp_aid <> c_F21    ;
298600100909           protOPZ =*on    ;
298700100909          endif   ;
298800100831
298900100831          // Se già confermato visualizzo
299000100831          if vshfmv='S'   ;
299100100831           EmisCONF  =*on  ;
299200100831          endif ;
299300101006          // Se già preparato materiale
299400101006          if vshfpr='P'   ;
299500101006           RevMAG   =*on  ;
299600101006          endif ;
299700100831
299800100831          // In fase di conferma accendo sflnexc per controllare tutti i record
299900100831          if §sdv0conf='S'   ;
300000100831           Sflnxtchg=*on  ;
300100100831          endif   ;
300200100830
300300100830          // Record annullato
300400100830          if vshANN='S'  ;
300500100830          protOPZ=*on  ;
300600100830          protrIGA=*on    ;
300700100830          recANN  =*on    ;
300800100830          endif  ;
300900100824
301000100826          if v1ccuf ='MAG'  or vscinvr='I'  ;
301100100826          protperINV=*on  ;
301200100824          endif    ;
301300100826
301400120523          if vscinvr='R' or vscinvr='F' or vscinvr='O';
301500110404           protperRIE=*on  ;
301600110404
301700120523          if vscinvr='R' or vscinvr='O';
301800110404           protperFOR=*on  ;
301900110404          endif  ;
302000100826          endif   ;
302100100826
302200100830          // Se immesso in proma videata il tipo invio/rientro lo proteggo
302300100826          if v1cinvr<>' ' ;
302400100826          protINVR=*on ;
302500100826          endif  ;
302600120524          // se non voglio la stampa della bolla visualizzo
302700120524          if vshfst='N'  ;
302800120524           NOstampaBO=*on  ;
302900120524          endif  ;
303000080703
303100080702          ENDSR    ;
303200100316       //--------------------------------------------------------------
303300100708       //?controllo Magazzino
303400100316       //--------------------------------------------------------------
303500100316        BEGSR ChkMag                 ;
303600100930        clear v1cmsg   ;
303700100930
303800100930        clear tnsdmagds  ;
303900100930        §magmag=wmag                            ;
304000100930        §imagtmag=wtmag                         ;
304100100930        §imagtorg=wazorg                        ;
304200100930        §imagcuf =v1ccuf                        ;
304300100930        §imaginvr=vscinvr                       ;
304400100930
304500100930        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
304600100930
304700100930        // Eseguita ricerca
304800100930        if §omagerr='R'   ;
304900100930        wmag=§magmag  ;
305000100930        wdmag = §omagdes  ;
305100100930        ErrGenerico=*on  ;
305200100930        leavesr   ;
305300100930        endif  ;
305400100930
305500100930        // Errore
305600100930        if §omagerr='E'   ;
305700100930        v1cmsg=§omagmsg  ;
305800100930        Errgenerico=*on  ;
305900100930        else   ;
306000100930        wdmag=§omagdes;
306100100930        endif  ;
306200100316
306300100316         if ErrGenerico =*on ;
306400100316         ErrMessage=*on  ;
306500100316         wdmag=*all'?'   ;
306600100316         endif  ;
306700100316
306800100316          ENDSR    ;
306900100708       //--------------------------------------------------------------
307000100708       //?controllo Tipo Unità
307100100708       //--------------------------------------------------------------
307200100708        BEGSR ChkTip                 ;
307300100708        clear WDTIP    ;
307400100824        indx=%scan('?':wtip )   ;
307500100827
307600100824        if Indx>0             ;
307700100708         clear tibs02ds  ;
307800100708         t02tla=' '      ;
307900100708         t02mod='R'      ;
308000100708         t02sif=knsif    ;
308100100708         t02cod='UNT'    ;
308200100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
308300100708         if t02ke1<>*blanks;
308400100824         wtip =t02ke1    ;
308500100708         wdtip=t02uni    ;
308600100708         else   ;
308700100708         clear  wtip ;
308800100708         endif  ;
308900100708
309000100708        ErrGenerico=*on   ;
309100100708        leavesr  ;
309200100708        endif   ;
309300100708
309400100708         clear tibs02ds  ;
309500100708         t02tla=' '      ;
309600100708         t02mod='C'      ;
309700100708         t02sif=knsif    ;
309800100708         t02cod='UNT'    ;
309900100708         t02ke1=wtip     ;
310000100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
310100100708
310200100708         if  t02err<>*blanks ;
310300100824         ErrGenerico=*on  ;
310400100824         ErrMessage =*on  ;
310500100824         wdtip=*all'?'   ;
310600100708         else  ;
310700100824         wdtip=t02uni    ;
310800100708         endif    ;
310900100708
311000100708          ENDSR    ;
311100080702       //--------------------------------------------------------------
311200100830       //?Aggiornamento record nuova immissione da SFL
311300080702       //--------------------------------------------------------------
311400080702        BEGSR ConfAGGIO              ;
311500081003
311600100315       s02nrr=1  ;
311700100824       readc    SDUOs02    ;
311800080704
311900100824 1     dow not %eof  ;
312000100831       // Per Gestione, se manca opzione, non c'e' nulla da aggiornare
312100100924       // Per immisssione, devono esserci tipo e mag destinazione
312200100924 2     if (§sdv0imm='S' and vshimm=' ' and vscopz=' ') or
312300100924 2        (§sdv0imm='S' and vshimm='S' and (vsctip<=*zeros or vscmag=0));
312400100831 x2    else   ;
312500100831
312600100831         // chain record
312700100831 3       if vshimm=' '    ;
312800100831           chain(e) vshnrr   ungio00f   ;
312900100831 4          if %error or not %found ;
313000100831             v1cmsg=msg(25)  ;
313100100831             ErrGenerico=*on  ;
313200100831             ErrMessage =*on  ;
313300100831             EXSR AggioSFL   ;
313400100831             leavesr  ;
313500100831 4          endif  ;
313600100831 3       endif  ;
313700100831
313800100907          exsr ScriviUNGIO  ;
313900100907 2     endif  ;
314000100830
314100100830       readc    SDUOs02    ;
314200100831 0      enddo           ;
314300100830       ENDSR            ;
314400100830       //--------------------------------------------------------------
314500100830       //?Aggiornamento file UNGIO
314600100830       //--------------------------------------------------------------
314700100830        BEGSR SCriviUNGIO            ;
314800100830
314900100830 1     if    vshimm=' '  and vscopz='4'  ;
315000100830       delete ungioFIS  ;
315100100830
315200100830 x1    else ;
315300100830
315400100830 2     if    vshimm='S'   ;
315500100827        clear ungiofis  ;
315600100901        timeiso=%time        ;
315700100901        unghim =%dec(timeiso);
315800100930        else   ;
315900100930        // richaino il record per non avere problemi  di "sporco" nei campi
316000100930         chain(e) vshnrr   ungio00f   ;
316100100830 2      endif   ;
316200100830
316300100824       ungdim  =vshdim  ;
316400100824       ungute  =vscute  ;
316500100826       ungcuf  =v1ccuf  ;
316600100825       if vsctip>*zeros  ;
316700100824       ungtip  =%int(vsctip) ;
316800100825       else  ;
316900100825       clear ungtip ;
317000100825       endif  ;
317100100824       ungmat  =vscmat  ;
317200100827       ungqta  =vscqta  ;
317300100827       if ungqta=0 ;
317400100827       ungqta=1 ;
317500100827       endif ;
317600100830       if $Video='S2' ;
317700100825       ungmag  =vscmag  ;
317800100830       else  ;
317900100830       ungmag  =%int(vscmag3)  ;
318000100830       endif  ;
318100100827       ungmgc  =vscmgc  ;
318200100825       unginvr =vscinvr ;
318300100824       ungdes  =vscdes  ;
318400100827       ungnot  =vscnot  ;
318500110404       // Per mag memorizzo vsctru un ungtru  come unità in aumento/sostituz
318600110404       if ProtperRIE=*off  ;
318700110404         ungtru  =vsctru  ;
318800110404         clear §giogsf  ;
318900110404       else  ;
319000110404       // Per "FOR" memorizzo nella DS DGIO01 come guasto fisico
319100110404        §giogsf=vsctru  ;
319200110404        clear ungtru  ;
319300120524       // per FOR "O" aggiorno "N" nella stampa bolla
319400120524       if ImmCMOV=*on  and  vscinvr='O'  ;
319500120524        ungfst='N'  ;
319600120524       endif  ;
319700110404       endif  ;
319800110404
319900110404       ungflo=dgio01  ;
320000110404
320100100827       clear ungksc   ;
320200100827       if vscksc>0 and vscinvr='I'  ;
320300100827       ungksc  =vscksc  ;
320400100827       endif  ;
320500100825       if vsctir>*zeros  ;
320600100824       ungtir  =%int(vsctir) ;
320700100923       ungmatr=vscmatr ;
320800100825       else  ;
320900100825       clear ungtir ;
321000100923       clear ungmatr;
321100100825       endif  ;
321200100827       if vscdtm>0 ;
321300100827       dataeur=%date(vscdtm:*dmy)  ;
321400100827       dataiso=dataeur  ;
321500100827       ungdtm=%dec(dataiso)  ;
321600100922       else  ;
321700100922       clear ungdtm  ;
321800100827       endif  ;
321900100922
322000100827       if vscddo>0 ;
322100100827       dataeur=%date(vscddo:*dmy)  ;
322200100827       dataiso=dataeur  ;
322300100827       ungddo=%dec(dataiso)  ;
322400100922       else  ;
322500100922       clear ungddo ;
322600100827       endif  ;
322700100827       ungdoc=vscdoc  ;
322800100827       ungreso=vscreso ;
322900100827       ungslr =vscslr  ;
323000101008       ungncl= vscncl  ;
323100101008       ungpkg= vscpkg  ;
323200100824
323300100908       // Richiamo pgm che crea i movimenti
323400100930       //  se non è escluso e se sono in conferma
323500100922 2     if §sdv0conf='S'  and vscopz= ' '  ;
323600100908
323700100908       // il tipo "99" lo flaggo senza richiamo
323800100908 3     if ungtip<> 99  ;
323900110412
324000110412       // Se FOR"F" e c'e' nuova matricola la scrivo in UNANA prima di RENDERE
324100110412
324200110412 4     if ProtperFOR=*on and ungnot<>*blanks ;
324300110412       chain (ungtip:ungmat)  unana02l    ;
324400110412 5     if %found(unana02l) ;
324500110412        unamat=ungnot;
324600110412        unadtr=%dec(datasys)    ;
324700110412        update unana2   ;
324800110412 5     endif  ;
324900110412 4     endif  ;
325000100922
325100100907       clear dsunit  ;
325200100907       dsuop1='F6N'  ;
325300100907       dsutip=ungtip  ;
325400100907       kpjbu = dsunit  ;
325500100907       callp TNSDU5R (kpjba: dsungio)  ;
325600100922
325700100922       // Aggiorno file dotazioni se previsto e se tipo unità inviato e di reso diversi
325800101026 4     if unginvr='I'    and ungtru<>'  ' and ungtip<>ungtir and ungksc=0 ;
325900100922       chain (ungtip:ungmag)  undot01l   ;
326000100922 5     if %found(undot01l)  ;
326100100922        dotqta=dotqta+1  ;
326200100922        update undot000 ;
326300100922 x5     else    ;
326400100922        clear undot000;
326500100922        dottip=ungtip  ;
326600100922        dotqta=1 ;
326700100922        dotfil= ungmag ;
326800100922        write undot000 ;
326900100922 5      endif  ;
327000100922
3271001009225      if ungtru='S' and ungtir>0   ;
327200100922       chain (ungtir:ungmag)  undot01l   ;
3273001009226      if %found(undot01l)  ;
327400100922        dotqta=dotqta-1  ;
327500100922        update undot000 ;
3276001009226      endif  ;
3277001010285      endif  ;
3278001010284      endif  ;
327900100922
328000100930       // Se presente anche la matricola aggiorno UNANA con la "S" di RENDERE
328100101028 4     if ungmatr<>*blanks  ;
328200110421
328300110421       //  non imposto al "S" di rientro se già rientrata in data di oggi con FOR "F"
328400110421       exsr SelectSQLFORF   ;
328500110421
328600100930       chain (ungtir:ungmatr)  unana02l    ;
328700101028 5     if %found(unana02l) ;
328800110421
328900110421 4a    if wtrov=' ' ;
329000130321       // se però il magazzino è sede --> non imposto lo stesso la "S"
329100130321          clear tnsdmagds  ;
329200130321          §magmag=%editc(unamag:'X')  ;
329300130321          §imagtmag='C' ;
329400130321          §imagtorg='E  '   ;
329500130321
329600130321        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
329700130321
329800130321        if §omagerr<>' ' or (§omagfag<>'W' and §omagfag<>'Y')  ;
329900100930        %subst(unalsw:15:1)='S'  ;
330000110421 5     endif  ;
330100130321 5     endif  ;
330200130321
330300101027        unadtr=%dec(datasys)    ;
330400100930        update unana2   ;
330500110421 4a    endif  ;
330600110421 4     endif  ;
330700100930
330800100908 3     endif  ;
330900100922
331000100908       ungfmv='S'  ;
331100100908 3     if ungdtm=0  ;
331200100908         ungdtm  =%dec(datasys) ;
331300100908 3     endif  ;
331400100922
331500100908
331600100908         update ungioFIS  ;
331700100907
331800100908 x2    else   ;
331900100907
332000100908  3    if vshimm=' '   ;
332100100830         update ungioFIS  ;
332200100830         else ;
332300100827         write ungiofis  ;
332400100908  3    endif  ;
332500100908 2     endif  ;
332600100907
332700100908 1     endif  ;
332800080704
332900080702        ENDSR;
333000110421       //--------------------------------------------------------------
333100110421       //?Select con SQL su ungio00F   per FOR "F"
333200110421       //--------------------------------------------------------------
333300110421       BEGSR  SelectSQLForF ;
333400110421 1     if ungdtm=0  ;
333500110421         sqdtm  =%dec(datasys) ;
333600110421         else  ;
333700110421         sqdtm=ungdtm     ;
333800110421 1     endif  ;
333900110421
334000110421       clear wtrov  ;
334100110421
334200110421         wrkgetlista='SELECT count(*)  +
334300110421                          from UNGIO00F WHERE ungcuf=''FOR'' and unginvr=''F'' +
334400110421            and ungmat=''' + ungmatr +''' and ungtip=' + %editc(ungtir:'X')  +
334500110421            ' and ungdtm=' + %editc(sqdtm :'X') ;
334600110421
334700110421         exec sql   PREPARE sF FROM :wrkgetlista   ;
334800110421
334900110421         exec sql   DECLARE aF cursor FOR  sF   ;
335000110421
335100110421         exec sql OPEN aF   ;
335200110421
335300110421 0       if  sqlcod=0   ;
335400110421         exec sql FETCH NEXT  FROM aF INTO:sqcont;
335500110421 0         if  sqcont>0   ;
335600110421             wtrov='S'  ;
335700110421 0         endif   ;
335800110421 0       endif   ;
335900110421
336000110421         exec sql CLOSE aF   ;
336100110421
336200110421       ENDSR;
336300110421
336400100830       //--------------------------------------------------------------
336500101001       //?Gestione videata 03
336600100830       //--------------------------------------------------------------
336700100830       BEGSR Gesd03;
336800100830
336900101018       // salvo la matricola: se imposta nel sfl la riporto
337000100831         savMat=vscmat  ;
337100101005         savMatr=vscmatr ;
337200100830         exsr CarRECORD  ;
337300100831         vscMat=savmat  ;
337400101005         vscMatr=savmatr  ;
337500100831
337600100830
337700100830         // Controllo opzione
337800100830         Emismod1=*off ;
337900100830         clear vsdf6           ;
338000120524         clear vsdf15          ;
338100120524         if vscopz<>'4'  and
338200120524            vscinvr<>'O' ;
338300120524           if  vshfst=' '  ;
338400120524            VSDf15=cF15BLO                ;
338500120524           else  ;
338600120524            VSDf15=cF15SBLO                ;
338700120524           endif  ;
338800120524         endif  ;
338900120524
339000100830 1       select   ;
339100100830         when vscopz='4'   ;
339200100901         vsdf6='F6=Conferma ANNULLAMENTO' ;
339300100830         v3dmod=' ANNULLAMENTO '  ;
339400100830
339500100830 1       when vscopz='2'   ;
339600100901         VSDf6='F6=CONFERMA Modifica' ;
339700120524
339800100830         v3dmod='   MODIFICA   '  ;
339900120524         // Se non conferato sproteggo riga
340000100831         if vshfmv=' ' ;
340100100831          protRiga=*off  ;
340200100831         endif  ;
340300100830
340400100830 1       when vscopz='5'   ;
340500100830         v3dmod='  VISUALIZZA  '  ;
340600100830         endsl   ;
340700100830
340800100830         // Imposto i campi diversi
340900100830         vscmag3=%editc(vscmag:'X')    ;
341000100830
341100100830 0       dow    $video='D3'   ;
341200100830
341300100830         // Emissione Testata
341400100830         if  errMessage  = *off;
341500100830           write  SDUOT01;
341600100830         endif;
341700100830
341800100830         // Emissione videata
341900100830         exfmt  SDUOD03;
342000100830
342100100830         reset errMessage;
342200100830         reset errGenerico;
342300100830         clear V1cmsg    ;
342400100830         clear wtmag     ;
342500100830         errMAG=*off     ;
342600100830         errMGC=*off     ;
342700100830         errTIP=*off     ;
342800100830         errMAT=*off     ;
342900100830         errQTA=*off     ;
343000100830         errTRU=*off     ;
343100100830         errTIR=*off     ;
343200100830         errDES=*off     ;
343300110412         errNOT=*off     ;
343400100830         errKSC=*off     ;
343500100830         errDTM=*off     ;
343600100909         errDDO=*off     ;
343700101012         errNCL=*off     ;
343800100923         errMATR=*off     ;
343900101012         errINVR=*off     ;
344000100830
344100100830 1       SELECT;
344200100830
344300100830         // - F12=Ritorno
344400100830 1         WHEN  dsp_aid = c_F12;
344500100930            chain(n) vshnrr   ungio00f   ;
344600100831            exsr CarRECORD  ;
344700100924            ErrGenerico=*off ;
344800100924            ErrMessage =*off ;
344900100924            clear v1cmsg  ;
345000100924            vscMat=savmat  ;
345100101005            vscMatr=savmatr  ;
345200100830            $Video='S2'   ;
345300100831            Emismod1=*on  ;
345400100830            leavesr  ;
345500100901
345600100901         // - F2=Gestione Unità EDP
345700100901 1         WHEN  dsp_aid = c_F02;
345800100927            clear dsunana  ;
345900100928            if vscmat<>*blanks  and
346000100928               (rnmfl<>'VSCMATR ' or v1ccuf='FOR' )  ;
346100100927              UIMmat=vscmat  ;
346200100927             endif  ;
346300100928           if vscmatr<>*blanks  and
346400100928 2          (rnmfl= 'VSCMATR ' and v1ccuf='MAG' )  ;
346500100928              UIMmat=vscmatr ;
346600100928             endif  ;
346700100901           exsr CallGESUNIT     ;
346800100901
346900100909         // - F7=Ricerca Matricola
347000100901 1         WHEN  dsp_aid = c_F07;
347100100901           // Per annullamento opzione non ammmessa
347200100901           if vscopz='4'  ;
347300100901            ErrGenerico=*on   ;
347400100901            ErrMessage =*on   ;
347500100901            v1cmsg=msg(36)  ;
347600100901           else  ;
347700100901           exsr CallRicMat      ;
347800100901           endif  ;
347900101018
348000101018         // - F11=elimina "Materiale preparato"
348100101018 1         WHEN  dsp_aid = c_F11;
348200101018         chain(e) vshnrr   ungio00f   ;
348300101018         clear ungfpr  ;
348400101018         clear vshfpr  ;
348500101018         update ungiofis  ;
348600101018         RevMag=*off  ;
348700120524
348800120524         // - F15=Blocca/sblocca STAMPA BOLLA
348900120524 1         WHEN  dsp_aid = c_F15;
349000120525           //  In modifica  e visulizzazione
349100120525 2         if vscopz='2' or vscopz='5' ;
349200120524           // e solo se corretto il magazzino di destino
349300120524 3         if   vscinvr='O'   ;
349400120524            ErrGenerico=*on   ;
349500120524            ErrMessage =*on   ;
349600120524            v1cmsg=msg(60)  ;
349700120524 x3        else  ;
349800120524           // solo per magazzini fornitori che prevedono la stampa
349900120524          wmag= vscmag3         ;
350000120524          wazorg='E  '   ;
350100120524          exsr  chkmag   ;
350200120524
350300120524  4       if §omagfag='Y' or §omagfag='F' or §omagfag='A'  or
350400120524             §omaglue='S'  ;
350500120525
350600120524           chain(e) vshnrr   ungio00f   ;
350700120525           select   ;
350800120525  5        when ungfst='N'  ;
350900120524           ungfst=' '  ;
351000120524           vshfst=' '  ;
351100120525            VSDf15=cF15BLO                ;
351200120524           NOstampaBO=*off ;
351300120525
351400120525  5        when ungfst='S' ;
351500120525             v1cmsg=msg(62)  ;
351600120525             ErrGenerico=*on  ;
351700120525             ErrMessage =*on  ;
351800120525  x5         other ;
351900120524           ungfst='N'  ;
352000120524           vshfst='N'  ;
352100120525            VSDf15=cF15SBLO                ;
352200120524           NOstampaBO=*on  ;
352300120525  5       endsl   ;
352400120524         update ungiofis  ;
352500120524  x4      else  ;
352600120524         // Errore --> magazzino che non prevede la stampa
352700120524            ErrGenerico=*on   ;
352800120524            ErrMessage =*on   ;
352900120524            v1cmsg=msg(61)  ;
353000120524  4       endif  ;
353100120524  3       endif  ;
353200120524  2       endif  ;
353300120524
353400100901
353500100901         // - F14=Gestione tabella scheda clienti x scambio dati
353600100901 1         WHEN  dsp_aid = c_F14;
353700100901           // Per annullamento opzione non ammmessa
353800100901           if vscopz='4'  ;
353900100901            ErrGenerico=*on   ;
354000100901            ErrMessage =*on   ;
354100100901            v1cmsg=msg(36)  ;
354200100901           else  ;
354300100901           exsr Callpgm3R       ;
354400100901           endif  ;
354500100909
354600100830 x1      // Invio
354700100830           OTHER;
354800100830
354900100924           // non controllo record per opzione '4'
355000100924 3         if vscopz<>'4'   ;
355100100830           exsr contrRIGA ;
355200100924           endif   ;
355300100830
355400100830 2         if ErrGenerico=*Off   ;
355500100830
355600100830           //  Aggiornamento record di UNGIO
355700100830 2         if   dsp_aid = c_F06  and vscopz<>'5';
355800100831            exsr   ScriviUNGIO ;
355900100831            $Video='S2'   ;
356000100831            Emismod1=*on  ;
356100100830
356200100830           //  Ricarico i dati del record nel sfl aggiornati per opz "2"
356300100830 3         if vscopz='2'   ;
356400100830           exsr CarRECORD  ;
356500100830 x3        else  ;
356600100830           // scrivo la dicitura "ANNULLATO"
356700100830           clear  vscnot;
356800100830           vscnot='ANNULLATO  ' ;
356900100830           vshann='S' ;
357000100830 2         endif   ;
357100100830
357200100830         protRiga=*on   ;
357300100830 2       endif   ;
357400100830 2       endif   ;
357500100830
357600100830 1       ENDSL;
357700100830
357800100830 0     enddo   ;
357900100830
358000100830       ENDSR;
358100100830
358200101001       //--------------------------------------------------------------
358300101001       //?Gestione videata 04 - inserimento multiplo
358400101001       //--------------------------------------------------------------
358500101001       BEGSR Gesd04;
358600101001
358700101001         v3dmod='INSER.MULTIPLO'  ;
358800101001
358900101001         // pulisco i campi
359000101001         clear vscmag3  ;
359100101001         clear vsdmag   ;
359200101001         clear vscqta   ;
359300101001         clear vsctip   ;
359400101001         clear vsdtip3  ;
359500101001         clear vsctru   ;
359600101001         clear vsctir   ;
359700101001         clear vsdtir   ;
359800101001         clear vscksc   ;
359900101001         clear vsdksc3  ;
360000101001         clear vscdoc   ;
360100101001         clear vscddo   ;
360200101001         clear vscdtm   ;
360300101008         clear v4cncl   ;
360400101008         clear v4cpkg   ;
360500101001         clear v4cinse  ;
360600101001
360700101001 0       dow    $video='D4'   ;
360800101001
360900101001         // Emissione Testata
361000101001         if  errMessage  = *off;
361100101001           write  SDUOT01;
361200101001         endif;
361300101001
361400101001         // Emissione videata
361500101001         exfmt  SDUOD04;
361600101001
361700101001         reset errMessage;
361800101001         reset errGenerico;
361900101001         clear V1cmsg    ;
362000101001         clear wtmag     ;
362100101001         errMAG=*off     ;
362200101001         errTIP=*off     ;
362300101001         errQTA=*off     ;
362400101001         errTRU=*off     ;
362500101001         errTIR=*off     ;
362600101001         errKSC=*off     ;
362700101001         errDTM=*off     ;
362800101001         errDDO=*off     ;
362900101012         errNCL=*off     ;
363000101001         errINSE=*off     ;
363100101001
363200101001 1       SELECT;
363300101001
363400101001         // - F12=Ritorno
363500101001 1         WHEN  dsp_aid = c_F12;
363600101001            $Video='S2'   ;
363700101001            Emismod1=*on  ;
363800101001            leavesr  ;
363900101001
364000101001 x1      // Invio
364100101001           OTHER;
364200101001
364300101001           exsr contrRIGA ;
364400101001
364500101001 2         if ErrGenerico=*Off   ;
364600101001
364700101001           //  Aggiornamento record di UNGIO
364800101001 3         if   dsp_aid = c_F06 ;
364900101001            $Video='S2'   ;
365000101001            exsr   Scrivirighe ;
365100101001            Emismod1=*on  ;
365200101001
365300101001         protRiga=*on   ;
365400101001 3       endif   ;
365500101001 2       endif   ;
365600101001
365700101001 1       ENDSL;
365800101001
365900101001 0     enddo   ;
366000101001
366100101001       ENDSR;
366200101001
366300101001       //--------------------------------------------------------------
366400101001       //?Per inrimento multiplo scrivo righe nel sfl
366500101001       //--------------------------------------------------------------
366600101001       BEGSR Scrivirighe  ;
366700101001
366800101001       // salvo i campi per la scrittura del sfl
366900101001       if vscmag3>*zeros ;
367000101001       v4cmag=%int(vscmag3)  ;
367100101001       endif  ;
367200101001       v4cqta=vscqta  ;
367300101001       v4cqta=vscqta  ;
367400101001       v4ctip=vsctip  ;
367500101001       v4ctru=vsctru  ;
367600101001       v4ctir=vsctir  ;
367700101001       v4cksc=vscksc  ;
367800101001       v4dksc=vsdksc3 ;
367900101001       v4cdtm=vscdtm  ;
368000101001       v4cdoc=vscdoc  ;
368100101001       v4cddo=vscddo  ;
368200101001
368300101001
368400101001       // Dal primo vuoto in poi mi posizione e cerco il primo record libero
368500101001       s02nrr=Primovuoto  ;
368600101001
368700101001       Indx= 1 ;
368800101001       dow Indx<=v4cinse  ;
368900101001
369000101001       Lib='0'  ;
369100101001       dow Lib='0'  ;
369200101001        chain s02nrr sduos02 ;
369300101001        if %found  and (vscmag<>0 or vscdes<>*blanks  or vscksc<>0 or
369400101001         vsdksc<>*blanks  or vscnot<>*blanks or (vsctip<>*zeros and vsctip
369500101001         <>*blanks ) or vscmat<>*blanks) ;
369600101001         exsr Aggiosfl  ;
369700101001         s02nrr=s02nrr+1  ;
369800101001        else  ;
369900101001         Lib='1'  ;
370000101001        endif  ;
370100101001       enddo  ;
370200101001
370300101001       if not %found ;
370400101001       savnrr=s02nrr ;
370500101001       s02nrr=s02nrr-1 ;
370600101001       exsr RigheVuote  ;
370700101001       s02nrr=savnrr  ;
370800101001        chain s02nrr sduos02 ;
370900101001       endif  ;
371000101001       vscmag=v4cmag  ;
371100101001       vscqta=v4cqta  ;
371200101001       vscqta=v4cqta  ;
371300101001       vsctip=v4ctip  ;
371400101001       vsctru=v4ctru  ;
371500101001       vsctir=v4ctir  ;
371600101001       vscksc=v4cksc  ;
371700101001       vsdksc=v4dksc  ;
371800101001       vscdtm=v4cdtm  ;
371900101001       vscdoc=v4cdoc  ;
372000101001       vscddo=v4cddo  ;
372100101008       vscncl=v4cncl  ;
372200101008       vscpkg=v4cpkg  ;
372300101001
372400101001       exsr Aggiosfl  ;
372500101001
372600101001       s02nrr=s02nrr+1  ;
372700101001       indx=indx+1  ;
372800101001       enddo  ;
372900101001
373000101001       c02csr=s02nrr  ;
373100101001
373200101001       ENDSR;
373300080206       //--------------------------------------------------------------
373400080206       //?Operazioni finali.
373500080206       //--------------------------------------------------------------
373600080206       BEGSR RoutEnd;
373700080627
373800080704         // Chiusura pgm   ;
373900080704         clear tibs02ds  ;
374000080704         t02tla='C'      ;
374100080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
374200100903
374300100903         // Sblocco data area per pgm manut matricole
374400100907         if (§sdv0mmat='S'  and savdatblo=0) or
374500100907            (§sdv0conf='S'  and savdatblo=0) ;
374600100903         in *lock  tnsduo ;
374700100903         clear tnsduo   ;
374800100903         out tnsduo  ;
374900100903         endif  ;
375000080704
375100100920         if %open(fnlv22p) ;
375200100920         close    fnlv22p  ;
375300100920         endif  ;
375400100920
375500080206         *inLR = *on;
375600080704
375700080206         return;
375800080206
375900080206       ENDSR;
376000080206
376100080206      /end-free
376200100920     c*--------------------------------------------------------------
376300100920     c* videata richeista stampanti e ovrdbf
376400100920     c*--------------------------------------------------------------
376500100920     c     RICHSTAMPAN   BEGSR
376600100920     C                   CLEAR                   TRUL90DS
376700100920      *
376800100920     C                   MOVEL     'S'           d90RSE
376900100920     C                   CALL      'TRUL90R'
377000100920     C                   PARM                    KPJBA
377100100920     C                   PARM                    trul90DS
377200100920      *
377300100920     C* F3 - FINE
377400100920     c                   if        d90f3<>'1'
377500100920     c                   eval      comman=ovrp(1)
377600100920     c                   eval      lung=91
377700100920     c                   eval      %subst(comman:28:10)=d90pre
377800100920      *
377900100920     C*
378000100920     C                   CALL      'QCMDEXC'                            91
378100100920     C                   PARM                    COMMAN
378200100920     C                   PARM                    LUNG
378300100920      *
378400100920     C                   if        not *in91
378500100920     c                   open      fnlv22p
378600100920     c                   endif
378700100920     c                   endif
378800100920      *
378900100920     c                   ENDSR
379000080206
379100080206       //--------------------------------------------------------------
379200080206       //?Schiere a tempo di compilazione.
379300080206       //--------------------------------------------------------------
379400080206
379500080410** - MSG ------------------------------------------------------------------ -*
379600100316Utente errato                                                                   01
379700100316Data Errata                                                                     02
379800100316Magazzino Destinazione Errato                                                   03
379900100316Magazzino Correlato Errato                                                      04
380000100708Tipo unità errato                                                               05
380100100826Tipo / Matricola inesistente in anagrafica                                      06
380200100708Matricola inesistente in anagrafica                                             07
380300100708Matricola Esistente in anagrafica   ma NON univoca                              08
380400100824Scorrimento non più consentito                                                  09
380500100826L'indicazione dell'ufficio è obbligatoria                                       10
380600100826Per magazzino CED possibile solo l'opzione "INVII"                              11
380700100908Indicare anche dtaImmissione o dtaConf. o dati Da Confermare o unità da Rendere 12
380800100826La matricola NON ci vuole per tipo unità "99"                                   13
380900100826Tipo unità "99" accettato solo per Magazzino CED ("MAG")                        14
381000101112La quantità maggiore di 1 è da immettere solo per tipo unità 99                 15
381100100826Aumento/Sostituzione unità indicabile non per tipo "99"                         16
381200100826L'unità di RESO è indicabile solo se prevista la sostituizione dell'unità       17
381300100826Tipo unità di reso errata                                                       18
381400100826Descrizione obbligatoria                                                        19
381500100930Per tipi"48 e 49"indicare Cliente destinatario unità o digita"SCORTA" nelleNOTE 20
381600100826Codice cliente errato                                                           21
381700100924La data conferma movimento non può essere maggiore di 3 mesi circa...           22
381800130321Esiste già un invio/rientro giornaliero da confermare x questo tipo/matricola   23
381900100826Tipo/matricola già inseriti in questa pagina di lavoro!!                        24
382000100830Modifica/annull. al momento non eseguibili: record vincolato da altro lavoro    25
382100100830Opzione non ammessa                                                             26
382200100831Nessun dato da visualizzare                                                     27
382300100831Indicare se Includere o Escludere il tipo unità indicato                        28
382400100831Impossibile annullare: record già confermato                                    29
382500121130Impostare data conferma movimenti                                               30
382600121130Data conferma non inferiore a oggi                                              31
382700100831In fase di conferma movimenti obbligatorio inserire la matricola                32
382800100909Numero e data documento obbligatori se RIENTRO da fornitore                     33
382900100909Per interrogare i movimenti è necessaria la matricola                           34
383000100923Matricola errata perchè di unità diversa da quella immessa e NON è modificabile 35
383100100901Tasto funzionale non ammesso in Annullamento                                    36
383200100907Opz non utilizzabile:pgm "                    "  in uso da xxxxxxxxxx il xx/xx  37
383300100903Magazzino di Destinazione e Correlato non possone essere uguali                 38
383400100908Indicare se invio o rientro da fornitore o filiale I/R/F                        39
383500100908Data AL non può essere minore data DAL                                          40
383600100908Immettere o i dati da confermare o la data conferma dal/al                      41
383700100920Il magazzino DESTINAZIONE deve essere XXX                                       42
383800101006Per opzione"9"-->Stampa Etichetta,premere tasto F10 e selezionare la stampante  43
383900100923Se immessa la matricola di reso immettere anche il TIPO unità di reso           44
384000101005Mag.Ubicazione matricola di RESO  non  uguale al mag.di INVIO. Enter forza      45
384100100923Tipo invio / rientro obbligatorio per "FOR"                                     46
384200100927Se Esclusione di tipo unità, non indicare la matricola                          47
384300100930Tasto funz. F07 abilitato solo per la ricerca matricole di RESO: posizionarsi!! 48
384400100930Matricola con tipo unità diverso da quello indicato                             49
384500101001Immettere in numero degli inserimenti da effettuare                             50
384600101001Per inserimento multiplo, immettere almeno un dato                              51
384700101012F8-Materiale preparato ammesso solo per INVII                                   52
384800120525Per opzioni: P=Materiale preparato o B=blocca stampa bolla, premere F8          53
384900101012Mancano colli e peso per il magazzino destinazione indicato                     54
385000111024Se unità in proprietà alla 923 magazzino destinazione non può essere "990 980"  55
385100110404Immettere "S" = Si per indicare il guasto fisico o lasciare vuoto               56
385200110412Nuova matricola errata perchè già esistente !!                                  57
385300110412Immettere una matricola di massino 15 caratteri                                 58
385400110610Campo note obbligatorio se guasto fisico                                        59
385500120524La stampa bolla non viene eseguita per rientro fornitore diretto in filiale     60
385600120524F15 non ammesso: il magazzino non prevede la stampa                             61
385700120525Impossibile bloccare la stampa bolla: già stampata!!                            62
385800121130Data conferma movimento no > di 3 mesi                                          63
385900121203Immettere anche data DAL per richiesta movimenti CONFERMATI                     64
386000121203Massimo un mese e possibile richiedere per i movimenti CONFERMATI               65
386100121203Impossibile modificare la data a movimenti già confermati                       66
386200130116Il magazzino di destinazione non corrisponde alla ditta manutenzione nell'anagr.67
386300100920**
386400100920OVRPRTF FILE(FNLV22P) OUTQ(xxxxxxxxxx) FORMTYPE('ETCH_EDP') USRDTA('Unità EDP') SHARE(*YES)
