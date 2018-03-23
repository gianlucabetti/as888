000100111128       //==============================================================
000200121130       // tnsduor1 - Finestra di richiesta modifica data conferma movimenti
000300120405       //==============================================================
000400111128
000500111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600111128
000700111128       //--------------------------------------------------------------
000800121203     fTNSDUOD1  cf   e             workstn indds(IndDspF)
000900120207     f                                     infds(InfDspF)
001000121130       //--------------------------------------------------------------
001100121130     d
001200120207       // - Tasti funzionali a video
001300120207     d c_F01           c                   const(x'31')
001400120207     d c_F02           c                   const(x'32')
001500120207     d c_F03           c                   const(x'33')
001600120207     d c_F05           c                   const(x'35')
001700120207     d c_F06           c                   const(x'36')
001800120207     d c_F07           c                   const(x'37')
001900120207     d c_F08           c                   const(x'38')
002000120207     d c_F09           c                   const(x'39')
002100120207     d c_F10           c                   const(x'3A')
002200120207     d c_F12           c                   const(x'3C')
002300120207     d c_F13           c                   const(x'B1')
002400120207     d c_F14           c                   const(x'B2')
002500120207     d c_F15           c                   const(x'B3')
002600120207     d c_F16           c                   const(x'B4')
002700120207     d c_F17           c                   const(x'B5')
002800120207     d c_F18           c                   const(x'B6')
002900120207     d c_F19           c                   const(x'B7')
003000120207     d c_F20           c                   const(x'B8')
003100120207     d c_F21           c                   const(x'B9')
003200120207     d c_F22           c                   const(x'BA')
003300120207     d c_F23           c                   const(x'BB')
003400120207     d c_F24           c                   const(x'BC')
003500120207     d c_Enter         c                   const(x'F1')
003600120207     d c_RollDown      c                   const(x'F4')
003700120207     d c_RollUp        c                   const(x'F5')
003800111128
003900111128
004000111128       //--------------------------------------------------------------
004100120207       // Definizione schiere.                                         ?
004200111128       //--------------------------------------------------------------
004300121130     d MSG             s             78    dim(4) ctdata perrcd(1)
004400111128
004500111128       //--------------------------------------------------------------
004600120207
004700120207       // - InfDS
004800120207     d InfDspF         ds
004900120207     d   dsp_aid             369    369a                                        AID byte
005000121130     d
005100121130     d IndDspF         ds
005200121130     d  errMessage                    1n   overlay(IndDspF : 28)
005300121130     d  errGenerico                   1n   overlay(IndDspF : 99)
005400121130     d  ErrWdtm                       1n   overlay(IndDspF : 61)
005500120207
005600120404
005700120404     d KPJBA         e ds
005800121130       // - Flags booleani
005900121130     d $Fine           s               n   inz(*off)
006000121130     d $InzW01         s               n   inz(*on)
006100121130     d ErrBlock        s               n   inz(*off)
006200121130     d Qcmd            s            500    inz
006300121130     d
006400121130     d tnsduods_1      ds
006500121203     d   o05gma_dtm            1      6s 0
006600121203     d   o05amg_dtm            7     14s 0
006700121203     d   o05F06               15     15
006800121203     d   o05F12               16     16
006900121203     d   datapiu200           17     24s 0
007000121203     d   datasys              25     32s 0
007100120207
007200120207
007300121130
007400121130     d wlbdat          ds                  inz
007500121130     d  g02dat                 1      8  0
007600121130     d  g02inv                 9     16  0
007700121130     d  g02err                17     17
007800121130     d  g02tgi                18     22  0
007900121130
008000120207       // - Campi associati al video
008100121130     d $Video          s              2    inz('W5')
008200120207
008300120207       // - Variabili di appoggio
008400120510     d wData08         s              8s 0
008500120405     d wX              s              5s 0
008600111128
008700121130      /copy gaitrasrc/srcprotopr,xsrda8
008800111128       //--------------------------------------------------------------
008900120208       // M A I N - L I N E                                            ?
009000111128       //--------------------------------------------------------------
009100121130     c     *entry        plist
009200121130     c                   parm                    kpjba
009300121130     c                   movel     kpjbu         tnsduods_1
009400111128
009500111128      /free
009600111128
009700120208       // - Operazioni iniziali
009800111128       exsr  sr_RoutInz;
009900111128
010000120207       // - Gestione video
010100120207       DOW  $Fine = *off;
010200120207         select;
010300121130           when  $Video = 'W5';
010400121130             exsr  GesW05;
010500120207           other;
010600120207             $Fine = *on;
010700120207         endsl;
010800120207       ENDDO;
010900120207
011000120208       // - Operazioni finali
011100121130       kpjbu = tnsduods_1   ;
011200121130
011300121130       *inlr=*on  ;
011400111128
011500111128       //--------------------------------------------------------------
011600120208       // Operazioni iniziali
011700111128       //--------------------------------------------------------------
011800111128       BEGSR sr_RoutInz;
011900121130        clear o05f12  ;
012000121130        clear o05f06  ;
012100121203        clear o05gma_DTM  ;
012200121203        clear o05amg_DTM  ;
012300120620
012400111128       ENDSR;
012500120207
012600120207       //--------------------------------------------------------------
012700121130       // Gestione videata W05
012800120207       //--------------------------------------------------------------
012900121130       BEGSR GesW05;
013000121130
013100121130 0     dou   errGenerico=*off  and *inkf  ;
013200121130
013300121130         exfmt  SDUOW05;
013400121130
013500121130         reset errMessage;
013600121130         reset errGenerico;
013700121130         clear w05msg;
013800121130         errwDTM =*off     ;
013900121130
014000121130 1     if *inkl =*on  ;
014100121130        clear w05dtm  ;
014200121130        o05f12='1'  ;
014300121130        leave   ;
014400121130 1     endif  ;
014500121130
014600121130 1     if w05dtm=0  ;
014700121130               errMessage  = *on;
014800121130               errGenerico = *on;
014900121130               ErrWDTM    = *on;
015000121130               w05msg = Msg(02);
015100121130 1     endif;
015200121130
015300121130 1     if ErrGenerico = *off   ;
015400121130             clear wlbdat;
015500121130             g02dat = w05dtm ;
015600121130             xsrda8(wlbdat);
015700121130 2           if g02err = '1';
015800121130               errMessage  = *on;
015900121130               errGenerico = *on;
016000121130               ErrwDTM    = *on;
016100121130               w05msg = Msg(01);
016200121130 2           endif;
016300121130 1           endif;
016400121130
016500121130 1     if ErrGenerico = *off   ;
016600121130 2           if g02inv>datapiu200;
016700121130               errMessage  = *on;
016800121130               errGenerico = *on;
016900121130               ErrwDTM    = *on;
017000121130               w05msg = Msg(04);
017100121130 2           endif   ;
017200121130 1           endif   ;
017300121130
017400121130 1     if ErrGenerico = *off   ;
017500121203 2           if g02inv<%dec(datasys)  ;
017600121130               errMessage  = *on;
017700121130               errGenerico = *on;
017800121130               ErrwDTM    = *on;
017900121130               w05msg = Msg(03);
018000121130 2           endif   ;
018100121130 1           endif   ;
018200121130
018300121130 0         enddo   ;
018400121130
018500121130       o05f06=*inkf  ;
018600121203       o05gma_dtm=w05dtm ;
018700121203       o05amg_dtm=g02inv ;
018800121130       $Fine = *on;
018900121130
019000121130       ENDSR;
019100121130
019200120207
019300111128      /end-free
019400121130**
019500121130Data Errata                                                                     02
019600121130Impostare data conferma movimenti                                               30
019700121130Data conferma non inferiore a oggi                                              31
019800121130Data conferma movimento no > di 3 mesi                                          63
