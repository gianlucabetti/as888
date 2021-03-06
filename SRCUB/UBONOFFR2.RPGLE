000100100817       //==============================================================
000200100817       //?                                                             ?
000300100817       //?VISUALIZZAZIONE  INDICATORI UTILIZZATI DA UN PGM. RPG ILE    ?
000400100817       //?                                                             ?
000500100817       //==============================================================
000600110218
000700110218       //--------------------------------------------------------------
000800110218       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900110218       //--------------------------------------------------------------
001000110218
001100110218     /*CMD ovrdbf file(SOURCE) tofile(GAITRASRC/SRCTRUL) ovrscope(*calllvl)
001200110218     /*END dltovr file(SOURCE) lvl(*)
001300110218     /*END
001400110218
001500110218       //--------------------------------------------------------------
001600110218       //?Specifiche di controllo.                                     ?
001700110218       //--------------------------------------------------------------
001800100817
001900100817     h debug decedit('0,') datedit(*DMY/)
002000100817
002100110218       //--------------------------------------------------------------
002200110218       //?Dichiarazione file.                                          ?
002300110218       //--------------------------------------------------------------
002400110218
002500110218       // -?Sorgente?
002600110218     fSOURCE    if   f   92        disk
002700110218
002800110218       // -?Video?
002900100817     fUBONOFFD  cf   e             workstn
003000110218
003100110218       //--------------------------------------------------------------
003200110218       //?Definizione costanti.                                        ?
003300110218       //--------------------------------------------------------------
003400110218
003500110218       // -?Attributi di Visualizzazione?
003600110218     d c_DspAtrNorm    c                   const(x'A0')
003700110218     d c_DspAtr_HI_RI  c                   const(x'A3')
003800110218     d c_DspAtr_ND     c                   const(x'A7')
003900110218     d c_DspAtrRed_RI  c                   const(x'A9')
004000110218
004100110218       // -?Costante x CHECK numerico di un campo alfa?
004200110218     d c_Digits        c                   const('0123456789')
004300110218
004400110218       // -?Caratteri minuscoli?
004500110218     d c_Minusc        c                   const('abcdefghijklm+
004600110218     d                                            nopqrstuvwxyz+
004700110218     d                                            ������+
004800110218     d                                            ��+
004900110218     d                                            ����+
005000110218     d                                            ����+
005100110218     d                                            �����+
005200110218     d                                            ����')
005300110218
005400110218       // -?Caratteri maiuscoli?
005500110218     d c_Maiusc        c                   const('ABCDEFGHIJKLM+
005600110218     d                                            NOPQRSTUVWXYZ+
005700110218     d                                            ������+
005800110218     d                                            ��+
005900110218     d                                            ����+
006000110218     d                                            ����+
006100110218     d                                            �����+
006200110218     d                                            ����')
006300110218
006400110218       //--------------------------------------------------------------
006500110218       //?Definizione schiere.                                         ?
006600110218       //--------------------------------------------------------------
006700100817
006800110218       // -?Indicatorei in spedifiche "I" del sorgente?
006900100817     d $I              s              2    dim(5)
007000110218       // -?Indicatorei in spedifiche "C" del sorgente?
007100100817     d $C              s              2    dim(4)
007200110218     d SK              s              6    dim(3)
007300110218       // -?Indicatorei in spedifiche "O" del sorgente?
007400100817     d $O              s              2    dim(3)
007500110218
007600110218       // -?Elenco completo indicatori usati?
007700100817     d SavInd          s              1    dim(99)
007800110218
007900110218       //--------------------------------------------------------------
008000110218       //?Definizione aree dati.                                       ?
008100110218       //--------------------------------------------------------------
008200110218
008300110218
008400110218       //--------------------------------------------------------------
008500110218       //?Definizione strutture dati.                                  ?
008600110218       //--------------------------------------------------------------
008700110218
008800100817     d dsIN            ds             2    inz
008900100817     d  dsIN1                  1      1    inz
009000100817     d  dsIN2                  2      2    inz
009100110218
009200110218       //--------------------------------------------------------------
009300110218       //?Definizione variabili globali.                               ?
009400110218       //--------------------------------------------------------------
009500110218
009600110218       // -?Flags booleani?
009700110218     d $All            s               n   inz(*off)
009800100817     d $Free           s               n   inz(*off)
009900100817     d $IndDs          s               n   inz(*off)
010000100817     d $Overlay        s               n   inz(*off)
010100151210     d $SeeNextRow     s               n   inz
010200151210     d $Almeno_1_ind   s               n   inz
010300110218
010400110218       // -?Indici di schiera?
010500151210     d xx              s              2  0 inz
010600151210     d yy              s              2  0 inz
010700100817     d wIN             s              2  0 inz
010800110218
010900110218       // -?Campi di comodo?
011000100817     d wIndDs          s             44    inz
011100110218
011200110218       //--------------------------------------------------------------
011300110218       //?Definizione prototipi procedure.                             ?
011400110218       //--------------------------------------------------------------
011500110218
011600110218
011700110218       //--------------------------------------------------------------
011800110218       //?Definizione key-list.                                        ?
011900110218       //--------------------------------------------------------------
012000110218
012100110218
012200110218       //--------------------------------------------------------------
012300110218       //?Specifiche di input.                                         ?
012400110218       //--------------------------------------------------------------
012500100817
012600110218       // -?Membro Sorgente?
012700100817     iSOURCE    nn
012800100817     i                                 13   14  EOF
012900100817     i                                 18   18  SPEC
013000100817     i                                 19   19  CHAR
013100100817     i                                 19   27  FREE
013200100817     i                                 19   92  FreeSpec
013300100817     i                                 38   47  CodOpe
013400110218       //   �?Specifiche "I"?
013500100817     i                                 33   34  $I(1)
013600100817     i                                 79   80  $I(2)
013700100817     i                                 81   82  $I(3)
013800100817     i                                 83   84  $I(4)
013900100817     i                                 85   86  $I(5)
014000110218       //   �?Specifiche "F"?
014100100817     i                                 56   60  IndDs
014200100817     i                                 61   92  SpecF
014300110218       //   �?Specifiche "D"?
014400151210     i                                 19   92  d_ds_Name_cmpl
014500151210     i                                 19   33  d_ds_Name
014600100817     i                                 35   38  d_ds_
014700100817     i                                 41   44  d_IndDs_i
014800100817     i                                 48   51  d_IndDs_f
014900100817     i                                 56   92  Overlay
015000110218       //   �?Specifiche "C"?
015100100817     i                                 22   23  $C(1)
015200100817     i                                 83   84  $C(2)
015300100817     i                                 85   86  $C(3)
015400100817     i                                 87   88  $C(4)
015500100817     i                                 24   29  SK(1)
015600100817     i                                 48   53  SK(2)
015700100817     i                                 62   67  SK(3)
015800100817     i                                 48   91  Fatt2
015900110218       //   �?Specifiche "O"?
016000100817     i                                 36   37  $O(1)
016100100817     i                                 39   40  $O(2)
016200100817     i                                 42   43  $O(3)
016300110218
016400110218       //--------------------------------------------------------------
016500110218       //?M A I N - L I N E                                            ?
016600110218       //--------------------------------------------------------------
016700100817
016800100817     c     *Entry        plist
016900100817     c                   parm                    �Lib             10
017000100817     c                   parm                    �SrcF            10
017100100817     c                   parm                    �Mbr             10
017200100817     c                   parm                    �Type            10
017300100817     c                   parm                    �IndDs            4
017400100817      /free
017500100817
017600100817       *inLR = *on;
017700100817
017800110218       // -?Ciclo di lettura specifiche nel membro sorgente?
017900100817       DoU  %eof(Source);
018000100817
018100100817         read  Source;
018200100817
018300100817         if  %eof(SOURCE)   or   EoF = '**';
018400100817           leave;
018500100817         endif;
018600100817
018700110218         // -?Record "particolari"?
018800100817         if  Spec = '/'   or
018900100817             Char = '/'   or
019000100817             Char = '+'   or
019100100817             Char = '*';
019200170120           //*�Free = %xlate( c_Minusc : c_Maiusc : Free );
019300170120           //*�select;
019400170120           //*�  when  Free = '/FREE     ';
019500170120           //*�    $Free = *on;
019600170120           //*�  when  Free = '/END-FREE ';
019700170120           //*�    $Free = *off;
019800170120           //*�endsl;
019900100817           iter;
020000100817         endif;
020100170120
020200170120         // -?"/Free" pare NON essere pi� richiesto per iniziare a?
020300170120         //  ?scrivere specifiche di calcolo in free-format...?
020400170120         $Free = ( Spec <> 'H'  and
020500170120                   Spec <> 'h'  and
020600170120                   Spec <> 'F'  and
020700170120                   Spec <> 'f'  and
020800170120                   Spec <> 'D'  and
020900170120                   Spec <> 'd'  and
021000170120                   Spec <> 'I'  and
021100170120                   Spec <> 'i'  and
021200170120                   Spec <> 'C'  and
021300170120                   Spec <> 'c'  and
021400170120                   Spec <> 'O'  and
021500170120                   Spec <> 'o'  and
021600170120                   Char <> '*' );
021700100817
021800100817         select;
021900110218           // -?Specifica in free-format?
022000100817           when  $Free = *on;
022100100817             exsr  sr_Free;
022200110218           // -?Specifica "F"?
022300100817           when  (Spec = 'F'   or   Spec = 'f')   and
022400100817                 wIndDs = *blank                  and
022500100817                 �IndDs = '*YES';
022600100817             exsr  sr_F;
022700110218           // -?Specifica "D"?
022800100817           when  (Spec = 'D'   or   Spec = 'd')   and
022900100817                 wIndDs <> *blank;
023000100817             exsr  sr_D;
023100110218           // -?Specifica "I"?
023200100817           when   Spec = 'I'   or   Spec = 'i';
023300100817             exsr  sr_I;
023400110218           // -?Specifica "C"?
023500100817           when   Spec = 'C'   or   Spec = 'c';
023600100817             exsr  sr_C;
023700110218           // -?Specifica "O"?
023800110218           when   Spec = 'O'   or   Spec = 'o';
023900100817             exsr  sr_O;
024000100817         endsl;
024100100817
024200100817       EndDo;
024300100817
024400110218       // -?Emissione window?
024500100817       W1typ = �Type + ':';
024600100817       W1pgm = �Mbr;
024700100817       W1lib = �Lib;
024800100817       W1fil = �SrcF;
024900100817       *in   = SavInd;
025000110218       if  $All;
025100110218         DAxAll = c_DspAtr_HI_RI;
025200110218       else;
025300110218         DAxAll = c_DspAtr_ND;
025400110218       endif;
025500100817       exfmt  OnOffW1;
025600100817
025700100817       //--------------------------------------------------------------
025800100817       //?TEST X INDICATORI SPECFICHE DI DEFINIZIONE FILE ("F")        ?
025900100817       //--------------------------------------------------------------
026000100817       BEGSR  sr_F;
026100100817
026200100817         IndDs = %xlate ( c_Minusc : c_Maiusc : IndDs );
026300100817
026400100817         if  IndDs = 'INDDS';
026500100817           xx = %scan( '(' : SpecF );
026600100817           yy = %scan( ')' : SpecF );
026700100817           if  xx = *zero   or   yy = *zero;
026800100817             leavesr;
026900100817           endif;
027000100817           xx += 1;
027100100817           yy -= 1;
027200100817           wIndDs = %trim( %subst( SpecF : xx : yy - xx + 1 ) );
027300100817           wIndDs = %xlate ( c_Minusc : c_Maiusc : wIndDs );
027400100817         endif;
027500100817
027600100817       ENDSR;
027700100817
027800100817       //--------------------------------------------------------------
027900100817       //?TEST X INDICATORI SPECFICHE "D"                              ?
028000100817       //--------------------------------------------------------------
028100100817       BEGSR  sr_D;
028200100817
028300151210         //*//d_ds_Name = %xlate ( c_Minusc : c_Maiusc : d_ds_Name );
028400151210         d_ds_Name_cmpl = %xlate ( c_Minusc : c_Maiusc : d_ds_Name_cmpl );
028500151210         d_ds_     = %xlate ( c_Minusc : c_Maiusc : d_ds_   );
028600100817         Overlay   = %xlate ( c_Minusc : c_Maiusc : Overlay );
028700151210
028800151210         if  Not $IndDs  and  Not $SeeNextRow;
028900151210           $SeeNextRow = ( %scan( '...' : d_ds_Name_cmpl ) > *zero );
029000151210         endif;
029100100817
029200110218         // -?Verifica se inizia o finisce la definizione "INDDS"?
029300100817         select;
029400151210           when  Not $IndDs  and  d_ds_Name_cmpl <> *blank
029500151210                             and  $SeeNextRow;
029600151210             $IndDs = ( %scan( %trim(wIndDs) :
029700151210                               %trimR(d_ds_Name_cmpl) ) > *zero );
029800151210           when  Not $IndDs  and  d_ds_Name <> *blank
029900151210                             and  d_ds_     =  ' DS ';
030000100817             $IndDs = ( d_ds_     =  ' DS '   and
030100100817                        d_ds_Name <> *blank   and
030200160413                        %scan( %trim(wIndDs) : %trimr(d_ds_Name_cmpl) )
030300160413                                  >  *zero );
030400151210           when  $IndDs      and  d_ds_     <> ' DS ';
030500151210           when  $IndDs      and  d_ds_     =  ' DS '
030600151210                             and  $SeeNextRow
030700151210                             and  Not $Almeno_1_ind;
030800151210           when  $IndDs      and  %scan( '...' : d_ds_Name_cmpl ) > *zero;
030900151210             $IndDs = *off;
031000151210           when  $IndDs      and  d_ds_     =  ' DS ';
031100100817             $IndDs = *off;
031200100817         endsl;
031300100817         if  Not $IndDs;
031400100817           leavesr;
031500100817         endif;
031600100817
031700110218         // -?Verifica se indicatori definiti via "OVERLAY" o?
031800110218         //  ?se definiti a posizioni fisse?
031900100817         clear  xx;
032000100817         xx = %scan( 'OVERLAY' : Overlay );
032100100817         if  xx > *zero    and
032200100817             %scan( %trim(wIndDs) : Overlay : xx + 1 ) > *zero;
032300100817           $Overlay = *on;
032400100817         Else;
032500100817           $Overlay = *off;
032600100817         endif;
032700100817
032800110218         // -?Reperimento indicatori utilizzati?
032900100817         clear  xx;
033000100817         clear  yy;
033100100817         select;
033200100817           when  $Overlay;
033300100817             xx = %scan( ':' : Overlay : xx + 1 );
033400100817             yy = %scan( ')' : Overlay : xx + 1 );
033500100817           when  d_IndDs_i = *blank   or
033600100817                 d_IndDs_f = *blank   or
033700100817                 %check ( c_Digits : %triml(d_IndDs_i) ) > *zero   or
033800100817                 %check ( c_Digits : %triml(d_IndDs_f) ) > *zero;
033900100817           other;
034000100817             xx = %int( %triml( d_IndDs_i ) );
034100100817             yy = %int( %triml( d_IndDs_f ) );
034200100817         endsl;
034300100817
034400100817         if  xx = *zero   or   yy = *zero;
034500100817           leavesr;
034600100817         endif;
034700100817
034800151209         If  $Overlay;
034900151209           xx += 1;
035000151209           yy -= 1;
035100151209           wIn = %int( %trim( %subst( Overlay : xx : yy - xx + 1 ) ) );
035200151209         Else;
035300151209           wIn = xx;
035400151209         EndIf;
035500100817
035600100817         if  wIN > *zero;
035700100817           SavInd(wIN) = *on;
035800100817         endif;
035900151210
036000151210         $Almeno_1_ind = *on;
036100100817
036200100817       ENDSR;
036300100817
036400100817       //--------------------------------------------------------------
036500100817       //?TEST X INDICATORI SPECFICHE DI INPUT                         ?
036600100817       //--------------------------------------------------------------
036700100817       BEGSR  sr_I;
036800100817
036900100817         For  xx = 1  To  5;
037000100817
037100100817           if  %check( c_Digits : $I(xx) ) = *zero;
037200100817             wIN = %int( $I(xx) );
037300100817             if  wIN > *zero;
037400100817               SavInd(wIN) = *on;
037500100817             endif;
037600100817           endif;
037700100817
037800100817         EndFor;
037900100817
038000100817       ENDSR;
038100100817
038200100817       //--------------------------------------------------------------
038300100817       //?TEST X INDICATORI SPECFICHE DI CALCOLO                       ?
038400100817       //--------------------------------------------------------------
038500100817       BEGSR  sr_C;
038600100817
038700100817         if  CodOpe <> *blank;
038800100817           Free = %xlate( c_Minusc : c_Maiusc : CodOpe );
038900100817           exsr  sr_C1;
039000100817         endif;
039100100817
039200151209         if  Free <> 'IF   '   and
039300151209             Free <> 'DOU  '   and
039400151209             Free <> 'DOW  '   and
039500151209             Free <> 'WHEN '   and
039600151209             Free <> 'EVAL '   and
039700151209             Free <> *blank;
039800100817           exsr  sr_C2;
039900100817         else;
040000100817           exsr  sr_C3;
040100100817         endif;
040200100817
040300100817       ENDSR;
040400100817
040500100817       //--------------------------------------------------------------
040600100817       //?Specifiche "C": 'nn' a sinistra (1) e a destra (3)           ?
040700100817       //--------------------------------------------------------------
040800100817       BEGSR  sr_C1;
040900100817
041000100817         For  xx = 1  To  4;
041100100817
041200100817           if  %check( c_Digits : $C(xx) ) = *zero;
041300100817             wIN = %int( $C(xx) );
041400100817             if  wIN > *zero;
041500100817               SavInd(wIN) = *on;
041600100817             endif;
041700100817           endif;
041800100817
041900151209           if  Free = 'IF   '   or
042000151209               Free = 'DOU  '   or
042100151209               Free = 'DOW  '   or
042200151209               Free = 'WHEN '   or
042300151209               Free = 'EVAL ';
042400100817             leave;
042500100817           endif;
042600100817
042700100817         EndFor;
042800100817
042900100817       ENDSR;
043000100817
043100100817       //--------------------------------------------------------------
043200100817       //?Specifiche "C": '*INnn' in fatt.1/2 o risultato              ?
043300100817       //--------------------------------------------------------------
043400100817       BEGSR  sr_C2;
043500100817
043600100817         For  xx = 1  To  3;
043700100817
043800100817           if  %subst(SK(xx):1:3) = '*IN'   or
043900100817               %subst(SK(xx):1:3) = '*in'   or
044000100817               %subst(SK(xx):1:3) = '*In'   or
044100100817               %subst(SK(xx):1:3) = '*iN';
044200100817
044300110218             select;
044400110218
044500110218               when  %subst( SK(xx) : 4 : 1 ) = *blank;
044600110218                 $All = *on;
044700110218                 iter;
044800100817
044900110218               when  %subst( SK(xx) : 4 : 1 ) = '(';
045000110218                 dsIN = %subst( SK(xx) : 5 : 2 );
045100110218
045200110218               other;
045300110218                 dsIN = %subst( SK(xx) : 4 : 2 );
045400110218
045500110218             endsl;
045600100817
045700100817             if  dsIN2 = *blank;
045800100817               dsIN2 = dsIN1;
045900100817               dsIN1 = *zero;
046000100817             endif;
046100100817
046200100817             if  %check( c_Digits : dsIN ) = *zero;
046300100817               wIN = %int( dsIN );
046400100817               if  wIN > *zero;
046500100817                 SavInd(wIN) = *on;
046600100817                 leavesr;
046700100817               endif;
046800100817             endif;
046900100817
047000100817           endif;
047100100817
047200100817         EndFor;
047300100817
047400100817       ENDSR;
047500100817
047600100817       //--------------------------------------------------------------
047700100817       //?Specifiche "C": '*INxx' in fatt.2 esteso                     ?
047800100817       //--------------------------------------------------------------
047900100817       BEGSR  sr_C3;
048000100817
048100100817         clear  xx;
048200100817         clear  yy;
048300100817
048400100817         DoU  xx = *zero;
048500100817
048600100817           yy = xx + 1;
048700100817           xx = %scan('*in' : Fatt2 : yy);
048800100817           if  xx = *zero;
048900100817             xx = %scan('*IN' : Fatt2 : yy);
049000100817             if  xx = *zero;
049100100817               xx = %scan('*In' : Fatt2 : yy);
049200100817               if  xx = *zero;
049300100817                 xx = %scan('*iN' : Fatt2 : yy);
049400100817               endif;
049500100817             endif;
049600100817           endif;
049700100817
049800100817           if  xx = *zero;
049900100817             leave;
050000100817           endif;
050100100817
050200110218           select;
050300110218
050400151210             when  %subst( Fatt2 : XX+3 : 1 ) = *blank;
050500110218               $All = *on;
050600110218               leave;
050700110218
050800151210             when  %subst( Fatt2 : XX+3 : 1 ) = '(';
050900110218               dsIN = %subst( Fatt2 : XX+4 : 2);
051000110218
051100110218             other;
051200110218               dsIN = %subst( Fatt2 : XX+3 : 2 );
051300110218
051400110218           endsl;
051500100817
051600100817           if  dsIN2 = *blank;
051700100817             dsIN2 = dsIN1;
051800100817             dsIN1 = *zero;
051900100817           endif;
052000100817
052100100817           if  %check( c_Digits : dsIN ) = *zero;
052200100817             wIN = %int( dsIN );
052300100817             if  wIN > *zero;
052400100817               SavInd(wIN) = *on;
052500100817               leavesr;
052600100817             endif;
052700100817           endif;
052800100817
052900100817         EndDo;
053000100817
053100100817       ENDSR;
053200100817
053300100817       //--------------------------------------------------------------
053400100817       //?TEST X INDICATORI SPECFICHE "FREE"                           ?
053500100817       //--------------------------------------------------------------
053600100817       BEGSR  sr_Free;
053700110217
053800110217         If  $Free;
053900110217           // -?"Exec Sql"?
054000110217           xx = %scan( 'EXEC ' : FreeSpec );
054100110217           if  xx > *zero   and
054200110217               %scan( ' SQL ' : FreeSpec : xx ) > *zero;
054300110217             leavesr;
054400110217           endif;
054500110217           // -?Commento ("//")?
054600110217           xx = %scan( '//' : FreeSpec );
054700110217           if  xx > *zero;
054800110217             %subst( FreeSpec : xx ) = *blank;
054900110217           endif;
055000110217           if  xx = 1   or
055100110217              (xx > 1   and   %subst( FreeSpec : 1 : xx ) = *blank);
055200110217             leavesr;
055300110217           endif;
055400110217         EndIf;
055500100817
055600100817         clear  xx;
055700100817         clear  yy;
055800100817
055900100817         DoU  xx = *zero;
056000100817
056100100817           yy = xx + 1;
056200110218           xx = %scan('*in' : FreeSpec : yy);
056300100817           if  xx = *zero;
056400110218             xx = %scan('*IN' : FreeSpec : yy);
056500100817             if  xx = *zero;
056600110218               xx = %scan('*In' : FreeSpec : yy);
056700100817               if  xx = *zero;
056800110218                 xx = %scan('*iN' : FreeSpec : yy);
056900100817               endif;
057000100817             endif;
057100100817           endif;
057200100817
057300100817           if  xx = *zero;
057400100817             leave;
057500100817           endif;
057600100817
057700100817           select;
057800100817             when  %subst( FreeSpec : xx+3 : 1 ) = *blank;
057900110218               $All = *on;
058000110218               leave;
058100100817             when  %subst( FreeSpec : xx+3 : 1 ) = '(';
058200100817               dsIN = %subst( FreeSpec : xx+4 : 2 );
058300100817             other;
058400100817               dsIN = %subst( FreeSpec : xx+3 : 2 );
058500100817           endsl;
058600100817
058700100817           if  dsIN2 = *blank;
058800100817             dsIN2 = dsIN1;
058900100817             dsIN1 = *zero;
059000100817           endif;
059100100817
059200100817           if  %check( c_Digits : dsIN ) = *zero
059300100817               and  dsIN > *zero;
059400100817             wIN = %int( dsIN );
059500100817             if  wIN > *zero;
059600100817               SavInd(wIN) = *on;
059700100817             endif;
059800100817           endif;
059900100817
060000100817         EndDo;
060100100817
060200100817       ENDSR;
060300100817
060400100817       //--------------------------------------------------------------
060500100817       //?TEST X INDICATORI SPECFICHE DI OUTPUT                        ?
060600100817       //--------------------------------------------------------------
060700100817       BEGSR  sr_O;
060800100817
060900100817         For  xx = 1  To  3;
061000100817
061100100817           if  $O(xx) <> '1P';
061200100817
061300100817             if  %check( c_Digits : $O(xx) ) = *zero;
061400100817               wIN = %int( $O(xx) );
061500100817               if  wIN > *zero;
061600100817                 SavInd(wIN) = *on;
061700100817               endif;
061800100817             endif;
061900100817           endif;
062000100817
062100100817         EndFor;
062200100817
062300100817       ENDSR;
062400100817
062500100817      /end-free
