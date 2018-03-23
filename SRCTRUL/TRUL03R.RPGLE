000100131107       //==============================================================
000200131107       //?Controllo Orari di Apertura (max 4)                          ?
000300131107       //==============================================================
000400131107
000500131107       //--------------------------------------------------------------
000600131107       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700131107       //--------------------------------------------------------------
000800131107
000900131107     /*PRM  dbgview(*source)
001000131107     /*END
001100131107
001200131107       //--------------------------------------------------------------
001300131107       //?Specifiche di controllo.                                     ?
001400131107       //--------------------------------------------------------------
001500131107
001600131107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700131107     h dftactgrp(*no)
001800131107
001900131107       //--------------------------------------------------------------
002000131107       //?Dichiarazione file.                                          ?
002100131107       //--------------------------------------------------------------
002200131107
002300131107
002400131107       //--------------------------------------------------------------
002500131107       //?Definizione costanti.                                        ?
002600131107       //--------------------------------------------------------------
002700131107
002800131107       // -?Costante per controllo "caratteri solo numerici"?
002900131107     d c_Digits        c                   const('0123456789')
003000131108
003100131108       // -?Numero di caratteri richiesti (liberi) nella Nota-2?
003200131108       //  ?per gl orari?
003300131108     d c_NrBlanks      c                   const(23)
003400131107
003500131107       //--------------------------------------------------------------
003600131107       //?Definizione schiere.                                         ?
003700131107       //--------------------------------------------------------------
003800131107
003900131107       // -?Messaggi di errore?
004000131112     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
004100131107
004200131107       //--------------------------------------------------------------
004300131107       //?Definizione aree dati.                                       ?
004400131107       //--------------------------------------------------------------
004500131107
004600131107
004700131107       //--------------------------------------------------------------
004800131107       //?Definizione strutture dati.                                  ?
004900131107       //--------------------------------------------------------------
005000131107
005100131107       // -?Parametri ricevuti?
005200131107     d*// KPJBA         e ds
005300131107     d TRUL03ds      e ds                  inz
005400131108     d   sk_HM                 1     16s 0 inz  dim(4)
005500131107
005600131107       // -?Definizione orario (hh:mm)?
005700131107     d HHMM_ds         ds             4    inz  qualified
005800131107     d   hh                           2s 0 inz
005900131107     d   mm                           2s 0 inz
006000131107
006100131107       //--------------------------------------------------------------
006200131107       //?Definizione variabili globali.                               ?
006300131107       //--------------------------------------------------------------
006400131107
006500131107       // -?Parametri di input?
006600131107     d p_TRUL03ds      s                   like(TRUL03ds)
006700131107
006800131107       // -?Flags booleani?
006900131107     d $Fine           s               n   inz
007000131107
007100131107       // -?Indici di schiera / Contatori?
007200131107     d xx              s              3  0 inz
007300131107
007400131107       // -?Campi di comodo?
007500131107     d wTime_1         s               t   timfmt(*hms)   inz(*loval)
007600131107     d wTime_2         s               t   timfmt(*hms)   inz(*loval)
007700131107
007800131107       //--------------------------------------------------------------
007900131107       //?Definizione prototipi procedure.                             ?
008000131107       //--------------------------------------------------------------
008100131107
008200131107
008300131107       //--------------------------------------------------------------
008400131107       //?Definizione key-list.                                        ?
008500131107       //--------------------------------------------------------------
008600131107
008700131107
008800131107       //--------------------------------------------------------------
008900131107       //?M A I N - L I N E                                            ?
009000131107       //--------------------------------------------------------------
009100131107
009200131107     c     *Entry        plist
009300131107     c*//                parm                    KPJBA
009400131107     c                   parm                    p_TRUL03ds
009500131107
009600131107      /free
009700131107
009800131107       // -?Operazioni iniziali?
009900131107       exsr  sr_RoutInz;
010000131107
010100131107       // -?Controllo orari?
010200131107       if  i03hm1 + i03hm2 + i03hm3 + i03hm4 > *zero;
010300131107         exsr  sr_CtrlOrari;
010400131107       endif;
010500131107
010600131107       // -?Operazioni finali?
010700131107       exsr  sr_RoutEnd;
010800131107
010900131107       //--------------------------------------------------------------
011000131107       //?Operazioni iniziali.                                         ?
011100131107       //--------------------------------------------------------------
011200131107       BEGSR  sr_RoutInz;
011300131107
011400131107         *inLR = *on;
011500131107
011600131107         // -?Ricezione parametri?
011700131107         //if  KPJBU <> *blank;
011800131107         //  TRUL03ds = KPJBU;
011900131107         //endif;
012000131107         if  %parms() > *zero;
012100131107           TRUL03ds = p_TRUL03ds;
012200131107         endif;
012300131107
012400131107         // -?Impostazione iniziale parametri di output?
012500131107         o03err = *off;
012600131112         clear  o03errPos;
012700131107         clear  o03msg;
012800131107
012900131107         // -?Controllo numericità degli orari?
013000131107         For  xx = 1  To 4;
013100131107           if  %check( c_Digits :
013200131108                       %subst( p_TRUL03ds :
013300131108                               ( (xx-1) * %len( i03hm1 ) ) + 1 :
013400131108                               %len( i03hm1 ) ) ) > *zero;
013500131111             o03errpos = xx;
013600131107             o03err = *on;
013700131107             o03msg = sk_Msg(01);
013800131107             o03msg = %replace( %subst( %editc( xx : 'X' )
013900131107                                        : %len(xx) : 1 )
014000131107                      : o03msg : %scan( '_' : o03msg ) );
014100131107             exsr  sr_RoutEnd;
014200131107           endif;
014300131107         EndFor;
014400131107
014500131107       ENDSR;
014600131107
014700131107       //--------------------------------------------------------------
014800131107       //?Controllo orari (e spazio per le note)                       ?
014900131107       //--------------------------------------------------------------
015000131107       BEGSR  sr_CtrlOrari;
015100131107
015200131107
015300131107         // -?Verifica correttezza degli orari?
015400131107         For  xx = 1  To  4;
015500131107
015600131107           If  sk_HM(xx) > *zero;
015700131107
015800131107             HhMm_ds = %editc( sk_HM(xx) : 'X' );
015900131107
016000131108             Select;
016100131108
016200131108               // -?Orario NON ammesso (mezzanotte precisa)?
016300131108               When  sk_HM(xx)  < 0001;
016400131111                 o03errpos = xx;
016500131108                 o03err = *on;
016600131108                 o03msg = sk_Msg(02);
016700131108                 o03msg = %replace( %subst( %editc( xx : 'X' )
016800131108                                            : %len(xx) : 1 )
016900131108                          : o03msg : %scan( '_' : o03msg ) );
017000131108                 leavesr;
017100131108
017200131108               // -?Orario formalmente errato?
017300131108               When  HhMm_ds.hh < 00  or  HhMm_ds.hh > 23  or
017400131108                     HhMm_ds.mm < 00  or  HhMm_ds.mm > 59;
017500131111                 o03errpos = xx;
017600131108                 o03err = *on;
017700131108                 o03msg = sk_Msg(02);
017800131108                 o03msg = %replace( %subst( %editc( xx : 'X' )
017900131108                                            : %len(xx) : 1 )
018000131108                          : o03msg : %scan( '_' : o03msg ) );
018100131108                 leavesr;
018200131108
018300131108               // -?Orario NON successivo a quello precedente?
018400131108               When  xx > 1  and
018500131108                     sk_HM(xx) <= sk_HM(xx-1);
018600131111                 o03errpos = xx;
018700131108                 o03err = *on;
018800131108                 o03msg = sk_Msg(03);
018900131108                 o03msg = %replace( %subst( %editc( xx : 'X' )
019000131108                                            : %len(xx) : 1 )
019100131108                          : o03msg : %scan( '_' : o03msg ) );
019200131108                 leavesr;
019300131108
019400131108             EndSl;
019500131107
019600131107           EndIf;
019700131107
019800131107         EndFor;
019900131107
020000131107
020100131107
020200131107         // -?Verifica correttezza della sequenza di inserimento?
020300131107         Select;
020400131112
020500131112           // -?Inserito un solo orario "ALLE"?
020600131112           When  (sk_HM(01) = *zero  and  sk_HM(02) > *zero  and
020700131112                  sk_HM(03) = *zero  and  sk_HM(04) = *zero)  OR
020800131112                 (sk_HM(01) = *zero  and  sk_HM(02) = *zero  and
020900131112                  sk_HM(03) = *zero  and  sk_HM(04) > *zero);
021000131112           // -?Inserito un solo orario completo (per 1°)?
021100131112           When  sk_HM(01) > *zero  and  sk_HM(02) > *zero  and
021200131112                 sk_HM(03) = *zero  and  sk_HM(04) = *zero;
021300131112           // -?Inseriti due orari completi?
021400131112           When  sk_HM(01) > *zero  and  sk_HM(02) > *zero  and
021500131112                 sk_HM(03) > *zero  and  sk_HM(04) > *zero;
021600131112
021700131112
021800131107
021900131107           // -?Ammesso un solo orario "ALLE"?
022000131112           When  sk_HM(01) = *zero  and  sk_HM(02) > *zero  and
022100131112                 sk_HM(03) = *zero  and  sk_HM(04) > *zero;
022200131112             o03errpos = 4;
022300131107             o03err = *on;
022400131112             o03msg = sk_Msg(04);
022500131107             leavesr;
022600131107
022700131107           // -?Un'unica coppia di orari è inseribile SOLO per prima?
022800131112           When  sk_HM(01) = *zero  and  sk_HM(02) = *zero  and
022900131112                 sk_HM(03) > *zero  and  sk_HM(04) > *zero;
023000131112             o03errpos = 1;
023100131107             o03err = *on;
023200131112             o03msg = sk_Msg(05);
023300131107             leavesr;
023400131112           When  sk_HM(01) = *zero  and  sk_HM(02) = *zero  and
023500131112                 sk_HM(03) > *zero;
023600131112             o03errpos = 1;
023700131112             o03err = *on;
023800131112             o03msg = sk_Msg(05);
023900131112             leavesr;
024000131112
024100131112           // -?Orario "DALLE" obbligatorio SE inserito quello "ALLE"?
024200131112           //  ?nella 1ª coppia di orari (se inserita la 2ª)?
024300131112           When  sk_HM(01) = *zero  and  sk_HM(02) > *zero  and
024400131112                 sk_HM(03) > *zero  and  sk_HM(04) > *zero;
024500131112             o03errpos = 1;
024600131112             o03err = *on;
024700131112             o03msg = sk_Msg(06);
024800131112             leavesr;
024900131112           When  sk_HM(01) = *zero  and  sk_HM(02) > *zero  and
025000131112                 sk_HM(03) > *zero;
025100131112             o03errpos = 1;
025200131112             o03err = *on;
025300131112             o03msg = %trimr( sk_Msg(06) ) + ', o togliere il 3°';
025400131112             leavesr;
025500131112
025600131112           // -?Orario "DALLE" obbligatorio SE inserito quello "ALLE"?
025700131112           //  ?nella 2ª coppia di orari (se inserita la 1ª)?
025800131112           When  (sk_HM(01) > *zero  and  sk_HM(02) > *zero)  and
025900131112                 (sk_HM(03) = *zero  and  sk_HM(04) > *zero);
026000131112             o03errpos = 3;
026100131112             o03err = *on;
026200131112             o03msg = %trimr( sk_Msg(06) ) + ', o togliere il 4°';
026300131112             o03msg = %replace( '3' : o03msg :
026400131112                                %scan( '1' : o03msg ) );
026500131112             leavesr;
026600131112
026700131112           // -?Orario "ALLE" obbligatorio SE inserito quello "DALLE"?
026800131112           When  (sk_HM(01) > *zero  and  sk_HM(02) = *zero)  OR
026900131112                 (sk_HM(03) > *zero  and  sk_HM(04) = *zero);
027000131112             if  sk_HM(01) > *zero  and  sk_HM(02) = *zero;
027100131112               o03errpos = 2;
027200131112             else;
027300131112               o03errpos = 4;
027400131112             endif;
027500131112             o03err = *on;
027600131112             o03msg = sk_Msg(07);
027700131112             leavesr;
027800131112
027900131112           //Other;
028000131112           //  o03errpos = 1;
028100131112           //  o03err = *on;
028200131112           //  o03msg = 'Inserire correttamente gli orari';
028300131112           //  leavesr;
028400131107
028500131107         EndSl;
028600131107
028700131107
028800131107
028900131107         // -?Verifica NOTE inserite oltre agli orari (numero di caratteri)?
029000131111         //  ?- serviranno 23 caratteri per gli orari -?
029100131108         If  (i03nt1 <> *blank  or  i03nt2 <> *blank)  and
029200131108             %size( i03nt1 ) + %size( i03nt2 ) -
029300131108               %len( %trim( i03nt1 ) ) - %len( %trim( i03nt2 ) )
029400131108               < c_NrBlanks;
029500131111           o03errpos = 6;
029600131108           o03err = *on;
029700131112           o03msg = sk_Msg(08);
029800131108           o03msg = %replace( %subst( %trim( i03nt1 ) + %trim( i03nt2 ) +
029900131108                                      '                                   ' +
030000131108                                      '                                   ' :
030100131108                                      %size( i03nt1 ) + %size( i03nt2 )
030200131108                                                      - c_NrBlanks + 1 :
030300131108                                      c_NrBlanks )
030400131108                    : o03msg : %scan( '&Note' : o03msg ) );
030500131108           leavesr;
030600131107         EndIf;
030700131107
030800131107
030900131107       ENDSR;
031000131107
031100131107       //--------------------------------------------------------------
031200131107       //?Operazioni finali.                                           ?
031300131107       //--------------------------------------------------------------
031400131107       BEGSR  sr_RoutEnd;
031500131108
031600131112         // -?Ritorno parametri?
031700131108         p_TRUL03ds = TRUL03ds;
031800131107
031900131107         // -?Chiusura pgm?
032000131107         return;
032100131107
032200131107       ENDSR;
032300131107
032400131107      /end-free
032500131107
032600131107       //--------------------------------------------------------------
032700131107       //?Definizione schiere a tempo di compilazione                  ?
032800131107       //--------------------------------------------------------------
032900131107
033000131107** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
033100131107Rilevati caratteri NON numerici nel _° orario                                   1
033200131112_° orario formalmente errato                                                    2
033300131107_° orario NON successivo a quello precedente                                    3
033400131112Impostare un solo orario "ALLE"  o  anche i due orari "DALLE"                   4
033500131112Un'unica coppia di orari ("DALLE" - "ALLE") dev'essere impostata per prima      5
033600131112Inserire anche il 1° orario                                                     6
033700131112L'orario "DALLE" richiede un orario "ALLE"                                      7
033800131113Superato massimi caratteri consentiti per le NOTE: "&Note                  "    8
