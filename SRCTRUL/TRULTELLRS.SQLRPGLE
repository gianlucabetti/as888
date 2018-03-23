000100110715      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*YMD.)
000200110715     H decedit('0,') datedit(*ymd/) option(*nodebugio)
000300080206
000400080206      //---------------------------------------------------------------
000500080206      //?Dichiarazione file.
000600080206      //---------------------------------------------------------------
000700130618     FPRTF198   O    F  220        PRINTER USROPN
000800080206      //---------------------------------------------------------------
000900080206      //?Definizione schiere.
001000080206      //---------------------------------------------------------------
001100080206      //---------------------------------------------------------------
001200080206      //?Definizione aree dati.
001300080206      //---------------------------------------------------------------
001400080206      //---------------------------------------------------------------
001500080206      //?Definizione strutture dati.
001600080206      //---------------------------------------------------------------
001700110715      //---------------------------------------------------------------
001800080206
001900080206     d KPJBA         e ds
002000130618     d wTitolo         s            150    inz
002100110715     d stringaSQL      s           4096    inz varying
002200110715     d wRigaPrt        s            200    inz
002300110715     d idxX            s              2s 0 inz
002400110715     d idxY            s              2s 0 inz
002500080206
002600110715     d wftlldsold    e ds                  extname(wftell0f) prefix(o_) inz
002700110715     d wftlldsnew    e ds                  extname(wftell0f) prefix(n_) inz
002800100316
002900110715     d OldResults      ds                  inz  occurs(10) qualified
003000110715     d   NewResults                  10i 0 inz  dim(10)
003100100316
003200101111
003300080206      //---------------------------------------------------------------
003400080206      //?Definizione procedure usate.
003500080206      //---------------------------------------------------------------
003600080206      //---------------------------------------------------------------
003700080206      //?Riepilogo indicatori.
003800080206      //---------------------------------------------------------------
003900080206      //---------------------------------------------------------------
004000080206
004100080206      //---------------------------------------------------------------
004200080206      //?M A I N - L I N E
004300080206      //---------------------------------------------------------------
004400080206
004500110715     c*
004600110715     c* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004700110715     c
004800110715     c/EXEC SQL
004900110715     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005000110715     c/END-EXEC
005100110715     c
005200110715     c*
005300080627     c     *Entry        plist
005400110718     c                   parm                    In_DataOraDa     14
005500110718     c                   parm                    In_DataOraAa     14
005600110718     c*
005700080702
005800080206      /free
005900080206
006000081215
006100110715       // Prepara SQL per abbinamento risultati vecchi/nuovi
006200130618       // N=tutto con Normalizzazioni differenti
006300130618       // G=tutto con Giri differenti
006400130618       // C=solo con Giro (nuovo) e Normalizzazoni differenti
006500130618       // T=tutto con Giro (vecchio e nuovo)
006600130618       // W=solo con NO Giro vecchio e SI Giro nuovo
006700130618       // O=solo con SI Giro vecchio e NO Giro nuovo
006800130618
006900130618
007000130618       wTitolo = '***  N=tutto con Normalizzazioni differenti  ***';
007100130618       exsr prpSQL_N;
007200130618       exsr exeSQL;
007300130618       exsr exeSTAMPA;
007400130618
007500130618
007600130618       wTitolo = '***  G=tutto con Giri differenti  ***';
007700130618       exsr prpSQL_G;
007800130618       exsr exeSQL;
007900130618       exsr exeSTAMPA;
008000130618
008100130618
008200130618       wTitolo = '***  C=solo con Giro (nuovo) e Normalizzazoni ' +
008300130618                 'differenti  ***';
008400130618       exsr prpSQL_C;
008500130618       exsr exeSQL;
008600130618       exsr exeSTAMPA;
008700130618
008800130618
008900130618       wTitolo = '***  T=tutto con Giro (vecchio e nuovo)  *** ';
009000130618       exsr prpSQL_T;
009100130618       exsr exeSQL;
009200130618       exsr exeSTAMPA;
009300110715
009400130618
009500130618       wTitolo = '***  W=solo con NO Giro vecchio e SI Giro nuovo ***';
009600130618       exsr prpSQL_W;
009700130618       exsr exeSQL;
009800130618       exsr exeSTAMPA;
009900130618
010000130618
010100130618       wTitolo = '***  O=solo con SI Giro vecchio e NO Giro nuovo  ***';
010200130618       exsr prpSQL_O;
010300130618       exsr exeSQL;
010400130618       exsr exeSTAMPA;
010500110715
010600130618
010700110715       *inlr = *on;
010800080206
010900110715
011000100901       //--------------------------------------------------------------
011100110715       //?Prepara SQL per abbinamento risultati vecchi/nuovi
011200100901       //--------------------------------------------------------------
011300130618       BEGSR prpSQL_N;
011400100901
011500110715        stringaSQL =
011600110715         'with '                                                              +
011700110715         'WOLD as ( '                                                         +
011800110715         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
011900110715         'wftell0f WHERE WFTETIP=''OLD'' and WFTEDAT between '                +
012000110718         In_DataOraDa + ' and ' + In_DataOraAa + ' ), '                       +
012100110715         'WNEW as ( '                                                         +
012200110715         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
012300110715         'wftell0f WHERE WFTETIP=''NEW'' and WFTEDAT between '                +
012400110718         In_DataOraDa + ' and ' + In_DataOraAa + ' ) '                        +
012500110715         'select '                                                            +
012600110715         'WOLD.WFTESPE,  WNEW.WFTESPE, '                                      +
012700110715         'WOLD.WFTECGI,  WNEW.WFTECGI, '                                      +
012800110715         'WOLD.WFTELNO,  WNEW.WFTELNO, '                                      +
012900110715         'WOLD.WFTELNON, WNEW.WFTELNON '                                      +
013000110715         'from WOLD left join WNEW '                                          +
013100110715         'on WOLD.WFTESPE = WNEW.WFTESPE '                                    +
013200130618         'where WOLD.WFTELNON <> WNEW.WFTELNON ';
013300100901
013400110715         ENDSR;
013500130618
013600130618
013700130618       //--------------------------------------------------------------
013800130618       //?Prepara SQL per abbinamento risultati vecchi/nuovi
013900130618       //--------------------------------------------------------------
014000130618       BEGSR prpSQL_G;
014100130618
014200130618        stringaSQL =
014300130618         'with '                                                              +
014400130618         'WOLD as ( '                                                         +
014500130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
014600130618         'wftell0f WHERE WFTETIP=''OLD'' and WFTEDAT between '                +
014700130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ), '                       +
014800130618         'WNEW as ( '                                                         +
014900130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
015000130618         'wftell0f WHERE WFTETIP=''NEW'' and WFTEDAT between '                +
015100130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ) '                        +
015200130618         'select '                                                            +
015300130618         'WOLD.WFTESPE,  WNEW.WFTESPE, '                                      +
015400130618         'WOLD.WFTECGI,  WNEW.WFTECGI, '                                      +
015500130618         'WOLD.WFTELNO,  WNEW.WFTELNO, '                                      +
015600130618         'WOLD.WFTELNON, WNEW.WFTELNON '                                      +
015700130618         'from WOLD left join WNEW '                                          +
015800130618         'on WOLD.WFTESPE = WNEW.WFTESPE '                                    +
015900130618         'where WOLD.WFTECGI  <> WNEW.WFTECGI  ';
016000130618
016100130618         ENDSR;
016200130618
016300130618
016400130618       //--------------------------------------------------------------
016500130618       //?Prepara SQL per abbinamento risultati vecchi/nuovi
016600130618       //--------------------------------------------------------------
016700130618       BEGSR prpSQL_C;
016800130618
016900130618        stringaSQL =
017000130618         'with '                                                              +
017100130618         'WOLD as ( '                                                         +
017200130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
017300130618         'wftell0f WHERE WFTETIP=''OLD'' and WFTEDAT between '                +
017400130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ), '                       +
017500130618         'WNEW as ( '                                                         +
017600130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
017700130618         'wftell0f WHERE WFTETIP=''NEW'' and WFTEDAT between '                +
017800130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ) '                        +
017900130618         'select '                                                            +
018000130618         'WOLD.WFTESPE,  WNEW.WFTESPE, '                                      +
018100130618         'WOLD.WFTECGI,  WNEW.WFTECGI, '                                      +
018200130618         'WOLD.WFTELNO,  WNEW.WFTELNO, '                                      +
018300130618         'WOLD.WFTELNON, WNEW.WFTELNON '                                      +
018400130618         'from WOLD left join WNEW '                                          +
018500130618         'on WOLD.WFTESPE = WNEW.WFTESPE '                                    +
018600130618         'where WNEW.WFTECGI <> '' '' and '                                   +
018700130618         '      WOLD.WFTELNON <> WNEW.WFTELNON ';
018800130618
018900130618         ENDSR;
019000130618
019100130618
019200130618       //--------------------------------------------------------------
019300130618       //?Prepara SQL per abbinamento risultati vecchi/nuovi
019400130618       //--------------------------------------------------------------
019500130618       BEGSR prpSQL_T;
019600130618
019700130618        stringaSQL =
019800130618         'with '                                                              +
019900130618         'WOLD as ( '                                                         +
020000130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
020100130618         'wftell0f WHERE WFTETIP=''OLD'' and WFTEDAT between '                +
020200130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ), '                       +
020300130618         'WNEW as ( '                                                         +
020400130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
020500130618         'wftell0f WHERE WFTETIP=''NEW'' and WFTEDAT between '                +
020600130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ) '                        +
020700130618         'select '                                                            +
020800130618         'WOLD.WFTESPE,  WNEW.WFTESPE, '                                      +
020900130618         'WOLD.WFTECGI,  WNEW.WFTECGI, '                                      +
021000130618         'WOLD.WFTELNO,  WNEW.WFTELNO, '                                      +
021100130618         'WOLD.WFTELNON, WNEW.WFTELNON '                                      +
021200130618         'from WOLD left join WNEW '                                          +
021300130618         'on WOLD.WFTESPE = WNEW.WFTESPE '                                    +
021400130618         'where WOLD.WFTECGI <> '' '' and '                                   +
021500130618         '      WNEW.WFTECGI <> '' '' ';
021600130618
021700130618         ENDSR;
021800110715
021900130618
022000130618       //--------------------------------------------------------------
022100130618       //?Prepara SQL per abbinamento risultati vecchi/nuovi
022200130618       //--------------------------------------------------------------
022300130618       BEGSR prpSQL_W;
022400130618
022500130618        stringaSQL =
022600130618         'with '                                                              +
022700130618         'WOLD as ( '                                                         +
022800130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
022900130618         'wftell0f WHERE WFTETIP=''OLD'' and WFTEDAT between '                +
023000130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ), '                       +
023100130618         'WNEW as ( '                                                         +
023200130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
023300130618         'wftell0f WHERE WFTETIP=''NEW'' and WFTEDAT between '                +
023400130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ) '                        +
023500130618         'select '                                                            +
023600130618         'WOLD.WFTESPE,  WNEW.WFTESPE, '                                      +
023700130618         'WOLD.WFTECGI,  WNEW.WFTECGI, '                                      +
023800130618         'WOLD.WFTELNO,  WNEW.WFTELNO, '                                      +
023900130618         'WOLD.WFTELNON, WNEW.WFTELNON '                                      +
024000130618         'from WOLD left join WNEW '                                          +
024100130618         'on WOLD.WFTESPE = WNEW.WFTESPE '                                    +
024200130618         'where WOLD.WFTECGI  = '' '' and '                                   +
024300130618         '      WNEW.WFTECGI <> '' '' ';
024400130618
024500130618         ENDSR;
024600110715
024700130618
024800130618       //--------------------------------------------------------------
024900130618       //?Prepara SQL per abbinamento risultati vecchi/nuovi
025000130618       //--------------------------------------------------------------
025100130618       BEGSR prpSQL_O;
025200130618
025300130618        stringaSQL =
025400130618         'with '                                                              +
025500130618         'WOLD as ( '                                                         +
025600130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
025700130618         'wftell0f WHERE WFTETIP=''OLD'' and WFTEDAT between '                +
025800130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ), '                       +
025900130618         'WNEW as ( '                                                         +
026000130618         'SELECT WFTESPE, WFTECGI, WFTELNO, WFTELNON, WFTETIP, WFTEDAT FROM ' +
026100130618         'wftell0f WHERE WFTETIP=''NEW'' and WFTEDAT between '                +
026200130618         In_DataOraDa + ' and ' + In_DataOraAa + ' ) '                        +
026300130618         'select '                                                            +
026400130618         'WOLD.WFTESPE,  WNEW.WFTESPE, '                                      +
026500130618         'WOLD.WFTECGI,  WNEW.WFTECGI, '                                      +
026600130618         'WOLD.WFTELNO,  WNEW.WFTELNO, '                                      +
026700130618         'WOLD.WFTELNON, WNEW.WFTELNON '                                      +
026800130618         'from WOLD left join WNEW '                                          +
026900130618         'on WOLD.WFTESPE = WNEW.WFTESPE '                                    +
027000130618         'where WOLD.WFTECGI <> '' '' and '                                   +
027100130618         '      WNEW.WFTECGI  = '' '' ';
027200130618
027300130618         ENDSR;
027400130618
027500130618
027600100902       //--------------------------------------------------------------
027700110715       //?Lettura record selezionati
027800100902       //--------------------------------------------------------------
027900110715       BEGSR exeSQL;
028000130618
028100130618         exec sql PREPARE stmt01 FROM :stringaSQL;
028200130618
028300130618         exec sql DECLARE crsr01 cursor FOR stmt01;
028400130618
028500130618         exec sql OPEN crsr01;
028600100902
028700110715         clear sqlcod;
028800110715         dow sqlcod=0;
028900100902
029000110715         exec sql FETCH NEXT FROM crsr01 INTO :o_WFTESPE  , :n_WFTESPE  ,
029100110715                                              :o_WFTECGI  , :n_WFTECGI  ,
029200110715                                              :o_WFTELNO  , :n_WFTELNO  ,
029300110715                                              :o_WFTELNON , :n_WFTELNON ;
029400110715
029500110715         select;
029600110715         when sqlcod=100;
029700100902
029800110715         when sqlcod<0;
029900100902
030000110715         other;
030100110715
030200110715           %occur(OldResults) = o_WFTELNON;
030300110715           OldResults.NewResults(n_WFTELNON) =
030400110715           OldResults.NewResults(n_WFTELNON) + 1;
030500110715
030600110715         endsl;
030700100902
030800110715         enddo;
030900101111
031000110715       exec sql CLOSE crsr01;
031100100902
031200110715       ENDSR;
031300110715
031400110715
031500110715       //--------------------------------------------------------------
031600110715       //?Stampa esiti confrontio
031700110715       //--------------------------------------------------------------
031800110715       BEGSR exeSTAMPA;
031900130618
032000130618         open PRTF198;
032100110715
032200130618         wRigaPrt = wTitolo;
032300130618         except staRiga;
032400110715
032500110715         wRigaPrt = '|     *NEW ' +
032600110715                    '|   ERR    ' +
032700110715                    '|    P     ' +
032800110715                    '|          ' +
032900110715                    '|          ' +
033000110715                    '|    CA    ' +
033100110715                    '|    LA    ' +
033200110715                    '|    C     ' +
033300110715                    '|    L     ' +
033400110715                    '|    VA    ' +
033500110715                    '|    V     |';
033600110715         except staRiga;
033700110715         wRigaPrt = '|*OLD      |';
033800110715         except staRiga;
033900110715
034000110715
034100110715         idxY = 1;
034200110715         dow idxY <= %elem(OldResults);
034300110715
034400110715           %occur(OldResults) = idxY;
034500110715
034600110715           clear wRigaPrt;
034700110715
034800110715           select;
034900110715               when idxY =  1;
035000110715                    wRigaPrt = '| ERR      |';
035100110715               when idxY =  2;
035200110715                    wRigaPrt = '|  P       |';
035300110715               when idxY =  3;
035400110715                    wRigaPrt = '|          |';
035500110715               when idxY =  4;
035600110715                    wRigaPrt = '|          |';
035700110715               when idxY =  5;
035800110715                    wRigaPrt = '|  CA      |';
035900110715               when idxY =  6;
036000110715                    wRigaPrt = '|  LA      |';
036100110715               when idxY =  7;
036200110715                    wRigaPrt = '|  C       |';
036300110715               when idxY =  8;
036400110715                    wRigaPrt = '|  L       |';
036500110715               when idxY =  9;
036600110715                    wRigaPrt = '|  VA      |';
036700110715               when idxY = 10;
036800110715                    wRigaPrt = '|  V       |';
036900110715           endsl;
037000110715
037100110715           idxX = 1;
037200110715           dow idxX <= %elem(OldResults.NewResults);
037300110715             wRigaPrt = %trimr(wRigaPrt) +
037400110715                         %editc(OldResults.NewResults(idxX):'3') + '|';
037500110715             idxX = idxX + 1;
037600110715           enddo;
037700110715
037800110715           except staRiga;
037900110715           idxY = idxY + 1;
038000110715         enddo;
038100110715
038200130618         close PRTF198;
038300110715
038400110715       ENDSR;
038500110715
038600110715
038700110715
038800110715      /end-free
038900110715
039000110715     C*------------------------------------------------------------------------*
039100110715     OPRTF198   E            staRiga     1
039200110715     O                       wRigaPrt            +1
