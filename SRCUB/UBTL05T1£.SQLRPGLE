000100140912     FPRTF198   O    f  198        PRINTER
000200140912     F                                     oflind(*inoa)
000300140911       // -?Stringa SQL da eseguire?
000400140911     d wSql            s           4096    inz  varying
000500140911     D wTS1            s               z
000600140911     D wTS2            s               z
000700140911     D wTS3            s               z
000800140911     D wRcdLetti       s              9s 0
000900140911     D wCallOK         s              9s 0
001000140912     D wCallErr        s              9s 0
001100140912     D Msg             s            198
001200140911
001300140912     D pIn_TpStp       S             10A
001400140912
001500140912     D   pInNazione    S              3A
001600140911     D   pInProvincia  S              4A
001700140911     D   pInComune     S            128A
001800140911     D   pInLocalita   S            128A
001900140911     D   pInCAP        S              9A
002000140911     D   pInVia        S            128A
002100140911     D   pInCivico     S             10A
002200140911     D   pOutEsatto    S             10A
002300140911     D   pOutRank      S             10A
002400140911     D   pOutLivNorm   S             10A
002500140911     D   pOutScore     S             10A
002600140911     D   pOutRegione   S            128A
002700140911     D   pOutProv      S              4A
002800140911     D   pOutCAP       S              9A
002900140911     D   pOutComune    S            128A
003000140911     D   pOutLocalit   S            128A
003100140911     D   pOutVia       S            128A
003200140911     D   pOutCivico    S             10A
003300140911     D   pOutXY_X      S             32A
003400140911     D   pOutXY_Y      S             32A
003500140912     D   pOutEsito     S              1A
003600140911     D   pOutEsitoDes  S            128A
003700140912
003800140912     d data            s               d
003900140912     d ora             s               t
004000140912
004100140912     D* campi per stampa
004200140912     D LNA             S              3A
004300140912     D Provincia       S              2A
004400140912     D Comune          S             20A
004500140912     D CAP             S              5A
004600140912     D Via             S             32A
004700140912     D OutLivNorm      S              1A
004800140912     D OutRegione      S             20A
004900140912     D OutProv         S              2A
005000140912     D OutCAP          S              5A
005100140912     D OutComune       S             20A
005200140912     D OutVia          S             30A
005300140912     D OutCivico       S              5A
005400140912     D Esito           S              5A
005500140912
005600140912     d Status         sds
005700140912     d  SDSpgm           *proc
005800140912     d  SDSprm           *parms
005900140912     d  SDSuser              254    263
006000140911
006100140911     D TITAS00F      e ds
006200140911     D TITASDS         ds
006300140912     D  SQL_ind                            like(tasind)
006400140911     D  SQL_lod                            like(taslod)
006500140911     D  SQL_cad                            like(tascad)
006600140911     D  SQL_prd                            like(tasprd)
006700140912     D  SQL_lna                            like(taslna)
006800140911
006900140912     C     *entry        plist
007000140912     C                   parm                    pIn_TpStp
007100140912
007200140911      /free
007300140912       data = %date();
007400140912       ora = %time();
007500140911
007600140911       wSQL =
007700140912             'select tasind, taslod, tascad, tasprd, taslna from titas00f +
007800140911              where tasnzd='' '' and tascad in +
007900140911              (select cpccap from azcpc00f +
008000140911              where cpcver=44 and cpcnar='' '' and cpcflg=''S'' +
008100140912              and cpcfit=''N'') +
008200140912              order by taslna, tasprd, taslod, tascad, tasind' ;
008300140911       exec sql  prepare S1  from :wSql;
008400140911       exec sql  declare C1  cursor for S1;
008500140911       exec sql  open C1;
008600140911
008700140911       WTS1 = %timestamp();
008800140912
008900140912       except testata;
009000140911
009100140911       DoU  SQLCode = 100 or SQLCode < 0;
009200140911         exec sql  fetch next  from C1  into :TITASds;
009300140911
009400140912         if *inOA = *on;
009500140912           except testata;
009600140912         endif;
009700140912
009800140911         select;
009900140911         // errore
010000140911         when SQLCode < 0;
010100140911           dsply 'ERRORE SQL';
010200140912           // - Chiusura cursore (visto che esco dal pgm)
010300140911           exec sql   close C1;
010400140911           *inlr = *on;
010500140911           return;
010600140911         // nessuna lettura
010700140911         when SQLCode = 100;
010800140912           // esco dal ciclo
010900140911         // lettura
011000140911         when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
011100140911           wRcdLetti += 1;
011200140911           exsr TL05R02;
011300140912           //chiamate corrette
011400140911           if pOutEsatto = '0' and pOutLivNorm = 'V' and
011500140911              pOutEsito <> 'E';
011600140911             wCallOK += 1;
011700140912             //stampo se richiesto
011800140912             if pIn_TpStp   = 'PRINTALL' or
011900140912                pIn_TpStp   = 'PRINTOK';
012000140912                LNA         = %editc(sql_LNA:'X');
012100140912                Provincia   = pInProvincia  ;
012200140912                Comune      = pInComune     ;
012300140912                CAP         = pInCAP        ;
012400140912                Via         = pInVia        ;
012500140912                OutLivNorm  = pOutLivNorm   ;
012600140912                OutRegione  = pOutRegione   ;
012700140912                OutProv     = pOutProv      ;
012800140912                OutCAP      = pOutCAP       ;
012900140912                OutComune   = pOutComune    ;
013000140912                OutVia      = pOutVia       ;
013100140912                OutCivico   = pOutCivico    ;
013200140912                Esito       = 'OK   '       ;
013300140912                except dettaglio;
013400140912             endif;
013500140912           //chiamate in errore
013600140912           else;
013700140912             wCallErr += 1;
013800140912             //stampo se richiesto
013900140912             if pIn_TpStp = 'PRINTALL' or
014000140912                pIn_TpStp = 'PRINTOK';
014100140912                LNA         = %editc(sql_LNA:'X');
014200140912                Provincia   = pInProvincia  ;
014300140912                Comune      = pInComune     ;
014400140912                CAP         = pInCAP        ;
014500140912                Via         = pInVia        ;
014600140912                OutLivNorm  = pOutLivNorm   ;
014700140912                OutRegione  = pOutRegione   ;
014800140912                OutProv     = pOutProv      ;
014900140912                OutCAP      = pOutCAP       ;
015000140912                OutComune   = pOutComune    ;
015100140912                OutVia      = pOutVia       ;
015200140912                OutCivico   = pOutCivico    ;
015300140912                Esito       = '   KO'       ;
015400140912                except dettaglio;
015500140912             endif;
015600140912           endif;
015700140911         // fine lettura
015800140911         other;
015900140911           // non faccio nulla
016000140911         endsl;
016100140911
016200140911       enddo;
016300140911
016400140911       //emetto risultati
016500140911       WTS2 = %timestamp();
016600140911       Msg = 'tot rcd letti ' + %char(wRcdLetti);
016700140911       Msg = %trim(Msg) + ' di cui normalizzato OK: ' +
016800140911                 %char(wCallOK);
016900140912       except totali;
017000140912       Msg = 'secondi impiegati: ' +
017100140912             %char(%diff(wTS2:wTS1:*S));
017200140912       except totali;
017300140911
017400140912       // - Chiusura cursore (visto che esco dal pgm)
017500140912       exec sql   close C1;
017600140912
017700140911      /END-free
017800140911     C                   seton                                        lr
017900140911
018000140911     C     TL05R02       BEGSR
018100140912
018200140911     C                   EVAL      pInNazione   = *blank
018300140911     C                   EVAL      pInProvincia = sql_prd
018400140911     C                   EVAL      pInComune    = sql_lod
018500140911     C                   EVAL      pInLocalita  = *blank
018600140911     C                   EVAL      pInCAP       = sql_cad
018700140911     C                   EVAL      pInVia       = sql_ind
018800140911     C                   EVAL      pInCivico    = *blanks
018900140911     C*
019000140911     C                   CALL      'UBTL05R02'
019100140911     C                   PARM                    pInNazione
019200140911     C                   PARM                    pInProvincia
019300140911     C                   PARM                    pInComune
019400140911     C                   PARM                    pInLocalita
019500140911     C                   PARM                    pInCAP
019600140911     C                   PARM                    pInVia
019700140911     C                   PARM                    pInCivico
019800140911     C                   PARM                    pOutEsatto
019900140911     C                   PARM                    pOutRank
020000140911     C                   PARM                    pOutLivNorm
020100140911     C                   PARM                    pOutScore
020200140911     C                   PARM                    pOutRegione
020300140911     C                   PARM                    pOutProv
020400140911     C                   PARM                    pOutCAP
020500140911     C                   PARM                    pOutComune
020600140911     C                   PARM                    pOutLocalit
020700140911     C                   PARM                    pOutVia
020800140911     C                   PARM                    pOutCivico
020900140911     C                   PARM                    pOutXY_X
021000140911     C                   PARM                    pOutXY_Y
021100140911     C                   PARM                    pOutEsito
021200140911     C                   PARM                    pOutEsitoDes
021300140911     C*
021400140911     C                   ENDSR
021500140912     OPRTF198   E            testata           2
021600140912     O                       SDSUSER
021700140912     O                                          101 '* Stress-test e misurazion'
021800140912     O                                          123 'e performance TELLUS *'
021900140912     O                       data               196
022000140912     O          E            testata     1
022100140912     O                       SDSPGM
022200140912     O                       ORA                196
022300140912     O          E            testata     1
022400140912     O                                              'Tipo stampa: '
022500140912     O                       pIn_TpStp
022600140912     O          E            testata     1
022700140912     O                                            3 'LNA'
022800140912     O                                            8 'Prv'
022900140912     O                                           17 'Località'
023000140912     O                                           34 'CAP'
023100140912     O                                           47 'Indirizzo'
023200140912     O                                           76 '- LN'
023300140912     O                                           84 'Regione'
023400140912     O                                          102 'Prv'
023500140912     O                                          106 'CAP'
023600140912     O                                          116 'Comune'
023700140912     O                                          141 'Indirizzo'
023800140912     O                                          170 'Civico'
023900140912     O                                          178 '- OK/KO'
024000140912     O          E            dettaglio   1
024100140912     O                       LNA                  3
024200140912     O                       Provincia            7
024300140912     O                       Comune              29
024400140912     O                       CAP                 36
024500140912     O                       Via                 70
024600140912     O                                           73 '|'
024700140912     O                       OutLivNorm          75
024800140912     O                       OutRegione          97
024900140912     O                       OutProv            101
025000140912     O                       OutCAP             108
025100140912     O                       OutComune          130
025200140912     O                       OutVia             162
025300140912     O                       OutCivico          169
025400140912     O                       Esito              178
025500140912     O          E            totali      1
025600140912     O                       msg                198
